use std::{collections::{HashMap, HashSet}, vec};
use crate::dbg_println;
use crate::parser::{Expr, type_expr, TypeExpr, ClassExpr, Sttm, PathExpr};
use crate::interpreter::{Context, Value, GLOBAL_NAMESPACE};
use crate::interpreter;

pub type Path = Vec<(String, Vec<Type>)>;

fn path_to_string(path: &Path) -> String {
	let mut s = String::new();
	for (i, (name, args)) in path.iter().enumerate() {
		if i > 0 {
			s.push_str("::");
		}
		s.push_str(name);
		if !args.is_empty() {
			s.push('<');
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					s.push_str(", ");
				}
				s.push_str(&arg.to_string());
			}
			s.push('>');
		}
	}
	s
}
 
#[derive(Clone, Debug, Eq, Hash)]
pub enum Type {
    Unit, 
	Named {
		path: Path,
        name: String
    },
	GenericNamed {
		path: Path,
		name: String,
		args: Vec<Type>
	},
	Variable {
		constraint: Option<Class>,
        id: i32
    },
	GenericVariable {
		constraint: Option<Class>,
		id: i32,
		args: Vec<Type>
	},
    Forall {
		constraint: Option<Class>,
        quant: String,
        t: Box<Type>
    },
    Polymorphic {
        name: String,
		// used by data types to remeber the index of the ploymorphic type in the generic arguments
		id: usize,
    },
	GenericPolymorphic {
		name: String,
		id: usize,
		args: Vec<Type>
	},
    Function {
        arg: Box<Type>,
        ret: Box<Type>
    },
	Tuple {
		els: Vec<Type>
	}
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
			(Self::Named { path: l_path, name: l_name }, 
				Self::Named { path: r_path, name: r_name }) 
				=> l_name == r_name && l_path == r_path,
			(Self::GenericNamed { path: l_path, name: l_name, args: l_args }, 
				Self::GenericNamed { path: r_path, name: r_name, args: r_args }) 
				=> l_name == r_name && l_args == r_args && l_path == r_path,
			(Self::GenericVariable { id: l_id, args: l_args, .. },
				Self::GenericVariable { id: r_id, args: r_args, .. }) => l_id == r_id && l_args == r_args,
            (Self::Variable { id: l_id, .. }, Self::Variable { id: r_id, .. }) => l_id == r_id,
            (Self::Forall { quant: l_quant, constraint: l_constraint, t: l_t }, 
				Self::Forall { quant: r_quant, constraint: r_constraint, t: r_t }) => l_quant == r_quant && l_t == r_t && l_constraint == r_constraint,
            (Self::Polymorphic { name: l_name, .. }, Self::Polymorphic { name: r_name, .. }) => l_name == r_name,
			(Self::GenericPolymorphic { name: l_name, args: l_args, .. },
				Self::GenericPolymorphic { name: r_name, args: r_args, .. }) => l_name == r_name && l_args == r_args,
			(Self::Function { arg: l_arg, ret: l_ret }, Self::Function { arg: r_arg, ret: r_ret }) => l_arg == r_arg && l_ret == r_ret,
            (Self::Tuple { els: l_els }, Self::Tuple { els: r_els }) => l_els == r_els,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Unit => write!(f, "()"),
			Type::Named { path, name } 
				=> write!(f, "{}{}", if path.len() > 0 { path_to_string(path) + "::" } else { "".to_string() }, name),
			Type::GenericNamed { path, name, args } => {
				let (disp_name, bracks) = match name.as_str() {
					LIST => ("", ["[", "]"]),
					_ => (name.as_str(), ["<", ">"])
				};

				if path.len() > 0 {
					write!(f, "{}::", path_to_string(path))?;
				}
				write!(f, "{}", disp_name)?;
				write!(f, "{}", bracks[0])?;

				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", arg)?;
				}

				write!(f, "{}", bracks[1])
			},
			Type::Variable { id, constraint } => {
				let cnst_str = match constraint {
					Some(c) => format!("{{{}}}", c),
					None => "".to_string()
				};
				write!(f, "V{}{}", id, cnst_str)
			},
			Type::GenericVariable { id, constraint, args } => {
				let cnst_str = match constraint {
					Some(c) => format!("{{{}}}", c),
					None => "".to_string()
				};
				write!(f, "V{}{}<", id, cnst_str)?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", arg)?;
				}
				write!(f, ">")
			},
			Type::Forall { quant, constraint, t } => {
				let cnst_str = match constraint {
					Some(c) => format!(" {}", c), // the space is important
					None => "".to_string()
				};
				write!(f, "forall{} {}: {}", cnst_str, quant, t)
			},
			Type::Polymorphic { name, .. } => write!(f, "*{}", name),
			Type::GenericPolymorphic { name, args, .. } => {
				write!(f, "*{}<", name)?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", arg)?;
				}
				write!(f, ">")
			},
			Type::Function { arg, ret } => {
				if matches!(**arg, Type::Function { .. }) {
					write!(f, "({}) -> {}", arg, ret)
				} else {
					write!(f, "{} -> {}", arg, ret)
				}
			},
			Type::Tuple { els } => {
				write!(f, "(")?;
				for (i, el) in els.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", el)?;
				}
				write!(f, ")")
			},
		}
	}
}

impl From<&str> for Type {
	fn from(s: &str) -> Self {
		type_expr_to_type(&type_expr(s).unwrap().0, &[]).unwrap()
	}
}

impl Type {
	pub fn new_basic_named(name: &str) -> Self {
		Self::Named { path: vec![], name: name.to_string() }
	}
}

#[derive(Clone, Debug, Eq, Hash)]
pub enum Class {
	Named {
		path: Path,
		name: String
	},
	Generic {
		path: Path,
		name: String,
		args: Vec<Type>
	},
	Forall {
		constraint: Option<Box<Class>>,
		quant: String,
		t: Box<Class>
	},
	Multi {
		classes: Vec<Class>
	}
}

impl PartialEq for Class {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Named { path: p1, name: l_name }, 
				Self::Named { path: p2, name: r_name }) 
				=> l_name == r_name && p1 == p2,
			(Self::Generic { path: p1, name: l_name, args: l_args }, 
				Self::Generic { path: p2, name: r_name, args: r_args }) 
				=> l_name == r_name && l_args == r_args && p1 == p2,
			(Self::Multi { classes: l_classes }, 
				Self::Multi { classes: r_classes }) => l_classes == r_classes,
			_ => core::mem::discriminant(self) == core::mem::discriminant(other),
		}
	}
}

impl std::fmt::Display for Class {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Class::Forall { quant, constraint, t } => {
				let cnst_str = match constraint {
					Some(c) => format!(" {}", c), // the space is important
					None => "".to_string()
				};
				write!(f, "forall{} {}: {}", cnst_str, quant, t)
			},
			Class::Named { path, name } 
			=> write!(f, "{}{}", if path.len() > 0 { path_to_string(path) + "::" } else { "".to_string() }, name),
			Class::Generic { path, name, args } => {
				let (disp_name, bracks) = (name.as_str(), ["<", ">"]);

				if path.len() > 0 {
					write!(f, "{}::", path_to_string(path))?;
				}
				write!(f, "{}", disp_name)?;
				write!(f, "{}", bracks[0])?;

				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", arg)?;
				}

				write!(f, "{}", bracks[1])
			},
			Class::Multi { classes } => {
				for (i, class) in classes.iter().enumerate() {
					if i > 0 {
						write!(f, " & ")?;
					}
					write!(f, "{}", class)?;
				}
				Ok(())
			}
		}
	}
}


/*
namespace NS1 {
	struct S<T> {
		T t;
	}
	class C<T> {
		func calc(self) -> T;
	}
}
namespace NS2 {
	impl<T> NS1::C<T> for NS1::S<T> {
		calc(self) -> T = self.t;
	}
	func f<T>(NS1::S<T> s) -> T = s.calc();
}
*/

#[derive(Clone)]
pub struct Namespace {
	// name of type -> type spec
    pub type_specs: HashMap<String, TypeSpec>,
	// name of class -> class spec
    pub class_specs: HashMap<String, ClassSpec>,
	// name of func -> vector of all funcs with that name -> (generic args for specific func, type of func, value of func, is method)
	pub funcs: HashMap<String, Vec<(Vec<Type>, Type, interpreter::Value, bool)>>,
	// i know this is a mess, but it's a good compromise between speed of access (hashmaps for names) and flexibility (vec for generics)
	pub impls: HashMap<(Path, String), // instance path, name
					Vec<(Vec<Type>, // instance generic arguments
						HashMap<(Path, String), // class path, name
							Vec<(Vec<Type>, // class generic arguments
								HashMap<String, (Type, interpreter::Value, bool)> // implemented function name -> (type, value, is method)
							)>
						>
					)>
				>,
	// name of namespace -> (namespace, generic args to add when accessing)
    pub namespaces: HashMap<String, (Namespace, Vec<Type>)>,
}

impl Namespace {
	pub fn new() -> Self {
		Self {
			type_specs: HashMap::new(),
			class_specs: HashMap::new(),
			funcs: HashMap::new(),
			impls: HashMap::new(),
			namespaces: HashMap::new(),
		}
	}

	pub fn resolve_type_spec(&self, path: &[(String, Vec<Type>)], name: &String) -> Option<&TypeSpec> {
		if !path.is_empty() {
			if let Some((ns, _new_gargs)) = self.namespaces.get(&path[0].0) {
				ns.resolve_type_spec(&path[1..], name)
			} else { None }
		} else {
			self.type_specs.get(name)
		}
	}

	pub fn resolve_class_spec(&self, path: &[(String, Vec<Type>)], name: &String) -> Option<&ClassSpec> {
		if !path.is_empty() {
			if let Some((ns, _new_gargs)) = self.namespaces.get(&path[0].0) {
				ns.resolve_class_spec(&path[1..], name)
			} else { None }
		} else {
			self.class_specs.get(name)
		}
	}

	pub fn resolve_func(&self, path: &[(String, Vec<Type>)], name: &String, gargs: &Vec<Type>, ctx: &Context) -> 
		Option<(&Type, &interpreter::Value, bool, &Vec<Type>)> {

		pub fn rs_func_acc_gargs<'a>(ns: &'a Namespace, path: &[(String, Vec<Type>)], name: &String, mut acc_gargs: Vec<Type>, outside_gargs: &Vec<Type>, ctx: &Context) 
		-> Option<(&'a Type, &'a interpreter::Value, bool, &'a Vec<Type>)> {
			if !path.is_empty() {
				if let Some((ns, new_gargs)) = ns.namespaces.get(&path[0].0) {
					if path[0].1.len() != 0 {
						if path[0].1.len() != new_gargs.len() { return None; }
						acc_gargs.extend(path[0].1.iter().map(|t| t.clone()));
					} else {
						acc_gargs.extend(new_gargs.iter().map(|t| t.clone()));
					}
					rs_func_acc_gargs(ns, &path[1..], name, acc_gargs, outside_gargs, ctx)
				} else { None }
			} else {
				acc_gargs.extend(outside_gargs.iter().map(|t| t.clone()));
				ns.funcs.get(name).and_then(|fs| {
					// skip the generic check if there is only one function
					if fs.len() == 1 {
						let (gs, t, v, m) = &fs[0];
						return Some((t, v, *m, gs));
					}
					fs.iter().find_map(|(gs, t, v, m)|
						if outside_gargs.is_empty() || (
							gs.len() == acc_gargs.len() && 
							gs.iter().zip(acc_gargs.iter()).all(|(h, w)| type_scope_matches(w, h, ctx)))
						{
							Some((t, v, *m, gs))
						} else { None }
					)
				})
			}
		}
		
		rs_func_acc_gargs(self, path, name, vec![], gargs, ctx)
	} 

	pub fn resolve_namespace(&self, path: &[(String, Vec<Type>)], name: &String) -> Option<&Namespace> {
		if !path.is_empty() {
			if let Some((ns, _new_gargs)) = self.namespaces.get(&path[0].0) {
				ns.resolve_namespace(&path[1..], name)
			} else { None }
		} else {
			self.namespaces.get(name).map(|t| &t.0)
		}
	}

	pub fn resolve_impl(&self, t: &Type, c: &Class, ctx: &Context) -> Option<(&HashMap<String, (Type, interpreter::Value, bool)>, HashMap<String, Type>)> {
		let empty_vec = vec![];
		let (t_path, t_name, t_gargs) = match t {
			Type::Named { path, name } => (path.clone(), name.clone(), &empty_vec),
			Type::GenericNamed { path, name, args } => (path.clone(), name.clone(), args),
			_ => return None
		};
		let (c_path, c_name, c_gargs) = match c {
			Class::Named { path, name } => (path.clone(), name.clone(), &empty_vec),
			Class::Generic { path, name, args } => (path.clone(), name.clone(), args),
			_ => return None
		};
		let c_path_name = (c_path, c_name);

		self.impls.get(&(t_path, t_name)).and_then(|v| 
			v.iter().filter_map(|(gargs, vv)| 
				if gargs.is_empty() || gargs.iter().zip(t_gargs)
					.all(|(a,b)|type_scope_matches(a,b, ctx)) 
				{
					let poly_map = gargs.iter().zip(t_gargs.iter()).filter_map(|(mut ga, inst)| {
						while let Type::Forall { t, .. } = ga { ga = t; }
						if let Type::Polymorphic { name, .. } = ga {
							Some((name.clone(), inst.clone()))
						} else { None }
					}).collect::<HashMap<String, Type>>();
					Some((vv.get(&c_path_name)?, poly_map)) 
				} else { None }
			).filter_map(move |(v,pm)| 
				v.iter().find_map(move |(gargs, vv)| 
						if gargs.is_empty() || gargs.iter().zip(c_gargs)
							.all(|(a,b)|type_scope_matches(a,b, ctx)) 
						{ Some(vv) } else { None }
				).and_then(|v| Some((v, pm)))
			).next()
		)
	}

}


#[derive(Clone)]
pub enum TypeSpecData {
	Primitive,
	Variant(Vec<(String, Type, interpreter::Value)>),
	Struct(Vec<(Type, String)>)
}

#[derive(Clone)]
pub struct TypeSpec {
	pub basis: Type,
	pub data: TypeSpecData,
}

#[derive(Clone)]
pub struct ClassSpec {
	pub basis: Class,
	// the String in the data is the name of the dependant polymorphic type; the bool is whether it's a method
	pub funcs: HashMap<String, (Type, Option<interpreter::Value>, String, bool)>
}


pub const INT : &'static str = "int";
pub const CHAR : &'static str = "char";
pub const BOOL : &'static str = "bool";

pub const LIST : &'static str = "List#";
pub const LIST_NIL : i32 = 0;
pub const LIST_CONS : i32 = 1;
// variant 0 is empty
// variant 1 is a (head, tail) pair

pub type Subst = HashMap<i32, Type>;
pub type InferRes = Result<(Type, Subst), String>;

pub fn is_string(t: &Type) -> bool {
	if let Type::GenericNamed { name, args, .. } = t {
		if name == LIST && args.len() == 1 {
			if let Type::Named { name, .. } = &args[0] {
				name == CHAR
			} else { false } 
		} else { false }
	} else { false }
}

pub fn type_scope_matches(want: &Type, have: &Type, ctx: &Context) -> bool {
	fn match_rec(want: &Type, have: &Type, ctx: &Context, poly_constraints: &mut HashMap<String, Option<Class>>) -> bool {
		match (want, have) {
			(Type::Forall { t, constraint, quant }, _) => {
				if let Some(c) = constraint {
					poly_constraints.insert(quant.clone(), Some(c.clone()));
				}
				match_rec(t, have, ctx, poly_constraints)
			}
			(_, Type::Forall { t, .. }) => match_rec(want, t, ctx, poly_constraints),
			(Type::Named { path: p1, name: n1 }, 
				Type::Named { path: p2, name: n2 }) 
				=> n1 == n2 && p1 == p2,
			(Type::Named { path: p1, name: n1 }, 
				Type::GenericNamed { path: p2, name: n2, .. }) => 
				n1 == n2 && p1 == p2,
			(Type::GenericNamed { path: p1, name: n1, args: a1 }, 
				Type::GenericNamed { path: p2, name: n2, args: a2 }) 
				=> {
					if n1 != n2 || a1.len() != a2.len() || p1 != p2 {
						return false;
					}
					for (t1, t2) in a1.iter().zip(a2.iter()) {
						if !match_rec(t1, t2, ctx, poly_constraints) {
							return false;
						}
					}
					true
			}
			(_, Type::Polymorphic { .. }) => true,
			(_, Type::Variable { constraint: None, .. }) => true,
			(Type::Polymorphic { name: n1, .. }, _) => {
					let constraint = if let Some(c) = poly_constraints.get(n1) { c } else { &None };
					if !ctx.fulfils_constraint(have, constraint.as_ref()) {
						return false;
					}
					true
				}
			_ => false
		}
	}
	match_rec(want, have, ctx, &mut HashMap::new())
}

#[allow(dead_code)]
fn class_scope_matches(want: &Type, have: &Class, ctx: &Context) -> bool {
	match (want, have) {
		(Type::Forall { t, .. }, _) => class_scope_matches(t, have, ctx),
		(_, Class::Forall { t, .. }) => class_scope_matches(want, t, ctx),
		(Type::Named { path: p1, name: n1 }, 
			Class::Named { path: p2, name: n2 }) => n1 == n2 && p1 == p2,
		(Type::Named { path: p1, name: n1 }, 
			Class::Generic { path: p2, name: n2, .. }) => n1 == n2 && p1 == p2,
		(Type::GenericNamed { path: p1, name: n1, args: a1 }, 
			Class::Generic { path: p2, name: n2, args: a2 }) 
			=> {
				if n1 != n2 || a1.len() != a2.len() || p1 != p2 {
					return false;
				}
				for (t1, t2) in a1.iter().zip(a2.iter()) {
					if !type_scope_matches(t1, t2, ctx) {
						return false;
					}
				}
				true
		}
		_ => false
	}
}

// ------------------

impl Context {
	fn add_type_var(&mut self, name: &str) -> Type {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;
        let t = Type::Variable { constraint: None, id };
        self.idents_stack.last_mut().unwrap().insert(name.to_string(), (interpreter::Value::Bottom, t.clone()));
        t
    }

	pub fn anon_type_id(&mut self) -> i32 {
		let ret = self.next_type_var_id;
		self.next_type_var_id += 1;
		ret
	}

    pub fn anon_type_var(&mut self) -> Type {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;
        Type::Variable { constraint: None, id }
    }

	pub fn apply_subst(&mut self, s: &Subst) {
		for (_, t) in self.idents_stack.last_mut().unwrap() {
			t.1 = subst(&t.1, s);
		}
	}

	pub fn get_variant(&self, exp_typ: Option<&Type>, path: &Path, vr_name: &str) -> Option<(&Type, &Value)> {
		if exp_typ.is_none() && path.is_empty() {
			return None;
		}
		let mut new_path = path.clone();
		let t_name = if new_path.is_empty() {
			match exp_typ.unwrap() {
				Type::Named { name, .. } => name.clone(),
				Type::GenericNamed { name, .. } => name.clone(),
				_ => return None
			}
		} else {
			new_path.pop().unwrap().0
		};
		if let Ok(TypeSpec { data: TypeSpecData::Variant(vrs), .. }) 
			= self.type_spec(&new_path, &t_name) {
			// if any of the variants have the name, return the type and value
			for (vr_name_, vr_typ, vr_val) in vrs {
				if vr_name_ == vr_name {
					return Some((vr_typ, vr_val));
				}
			}
			None
		}
		else { None }
	}

	pub fn add_type_spec(&mut self, name: String, gargs: Vec<Type>, spec: TypeSpec) {
		self.cur_namespace().type_specs.insert(name.clone(), spec);
		let had_before = self.cur_namespace().namespaces.insert(name.clone(), (Namespace::new(), gargs)).is_some();
		if had_before {
			panic!("Type or namespace '{}' already exists", name);
		}
	}

	pub fn type_spec(&self, path: &Path, name: &String) -> Result<&TypeSpec, String> {
		self.accessible_namespaces().iter().rev()
			.find_map(|ns| ns.resolve_type_spec(&path, name))
			.ok_or_else(|| format!("Algebraic type '{}' not found", name))
	}

	pub fn add_func_to_cur_namespace(&mut self, fname: &str, gargs: Vec<Type>, ftype: Type, fval: interpreter::Value, is_method: bool) {
		// self.cur_namespace().funcs.insert(fname.to_string(), (ftype, fval, is_method));
		let fs = self.cur_namespace().funcs.entry(fname.to_string()).or_insert_with(|| Vec::new());
		fs.push((gargs, ftype, fval, is_method));
	}

	pub fn add_class_spec(&mut self, name: String, gargs: Vec<Type>, spec: ClassSpec) {
		let cur_ns = self.cur_namespace();
		if cur_ns.namespaces.insert(name.clone(), (Namespace::new(), gargs)).is_some() {
			panic!("Class or namespace '{}' already exists", name);
		}
		let ns = &mut cur_ns.namespaces.get_mut(&name).unwrap().0;

		// get the basis but without foralls to use in function values
		let mut cls_basis = &spec.basis;
		while let Class::Forall { t, .. } = cls_basis {
			cls_basis = &**t;
		}

		// add all the spec funcs to the namespace
		for (fname, (ftype, fval, dep, is_method)) in &spec.funcs {
			if fname.starts_with("1") || fname.starts_with("2") { continue; }
			
			let v = if let Some(v) = fval {
				v.clone()
			} else {
				Value::ClassFuncN(cls_basis.clone(), fname.clone(), dep.clone())
			};
			// a little hack: we need the generic arguments in order to add them to the namespace
			// since they are not contained in spec, we can get them from the type
			// and since the in-class function is always as generic as it will get, we don't worry about missing anything
			let mut gpolys = vec![];
			let mut cur_t = ftype;
			while let Type::Forall { quant, t, .. } = cur_t {
				gpolys.push(quant);
				cur_t = &*t;
			}

			ns.funcs.insert(fname.clone(), vec![(
				gpolys.into_iter().map(|n| Type::Polymorphic { id: 0, name: n.clone() } ).collect(),
				ftype.clone(), v, is_method.clone()
			)]);
		}
		cur_ns.class_specs.insert(name.clone(), spec);
	}

	pub fn add_class_instance(&mut self, cls: Class, mut inst_typ: Type, inst_funcs: HashMap<String, (Type, interpreter::Value, bool)>) -> Result<(), String> {
		let mut inst_g_constraints: HashMap<String, Option<Class>> = HashMap::new();
		while let Type::Forall { quant, t, constraint } = inst_typ {
			inst_g_constraints.insert(quant, constraint);
			inst_typ = *t;
		}
		let (inst_pathname, mut inst_generics) = match inst_typ.clone() {
			Type::Named { path, name } => ((path, name), vec![]),
			Type::GenericNamed { path, name, args } => ((path, name), args), 
			_ => return Err(format!("Cannot add instance of type {inst_typ}"))
		};
		inst_generics.iter_mut().for_each(|t| 
			if let Type::Polymorphic { name, .. } = t {
				if let Some(cls) = inst_g_constraints.get(name) {
					*t = Type::Forall { constraint: cls.clone(), quant: name.clone(), t: Box::new(t.clone()) };
				}
			}
		);

		let (cls_pathname, cls_generics) = match cls.clone() {
			Class::Named { path, name } => ((path, name), vec![]),
			Class::Generic { path, name, args } => ((path, name), args), 
			_ => return Err(format!("Cannot add instance of class {cls}"))
		};

		// add functions to the instance namespace in the current namespace (yes that means that a single type may have multiple namespaces)
		// this is done so that implementations within a namespace don't pollute the namespaces of types external to them
		let inst_fs_gs = 
			inst_generics.clone().into_iter()
			.chain(cls_generics.clone().into_iter()).collect::<Vec<_>>();
		let inst_ns = 
			&mut self.cur_namespace().namespaces
				.entry(inst_pathname.1.clone())
				.or_insert_with(|| (Namespace::new(), 
					inst_generics.iter().map(|_| Type::Polymorphic { name: "T".to_string(), id: 0 }).collect::<Vec<_>>()
				)).0;
		for (fname, (ftype, fval, is_method)) in inst_funcs.clone() {
			if fname.starts_with("1") || fname.starts_with("2") { continue; }
			inst_ns.funcs.entry(fname.clone()).or_insert_with(|| Vec::new()).push((inst_fs_gs.clone(), ftype, fval, is_method));
		}
 
		// add the implementation to the namespace
		let inst_impls = self.cur_namespace().impls.entry(inst_pathname).or_insert(Vec::new());
		let inst_gen_impls = inst_impls.iter_mut().find(|(gen, _)| gen == &inst_generics);
		let inst_class_impls = if let Some(inst_gen_impls) = inst_gen_impls {
			&mut inst_gen_impls.1
		} else {
			inst_impls.push((inst_generics.clone(), HashMap::new()));
			&mut inst_impls.last_mut().unwrap().1
		};
		let inst_class_impl = inst_class_impls.entry(cls_pathname).or_insert(Vec::new());
		let has_impl = inst_class_impl.iter_mut().find(|(gen, _)| gen == &cls_generics).is_some();
		if has_impl {
			return Err(format!("Instance of class {cls} already has implementation for {inst_typ}"));
		}
		inst_class_impl.push((cls_generics, inst_funcs));

		Ok(())
    }

	pub fn add_tmp_class_instance(&mut self, cls: &Class, inst_typ: &Type) -> Result<(), String> {
		self.tmp_class_specs.push((cls.clone(), inst_typ.clone()));
		Ok(())
	}

	pub fn pop_tmp_class_instance(&mut self) {
		self.tmp_class_specs.pop();
	}

	pub fn accessible_namespaces(&self) -> Vec<&Namespace> {
        let mut iter = self.namespace_stack.iter();
        if iter.len() == 0 { panic!("No namespaces are accessible")}
        if iter.next().unwrap().0 != GLOBAL_NAMESPACE {
            panic!("The global namespace is not accessible");
        }
        let mut cur = &self.global_namespace;
        let mut namespaces = vec![cur];
        for (name, _) in iter {
			cur = if let Some((new_cur, _)) = cur.namespaces.get(name) {
				new_cur
			} else {
				panic!("Namespace '{}' is not accessible", name);
			};
			namespaces.push(cur);
        }
		namespaces
    }

	pub fn cur_namespace(&mut self) -> &mut Namespace {
		let mut iter = self.namespace_stack.iter();
		if iter.len() == 0 { panic!("No namespaces are accessible")}
		if iter.next().unwrap().0 != GLOBAL_NAMESPACE  {
			panic!("The global namespace is not accessible");
		}
		let mut cur = &mut self.global_namespace;
		for (name, _) in iter {
			cur = if let Some((new_cur, _)) = cur.namespaces.get_mut(name) {
				new_cur
			} else {
				panic!("Namespace '{}' is not accessible", name);
			};
		}
		cur
	}
	
	pub fn get_class_funcs(&self, cls: &Class, _inst: &Type, _cs: &[Option<Class>], _qs: &[&str]) -> 
		Result<HashMap<String, (Type, Option<interpreter::Value>, bool)>, String> {
		let (cls_path, cls_name) = match cls {
			Class::Named { path, name } => (path,name),
			Class::Generic { path, name, .. } => (path, name),
			_ => return Err("Cannot get class functions for a multi class".to_string())
		};
		let spec =  {
			let mut r = None;
			for ns in self.accessible_namespaces().iter().rev() {
				if let Some(t) = ns.resolve_class_spec(cls_path, cls_name) {
					r = Some(t);
				}
			}
			if let Some(r) = r {
				r
			} else {
				return Err(format!("Class '{}' not found", cls_name));
			}
		};

		let poly_map = class_poly_map(cls, &spec.basis);

		// since the class is not generic, we can use the functions from the spec as-is
		let funcs: HashMap<String, (Type, Option<Value>, bool)> = 
			spec.funcs.iter().map(|(k,(t,v,_,m))| 
			(k.clone(), (subst_polymorphic(t, &poly_map), v.clone(), m.clone()))
		).collect();

		Ok(funcs)
	}

	pub fn fulfils_constraint(&self, t: &Type, c: Option<&Class>) -> bool {
		let c = if let Some(c) = c { c } else {
			return true;
		};
		if let Type::Variable { constraint, .. } = t {
			if constraint_is_super(Some(c), constraint.as_ref()) {
				return true;
			}
		}

		if let Class::Multi { classes } = c {
			return classes.iter().map(|c| self.fulfils_constraint(t, Some(c))).all(|b| b);
		}

		// first check every temp class instance
		for (tmp_cls, tmp_inst) in self.tmp_class_specs.iter().rev() {
			if tmp_cls == c {
				if let Ok(_) = unify(t, tmp_inst, self) {
					return true;
				}
			}
		}

		if self.is_instance(t, c) {
			return true;
		}

		false
	}

	pub fn is_instance(&self, t: &Type, c: &Class) -> bool {
		for ns in self.accessible_namespaces().iter().rev() {
			if let Some(..) = ns.resolve_impl(t, c, self) {
				return true;
			}
		}
		false
	}
}

// ------------------
pub fn path_expr_to_path(path_expr: &PathExpr) -> Result<Path, String> {
	let mut path = Path::new();
	for (name, g_args) in path_expr.iter() {
		path.push((
			name.to_string(), 
			g_args.iter()
			.map(|t| type_expr_to_type(t, &[]))
			.collect::<Result<_, String>>()?
		));
	}
	Ok(path)
}

fn class_poly_map(cls: &Class, basis: &Class) -> HashMap<String, Type> {
	match (cls, basis) {
		(_, Class::Forall { t, .. }) => class_poly_map(cls, t),
		(Class::Generic { args: cls_args, .. }, Class::Generic { args: basis_args, .. }) => {
			let mut r = HashMap::new();
			for (k, v) in cls_args.iter().zip(basis_args.iter()) {
				match (k, v) {
					(t, Type::Polymorphic { name, .. }) => {
						r.insert(name.clone(), t.clone());
					}
					_ => {}
				}
			}
			r
		}
		_ => HashMap::new(),
	}
}

fn combine_classes(a: &Option<Class>, b: &Option<Class>) -> Option<Class> {
	if a.is_none() {
		return b.clone();
	}
	if b.is_none() {
		return a.clone();
	}
	if a == b {
		return a.clone();
	}
	if let Class::Multi { classes: a_classes } = a.as_ref().unwrap() {
		if let Class::Multi { classes: b_classes } = b.as_ref().unwrap() {
			let mut a_classes = a_classes.clone();
			a_classes.extend(b_classes.iter().cloned());
			return Some(Class::Multi { classes: a_classes });
		} else {
			let mut a_classes = a_classes.clone();
			a_classes.push(b.as_ref().unwrap().clone());
			return Some(Class::Multi { classes: a_classes });
		}
	} else {
		if let Class::Multi { classes: b_classes } = b.as_ref().unwrap() {
			let mut b_classes = b_classes.clone();
			b_classes.push(a.as_ref().unwrap().clone());
			return Some(Class::Multi { classes: b_classes });
		} else {
			return Some(Class::Multi { classes: vec![a.as_ref().unwrap().clone(), b.as_ref().unwrap().clone()] });
		}
	}
}

fn constraint_is_super(have: Option<&Class>, need: Option<&Class>) -> bool {
	let need = if let Some(need) = need {
		need
	} else {
		return true;
	};
	let have = if let Some(have) = have {
		have
	} else {
		return false;
	};
	match need {
		Class::Multi { classes } => 
			classes.iter().all(|c| constraint_is_super(Some(have), Some(c))),
		_ => match have {
			Class::Multi { classes } => 
				classes.iter().any(|c| constraint_is_super(Some(c), Some(need))),
			_ => have == need
		}
	}
}

type FreeVars = HashMap<i32, Option<Class>>;
fn free_vars(t: &Type) -> FreeVars {
	match t {
		Type::Variable { constraint, id } => {
			let mut res = FreeVars::new();
			res.insert(*id, constraint.clone());
			res
		}
		Type::Forall { t, .. } => free_vars(t),
		Type::Function { arg, ret } => {
			let mut res = free_vars(arg);
			res.extend(free_vars(ret));
			res
		},
		Type::Tuple { els } => {
			let mut res = FreeVars::new();
			for el in els {
				res.extend(free_vars(el));
			}
			res
		},
		Type::GenericNamed { args, .. } | 
		Type::GenericVariable { args, .. } | 
		Type::GenericPolymorphic { args, .. } => {
			let mut res = FreeVars::new();
			for arg in args {
				res.extend(free_vars(arg));
			}
			res
		},
		_ => FreeVars::new()
	}
}

fn pull_foralls(t: &Type) -> Type {
	match t {
		Type::Function { arg, ret } => {
			// we can't pull a forall out of the argument
			let mut pulled = pull_foralls(ret);
			let mut quants = Vec::new();
			loop {
				match pulled {
					Type::Forall { constraint, quant, t } => {
						quants.push((constraint, quant));
						pulled = *t;
					}
					_ => break
				}
			}
			let mut res = Type::Function {
				arg: arg.clone(),
				ret: Box::new(pulled.clone())
			};
			for (constraint, quant) in quants.into_iter().rev() {
				res = Type::Forall {
					constraint: constraint,
					quant: quant,
					t: Box::new(res)
				};
			}

			res
		},
		// Type::Tuple { els } => {
		// 	let mut res_els = Vec::new();
		// 	for el in els {
		// 		res_els.push(pull_foralls(el));
		// 	}
		// },
		Type::Forall { constraint, quant, t } => {
			Type::Forall {
				constraint: constraint.clone(),
				quant: quant.to_string(),
				t: Box::new(pull_foralls(t))
			}
		},
		_ => t.clone()
	}
}

pub fn canonize(mut t: Type) -> Type {
	// forall b: forall a: b -> a    =>   forall a: forall b: a -> b
	// forall Show b: forall a: b -> a     =>     forall Show a: forall b: a -> b 
	if !matches!(t, Type::Forall { .. }) {
		return t;
	}

	// first get out all forall quantifiers and constraints
	let mut quants = Vec::new();
	let mut cur = &mut t;
	{
		loop {
			match cur {
				Type::Forall { constraint, quant, t } => {
					quants.push((constraint.clone(), quant.clone()));
					cur = t;
				}
				_ => break
			}
		}
	}

	// sort them
	quants.sort_by(|(_, q1), (_, q2)| q1.cmp(q2));

	// now we need to sort the polymorphic types
	// (so the first one that appears will be the first letter, the second - the second, ect...)
	fn all_polymorphic(t: &mut Type) -> HashMap<String, (usize, Vec<&mut Type>)> {
		match t {
			Type::Polymorphic { name, .. } => {
				let mut res = HashMap::new();
				res.insert(name.clone(), (0, vec![t]));
				res
			},
			Type::Function { arg, ret } => {
				let mut arg_res = all_polymorphic(arg);
				let ret_res = all_polymorphic(ret);
				let mut new = Vec::new();
				for (k, v) in ret_res {
					if let Some(arg_val) = arg_res.get_mut(&k) {
						arg_val.1.extend(v.1);
					} else {
						new.push((v.0, k.clone(), v.1));
					}
				}
				// sort the new ones by order of appearance
				new.sort_by_key(|el| el.0);
				// and make their indices sequential, starting from 0
				for (i, e) in new.iter_mut().enumerate() {
					e.0 = i;
				}
				// now add them to the result
				let arg_res_len = arg_res.len();
				for (i, k, v) in new {
					arg_res.insert(k, (arg_res_len + i, v));
				}

				arg_res
			},
			Type::Tuple { els } | 
			Type::GenericNamed { args: els, .. } |
			Type::GenericVariable { args: els, .. } |
			Type::GenericPolymorphic { args: els, .. } => {
				let mut res: HashMap<String, (usize, Vec<&mut Type>)> = HashMap::new();
				for el in els {
					let el_res = all_polymorphic(el);
					let mut new = Vec::new();
					for (k, v) in el_res {
						if let Some(val) = res.get_mut(&k) {
							val.1.extend(v.1);
						} else {
							new.push((v.0, k.clone(), v.1));
						}
					}
					// sort the new ones by order of appearance
					new.sort_by_key(|el| el.0);
					// and make their indices sequential, starting from 0
					for (i, e) in new.iter_mut().enumerate() {
						e.0 = i;
					}
					// now add them to the result
					let res_len = res.len();
					for (i, k, v) in new {
						res.insert(k, (res_len + i, v));
					}
				}
				res
			},
			_ => HashMap::new()
		}
	}

	let poly = all_polymorphic(cur);

	let mut new_constraints = HashMap::new();
	for (name, (i, types)) in poly {
		let (_, new_name) = &quants[i];
		let cnst = &quants.iter().find(|(_, n)| n == &name).unwrap().0;
		new_constraints.insert(new_name.clone(), cnst.clone());
		if &name == new_name { continue; }
		for t in types {
			if let Type::Polymorphic { name: old_name, .. } = t {
				*old_name = new_name.clone();
			}
		}
	}

	// now put the quants back in
	{		
		let mut cur = &mut t;
		for (_, q) in &quants {
			match cur {
				Type::Forall { constraint, quant, t } => {
					*constraint = new_constraints.get(q).unwrap().clone();
					*quant = q.clone();
					cur = t;
				}
				_ => break
			}
		}
	}

	t
}

pub fn generalise(t: &Type, names: Option<&HashMap<i32, String>>) -> (Type, Subst) {
	let fvs = free_vars(t);
	if fvs.is_empty() {
		return (t.clone(), Subst::new())
	}

	// chose first free letter as quantifier
	// todo: since we're using single letters, we can only have 26 of them
	// it should be made so if we run out of letters, we start using two-letter quantifiers
	let mut letter = 'a';
	if names.is_none() {
		let mut cur = t;
		loop {
			match cur {
				Type::Forall { t, .. } => {
					letter = char::from_u32(letter as u32 + 1).unwrap();
					cur = t;
				}
				_ => break
			};
		};
	}

	let mut sub = Subst::new();
	let mut cur = t.clone();
	for (fv, fv_cnst) in fvs {
		if let Some(names) = names {
			let name = names.get(&fv);
			if let Some(name) = name {
				let replace = Type::Polymorphic { name: name.clone(), id: 0 };
				sub.insert(fv, replace);

				cur = Type::Forall { 
					constraint: fv_cnst,
					quant: name.clone(), 
					t: Box::new(cur)
				};

				continue;
			}
		}
		let replace = Type::Polymorphic { name: letter.to_string(), id: 0 };
		sub.insert(fv, replace);

		cur = Type::Forall { 
			constraint: fv_cnst,
			quant: letter.to_string(), 
			t: Box::new(cur)
		};
		letter = char::from_u32(letter as u32 + 1).unwrap();
	}
	return (subst(&cur, &sub), sub);
}

pub fn instantiate(t: Type, ctx: &mut Context) -> (Type, HashMap<String, i32>) {
	// since instantiate and instantiate_spec have a lot in common, we'll just call the latter
	let (sp, tvs) = 
		instantiate_spec(TypeSpec{ basis: t, data: TypeSpecData::Primitive }, ctx);
	(sp.basis, tvs)
}

pub fn instantiate_spec(spec: TypeSpec, ctx: &mut Context) -> (TypeSpec, HashMap<String, i32>) {
	fn repl_polymorphic_class(c: Class, name: &str, new_t: &Type) -> Class
	{
		match c {
			Class::Generic { path, name: cname, args } => {
				Class::Generic { path, name: cname, args: args.into_iter().map(|t| repl_polymorphic(t, name, new_t)).collect() }
			}
			_ => c,
		}
	}

	fn repl_polymorphic(t: Type, name: &str, new_t: &Type) -> Type
	{
		match t {
			Type::Polymorphic { name: n, .. } if n == name => new_t.clone(),
			Type::Forall { constraint, quant, t } => if quant == name {
				repl_polymorphic(*t, name, new_t)
			} else { Type::Forall {
				constraint: constraint.map(|c| repl_polymorphic_class(c, name, new_t)),
				quant: quant,
				t: Box::new(repl_polymorphic(*t, name, new_t))
			} },
			Type::Function { arg, ret } => Type::Function {
				arg: Box::new(repl_polymorphic(*arg, name, new_t)),
				ret: Box::new(repl_polymorphic(*ret, name, new_t))
			},
			Type::Tuple { els } => Type::Tuple {
				els: els.into_iter().map(|el| repl_polymorphic(el, name, new_t)).collect()
			},
			Type::GenericNamed { path, name: g_name, args } => Type::GenericNamed {
				path: path,
				name: g_name.clone(),
				args: args.into_iter().map(|arg| repl_polymorphic(arg, name, new_t)).collect()
			},
			Type::GenericVariable { args, constraint, id } => Type::GenericVariable {
				args: args.into_iter().map(|arg| repl_polymorphic(arg, name, new_t)).collect(),
				constraint: constraint.map(|c| repl_polymorphic_class(c, name, new_t)),
				id: id
			},
			Type::GenericPolymorphic { args, name: n, .. } if n == name => 
				if let Type::Variable { constraint, id } = new_t {
					Type::GenericVariable {
						args: args.into_iter().map(|arg| repl_polymorphic(arg, name, new_t)).collect(),
						constraint: constraint.clone(),
						id: *id
					}
				} else { unreachable!() },
			Type::GenericPolymorphic { args, name: n, id } => Type::GenericPolymorphic {
				args: args.into_iter().map(|arg| repl_polymorphic(arg, name, new_t)).collect(),
				name: n,
				id: id
			},
			a => return a.clone()
		}
	}

	fn repl_specdata(data: TypeSpecData, name: &str, new_t: &Type) -> TypeSpecData 
	{
		match data {
			TypeSpecData::Primitive => data,
			TypeSpecData::Variant(_) => data,
			TypeSpecData::Struct(fields) => 
				TypeSpecData::Struct(fields.into_iter().map(|(t, fname)| {
					(repl_polymorphic(t, name, new_t), fname)
				}).collect()),
		}
	}

	let mut tvs = HashMap::new();
	let mut spec = spec;
	loop {
		match spec.basis {
			Type::Forall { constraint, quant, t } => {
				let mut vr = ctx.anon_type_var();
				if let Type::Variable { constraint: v_cnst, id: vrid } = &mut vr {
					*v_cnst = constraint;
					tvs.insert(quant.clone(), *vrid);
				}
				spec.basis = repl_polymorphic(*t, quant.as_str(), &vr);
				spec.data = repl_specdata(spec.data, quant.as_str(), &vr);
			}
			_ => return (spec, tvs)
		}
	}
}

pub fn subst(t: &Type, s: &Subst) -> Type {
    if s.len() == 0 {
        return t.clone()
    }

    match t {
        Type::Variable { id, constraint } => {
            if let Some(new_t) = s.get(id) {
                return new_t.clone()
            }
            Type::Variable { 
				id: *id, 
				constraint: constraint.as_ref()
					.map(|c| match c { 
						Class::Generic { path, name, args } =>  
							Class::Generic { path: path.clone(), name: name.clone(), args: args.iter().map(|t| subst(t, s)).collect() },
						_ => c.clone()
					}) 
			}
        },
        Type::Forall { constraint, quant, t } => {
            Type::Forall {
                quant: quant.clone(),
                t: Box::new(subst(&*t, s)),
				constraint: constraint.as_ref()
					.map(|c| match c { 
						Class::Generic { path ,name, args } =>  
							Class::Generic { path: path.clone(), name: name.clone(), args: args.iter().map(|t| subst(t, s)).collect() },
						_ => c.clone()
					}) 
            }
        },
        Type::Function { arg, ret } => {
            Type::Function {
                arg: Box::new(subst(&*arg, s)),
                ret: Box::new(subst(&*ret, s))
            }
        },
		Type::Tuple { els } => {
			Type::Tuple {
				els: els.iter().map(|t| subst(t, s)).collect()
			}
		},
		Type::GenericNamed { path, name, args } =>  
			Type::GenericNamed {
				path: path.clone(),
				name: name.clone(),
				args: args.iter().map(|t| subst(t, s)).collect()
			},
		Type::GenericVariable { args, constraint, id } => 
			Type::GenericVariable {
				args: args.iter().map(|t| subst(t, s)).collect(),
				constraint: constraint.as_ref()
					.map(|c| match c {
						Class::Generic { path, name, args } =>
							Class::Generic { path: path.clone(), name: name.clone(), args: args.iter().map(|t| subst(t, s)).collect() },
						_ => c.clone()
					}),
				id: *id
			},
		Type::GenericPolymorphic { args, id, name } =>
			Type::GenericPolymorphic {
				args: args.iter().map(|t| subst(t, s)).collect(),
				id: *id,
				name: name.clone()
			},
        _ => t.clone()
    }
}

pub fn subst_polymorphic(t: &Type, poly_map: &HashMap<String, Type>) -> Type {
	if poly_map.len() == 0 {
		return t.clone()
	}

	match t {
		Type::Polymorphic { name, id } => {
			if let Some(new_t) = poly_map.get(name) {
				return new_t.clone()
			}
			Type::Polymorphic { name: name.clone(), id: *id }
		},
		Type::Forall { constraint, quant, t: inner } => {
			if poly_map.contains_key(quant) {
				return subst_polymorphic(inner, poly_map);
			}
			Type::Forall {
				constraint: constraint.as_ref().map(|c| match c {
					Class::Generic { path, name, args } => 
					Class::Generic { 
						path: path.clone(), 
						name: name.clone(), 
						args: args.iter().map(|t| subst_polymorphic(t, poly_map)).collect() 
					},
					_ => c.clone()
				}),
				quant: quant.clone(),
				t: Box::new(subst_polymorphic(inner, poly_map))
			}
		},
		Type::Function { arg, ret } => {
			Type::Function {
				arg: Box::new(subst_polymorphic(&*arg, poly_map)),
				ret: Box::new(subst_polymorphic(&*ret, poly_map))
			}
		},
		Type::Tuple { els } => {
			Type::Tuple {
				els: els.iter().map(|t| subst_polymorphic(t, poly_map)).collect()
			}
		},
		Type::GenericNamed { path, name, args } => {
			Type::GenericNamed {
				path: path.clone(),
				name: name.clone(),
				args: args.iter().map(|t| subst_polymorphic(t, poly_map)).collect()
			}
		},
		Type::GenericVariable { args, constraint, id } => {
			Type::GenericVariable {
				args: args.iter().map(|t| subst_polymorphic(t, poly_map)).collect(),
				constraint: constraint.as_ref().map(|c| match c {
					Class::Generic { path, name, args } => 
					Class::Generic { 
						path: path.clone(), 
						name: name.clone(), 
						args: args.iter().map(|t| subst_polymorphic(t, poly_map)).collect() 
					},
					_ => c.clone()
				}),
				id: *id
			}
		},
		_ => t.clone()
	}
}

// first apply s2 and then s1
pub fn compose_subst(s1: &Subst, s2: &Subst) -> Subst {
	let mut res = s1.clone();
	for (k, v) in s2 {
		res.insert(*k, subst(v, &s1));
	}
	res
}

// pub fn compose_subst_with_tvars(s1: Subst, tvs: &HashMap<String, i32>) -> Subst {
// 	s1.into_iter().map(|(k,v)| {
// 		if let Type::Polymorphic { name, id: _ } = &v {
// 			if let Some(new_id) = tvs.get(name) {
// 				return (k, Type::Variable { constraint: None, id: *new_id })
// 			}
// 		}
// 		(k, v)
// 	}).collect()
// }

pub fn unify(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, String> {
	fn bind(t: &Type, id: i32, c: &Option<Class>) -> Result<Subst, String> {
		fn contains(t: &Type, id: i32) -> bool {
			match t {
				Type::Variable { id: i, .. } if *i == id => true,
				Type::Forall { t, .. } => contains(t, id),
				Type::Function { arg, ret } => 
					contains(arg, id) || contains(ret, id),
				_ => false
			}
		}

		let (other_id, other_c) = if let Type::Variable { id: i, constraint } = t {
			if *i == id {
				return Ok(Subst::new())
			} else { (*i, constraint.clone()) }
		} else { (-1, None) };

		let mut res = Subst::new();

		let t = if other_id >= 0 {
			let new_t = Type::Variable{ id: other_id, constraint: combine_classes(&other_c, c) };
			// we also need to replace old typevars with this id with the new one, so add it to the subst
			res.insert(other_id, new_t.clone());
			new_t
		} else {
			t.clone()
		};

		if contains(&t, id) {
			return Err(format!("type {} contains a reference to itself (var{})", t, id))
		}
		res.insert(id, t);
		Ok(res)
	}

	// println!("--- unifying {} and {}", t1, t2);
	match (t1, t2) {
		(Type::Unit, Type::Unit) => Ok(Subst::new()),
		(Type::Named {path: p1, name: name1}, 
			Type::Named{ path: p2, name: name2}) 
			=> {
			if name1 == name2 && p1 == p2 {
				return Ok(Subst::new())
			}
			Err(format!("Cannot unify {} and {}", name1, name2))
		},
		(Type::Variable { id, constraint }, b) => {
			if !ctx.fulfils_constraint(b, constraint.as_ref()) {
				return Err(format!("Type '{}' does not fulfil constraint '{}'", b, 
					constraint.as_ref().map_or_else(|| "??".to_string(), |c| c.to_string())))
			}
			bind(b, *id, constraint)
		},
		(a, Type::Variable { id, constraint }) => {
			if !ctx.fulfils_constraint(a, constraint.as_ref()) {
				return Err(format!("Type '{}' does not fulfil constraint '{}'", a, 
					constraint.as_ref().map_or_else(|| "??".to_string(), |c| c.to_string())))
			}
			bind(a, *id, constraint)
		},
		(Type::Function { arg: arg1, ret: ret1 }, 
			Type::Function { arg: arg2, ret: ret2 }) => {
			let s1 = unify(arg1, arg2, ctx)?;
			let sret1 = subst(ret1, &s1);
			let sret2 = subst(ret2, &s1);
			let s2 = unify(&sret1, &sret2, ctx)?;
			Ok(compose_subst(&s2, &s1))
		},
		(Type::Tuple { els: els1 }, Type::Tuple { els: els2 }) => {
			if els1.len() != els2.len() {
				return Err(format!("Cannot unify {} and {}", t1, t2))
			}
			let mut res = Subst::new();
			for (t1, t2) in els1.iter().zip(els2.iter()) {
				let s = unify(t1, t2, ctx)?;
				res = compose_subst(&res, &s);
			}
			Ok(res)
		},
		(Type::Forall { t: t1, constraint: cnst1, .. }, 
			Type::Forall { t: t2, constraint: cnst2, .. }) => {
			if cnst1 != cnst2 {
				let cnst1_str = if cnst1.is_none() { "None".to_string() } else { format!("{}", cnst1.as_ref().unwrap()) };
				let cnst2_str = if cnst2.is_none() { "None".to_string() } else { format!("{}", cnst2.as_ref().unwrap()) };
				return Err(format!("Cannot unify foralls with constraints {} and {}", cnst1_str, cnst2_str))
			}
			unify(t1, t2, ctx)
		},
		(Type::GenericNamed { path: p1, name: name1, args: args1 }, 
			Type::GenericNamed { path: p2, name: name2, args: args2 }) => {
			if name1 != name2 || args1.len() != args2.len() || p1 != p2 {
				return Err(format!("Cannot unify {} and {}", t1, t2))
			}
			let mut res = Subst::new();
			for (a1, a2) in args1.iter().zip(args2.iter()) {
				let s = unify(a1, a2, ctx)?;
				res = compose_subst(&s,&res);
			}
			Ok(res)
		},
		(Type::GenericVariable { args: _args1, constraint: cnst1, id: id1 }, b) => {
			// if !ctx.fulfils_constraint(b, cnst1.as_ref()) {
			// 	return Err(format!("Type '{}' does not fulfil constraint '{}'", b, 
			// 		cnst1.as_ref().map_or_else(|| "??".to_string(), |c| c.to_string())))
			// }
			bind(b, *id1, cnst1)
		},


	
		// TODO: GenericVariable and GenericPolymorphic
		// (Type::GenericVariable { .. } | Type::GenericPolymorphic { .. }, _) => unimplemented!(),
		// (_, Type::GenericVariable { .. } | Type::GenericPolymorphic { .. }) => unimplemented!(),
		(a, b) => {
			Err(format!("Cannot unify {} with {}", a, b))
		}
	}
}

pub fn unify_polymorphic(t1: &Type, t2: &Type, ctx: &Context) -> Result<HashMap<String, Type>, String> {
	match (t1, t2) {
		(Type::Forall { t: t1, .. }, Type::Forall { t: t2, .. }) => {
			unify_polymorphic(t1, t2, ctx)
		},
		(Type::Forall { t: t1, .. }, t2) => {
			unify_polymorphic(t1, t2, ctx)
		},
		(t1, Type::Forall { t: t2, .. }) => {
			unify_polymorphic(t1, t2, ctx)
		},
		(Type::Unit, Type::Unit) => Ok(HashMap::new()),
		(Type::Named { path: p1, name: name1 }, 
			Type::Named { path: p2, name: name2 }) => {
			if name1 == name2 && p1 == p2 {
				return Ok(HashMap::new())
			}
			Err(format!("Cannot unify {} and {}", name1, name2))
		},
		(Type::GenericNamed { path: p1, name: name1, args: args1 }, 
			Type::GenericNamed { path: p2, name: name2, args: args2 }) => {
			if name1 != name2 || args1.len() != args2.len() || p1 != p2 {
				return Err(format!("Cannot unify {} and {}", t1, t2))
			}
			let mut res = HashMap::new();
			for (a1, a2) in args1.iter().zip(args2.iter()) {
				let s = unify_polymorphic(a1, a2, ctx)?;
				for (k, v) in s {
					res.insert(k, v);
				}
			}
			Ok(res)
		},
		(Type::Function { arg: arg1, ret: ret1 }, 
			Type::Function { arg: arg2, ret: ret2 }) => {
			let mut s1 = unify_polymorphic(arg1, arg2, ctx)?;
			let sret1 = subst_polymorphic(ret1, &s1);
			let sret2 = subst_polymorphic(ret2, &s1);
			let s2 = unify_polymorphic(&sret1, &sret2, ctx)?;
			for (k, v) in s2 {
				s1.insert(k, v);
			}
			Ok(s1)
		},
		(Type::Tuple { els: els1 }, Type::Tuple { els: els2 }) if els1.len() == els2.len() => {
			let mut res = HashMap::new();
			for (t1, t2) in els1.iter().zip(els2.iter()) {
				let s = unify_polymorphic(t1, t2, ctx)?;
				for (k, v) in s {
					res.insert(k, v);
				}
			}
			Ok(res)
		},
		(Type::Polymorphic { name, .. }, t) => {
			let mut res = HashMap::new();
			res.insert(name.clone(), t.clone());
			Ok(res)
		},
		(t, Type::Polymorphic { name, .. }) => {
			let mut res = HashMap::new();
			res.insert(name.clone(), t.clone());
			Ok(res)
		},
		(Type::Variable { .. }, _) => Ok(HashMap::new()),
		(_, Type::Variable { .. }) =>  Ok(HashMap::new()),

		_ => return Err(format!("Cannot unify {} and {}", t1, t2))
	}
}

pub fn make_lambda(param: &Type, mut body: Type, mut sub: Subst) -> InferRes {
	body = pull_foralls(&body);
	let param_t = subst(&param, &sub);

	let id = match param {
		Type::Variable { id, .. } => id,
		_ => return Err("... idk how this would happen".to_string())
	};
	sub.remove(&id);

	// let (new_body_type, gen_sub) = 
		// generalise(&pull_foralls(&Type::Function {
			// arg: Box::new(param_t),
			// ret: Box::new(body)
		// }));

	// body = pull_foralls(&new_body_type);
	// sub = compose_subst(&gen_sub, &sub);
	// Ok((body, sub))
	Ok((Type::Function {
		arg: Box::new(param_t),
		ret: Box::new(body)
	}, sub))
}

pub fn next_param(f: &Type) -> Option<Type> {
	match f {
		Type::Function { arg, ret: _ } => Some((**arg).clone()),
		_ => None
	}
}

pub fn call_type(mut f: Type, arg: &Type, mut sub: Subst, ctx: &mut Context) -> InferRes {
	dbg_println!(2, "call_type: f: {}, arg: {}", f, arg);

	let ret_var = ctx.anon_type_var();
	f = subst(&f, &sub);
	(f, _) = instantiate(f, ctx);
	
	if !matches!(f, Type::Function { .. } | Type::Variable { .. }) {
		return Err("Function call with non-function".to_string())
	}
	
	// if something like { let f = x->y->x; = f(f); } happens
	// and a function is able to be called with with itself,
	// it would detect the type as containing itself
	// so as a hack-fix, we change all free variables in arg
	// with newly generated ones 
	// (before instantiating, so we don't make new variables unnecessarily)
	// let mut f = {
	// 	let mut new_sub = Subst::new();
	// 	for id in free_vars(&f) {
	// 		new_sub.insert(id, ctx.anon_type_var());
	// 	}
	// 	subst(&f, &new_sub)
	// };

	let (arg, _) = instantiate(arg.clone(), ctx);

	let func_un = unify(&f, &Type::Function {
		arg: Box::new(arg.clone()),
		ret: Box::new(ret_var.clone())
	}, ctx)?;

	sub = compose_subst(&func_un, &sub);
	f = subst(&f, &sub);

	if let Type::Function { arg: farg, ret } = &f {
		let arg_un = 
			unify(&subst(farg, &sub), &subst(&arg, &sub), ctx)?;
			
		sub = compose_subst(&arg_un, &sub);

		// let (gen_ret_type, gen_ret_subst) = 
			// generalise(&subst(ret, &sub));
		let ret_type = subst(ret, &sub);
		
		f = ret_type;
		// sub = compose_subst(&gen_ret_subst, &sub);
		ctx.apply_subst(&sub);

		return Ok((f, sub));
		// if gen & matches!(f, Type::Function { .. }) {
		// 	let (gen_type, gen_subst) = generalise(&f);
		// 	sub = compose_subst(&gen_subst, &sub);
		// 	return Ok((gen_type, sub));
		// } else {
		// 	return Ok((f, sub));
		// }
	} else {
		return Err("???".to_string())
	}
}

pub fn if_type(cond: (Type, Subst), tb: (Type, Subst), fb: Option<(Type, Subst)>, ctx: &Context) -> InferRes {
	// check the condition's type unifies with a bool
	let cu = unify(&&Type::Named { path: vec![], name: format!("{BOOL}") }, &cond.0, ctx)?;

	// compose substitution 
	let mut sub = compose_subst(&cu, &cond.1);
	sub = compose_subst(&tb.1, &sub);
	// ctx.apply_subst(&sub);

	let ft = if let Some((ft, ft_sub)) = fb {
		sub = compose_subst(&ft_sub, &sub);
		ft
	} else { Type::Unit };
	// ctx.apply_subst(&sub);

	// unify the types of the true and false branches
	let tb = subst(&tb.0, &sub);
	let fb = subst(&ft, &sub);

	let uns = unify(&tb, &fb, ctx)?;
	sub = compose_subst(&uns, &sub);
	Ok((subst(&tb, &sub), sub))
}

pub fn infer(e: &Expr, ctx: &mut Context) -> InferRes {
	fn infer_expr(e: &Expr, extyp: Option<&Type>, ctx: &mut Context) -> InferRes {
		match e {
			Expr::Literal { kind, val } => {
				interpreter::literal(*kind, val).map(|(_, t)| (t, Subst::new()))
			},
			Expr::Identifier { path, name, generic_args } => {
				let path = path_expr_to_path(path)?;
				let g_args = 
					generic_args.iter()
					.map(|t| type_expr_to_type(t, &[]))
					.collect::<Result<Vec<_>, _>>()?;
				let (_, t) =  ctx.ident(&path, name, &g_args, extyp)?;
				let (new_t, _) = instantiate(t.clone(), ctx);
				Ok((new_t, HashMap::new()))
			},
			Expr::Access { expr, generic_args, member } => {
				use interpreter::MemberGet;

				let (t, s) = infer_expr(expr, None, ctx)?;
				let g_args = 
					generic_args.iter()
					.map(|t| type_expr_to_type(t, &[]))
					.collect::<Result<Vec<_>, _>>()?;
	
				let (mt, mg) = ctx.member_of(&t, &member, &g_args)?;
				if let MemberGet::Field(_) = mg {
					return Ok((mt, s));
				} else 
				if let MemberGet::Method(_mth) = mg {
					return call_type(mt, &t, s, ctx);
				} else {
					Err("???".to_string())
				}
			},
			Expr::Unary { op, expr } => {
				let op = *op;
				let e = &**expr;

				let (et, es) = infer_expr(e, None, ctx)?;

				let f = if op.starts_with('`') {
					Some(ctx.ident(&vec![], &op[1..], &vec![], None).map(|(_v, t)| t)?)
				} else {
					ctx.un_op(op, &et).map(|(t, _v)| t.clone())
				}.ok_or(format!("Undefined unary operator '{}'", op))?.clone();


				call_type(f, &et, es, ctx)
			},
			Expr::Binary { .. } => {
				fn handle_binary<'a>(e: &'a Expr, extyp: Option<&Type>, ctx: &mut Context, precedence: i32) -> 
					Result<(Type, Subst, Option<(Option<Type>, &'a str, i32, &'a Expr<'a>)>), String> 
				{
					if let Expr::Binary { left, op, right } = e {
						let left = &**left;
						let op = *op;
						let right = &**right;

						
						// get the type of the operator and its precedence
						let (prec, opt) = if op.starts_with('`') {
							let (_iv, it) = ctx.ident(&vec![], &op[1..], &vec![], None)?;
							Some((interpreter::BinOpPrec{ precedence: 0, right_assoc: true }, Some(it)))
						} else {
							ctx.bin_op_prec(op).map(|x| (x, None))
						}.ok_or(format!("Undefined binary operator '{}'", op))?;
						
						// get the type of the left expression
						let (lt, ls) = infer_expr(left, 
							opt.as_ref().and_then(|t| next_param(t)).as_ref(),
							ctx)?;

						// let ot = instantiate(&op.t, ctx);
						if prec.precedence > precedence || (prec.precedence == precedence && prec.right_assoc)
						{
							let mut lt = lt;
							let mut sub = ls;
							let mut opt = opt;
							let mut rest = right;
							let mut prec = prec.precedence;
							let mut oper_name = op;

							loop {
								let op_rest = if let Some(ot) = opt {
									let (new_lt, mut new_sub) = call_type(ot, &lt, sub, ctx)?;

									let (rt, rs, op_rest) = 
										handle_binary(rest, next_param(&new_lt).as_ref(), ctx, prec)?;
									new_sub = compose_subst(&rs, &new_sub);

									let (new_lt, new_sub) = call_type(new_lt, &rt, new_sub, ctx)?;
									lt = new_lt;
									sub = new_sub;

									op_rest
								} else {
									let (rt, rs, op_rest) = 
										handle_binary(rest, None, ctx, prec)?;
									let ot = if let Some((_, t, _)) = ctx.bin_op(oper_name, &lt, &rt) { t } else {
										return Err(format!("Undefined binary operator '{}' for types {} and {}", oper_name, lt, rt));
									};

									let (new_lt, mut new_sub) = call_type(ot, &lt, sub, ctx)?;
									new_sub = compose_subst(&rs, &new_sub);

									let (new_lt, new_sub) = call_type(new_lt, &rt, new_sub, ctx)?;
									lt = new_lt;
									sub = new_sub;

									op_rest
								};

								if let Some((new_t, new_oper_name, new_prec, new_rest)) = op_rest {
									opt = new_t;
									oper_name = new_oper_name;
									prec = new_prec;
									rest = new_rest;
								} else {
									break;
								}
							}
							
							return Ok((lt, sub, None));
							// if matches!(lt, Type::Function { .. }) {
							// 	let (gen_type, gen_subst) = generalise(&lt);
							// 	sub = compose_subst(&gen_subst, &sub);
							// 	return Ok((gen_type, sub, None));
							// } else {
							// }
						} else {
							return Ok((lt, ls, Some((opt, op, prec.precedence, right))));
						}
					}
					else {
						let ir = infer_expr(e, extyp, ctx)?;
						return Ok((ir.0, ir.1, None));
					}
				}

				let (t,s, ..) = handle_binary(e, extyp, ctx, -1)?;
				Ok((t, s))
			},
			Expr::Conditional { cond, true_branch, false_branch } => {
				let cond = &**cond;
				let true_brach = &**true_branch;
				let false_branch = false_branch.as_ref().map(|x| &**x);

				// get the type of the condition
				let cond = 
					infer_expr(cond, Some(&Type::Named { path: vec![], name: BOOL.to_string() }), ctx)?;

				// get the type of the true branch
				let tb = 
					infer_expr(true_brach, extyp, ctx)?;
				
				// get the type of the false branch
				let fb = if let Some(false_branch) = false_branch {
					Some(infer_expr(false_branch, Some(&tb.0), ctx)?)
				} else { None };

				if_type(cond, tb, fb, &ctx)
			},
			Expr::Lambda { params, body } => {
				let params = &**params;
				let body = &**body;
				
				// I couldn't make it iterate over just the param if it wasn't a tuple
				// so this vector is  the best I could do
				let mut tmp_vec: Vec<Expr> = vec![];
				let param_iter = match params {
					Expr::Tuple { els } => els.iter(),
					a => {tmp_vec.push(a.clone()); tmp_vec.iter()}
				};
				

				let mut ctx = ctx.child();
				
				// create type vars for the parameters
				let mut param_types: Vec<Type> = vec![];
				for e in param_iter {
					let name = match e {
						Expr::Identifier { path, name, generic_args } 
							if path.len() == 0 && generic_args.len() == 0 => name,
						_ => return Err("Lambda parameters must be identifiers".to_string())
					};
					let t = ctx.add_type_var(name);
					param_types.push(t);
				}

				let (mut body_type, mut body_subst) = infer_expr(&body, None, &mut ctx)?;

				for t in param_types.iter().rev() {
					// let t = subst(t, &body_subst);
					let (new_body, new_sub) = make_lambda(t, body_type, body_subst)?;
					body_type = new_body;
					body_subst = new_sub;
				}
				
				Ok((body_type, body_subst))
			},
			Expr::Call { func, args } => {
				let func = &**func;
				let args = &**args;

				// I couldn't make it iterate over just the arg if it wasn't a tuple
				// so this vector is the best I could do
				let mut tmp_vec: Vec<Expr> = vec![];
				let args_iter = match args {
					Expr::Tuple { els } => els.iter(),
					a => {tmp_vec.push(a.clone()); tmp_vec.iter()}
				};

				let (mut func_type, mut func_subst) = infer_expr(&func, extyp, ctx)?;
				ctx.apply_subst(&func_subst);
				
				for e in args_iter {
					let (arg_type, arg_subst) = infer_expr(&e, next_param(&func_type).as_ref(), ctx)?;
					func_subst = compose_subst(&arg_subst, &func_subst);
					
					let (new_type, new_sub) = 
						call_type(func_type, &arg_type, func_subst, ctx)?;
					func_type = new_type;
					func_subst = compose_subst(&new_sub, &new_sub);
					ctx.apply_subst(&func_subst);
				}
				
				// if matches!(func_type, Type::Function { .. }) {
				// 	let (gen_type, gen_subst) = generalise(&func_type);
				// 	func_subst = compose_subst(&gen_subst, &func_subst);
				// 	Ok((gen_type, func_subst))
				// } else {
				// 	Ok((func_type, func_subst))
				// }
				Ok((subst(&func_type, &func_subst), func_subst))
			},
			Expr::Block { sttms } => {
				let mut ctx = ctx.child();
				let mut sub = Subst::new();
				for sttm in sttms {
					match sttm {
						Sttm::Empty => (),
						// Sttm::TypeDecl { type_expr } => {
                        //     interpreter::decl_type(&type_expr, &mut ctx, true)?;
                        // },
						// Sttm::Expr { expr } => {
						// 	// even though we don't care about the expression,
						// 	// since there aren't any side-effects (yet),
						// 	// we still have to check for type errors
						// 	let (_, s) = infer_expr(expr, None, &mut ctx)?;
						// 	sub = compose_subst(&s, &sub);
						// },
						// Sttm::Decl { t: _, left, right } => {
						// 	let right = if right.is_some() { right.as_ref().unwrap() } else { 
						// 		// for now:
						// 		return Err("Declaration must have a right hand side".to_string());
						// 	};
						// 	let name = match &**left {
						// 		Expr::Identifier { path, name } if path.len() == 0 => *name,
						// 		// for now:
						// 		_ => return Err("Declaration left hand side must be an identifier".to_string())
						// 	};
						// 	let rec_typ = ctx.anon_type_var();
						// 	ctx.add_ident(name, None, Some(rec_typ.clone()));
						// 	let (mut it, is) = infer_expr(right, None, &mut ctx)?;
						// 	let un = unify(&it, &rec_typ, &ctx)?;
						// 	let is = compose_subst(&is, &un);
						// 	// if let Some(t) = t {
						// 	// 	// check that t unifies with what we inferred
						// 	// }
						// 	ctx.apply_subst(&is);
						// 	sub = compose_subst(&is, &sub);
						// 	if matches!(it, Type::Function { .. }) {
						// 		let (gen_type, gen_subst) 
						// 			= generalise(&subst(&rec_typ, &sub));
						// 		sub = compose_subst(&gen_subst, &sub);
						// 		it = gen_type;
						// 	} else {
						// 		it = subst(&rec_typ, &sub);
						// 	}
							
						// 	// sub = compose_subst(&is, &sub);
						// 	ctx.add_ident(name, None, Some(it));
						// },
						// _ => return Err("Statement type not supported".to_string())
						Sttm::BlockReturn { expr } => {
							let (t, s) = infer_expr(expr, extyp, &mut ctx)?;
							ctx.apply_subst(&s);
							sub = compose_subst(&s, &sub);
							return Ok((t, sub));
						},
						x => {
							let s = interpreter::do_sttm(x, &mut ctx, None);
							if s.is_none() { return Err("Statement type not supported".to_string()); }
							let s = s.unwrap()?;
							sub = compose_subst(&s, &sub);
						}
					}
				}
				Ok((Type::Unit, sub))
			},
			Expr::Match { expr, cases } => {
                let (mut et, mut es) = infer_expr(expr, None, ctx)?;
                ctx.apply_subst(&es);

                let mut res_typ = extyp.cloned().unwrap_or_else(|| ctx.anon_type_var());
                for (pate, re) in cases {
                    let mut ctx = ctx.child();
                    let (_, _, ps) = interpreter::pattern(pate, &et, &mut ctx, true)?;
                    es = compose_subst(&ps, &es);
                    ctx.apply_subst(&es);

                    let (rt, rs) = infer_expr(re, Some(&res_typ), &mut ctx)?;
                    es = compose_subst(&rs, &es);
                    ctx.apply_subst(&es);
					let (rt, _) = instantiate(subst(&rt, &es), &mut ctx);

                    let un = unify(&res_typ, &rt, &ctx)?;
                    es = compose_subst(&un, &es);
                    res_typ = instantiate(subst(&res_typ, &es), &mut ctx).0;
                    et = subst(&et, &es);
                }
                ctx.apply_subst(&es);

                Ok((subst(&res_typ, &es), es))
            },
			Expr::List { els, tail } => {
				let mut sub = Subst::new();
				let mut typ = ctx.anon_type_var();
				// for every element check it's type and unify them all
				for el in els {
					let (it, s) = infer_expr(el, Some(&typ), ctx)?;
					sub = compose_subst(&s, &sub);
					let un = unify(&typ, &it, &ctx)?;
					sub = compose_subst(&un, &sub);
					typ = subst(&typ, &sub);
					ctx.apply_subst(&sub);
				}

				// now unify the tail with List#<typ>
				if let Some(t) = tail {
					let list_of_typ = Type::GenericNamed { path: vec![], name: LIST.to_string(), args: vec![typ] };

					let (it, s) = infer_expr(t, Some(&list_of_typ), ctx)?;
					sub = compose_subst(&s, &sub);

					let un = unify(&list_of_typ, &it, &ctx)?;
					sub = compose_subst(&un, &sub);
					typ = subst(&list_of_typ, &sub);
					ctx.apply_subst(&sub);
				} else {
					typ = Type::GenericNamed { path: vec![], name: LIST.to_string() , args: vec![typ] };
				}
				Ok((typ, sub))
			},
			Expr::Struct { struct_type, fields } => {
				let (st_path, st_name) = match struct_type {
					TypeExpr::Named { path, name }
						=> (path, name),
					TypeExpr::Generic { path, name, .. }
						=> (path, name),
					_ => return Err("Anonymous struct types not supported yet".to_string())
				};
				let st_path = path_expr_to_path(&st_path)?;
				let (spec, _) = instantiate_spec(ctx.type_spec(&st_path, &st_name.to_string())?.clone(), ctx);
	
				let mut sub = Subst::new();
				if let TypeSpecData::Struct(needed) = spec.data {
					let mut seen = HashSet::new();
					let mut needed = needed.iter().zip(0..).map(|((t,n), i)| (n.as_str(), (t,i))).collect::<HashMap<_, _>>();
					for (f_name, vl) in fields {
						if !needed.contains_key(f_name) {
							if seen.contains(f_name) {
								return Err(format!("Duplicate field name: '{}'", f_name));
							} else {
								return Err(format!("Struct '{}' does not have field '{}'", spec.basis, f_name));
							}
						}
						let (exp_f_type, _) = needed.get(f_name).unwrap();
						let exp_f_type = subst(&exp_f_type, &sub);
						let (it, s) = infer_expr(vl, Some(&exp_f_type), ctx)?;
						sub = compose_subst(&s, &sub);
						let un = unify(&exp_f_type, &it, &ctx)?;
						sub = compose_subst(&un, &sub);
						needed.remove(f_name);
						seen.insert(f_name);
					}
	
					if !needed.is_empty() {
						return Err(format!("Struct '{}' is missing field(s): {}", &spec.basis, needed.keys().map(|x| format!("'{}'", x)).collect::<Vec<_>>().join(", ")));
					}
	
					Ok((subst(&spec.basis, &sub), sub))
				} else {
					return Err(format!("Type '{}' is not a struct", &spec.basis));
				}
			},
			Expr::Tuple { els } if els.len() == 0 => {
				Ok((Type::Unit, Subst::new()))
			},
			Expr::Tuple { els } if els.len() == 1 => {
				infer_expr(&els[0], extyp, ctx)
			},
			Expr::Tuple { els } => {
				let mut sub = Subst::new();
				let mut types: Vec<Type> = vec![];
				
				if let Some(Type::Tuple { els: expels }) = extyp {
					for (e, exp) in els.iter().zip(expels.iter()) {
						let (t, s) = infer_expr(e, Some(exp), ctx)?;
						sub = compose_subst(&s, &sub);
						types.push(t);
					}
				} else {
					for e in els {
						let (t, s) = infer_expr(e, None, ctx)?;
						sub = compose_subst(&s, &sub);
						types.push(t);
					}
				}

				Ok((Type::Tuple { els: types }, sub))
			},
			Expr::CompilerPrimitive { .. } => {
				// let t = if let Some(t) = extyp { t.clone() } else {
				// 	ctx.anon_type_var()
				// };
            	let t = ctx.anon_type_var();

	
				Ok((t, Subst::new()))
			}
			// _ => return Err("Infer not implemented for expression".to_string())
		}
	}

	// this is a hack since things in a block can affect the context
	let mut ctx = ctx.clone();

	let (t, s) = infer_expr(e, None, &mut ctx)?;
	let (gen_t, gen_s) = generalise(&t, None);
	compose_subst(&gen_s, &s);
	Ok((canonize(gen_t), gen_s))
}

pub fn type_expr_to_type(te: &TypeExpr, polys: &[&str]) -> Result<Type, String> {	
	fn with_poly<'a>(te: &TypeExpr<'a>, ps: &Vec<&'a str>) -> Result<Type, String> {
		match te {
			TypeExpr::Forall { constraint, quant, t } => {
				let mut ps = ps.clone();
				ps.push(quant);

				let cnst = if let Some(ce) = constraint {
					Some(class_expr_to_class(ce, &ps)?)
				} else { None };

				Ok( Type::Forall{ 
					constraint: cnst, 
					quant: quant.to_string(), 
					t: Box::new(with_poly(t, &ps)?)
				})
			},
			TypeExpr::Named { path, name } => {
				let pos = ps.iter().position(|n| n == name);
				if let Some(pos) = pos {
					Ok(Type::Polymorphic { name: name.to_string(), id: pos })
				} else {
					Ok(Type::Named { path: path_expr_to_path(path)?, name: name.to_string() })
				}
			},
			TypeExpr::Tuple { els } => {
				if els.len() == 0 {
					Ok(Type::Unit)
				} else if els.len() == 1 {
					with_poly(&els[0], ps)
				} else {
					let mut types: Vec<Type> = vec![];
					for e in els {
						types.push(with_poly(e, ps)?);
					}
					Ok(Type::Tuple { els: types })
				}
			},
			TypeExpr::Function { arg, ret } => {
				Ok(Type::Function { arg: Box::new(with_poly(arg, ps)?), ret: Box::new(with_poly(ret, ps)?) })
			},
			TypeExpr::Generic { path, name, args } => {
				let mut types: Vec<Type> = vec![];
				for a in args {
					types.push(with_poly(a, ps)?);
				}
				// Ok(Type::GenericNamed { path: path_expr_to_path(path)?, name: name.to_string(), args: types })

				let pos = ps.iter().position(|n| n == name);
				if let Some(pos) = pos {
					Ok(Type::GenericPolymorphic { name: name.to_string(), id: pos, args: types })
				} else {
					Ok(Type::GenericNamed { path: path_expr_to_path(path)?, name: name.to_string(), args: types })
				}

			},
			_ => Err("Type expression not supported".to_string())
		}
	}
	
	with_poly(te, &polys.to_vec())
}

pub fn class_expr_to_class(ce: &ClassExpr, polys: &[&str]) -> Result<Class, String> {
	match ce  {
		ClassExpr::Named { path, name } 
			=> Ok(Class::Named { path: path_expr_to_path(path)?, name: name.to_string() }),
		ClassExpr::Generic { path, name, args } => {
			let args = args.iter().map(|x| type_expr_to_type(x, polys)).collect::<Result<Vec<_>, _>>()?;
			Ok(Class::Generic { path: path_expr_to_path(path)?, name: name.to_string(), args })
		},
		ClassExpr::Multi { classes } => 
			Ok(Class::Multi { classes: classes.iter().map(|x| class_expr_to_class(x, polys)).collect::<Result<Vec<_>, _>>()? }),
	}
}

// ------------------------------

#[cfg(test)]
mod tests {
	use crate::{parser, interpreter};
	use super::{Context, Type, infer, INT, BOOL};

	fn check_infer(s: &str, expected: &str) -> Result<(), String> {
		check_infer_with_setup(&[], s, expected)
	}

	fn check_infer_with_setup(setup: &[&str], s: &str, expected: &str) -> Result<(), String> {
		let mut ctx = Context::new();
		if setup.len() > 0 {
			interpreter::execute(setup.join("\n").as_str(), &mut ctx)?;
		}
		let parse_res = parser::expr(s)?;
		let infer_res = infer(&parse_res.0, &mut ctx)?;
		// let actual = format!("{}", canonize(infer_res.0));
		// assert_eq!(actual, expected);
		let t1 = infer_res.0;
		let t2 = Type::from(expected);
		if t1 == t2 {
			Ok(())
		} else {
			Err(format!("\nExpected {} but got {}\n", t2, t1))
		}
	}

	#[test] fn infer_int() { check_infer("5", INT).unwrap() }
	#[test] fn infer_idl() { check_infer("x->x", "forall a: a -> a").unwrap() }
	#[test] fn infer_l1() { check_infer("x->y->x", "forall a: forall b: a -> b -> a").unwrap() }
	#[test] fn infer_l2() { check_infer("(x,y)->x", "forall a: forall b: a -> b -> a").unwrap() }
	#[test] fn infer_l3() { check_infer("x->y->y", "forall a: forall b: a -> b -> b").unwrap() }
	#[test] fn infer_ll() { check_infer("(x->x)(x->x)", "forall a: a -> a").unwrap() }
	#[test] fn infer_lc1() { check_infer("x->y->x(y)", "forall a: forall b: (a -> b) -> a -> b").unwrap() }

	#[test] fn infer_lc2() { check_infer(
		"(x,y,z)->x(y,z)", 
		"forall a: forall b: forall c: (a -> b -> c) -> a -> b -> c"
	).unwrap() }
	#[test] fn infer_lc3() { check_infer(
		"(x,y,z)->x(y(z))", 
		"forall a: forall b: forall c: (a -> b) -> (c -> a) -> c -> b"
	).unwrap() }
	#[test] fn infer_lc4() { check_infer(
		"(x,y,z)->x(y(z),z)", 
		"forall a: forall b: forall c: (a -> b -> c) -> (b -> a) -> b -> c"
	).unwrap() }
	#[test] fn infer_lc5() { check_infer(
		"x->y->y(x(z->z))", 
		"forall a: forall b: forall c: ((a -> a) -> b) -> (b -> c) -> c"
	).unwrap() }
	#[test] fn infer_un1() { check_infer(
		"x->-x", 
		format!("{INT} -> {INT}").as_str()
	).unwrap() }
	#[test] fn infer_un2() { check_infer(
		"x->y->-x", 
		format!("forall a: {INT} -> a -> {INT}").as_str()
	).unwrap() }
	#[test] fn infer_un3() { check_infer(
		"x->y->x(-y)", 
		format!("forall a: ({INT} -> a) -> {INT} -> a").as_str()
	).unwrap() }

	#[test] fn infer_bin1() { check_infer(
		"5+5", 
		format!("{INT}").as_str()
	).unwrap() }
	#[test] fn infer_bin2() { check_infer(
		"5==5", 
		format!("{BOOL}").as_str()
	).unwrap() }
	#[test] fn infer_bin3() { check_infer(
		"5+5==5-5", 
		format!("{BOOL}").as_str()
	).unwrap() }
	
	#[test] fn infer_if1() { check_infer(
		"(x,y)-> if x==y+5: x else 5", 
		format!("{INT} -> {INT} -> {INT}").as_str()
	).unwrap() }

	#[test] fn infer_block1() { check_infer("{}", "()").unwrap() }
	#[test] fn infer_block2() { check_infer("{={={};};}", "()").unwrap() }
	#[test] fn infer_block3() { check_infer(
		"{ let f = x->y->x; = f(f); }", 
		format!("forall a: forall b: forall c: a -> b -> c -> b").as_str()
	).unwrap() }
	#[test] fn infer_block4() { check_infer( "{ = x->x; }(5)", INT).unwrap() }
	#[test] fn infer_block5() { check_infer( "{ = 5 + 5; } + 5", INT).unwrap() }
	#[test] fn infer_block6() { check_infer( "if 5==5 { =3; } else { =9; }", INT).unwrap() }

	#[test] fn infer_rec1() { check_infer(
		"{let fact = n-> if n==0: 1 else n*fact(n-1); =fact;}", 
		format!("{INT} -> {INT}").as_str()
	).unwrap() }

	#[test] fn infer_tuple1() { check_infer(
		"x->(x,x)", 
		format!("forall a: a -> (a, a)").as_str()
	).unwrap() }
	#[test] fn infer_tuple2() { check_infer(
		"(x->(x,x))(5)", 
		format!("({INT}, {INT})").as_str()
	).unwrap() }
	#[test] fn infer_tuple3() { check_infer(
		"x->(x, y->x)", 
		format!("forall a: forall b: a -> (a, b -> a)").as_str()
	).unwrap() }
	#[test] fn infer_tuple4() { check_infer(
		"x->(y->x, y->x)", 
		format!("forall a: forall b: forall c: a -> (b -> a, c -> a)").as_str()
	).unwrap() }
	#[test] fn infer_tuple5() { check_infer(
		"{let ls = (l,r)->(l,r); =ls(0,ls(1,ls(2,3)));}", 
		format!("({INT}, ({INT}, ({INT}, {INT})))").as_str()
	).unwrap() }
	#[test] fn infer_match1() { check_infer(
		"match 5 { 5 => x->x, _ => x->x+1 }", 
		format!("{INT} -> {INT}").as_str()
	).unwrap() }
	#[test] fn infer_match2() { check_infer(
		"match (2,7) { (5,3) => 321, (5,_) => 123, (_,3) => 111, (x,y) => x+y }", 
		format!("{INT}").as_str()
	).unwrap() }
	#[test] fn infer_match3() { check_infer(
		"(x-> match x { 3 => y->y, _ => y->x })(2)", 
		format!("{INT} -> {INT}").as_str()
	).unwrap() }
	#[test] fn infer_match4() { check_infer(
		"x-> match x { 5 => (1, y->y), _ => (x, y->y+1) }", 
		format!("{INT} -> ({INT}, {INT} -> {INT})").as_str()
	).unwrap() }

	const VLISTS: &str = "
	variant List {
		Cons(int, List);
		Nil;
	}
	let head = l-> match l {
		List::Cons(x, _) => x
	};
	let tail = l-> match l {
		List::Cons(_, x) => x
	};
	let lget = (l,i)-> 
		if i == 0: match l { List::Cons(x, _) => x }
		else match l { List::Cons(_, x) => lget(x, i-1) };
	let map = (f,l)-> match l { 
		List::Nil => List::Nil, 
		List::Cons(x,xs) => List::Cons(f(x), map(f, xs)) 
	};
	let iota = List::Cons(0, map(x->x+1, iota));
	";
	#[test] fn infer_vlist_setup() { check_infer_with_setup(
		&[VLISTS],
		"(List::Cons, List::Nil, iota, map)", 
		"(int -> List -> List, List, List, (int -> int) -> List -> List)"
	).unwrap() }
	#[test] fn infer_vlist1() { check_infer_with_setup(
		&[VLISTS],
		"head(Cons(5, Nil))", 
		INT
	).unwrap() }
	#[test] fn infer_vlist2() { check_infer_with_setup(
		&[VLISTS],
		"tail(Cons(5, Nil))", 
		"List"
	).unwrap() }
	#[test] fn infer_vlist3() { check_infer_with_setup(
		&[VLISTS],
		"lget(Cons(5, Nil), 0)", 
		INT
	).unwrap() }
	#[test] fn infer_vlist4() { check_infer_with_setup(
		&[VLISTS],
		"lget(map(x->x*2, Cons(0, Cons(1, Cons(2, Cons(3, Nil)))) ), 3)", 
		INT
	).unwrap() }

	const LIST_HELPERS: &str = "
	let lget = (l,i)-> 
		if i == 0: match l { [x:_] => x }
		else match l { [_:xs] => lget(xs, i-1) };
	let head = l-> match l {
		[x:_] => x
	};
	let tail = l-> match l {
		[_:xs] => xs
	};
	let map = (f,l)-> match l { 
		[] => [], 
		[x:xs] => [f(x) : map(f, xs)]
	};
	let iota = [0 : map(x->x+1, iota)];
	"; // head is [a] -> b but should be [a] -> a
	#[test] fn infer_list_setup() { check_infer_with_setup(
		&[LIST_HELPERS],
		"(head, tail, map, iota)", 
		"forall a: forall b: forall c: forall d: ([a] -> a, [b] -> [b], (c -> d) -> [c] -> [d], [int])"
	).unwrap() }

	#[test] fn infer_list1() { check_infer_with_setup(
		&[LIST_HELPERS],
		"[]", 
		"forall a: [a]"
	).unwrap() }
	#[test] fn infer_list2() { check_infer_with_setup(
		&[LIST_HELPERS],
		"[1,2,3]", 
		format!("[{INT}]").as_str()
	).unwrap() }
	#[test] fn infer_list3() { check_infer_with_setup(
		&[LIST_HELPERS],
		"head([1,2,3])", 
		INT
	).unwrap() }
	#[test] fn infer_list4() { check_infer_with_setup(
		&[LIST_HELPERS],
		"[1,2,3:[4,5,6]]", 
		format!("[{INT}]").as_str()
	).unwrap() }
	#[test] fn infer_list5() { check_infer_with_setup(
		&[LIST_HELPERS],
		"[[1, 2], [3, 4]]", 
		format!("[[{INT}]]").as_str()
	).unwrap() }
	#[test] fn infer_list6() { check_infer_with_setup(
		&[LIST_HELPERS],
		"[([], [])]", 
		"forall a: forall b: [([a], [b])]"
	).unwrap() }

	const GVARIANT: &str = "
	variant EOrN<L, R> {
		Left(L);
		Right(R);
		None;
	}
	";
	#[test] fn infer_gvariant1() { check_infer_with_setup(
		&[GVARIANT],
		"EOrN::Left(1)", 
		format!("forall a: EOrN<{INT}, a>").as_str()
	).unwrap() }
	#[test] fn infer_gvariant2() { check_infer_with_setup(
		&[GVARIANT],
		"EOrN::Right(1)", 
		format!("forall a: EOrN<a, {INT}>").as_str()
	).unwrap() }
	#[test] fn infer_gvariant3() { check_infer_with_setup(
		&[GVARIANT],
		"EOrN::None", 
		"forall a: forall b: EOrN<a, b>"
	).unwrap() }
	#[test] fn infer_gvariant4() { check_infer_with_setup(
		&[GVARIANT],
		"x-> if x == 0: EOrN::Left(5) else Right([1])", 
		format!("{INT} -> EOrN<{INT}, [{INT}]>").as_str()
	).unwrap() }
	#[test] fn infer_gvariant5() { check_infer_with_setup(
		&[GVARIANT],
		"x-> if x == 0: EOrN::Left(5) else EOrN::None", 
		format!("forall a: {INT} -> EOrN<{INT}, a>").as_str()
	).unwrap() }

	const STRUCTEG: &str = "
	struct S {
		int x;
		[int] ys;
	}
	";
	#[test] fn infer_struct1() { check_infer_with_setup(
		&[STRUCTEG],
		"S { x = 1; ys = [2,3]; }", 
		"S"
	).unwrap() }
	#[test] fn infer_struct2() { check_infer_with_setup(
		&[STRUCTEG],
		"S { x = 1; ys = [2,3]; }.x", 
		INT
	).unwrap() }
	#[test] fn infer_struct3() { check_infer_with_setup(
		&[STRUCTEG],
		"S { x = 1; ys = [2,3]; }.ys", 
		format!("[{INT}]").as_str()
	).unwrap() }

	const GSTRUCT1: &str = "
	struct S<T> {
		T x;
		[T] ys;
	}
	";
	#[test] fn infer_gstruct1() { check_infer_with_setup(
		&[GSTRUCT1],
		"S{ x = 1; ys = []; }", 
		format!("S<{INT}>").as_str()
	).unwrap() }
	#[test] fn infer_gstruct2() { check_infer_with_setup(
		&[GSTRUCT1],
		"S{ x = 1; ys = []; }.x", 
		INT
	).unwrap() }
	#[test] fn infer_gstruct3() { check_infer_with_setup(
		&[GSTRUCT1],
		"S{ x = 1; ys = []; }.ys",
		format!("[{INT}]").as_str()
	).unwrap() }
	#[test] fn infer_gstruct4() { check_infer_with_setup(
		&[GSTRUCT1],
		"S{ x = []; ys = []; }", 
		"forall a: S<[a]>"
	).unwrap() }
	#[test] fn infer_gstruct5() { check_infer_with_setup(
		&[GSTRUCT1],
		"S{ x = []; ys = []; }.ys", 
		"forall a: [[a]]"
	).unwrap() }

	const GSTRUCT2: &str = "
	struct S<X, Y> {
		X x;
		Y y;
	}
	";
	#[test] fn infer_gstruct6() { check_infer_with_setup(
		&[GSTRUCT2],
		"S{ x = 1; y = 2; }", 
		format!("S<{INT}, {INT}>").as_str()
	).unwrap() }
	#[test] fn infer_gstruct7() { check_infer_with_setup(
		&[GSTRUCT2],
		"S{ x = 1; y = []; }",
		format!("forall a: S<{INT}, [a]>").as_str()
	).unwrap() }
	#[test] fn infer_gstruct8() { check_infer_with_setup(
		&[GSTRUCT2],
		"S{ x = 1; y = [2]; }", 
		format!("S<{INT}, [{INT}]>").as_str()
	).unwrap() }
	#[test] fn infer_gstruct9() { check_infer_with_setup(
		&[GSTRUCT2],
		"S{ x = 1; y = [2]; }.x", 
		INT
	).unwrap() }
	#[test] fn infer_gstruct10() { check_infer_with_setup(
		&[GSTRUCT2],
		"S{ x = 1; y = [2]; }.y", 
		format!("[{INT}]").as_str()
	).unwrap() }

	const BASIC_CLASS: &str = "
	class Intable for T {
		func as_int(self) -> int;
	}
	struct Foo {
		int x;
		[int] y;
	}
	impl Intable for Foo {
		func as_int(self) -> int = self.x;
	}
	impl Intable for int {
		func as_int(self) -> int = self;
	}
	func calc<Intable T>(T x) -> int = x.as_int() + 3;
	";

	#[test] fn infer_basic_class1() { check_infer_with_setup(
		&[BASIC_CLASS],
		"calc", 
		format!("forall Intable a: a -> {INT}").as_str()
	).unwrap() }
	#[test] fn infer_basic_class2() { check_infer_with_setup(
		&[BASIC_CLASS],
		"calc(5)", 
		INT
	).unwrap() }
	#[test] fn infer_basic_class3() { check_infer_with_setup(
		&[BASIC_CLASS],
		"calc(Foo { x = 5; y = []; })", 
		INT
	).unwrap() }
	#[test] fn infer_basic_class4() { check_infer_with_setup(
		&[BASIC_CLASS],
		"((x,y)->calc(x) + calc(y))(Foo { x = 3; y=[];}, 5)", 
		INT
	).unwrap() }
	#[test] fn infer_basic_class5() { check_infer_with_setup(
		&[BASIC_CLASS],
		"(x->(calc(x), x) )", 
		format!("forall Intable a: a -> ({INT}, a)").as_str()
	).unwrap() }
}