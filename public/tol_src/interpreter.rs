use std::{collections::{HashMap, HashSet}, ops::{Deref, DerefMut}, cell::RefCell, rc::{Rc, Weak}, vec, fmt::Display };
use crate::{parser::{Expr, LiteralKind, Sttm, TypeExpr}, types::{type_expr_to_type, Namespace, Path}};
use crate::types;
use crate::parser;

pub const DEBUG_LVL: usize = 0;
// macro for printing debug messages if DEBUG_LVL is higher than n and prepend "DBG{n}:"
#[macro_export]
macro_rules! dbg_println {
    ($lvl:expr, $($arg:tt)*) => {
        if crate::interpreter::DEBUG_LVL >= $lvl {
            println!("DBG{}: {}", $lvl, format!($($arg)*));
        }
    };
}


pub const GLOBAL_NAMESPACE: &str = "global#";


pub const PRELUDE: &str = "
    operator + _ (int a) -> int = a;
    operator - _ (int a) -> int = #compiler_primitive(\"-int\")(a);
    operator _ + _ ~ left 2 ~ (int a, int b) -> int = #compiler_primitive(\"int+int\")(a,b);
    operator _ - _ ~ left 2 ~ (int a, int b) -> int = #compiler_primitive(\"int-int\")(a,b);
    operator _ * _ ~ left 3 ~ (int a, int b) -> int = #compiler_primitive(\"int*int\")(a,b);
    operator _ / _ ~ left 3 ~ (int a, int b) -> int = #compiler_primitive(\"int/int\")(a,b);
    operator _ == _ ~ left 1 ~ (int a, int b) -> bool = #compiler_primitive(\"int==int\")(a,b);
    operator _ < _ ~ left 1 ~ (int a, int b) -> bool = #compiler_primitive(\"int<int\")(a,b);
    operator _ > _ ~ left 1 ~ (int a, int b) -> bool = #compiler_primitive(\"int>int\")(a,b);

    func concat<T>([T] xs, [T] ys) -> [T] = match (xs, ys) {
        ([], ys) => ys,
        ([x : xs], ys) => [x : concat(xs, ys)]
    };

    // current string converting class - subject to change
    class Str for T {
        func str(self) -> [char];
    }
    impl Str for int {
        func str(self) -> [char] = #compiler_primitive(\"int.str\")(self);
    }
    impl Str for char {
        func str(self) -> [char] = [self]; // todo add single quotes, but i need to make them escapable first
    }
    impl Str for bool {
        func str(self) -> [char] = if self: \"true\" else \"false\";
    }
    impl<Str T> Str for [T] {
        func str(self) -> [char] {
            let strl = l -> match l {
                [] => [']'],
                [x:xs] => [',', ' ' : Str::str(x) `concat strl(xs)] 
            };
            = match self {
                [] => \"[]\",
                [x:xs] => ['[' : Str::str(x) `concat strl(xs)] 
            };
        }
    }
";



// if it returns Err, then there is an error,
// but if it returns Ok(None), then all is fine, except a strict argument wasn't filled
// and is left to be filled later
type NativeFunc = fn(&'_ [Value]) -> Result<Option<Value>, String>;

// name of primitive, number of arguments, function
pub const PRIMITIVES: &[(&str, usize, NativeFunc)] = &[
    ("-int", 1, |args| {
        if args.len() != 1 {
            return Err("(shouldn't happen) unary '-' expects 1 argument".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        
        // if !thunk_filled(&a) {
        //     return Ok(None);
        // }
        if let Value::Data(DataValue::Int(a)) = a {
            Ok(Some(Value::Data(DataValue::Int(-a))))
        } else {
            Err("(shouldn't happen) unary '-' expects 1 int".to_string())
        }
    }),
    ("int+int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '+' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        if let (Value::Data(DataValue::Int(a)), Value::Data(DataValue::Int(b))) = (a, b)  {
            Ok(Some(Value::Data(DataValue::Int(a+b))))
        } else {
            Err("(shouldn't happen) '+' expects 2 ints".to_string())
        }
    }),
    ("int-int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '-' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        if let (Value::Data(DataValue::Int(a)), Value::Data(DataValue::Int(b))) = (a, b)  {
            Ok(Some(Value::Data(DataValue::Int(a-b))))
        } else {
            Err("(shouldn't happen) '-' expects 2 ints".to_string())
        }
    }),
    ("int*int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '*' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        if let (Value::Data(DataValue::Int(a)), Value::Data(DataValue::Int(b))) = (a, b)  {
            Ok(Some(Value::Data(DataValue::Int(a*b))))
        } else {
            Err("(shouldn't happen) '*' expects 2 ints".to_string())
        }
    }),
    ("int/int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '/' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        if let (Value::Data(DataValue::Int(a)), Value::Data(DataValue::Int(b))) = (a, b)  {
            Ok(Some(Value::Data(DataValue::Int(a/b))))
        } else {
            Err("(shouldn't happen) '/' expects 2 ints".to_string())
        }
    }),
    ("int==int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '==' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        Ok(Some(Value::Data(DataValue::Bool(a == b))))
    }),
    ("int<int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '<' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        if let (Value::Data(DataValue::Int(a)), Value::Data(DataValue::Int(b))) = (a, b)  {
            Ok(Some(Value::Data(DataValue::Bool(a < b))))
        } else {
            Err("(shouldn't happen) '<' expects 2 ints".to_string())
        }
    }),
    ("int>int", 2, |args| {
        if args.len() != 2 {
            return Err("(shouldn't happen) binary '>' expects 2 arguments".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let b = if let Some(v) = eval_whnf(&args[1])? { v } else {
            return Ok(None);
        };
        // if !thunk_filled(&a) || !thunk_filled(&b) {
        //     return Ok(None);
        // }
        if let (Value::Data(DataValue::Int(a)), Value::Data(DataValue::Int(b))) = (a, b)  {
            Ok(Some(Value::Data(DataValue::Bool(a > b))))
        } else {
            Err("(shouldn't happen) '>' expects 2 ints".to_string())
        }
    }),
    ("int.str", 1, |args| {
        if args.len() != 1 {
            return Err("(shouldn't happen) unary 'str' expects 1 argument".to_string());
        }
        let a = if let Some(v) = eval_whnf(&args[0])? { v } else {
            return Ok(None);
        };
        let num = if let Value::Data(DataValue::Int(a)) = a { a } else {
            return Err("(shouldn't happen) int.str expects 1 int".to_string());
        };
        let s = num.to_string();

        let mut v = DataValue::Pack(types::LIST_NIL, vec![]);
        for c in s.chars().rev() {
            v = DataValue::Pack(types::LIST_CONS, vec![Value::Data(DataValue::Char(c)), Value::Data(v)]);
        }
        Ok(Some(Value::Data(v)))
    })
];




impl std::fmt::Display for DataValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            DataValue::Unit => write!(f, "()"),
            DataValue::Int(i) => write!(f, "{}", i),
            DataValue::Char(c) => write!(f, "'{}'", c),
            DataValue::Bool(b) => write!(f, "{}", b),
            DataValue::Pack(i, vs) => {
                write!(f, "({}| ", i)?;
                // if vs.len() == 0 {
                //     write!(f, "()")?;
                // }
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "C{{a{} ", self.arity)?;
        if self.stored.len() > 0 {
            write!(f, "[")?;
            for (i, v) in self.stored.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", v)?;
            }
            write!(f, "] ")?;
        }

        write!(f, "{}}}", (*self.thunk).borrow())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        // match self {
        //     Value::Bottom => write!(f, "_|_"),
        //     Value::Data(d) => write!(f, "{}", d),
        //     Value::Closure(c) => write!(f, "{}", c),
        //     Value::Arg(i) => write!(f, "arg{}", i),
        //     Value::Thunk(t) => write!(f, "{}", (**t).borrow()),
        //     Value::RecThunk(t, arg_repl) => {
        //         write!(f, "rec({})", if t.upgrade().is_some() { 
        //             arg_repl.iter()
        //                 .map(|vs| format!("{}", vs.iter().map(|v| format!("{}", v)).collect::<Vec<String>>().join(", ")))
        //                 .collect::<Vec<String>>()
        //                 .join("; ")
        //         } else { "dead".to_string() })
        //         // write!(f, "rec(")?;
        //         // if let Some(addr) = t.upgrade().map(|x| x.as_ptr()) { 
        //         //     write!(f, "{:p})", addr)
        //         // } else { write!(f, "dead)") }
        //     }
        //     Value::ClassFuncV(class, func, tv_id) => write!(f, "'{}::{}' on v{}", class, func, tv_id),
        //     Value::ClassFuncN(class, func, tv_name) => write!(f, "'{}::{}' on '{}'", class, func, tv_name),
        //     Value::CallPlaceholder => write!(f, "[-]")
        // }
        write!(f, "{}", value_to_code(self))
    }
}

#[allow(dead_code)]
fn value_to_code(value: &Value) -> String {
    fn with_indent(v: &Value, indent: usize) -> String {
        const TAB: &str = "    ";
        match v {
            Value::Bottom => "_|_".to_string(),
            Value::Data(d) => format!("{}", d),
            Value::Closure(c) => {
                let f = format!("({}) ->\n", (0..c.arity).map(|n| format!("a{}", n)).collect::<Vec<String>>().join(", "))
                    + &TAB.repeat(indent+1) + &with_indent(&Value::Thunk(c.thunk.clone()), indent + 1);
                if c.stored.len() > 0 {
                    format!("({}\n{ind})(\n{}\n{ind})", 
                        f, 
                        c.stored.iter().map(|v|  TAB.repeat(indent + 1) + &with_indent(v, indent+1)).collect::<Vec<String>>().join(",\n"), 
                        ind = &TAB.repeat(indent)
                    )
                } else {
                    f
                }
            }
            Value::Thunk(t) => match &*(**t).borrow() {
                Thunk::Ret(v) => with_indent(v, indent),
                Thunk::Call(f, args) => {
                    let f = match f {
                        ThunkCall::Native(_f) => format!("native"),
                        ThunkCall::Value(v) => with_indent(v, indent)
                    };
                    let args = args.iter().map(|v| with_indent(v, indent+1)).collect::<Vec<String>>().join(", ");
                    format!("({})({})", f, args)
                },
                Thunk::If(cond, t, f) => {
                    let cond = with_indent(cond, indent);
                    let t = TAB.repeat(indent+1) + &with_indent(t, indent+1);
                    let f = TAB.repeat(indent+1) + &with_indent(f, indent+1);
                    format!("if {}:\n{}\n{}else\n{}\n{}", cond, t, TAB.repeat(indent), f, TAB.repeat(indent))
                }
                Thunk::Match(v, pats) => {
                    let v = with_indent(v, indent);
                    let pats = pats.iter().map(|(pat, t)| TAB.repeat(indent+1) + &format!("{} => {}", pat, with_indent(t, indent+1))).collect::<Vec<String>>().join("\n");
                    format!("match {}{{\n{}\n{}}}", v, pats, TAB.repeat(indent))
                }
                _ => "???".to_string()
            }
            Value::RecThunk(..) => format!("rec"),
            Value::Arg(i) => format!("a{}", i),
            Value::ClassFuncV(class, func, tv_id) => format!("'{}::{}' on v{}", class, func, tv_id),
            Value::ClassFuncN(class, func, tv_name) => format!("'{}::{}' on '{}'", class, func, tv_name),
            Value::CallPlaceholder => format!("[-]"),
            // _ => "???".to_string()
        }
    }

    with_indent(value, 0)
}


impl std::fmt::Display for Thunk {  
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "T{{")?;
        match &self {
            Thunk::Ret(v) => write!(f, "ret {}", v)?,
            Thunk::Call(call, args) => {
                write!(f, "call")?;
                match call {
                    // ThunkCall::Closure(c) => write!(f, " {}", c)?,
                    // ThunkCall::Thunk(t) => write!(f, " {}", (**t).borrow())?,
                    // ThunkCall::RecThunk(t, arg_repl) => {
                    //     // write!(f, " rec({})", if t.upgrade().is_some() { "..." } else { "dead" })?
                    //     write!(f, " rec({})", if t.upgrade().is_some() { 
                    //         arg_repl.iter()
                    //             .map(|vs| format!("{}", vs.iter().map(|v| format!("{}", v)).collect::<Vec<String>>().join(", ")))
                    //             .collect::<Vec<String>>()
                    //             .join("; ")
                    //     } else { "dead".to_string() })?
                    // },
                    // ThunkCall::ClassFunc(name, tv_id) => write!(f, " '{}' on v{}", name, tv_id)?,
                    // ThunkCall::Arg(i) => write!(f, " arg{}", i)?
                    ThunkCall::Value(v) => write!(f, " {}", v)?,
                    ThunkCall::Native(..) => write!(f, " native")?,
                }
                if args.len() > 0 {
                    write!(f, " [")?;
                    for (i, v) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", v)?;
                    }
                    write!(f, "]")?;
                }
            },
            Thunk::If(cond, if_true, if_false) => {
                write!(f, "if {} : {} else {}", cond, if_true, if_false)?;
            },
            Thunk::Match(v, pats) => {
                write!(f, "match {}", v)?;
                if pats.len() > 0 {
                    write!(f, " {{")?;
                    for (i, (pat, v)) in pats.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{} => {}", pat, v)?;
                    }
                    write!(f, "}}")?;
                }
            }
            Thunk::Unpack(v, i) => write!(f, "unpack{} {} ", i, v)?
        }
        write!(f, "}}")
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Var(n) => write!(f, "var{}", n),
            Pattern::Const(v) => write!(f, "{}", v),
            Pattern::Pack(i, ps) => {
                write!(f, "({}|", i)?;
                for (i, p) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ")")
            },
            Pattern::List(ps, p) => {
                write!(f, "[")?;
                for (i, p) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                if let Some(p) = p {
                    write!(f, ":{}", p)?;
                }
                write!(f, "]")
            }
        }
    }
}


// debug printing for values
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}


#[derive(Clone)]
pub enum Value {
    Bottom,
    Data(DataValue),
    Closure(Closure),
    Thunk(Rc<RefCell<Thunk>>),
    RecThunk(Weak<RefCell<Thunk>>, Vec<Vec<Value>>), // RecThunk stores argument replacements to be applied when the thunk is evaluated
    Arg(usize),
    ClassFuncV(types::Class, String, i32), // class, function name, type_var that it depends on
    ClassFuncN(types::Class, String, String), // class, function name, name of polymorphic type that it depends on
    CallPlaceholder // possibly useless?
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bottom, Value::Bottom) => true,
            (Value::Data(d1), Value::Data(d2)) => d1 == d2,
            (Value::Closure(c1), Value::Closure(c2)) => c1 == c2,
            (Value::Thunk(t1), Value::Thunk(t2)) => Rc::ptr_eq(t1, t2),
            (Value::RecThunk(t1, _), Value::RecThunk(t2, _)) => Weak::ptr_eq(t1, t2),
            (Value::Arg(i1), Value::Arg(i2)) => i1 == i2,
            (Value::CallPlaceholder, Value::CallPlaceholder) => true,
            _ => false
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum DataValue {
    Unit,
    Int(i32),
    Char(char),
    Bool(bool),
    Pack(i32, Vec<Value>),
}

#[derive(Clone)]
pub struct Closure {
    // whether this is a function with bound arguments or just a lambda
    arity: usize,
    // applied arguments 
    stored: Vec<Value>, 
    // the "code"
    thunk: Rc<RefCell<Thunk>>
}

impl PartialEq for Closure {
    // don't compare closures for now
    fn eq(&self, _other: &Self) -> bool { false }
}

#[derive(Clone)]
pub enum ThunkCall {
    // todo: since thunkcall is starting to re-implement value, maybe just have 2 variants: Value, Native
    // unfortunately, I am too lazy to do that right now, so I'll just add ClassFunc and deal with it later
    Value(Value),
    Native(NativeFunc),
    // Closure(Closure),
    // Thunk(Rc<RefCell<Thunk>>),
    // RecThunk(Weak<RefCell<Thunk>>, Vec<Vec<Value>>),
    // ClassFunc(String, i32), // function name, type_var that it depends on
    // Arg(usize)
}

#[derive(Clone)]
pub enum Thunk {
    Call(ThunkCall, Vec<Value>),
    Ret(Value),
    Unpack(Value, usize), 
    Match(Value, Vec<(Pattern, Value)>),
    If(Value, Value, Value), // cond, if true, if false
}

impl PartialEq for Thunk { // thunks can't be compared
    fn eq(&self, _other: &Self) -> bool { false }
}

#[derive(Clone)]
pub enum Pattern {
    Wildcard,
    Var(usize),
    Const(DataValue),
    Pack(i32, Vec<Pattern>),
    List(Vec<Pattern>, Option<Box<Pattern>>),
}


impl Value {
    // replace_args replaces args 0 to n-1 with the given values where n is the lenght of the passed in args
    // arg{>n} are replaced with arg{>n-n}
    pub fn replace_args(&mut self, args: &[&Value], update_recs: bool) -> Result<(), String> {
        fn replace_rec(val: &mut Value, args: &[&Value], update_recs: bool, add_to_recs: bool, rec_map: &HashMap<*const Thunk, Weak<RefCell<Thunk>>>) 
            -> Result<(), String>
        {
            match val {
                Value::Thunk(t) => {
                    let new_t = Rc::new(RefCell::new((**t).borrow().clone()));
                    let mut new_map;
                    let rec_map = if update_recs && Rc::downgrade(t).weak_count() > 1 {
                        new_map = rec_map.clone();
                        new_map.insert(t.as_ptr(), Rc::downgrade(&new_t));
                        &new_map
                    } else { rec_map };
                    match &mut *(*new_t).borrow_mut() {
                        Thunk::Ret(v) | Thunk::Unpack(v, _) => replace_rec(v, args, update_recs, add_to_recs, rec_map)?,
                        Thunk::Call(call, call_args) => {
                            if let ThunkCall::Value(call_val) = call {
                                replace_rec(call_val, args, update_recs, false, rec_map)?;
                            }
                            for v in call_args.iter_mut() {
                                replace_rec(v, args, update_recs, add_to_recs, rec_map)?;
                            };
                        }
                        Thunk::If(cond, if_true, if_false) => {
                            replace_rec(cond, args, update_recs, add_to_recs, rec_map)?;
                            replace_rec(if_true, args, update_recs, add_to_recs, rec_map)?;
                            replace_rec(if_false, args, update_recs, add_to_recs, rec_map)?;
                        },
                        Thunk::Match(mval, pats) => { 
                            replace_rec(mval, args, update_recs, add_to_recs, rec_map)?;
                            for (_, v) in pats.iter_mut() {
                                replace_rec(v, args, update_recs, add_to_recs, rec_map)?;
                            }
                        }
                    };
                    *val = Value::Thunk(new_t);
                },
                Value::RecThunk(rt, repl_args) if update_recs => {
                    // first get the current address of the thunk
                    let addr = rt.upgrade().ok_or("Tried to update a dead recursive thunk")?.as_ptr() as *const Thunk;
                    // search for the current address in the rec_map and replace rt with the new weak pointer if there is one
                    if let Some(new_rt) = rec_map.get(&addr) {
                        *val = Value::RecThunk(new_rt.clone(), repl_args.clone());
                    }
                }
                Value::RecThunk(_, repl_args) if add_to_recs => {
                    // add the args to the list of args to replace when evaluating the thunk
                    repl_args.push(args.iter().map(|v| (*v).clone()).collect());
                }
                Value::Arg(n) if *n < args.len() => {
                    *val = args[*n].clone();
                },
                Value::Arg(n) => {
                    *val = Value::Arg(*n - args.len());
                },
                Value::Closure(cls) => {
                    // a bit of a hack, since deep_clone was created after this and I haven't separated update_recs
                    // so if update recs is true, we'll update the thunk as well but
                    // we'll just pass in an empty args slice to only update the recs
                    if update_recs {
                        let mut t = Value::Thunk(cls.thunk.clone());
                        replace_rec(&mut t, &[], update_recs, add_to_recs, rec_map)?;
                        // cls.thunk = Rc::new(RefCell::new(Thunk::Call(ThunkCall::Value(t), vec![])));
                        if let Value::Thunk(tt) = t {
                            cls.thunk = tt;
                        } else {
                            cls.thunk = Rc::new(RefCell::new(Thunk::Ret(t)));
                        }
                    }


                    // replace only the stored values if not updating recursive thunks
                    for v in cls.stored.iter_mut() {
                        replace_rec(v, args, update_recs, add_to_recs, rec_map)?;
                    }
                }
                Value::Data(DataValue::Pack(_, els)) => {
                    for v in els.iter_mut() {
                        replace_rec(v, args, update_recs, add_to_recs, rec_map)?;
                    }
                }
                _ => {}
            };
            
            Ok(())
        }
        
        replace_rec(self, args, update_recs, true, &HashMap::new())
    }

    // clones the value, clones contained thunks and updates all recursive thunks to point to the new thunks
    pub fn deep_clone(&self) -> Result<Value, String> {
        fn deep_clone_rec(val: &Value, rec_map: &HashMap<*const Thunk, Weak<RefCell<Thunk>>>) -> Result<Value, String>
        {
            match val {
                Value::Thunk(t) => {
                    let new_t = Rc::new(RefCell::new((**t).borrow().clone()));
                    let mut new_map;
                    let rec_map = if Rc::downgrade(t).weak_count() > 1 {
                        new_map = rec_map.clone();
                        new_map.insert(t.as_ptr(), Rc::downgrade(&new_t));
                        &new_map
                    } else { rec_map };
                    match &mut *(*new_t).borrow_mut() {
                        Thunk::Ret(v) | Thunk::Unpack(v, _) => { *v = deep_clone_rec(v, rec_map)?; },
                        Thunk::Call(call, call_args) => {
                            if let ThunkCall::Value(call_val) = call {
                                *call_val = deep_clone_rec(call_val, rec_map)?;
                            }
                            for v in call_args.iter_mut() {
                                *v = deep_clone_rec(v, rec_map)?;
                            };
                        }
                        Thunk::If(cond, if_true, if_false) => {
                            *cond = deep_clone_rec(cond, rec_map)?;
                            *if_true = deep_clone_rec(if_true, rec_map)?;
                            *if_false = deep_clone_rec(if_false, rec_map)?;
                        },
                        Thunk::Match(mval, pats) => { 
                            *mval = deep_clone_rec(mval, rec_map)?;
                            for (_, v) in pats.iter_mut() {
                                *v = deep_clone_rec(v, rec_map)?;
                            }
                        }
                    };
                    Ok(Value::Thunk(new_t))
                }
                Value::RecThunk(rt, repl_args)=> {
                    // first get the current address of the thunk
                    let addr = rt.upgrade().ok_or("Tried to update a dead recursive thunk")?.as_ptr() as *const Thunk;
                    // search for the current address in the rec_map and replace rt with the new weak pointer if there is one
                    if let Some(new_rt) = rec_map.get(&addr) {
                        Ok(Value::RecThunk(new_rt.clone(), repl_args.clone()))
                    } else {
                        Ok(Value::RecThunk(rt.clone(), repl_args.clone()))
                    }
                }
                Value::Closure(cls) => {
                    // replace only the stored values
                    let mut cls = cls.clone();
                    for v in cls.stored.iter_mut() {
                        *v = deep_clone_rec(v, rec_map)?;
                    }
                    let mut t = Value::Thunk(cls.thunk.clone());
                    t = deep_clone_rec(&t, rec_map)?;
                    cls.thunk = match t {
                        Value::Thunk(thk) => thk,
                        _ => unreachable!()
                    };
                    Ok(Value::Closure(cls))
                }
                Value::Data(DataValue::Pack(id, els)) => {
                    let mut els = els.clone();
                    for v in els.iter_mut() {
                        *v = deep_clone_rec(v, rec_map)?;
                    }
                    Ok(Value::Data(DataValue::Pack(*id, els)))
                }
                _ => Ok(val.clone())
            }
        }
        deep_clone_rec(self, &HashMap::new())
    }

    // return bitfield of used arguments
    // NOTE: u128 means that only 128 arguments are supported
    pub fn args_used(&self) -> u128 {
        match self {
            Value::Thunk(t) => {
                let t = (**t).borrow();
                match &*t {
                    Thunk::Ret(v) | Thunk::Unpack(v, _) => v.args_used(),
                    Thunk::Call(call, args) => {
                        let mut bits = args.iter().map(|v| v.args_used()).
                        fold(0, |b1, b2| b1 | b2); // this looks weird because rust closures and bitwise or both use |
                        // match call {
                        //     ThunkCall::Arg(n) => { bits |= 1 << *n; },
                        //     _ => {}
                        // }
                        if let ThunkCall::Value(call_val) = call {
                            bits |= call_val.args_used();
                        }
                        bits
                    },
                    Thunk::If(cond, tr, fl) => {
                        cond.args_used() | tr.args_used() | fl.args_used()
                    }
                    Thunk::Match(mval, pats) => {
                        let mut bits = mval.args_used();
                        for (_, v) in pats.iter() {
                            bits |= v.args_used();
                        }
                        bits
                    }
                }
            },
            Value::Arg(n) => 1 << *n,
            Value::Closure(cls) => {
                cls.stored.iter().map(|v| v.args_used()).fold(0, |b1, b2| b1 | b2)
            },
            Value::Data(DataValue::Pack(_, els)) => {
                els.iter().map(|v| v.args_used()).fold(0, |b1, b2| b1 | b2)
            }
            _ => 0
        }
    }

    pub fn minimize_args(&mut self, patvar_start: usize) -> Result<Vec<usize>, String> {
        let mut args_used = self.args_used();
        let mut mapping = vec![];
        let mut n_nonpatvars = None;
        let mut has_a_0 = false;
        for i in 0..128 {
            if n_nonpatvars.is_none() && i >= patvar_start {
                n_nonpatvars = Some(mapping.len());
            }
            if args_used & 1 != 0 {
                if i >= patvar_start {
                    while mapping.len() < i - patvar_start + n_nonpatvars.unwrap() {
                        mapping.push(None); 
                    }    
                }
                mapping.push(Some(i));
            } else if args_used != 0 {
                has_a_0 = true;
            } else {
                break;
            }
            args_used >>= 1;
        }
        // if there is no unused argument, mapping will just be [0, 1, 2, ... up to whatever]
        // calling replace_args with this mapping will do nothing, so we can just return early
        if !has_a_0 { 
            return Ok(mapping.into_iter().filter_map(|v| v).collect());
        }

        // ex. mapping = [1, 3, 5]

        // we're gonna use replace_args to do our work for us
        // as such we need to turn the mapping from resulting index -> original index to original index -> resulting index

        // we're gonna use bottoms to fill in the gaps as they should never be used
        // for the example mapping above, replace should be
        // [_|_, 0, _|_, 1, _|_, 2]
        let mut replace = vec![];
        for (i, arg) in mapping.iter().enumerate() {
            let arg = if let Some(arg) = arg { arg } else { continue };
            if replace.len() < *arg {
                replace.resize(*arg, Value::Bottom);
            }
            replace.push(Value::Arg(i));
        }
        let refs = replace.iter().collect::<Vec<_>>();
        self.replace_args(&refs, true)?;
        Ok(mapping.into_iter().filter_map(|v| v).collect())
    }


    pub fn generalise_class_funcs(&mut self, sub: &types::Subst) -> Result<(), String> {
        if sub.is_empty() {
            return Ok(());
        }
        match self {
            Value::ClassFuncV(class, func, tv_id) => {
                if let Some(types::Type::Polymorphic { name, .. }) = sub.get(&tv_id) {
                    *self = Value::ClassFuncN(class.clone(), func.clone(), name.clone());
                }
                Ok(())
            },
            Value::Data(DataValue::Pack(_, els)) => {
                for v in els.iter_mut() {
                    v.generalise_class_funcs(sub)?;
                }
                Ok(())
            },
            Value::Thunk(thk) => {
                let mut thk = thk.borrow_mut();
                match &mut *thk {
                    Thunk::Ret(v) | Thunk::Unpack(v, _) => v.generalise_class_funcs(sub),
                    Thunk::Call(call, args) => {
                        match call {
                            ThunkCall::Value(v) => v.generalise_class_funcs(sub)?,
                            _ => {}
                        }
                        for v in args.iter_mut() {
                            v.generalise_class_funcs(sub)?;
                        }
                        Ok(())
                    },
                    Thunk::If(cond, tr, fl) => {
                        cond.generalise_class_funcs(sub)?;
                        tr.generalise_class_funcs(sub)?;
                        fl.generalise_class_funcs(sub)
                    },
                    Thunk::Match(mval, pats) => {
                        mval.generalise_class_funcs(sub)?;
                        for (_, v) in pats.iter_mut() {
                            v.generalise_class_funcs(sub)?;
                        }
                        Ok(())
                    }
                }
            },
            Value::Closure(cls) => {
                for v in cls.stored.iter_mut() {
                    v.generalise_class_funcs(sub)?;
                }
                let mut t = Value::Thunk(cls.thunk.clone());
                t.generalise_class_funcs(sub)?;
                cls.thunk = match t {
                    Value::Thunk(thk) => thk,
                    _ => unreachable!()
                };
                Ok(())
            },
            _ => Ok(())
        }
    }

    pub fn instantiate_class_funcs(&mut self, tvs: &HashMap<String, i32>) -> Result<(), String> {
        if tvs.is_empty() {
            return Ok(());
        }
        match self {
            Value::ClassFuncN(class, func, name) => {
                if let Some(tv_id) = tvs.get(name) {

                    let mut new_cls = class.clone();
                    if let types::Class::Generic { args, .. } = &mut new_cls {
                        for arg in args.iter_mut() {
                            if let types::Type::Polymorphic { name, .. } = arg {
                                if let Some(new_id) = tvs.get(name) {
                                    *arg = types::Type::Variable { id: *new_id, constraint: None };
                                }
                            }
                        }
                    }

                    *self = Value::ClassFuncV(new_cls, func.clone(), *tv_id);
                }
                Ok(())
            },
            Value::Data(DataValue::Pack(_, els)) => {
                for v in els.iter_mut() {
                    v.instantiate_class_funcs(tvs)?;
                }
                Ok(())
            },
            Value::Thunk(thk) => {
                let mut thk = thk.borrow_mut();
                match &mut *thk {
                    Thunk::Ret(v) | Thunk::Unpack(v, _) => v.instantiate_class_funcs(tvs),
                    Thunk::Call(call, args) => {
                        match call {
                            ThunkCall::Value(v) => v.instantiate_class_funcs(tvs)?,
                            _ => {}
                        }
                        for v in args.iter_mut() {
                            v.instantiate_class_funcs(tvs)?;
                        }
                        Ok(())
                    },
                    Thunk::If(cond, tr, fl) => {
                        cond.instantiate_class_funcs(tvs)?;
                        tr.instantiate_class_funcs(tvs)?;
                        fl.instantiate_class_funcs(tvs)
                    },
                    Thunk::Match(mval, pats) => {
                        mval.instantiate_class_funcs(tvs)?;
                        for (_, v) in pats.iter_mut() {
                            v.instantiate_class_funcs(tvs)?;
                        }
                        Ok(())
                    }
                }
            },
            Value::Closure(cls) => {
                for v in cls.stored.iter_mut() {
                    v.instantiate_class_funcs(tvs)?;
                }
                let mut t = Value::Thunk(cls.thunk.clone());
                t.instantiate_class_funcs(tvs)?;
                cls.thunk = match t {
                    Value::Thunk(thk) => thk,
                    _ => unreachable!()
                };
                Ok(())
            },
            _ => Ok(())
        }
    }

    pub fn resolve_class_funcs(&mut self, sub: &types::Subst, ctx: &Context) -> Result<(), String> {
        if sub.is_empty() {
            return Ok(())
        }
        match self {
            Value::ClassFuncV(class, func, tv_id) => {

                match class {
                    types::Class::Generic { args, .. } => {
                        for arg in args.iter_mut() {
                            if let types::Type::Variable { id, .. } = arg {
                                if let Some(t) = sub.get(id) {
                                    *arg = t.clone();
                                }
                            }
                        }
                    },
                    _ => {}
                }

                let resolved_type =  if let Some(t) = sub.get(&tv_id) {
                    t
                } else {
                    return Ok(())
                };
                if let types::Type::Variable { id: new_tv_id, .. } = resolved_type {
                    *self = Value::ClassFuncV(class.clone(), func.clone(), *new_tv_id);
                } else {
                    let (new_self, poly_map) = ctx.get_class_inst_func(class, resolved_type, &func)?;
                    // dbg_println!(0, "got class inst func: {} for {}", self, resolved_type);
                    *self = new_self;
                    // self.resolve_class_funcs(sub, ctx)?;
                    self.subst_named_class_funcs(&poly_map, ctx)?;
                }
                Ok(())
            },
            Value::Data(DataValue::Pack(_, els)) => {
                for v in els.iter_mut() {
                    v.resolve_class_funcs(sub, ctx)?;
                }
                Ok(())
            },
            Value::Thunk(thk) => {
                let mut thk = thk.borrow_mut();
                match &mut *thk {
                    Thunk::Ret(v) | Thunk::Unpack(v, _) => v.resolve_class_funcs(sub, ctx),
                    Thunk::Call(call, args) => {
                        match call {
                            ThunkCall::Value(v) => v.resolve_class_funcs(sub, ctx)?,
                            _ => {}
                        }
                        for v in args.iter_mut() {
                            v.resolve_class_funcs(sub, ctx)?;
                        }
                        Ok(())
                    },
                    Thunk::If(cond, tr, fl) => {
                        cond.resolve_class_funcs(sub, ctx)?;
                        tr.resolve_class_funcs(sub, ctx)?;
                        fl.resolve_class_funcs(sub, ctx)
                    },
                    Thunk::Match(mval, pats) => {
                        mval.resolve_class_funcs(sub, ctx)?;
                        for (_, v) in pats.iter_mut() {
                            v.resolve_class_funcs(sub, ctx)?;
                        }
                        Ok(())
                    }
                }
            },
            Value::Closure(cls) => {
                for v in cls.stored.iter_mut() {
                    v.resolve_class_funcs(sub, ctx)?;
                }
                let mut t = Value::Thunk(cls.thunk.clone());
                t.resolve_class_funcs(sub, ctx)?;
                cls.thunk = match t {
                    Value::Thunk(thk) => thk,
                    _ => unreachable!()
                };
                Ok(())
            },
            _ => Ok(())
        }
    }

    pub fn subst_named_class_funcs(&mut self, poly_map: &HashMap<String, types::Type>, ctx: &Context) -> Result<(), String> {
        if poly_map.is_empty() {
            return Ok(())
        }
        match self {
            Value::ClassFuncN(class, func, tname) => {
                match class {
                    types::Class::Generic { args, .. } => {
                        for arg in args.iter_mut() {
                            if let types::Type::Polymorphic { name, .. } = arg {
                                if let Some(t) = poly_map.get(name) {
                                    *arg = t.clone();
                                }
                            }
                        }
                    },
                    _ => {}
                }

                let resolved_type =  if let Some(t) = poly_map.get(tname) {
                    t
                } else {
                    return Ok(())
                };
                if let types::Type::Variable { id: new_tv_id, .. } = resolved_type {
                    *self = Value::ClassFuncV(class.clone(), func.clone(), *new_tv_id);
                } else {
                    let new_poly_map: HashMap<String, types::Type>;
                    (*self, new_poly_map) = ctx.get_class_inst_func(class, resolved_type, &func)?;
                    *self = self.deep_clone()?;
                    self.subst_named_class_funcs(&new_poly_map, ctx)?;
                }
                Ok(())
            },
            Value::Data(DataValue::Pack(_, els)) => {
                for v in els.iter_mut() {
                    v.subst_named_class_funcs(poly_map, ctx)?;
                }
                Ok(())
            },
            Value::Thunk(thk) => {
                let mut thk = thk.borrow_mut();
                match &mut *thk {
                    Thunk::Ret(v) | Thunk::Unpack(v, _) => v.subst_named_class_funcs(poly_map, ctx),
                    Thunk::Call(call, args) => {
                        match call {
                            ThunkCall::Value(v) => v.subst_named_class_funcs(poly_map, ctx)?,
                            _ => {}
                        }
                        for v in args.iter_mut() {
                            v.subst_named_class_funcs(poly_map, ctx)?;
                        }
                        Ok(())
                    },
                    Thunk::If(cond, tr, fl) => {
                        cond.subst_named_class_funcs(poly_map, ctx)?;
                        tr.subst_named_class_funcs(poly_map, ctx)?;
                        fl.subst_named_class_funcs(poly_map, ctx)
                    },
                    Thunk::Match(mval, pats) => {
                        mval.subst_named_class_funcs(poly_map, ctx)?;
                        for (_, v) in pats.iter_mut() {
                            v.subst_named_class_funcs(poly_map, ctx)?;
                        }
                        Ok(())
                    }
                }
            },
            Value::Closure(cls) => {
                for v in cls.stored.iter_mut() {
                    v.subst_named_class_funcs(poly_map, ctx)?;
                }
                let mut t = Value::Thunk(cls.thunk.clone());
                t.subst_named_class_funcs(poly_map, ctx)?;
                cls.thunk = match t {
                    Value::Thunk(thk) => thk,
                    _ => unreachable!()
                };
                Ok(())
            },
            _ => Ok(())
        }
    }

}

// ------------------

#[derive(Clone)]
pub struct BinOpPrec {
	pub precedence: i32,
	pub right_assoc: bool
}

#[derive(Clone)]
pub struct Context {
    pub idents_stack: Vec<HashMap<String, (Value, types::Type)>>,
    pub global_namespace: Namespace,
    // name of unary operator -> vector of all instances of that operator -> (type of arg, type of op, closure for op)
	pub un_ops: HashMap<String, Vec<(types::Type, types::Type, Value)>>,
    // name of binary operator -> precedance, vector of all instances of that operator -> (type of larg, of rarg, type of op, closure for op)
	pub bin_ops: HashMap<String, (BinOpPrec, Vec<(types::Type, types::Type, types::Type, Value)>)>,
    pub namespace_stack: Vec<(String, usize)>,
    pub n_args_stack: Vec<usize>,
    pub total_args: usize,


    pub next_type_var_id: i32,
    pub next_anon_type_id: i32,
    pub tmp_class_specs: Vec<(types::Class, types::Type)>
}

pub enum MemberGet {
    Field(usize),
    // not an OOP method; this is still a pure function, but one that implicitly takes self as the first argument
    Method(Value) 
}

impl Context {
    pub fn new() -> Self {
        use types::{INT, BOOL, CHAR, LIST, Type, TypeSpec, TypeSpecData};
        let list_type = Type::Forall { constraint: None, quant: "a".to_string(), t: Box::new(
            Type::GenericNamed { path: vec![], name: LIST.to_string(), args: vec![Type::Polymorphic { name: "a".to_string(), id: 0 }] }
        ) };
        let mut ctx = Self {
            idents_stack: vec![HashMap::from([])],
            global_namespace: Namespace { 
                type_specs: HashMap::from([
                    (INT.to_string(), TypeSpec { basis: Type::new_basic_named(INT), data: TypeSpecData::Primitive }),
                    (BOOL.to_string(), TypeSpec { basis: Type::new_basic_named(BOOL), data: TypeSpecData::Primitive }),
                    (CHAR.to_string(), TypeSpec { basis: Type::new_basic_named(CHAR), data: TypeSpecData::Primitive }),
                    (LIST.to_string(), TypeSpec{ 
                        basis: list_type,
                        data: TypeSpecData::Variant(vec![]), // empty vector should be fine since list syntax is special-cased
                    }),
                ]), 
                class_specs: HashMap::new(), 
                namespaces: HashMap::new(),
                funcs: HashMap::new(),
                impls: HashMap::new(),
            },
            un_ops: HashMap::new(), 
            bin_ops: HashMap::new(),
            namespace_stack: vec![(GLOBAL_NAMESPACE.to_string(), 1)],
            n_args_stack: vec![0],
            total_args: 0,
            next_type_var_id: 0,
            next_anon_type_id: 0,
            tmp_class_specs: vec![],
        };

        execute(PRELUDE, &mut ctx).unwrap();
        ctx
    }

    pub fn child<'a>(&'a mut self) -> ChildContext<'a> {
        self.idents_stack.push(HashMap::new());
        self.n_args_stack.push(self.total_args);
        for (_, n) in self.namespace_stack.iter_mut() {
            *n += 1;
        }
        ChildContext { ctx: self }
    }

    // this function should only be called manually when debugging
    // otherwise only ChildContext should call when it is dropped
    fn drop_level(&mut self) {
        self.idents_stack.pop();
        self.total_args = self.n_args_stack.pop().unwrap();
        self.namespace_stack.retain_mut(|(_, n)| if *n > 1 { *n -= 1; true } else { false });
    }

    pub fn ident(&self, path: &Path, name: &str, g_args: &Vec<types::Type>, typ: Option<&types::Type>) -> Result<(Value, types::Type), String> {     
        // if the path is empty, we first check in the identifiers stack
        if path.len() == 0 {
            for idents in self.idents_stack.iter().rev() {
                if let Some((vl, tp)) = idents.get(name) {
                    // dbg_println!(0, "all idents in here: {:?}", idents);
                    return Ok((vl.deep_clone()?, tp.clone()));
                }
            }
        }

        // check if it is a variant
        if let Some((t, v)) = self.get_variant(typ, path, name)
        {
            return Ok((v.deep_clone()?, t.clone()));
        }

        // let full_args = if let Some(types::Type::Generic { args, .. }) = typ {
        //     let mut full_args = args.clone();
        //     full_args.extend(g_args.iter().cloned());
        //     full_args
        // } else {
        //     g_args.clone()
        // };
        let mut full_args = vec![];
        for p in path.iter() {
            full_args.extend(p.1.iter().cloned());
        }
        full_args.extend(g_args.iter().cloned());

        // check if it is a function in a namespace
        for ns in self.accessible_namespaces().iter().rev() {
            if let Some((t, v, _, resolved_gargs)) = ns.resolve_func(path, &name.to_string(), g_args, self) {
                let garg_map = resolved_gargs.iter().filter_map(|mut t| {
                    while let types::Type::Forall { t: new_t, .. } = t {
                        t = &*new_t;
                    }
                    if let types::Type::Polymorphic { name, .. } = t { Some(name.clone()) } else { None }
                }).zip(full_args.iter().cloned()).collect();

                let t = types::subst_polymorphic(t, &garg_map);
                let mut v = v.deep_clone()?;
                v.subst_named_class_funcs(&garg_map, self)?;

                return Ok((v, t));
            }
        }
        Err(format!("Identifier '{}' not found", name))
    }
    
    pub fn add_ident(&mut self, name: &str, v: Option<Value>, t: Option<types::Type>) {
        let v = v.unwrap_or(Value::Bottom);
        let t = if t.is_some() { t.unwrap() } else { self.anon_type_var() };
        self.idents_stack.last_mut().unwrap().insert(name.to_string(), (v, t));
    }

	pub fn un_op(&self, name: &str, argt: &types::Type) -> Option<(types::Type, Value)> {
		let insts = if let Some(t) = self.un_ops.get(name) { t } else {
            return None;
        };

        // let mut full_args = vec![];
        // for p in path.iter() {
        //     full_args.extend(p.1.iter().cloned());
        // }
        // full_args.extend(g_args.iter().cloned());
        if insts.len() == 1 {
            let (_, t, v) = &insts[0];
            return Some((t.clone(), v.clone()));
        }
        if insts.len() == 0 {
            return None;
        }
        let (at, t, v) = if let types::Type::Variable { .. } = argt {
            (&insts[0].0, &insts[0].1, &insts[0].2)
        } else if let Some(tup) = insts.iter().rev().find_map(|(at, t, v)|
            if types::type_scope_matches(at, argt, self)
            {
                Some((at, t, v))
            } else { None }
        ) { tup } else {
            return None;
        };

        // let garg_map = {
        //     let mut t = at;
        //     while let types::Type::Forall { t: new_t, .. } = t {
        //         t = &*new_t;
        //     }
        //     if let types::Type::Polymorphic { name, .. } = t { Some(name.clone()) } else { None }
        // }.map_or_else(HashMap::new, |name| HashMap::from([(name, argt.clone())]));

        let garg_map = types::unify_polymorphic(at, argt, self).unwrap();
        let t = types::subst_polymorphic(t, &garg_map);

        let mut v = v.deep_clone().unwrap();
        v.subst_named_class_funcs(&garg_map, self).unwrap();

        Some((t, v))

	}

    pub fn add_un_op(&mut self, oper: &str, argt: types::Type, typ: types::Type, val: Value) -> Result<(), String> {
        let insts = self.un_ops.entry(oper.to_string()).or_insert(vec![]);
        insts.push((argt, typ, val));
        Ok(())
    }

    // this is a bit of a hack - basically to construct the value of the operator, we must first add a placeholder
    // for it in case the definition is recursive. after that we can update it.
    // in most cases the operator we nned to update will be the last one added, so we can just update that one.
    // this would break if we try to define an operator inside another operator.
    pub fn pop_un_op(&mut self, oper: &str) -> Result<(), String> {
        let insts = self.un_ops.get_mut(oper).unwrap();
        insts.pop();
        Ok(())
    }

	pub fn bin_op(&self, name: &str, largt: &types::Type, rargt: &types::Type) -> Option<(BinOpPrec, types::Type, Value)> {
		let (prec, insts) = if let Some(t) = self.bin_ops.get(name) { t } else {
            return None;
        };

        // if insts.len() == 1 {
        //     let (_, _, t, v) = &insts[0];
        //     return Some((prec.clone(), t.clone(), v.clone()));
        // }
        if insts.len() == 0 {
            return None;
        }

        let (lt, _rt, t, v) = 
        if let (types::Type::Variable { constraint: None, .. }, types::Type::Variable { constraint: None, .. }) = (largt, rargt) {
            (&insts[0].0, &insts[0].1, &insts[0].2, &insts[0].3)
        } else 
        if let Some(tup) = insts.iter().rev().find_map(|(lt, rt, t, v)|
            if types::type_scope_matches(lt, largt, self) && types::type_scope_matches(rt, rargt, self)
            {
                // dbg_println!(0, "matched {},{} -> {},{} for {}", lt, rt, largt, rargt, name);
                Some((lt, rt, t, v))
            } else { None }
        ) { tup } else {
            return None;
        };

        // dbg_println!(0, "before t: {}", t);

        // let garg_map =[lt,rt].iter().map(|t| {
        //     let mut t = *t;
        //     while let types::Type::Forall { t: new_t, .. } = t {
        //         t = &*new_t;
        //     }
        //     if let types::Type::Polymorphic { name, .. } = t { Some(name.clone()) } else { None }
        // }).zip([largt, rargt].iter()).filter_map(|(name, t)| name.map(|name| (name, (*t).clone()))).collect::<HashMap<_, _>>();
        
        let garg_map = types::unify_polymorphic(lt, largt, self).unwrap();
        // todo: also get the mappings for the right arg
        let t = types::subst_polymorphic(t, &garg_map);

        let mut v = v.deep_clone().unwrap();
        v.subst_named_class_funcs(&garg_map, self).unwrap();

        // dbg_println!(0, "after t: {}", t);

        Some((prec.clone(), t, v))
	}

    pub fn bin_op_prec(&self, name: &str) -> Option<BinOpPrec> {
        self.bin_ops.get(name).map(|(prec, _)| prec.clone())
    }
    
    pub fn add_bin_op(&mut self, oper: &str, largt: types::Type, rargt: types::Type, typ: types::Type, val: Value) -> Result<(), String> {
        // if there isn't an entry for this operator, return error telling the user to declare it's precedence
        if let Some((_, insts)) = self.bin_ops.get_mut(oper) {
            insts.push((largt, rargt, typ, val));
            Ok(())
        } else {
            Err(format!("Precedence for operator '{}' not declared", oper))
        }
    }

    pub fn pop_bin_op(&mut self, oper: &str) -> Result<(), String> {
        let insts = if let Some((_, insts)) = self.bin_ops.get_mut(oper) { insts } else {
            return Err(format!("Precedence for operator '{}' not declared", oper));
        };
        insts.pop();
        Ok(())
    }


    pub fn into_namespace(&mut self, name: String) {
        self.namespace_stack.push((name, 1));
    }

    // pub fn out_of_namespace(&mut self) {
    //     self.namespace_stack.pop();
    // }

    // moves idents, un_ops and bin_ops of the child context to the parent context
    pub fn move_up(&mut self) {
        let last_idents = self.idents_stack.pop().unwrap();
        self.idents_stack.last_mut().unwrap().extend(last_idents);
        self.idents_stack.push(HashMap::new());
    }

    fn add_arg(&mut self, name: &str) -> (Value, types::Type) {
        let arg = Value::Arg(self.total_args);
        let t = self.anon_type_var();
        self.idents_stack.last_mut().unwrap()
            .insert(name.to_string(), (arg.clone(), t.clone()));
        dbg_println!(3, "added arg {} idents: {:?}", name, &self.idents_stack.last().unwrap());
        self.total_args += 1;
        // *self.n_args_stack.last_mut().unwrap() += 1;
        (arg, t)
    }

    fn add_typed_arg(&mut self, name: &str, t: types::Type) -> (Value, types::Type) {
        let arg = Value::Arg(self.total_args);
        self.idents_stack.last_mut().unwrap()
            .insert(name.to_string(), (arg.clone(), t.clone()));
        self.total_args += 1;
        // *self.n_args_stack.last_mut().unwrap() += 1;
        (arg, t)
    }

    fn reset_args(&mut self) -> usize {
        let n = self.total_args;
        self.total_args = 0;
        n
    }

    fn restore_args(&mut self, n: usize) {
        self.total_args = n;
    }
    
    pub fn member_of(&self, t: &types::Type, member: &str, g_args: &Vec<types::Type>) -> Result<(types::Type, MemberGet), String> {
        match t {
            types::Type::Forall { t, .. } => {
                self.member_of(t, member, g_args)
            },
            types::Type::Named { path, name } => {
                let spec = self.type_spec(path, name)?;
                if let types::TypeSpecData::Struct (members) = &spec.data {
                    let memb_id = members.iter().position(|(_, n)| n == member);
                    if memb_id.is_some() {
                        let (t, _) = members.get(memb_id.unwrap()).unwrap();
                        return Ok((t.clone(), MemberGet::Field(memb_id.unwrap().try_into().unwrap())));
                    }
                }
                for ns in self.accessible_namespaces() {
                    // if it's not a member of a struct, we search for a namespace with the name of the type and look for a method there
                    if let Some((t, v, true, resolved_gargs)) = 
                        ns.resolve_namespace(path, name).and_then(|n| n.resolve_func(&[], &member.to_string(), g_args, self))
                    {
                        let garg_map = resolved_gargs.iter().filter_map(
                            |t| if let types::Type::Polymorphic { name, .. } = t { Some(name.clone()) } else { None }
                        ).zip(g_args.iter().cloned()).collect();
        
                        let t = types::subst_polymorphic(t, &garg_map);
                        let mut v = v.deep_clone()?;
                        v.subst_named_class_funcs(&garg_map, self)?;

                        return Ok((t.clone(), MemberGet::Method(v.clone())));
                    }

                    // we also check if it's a function from an implementation
                    // if let Some((t, v, is_method)) = ns.resolve_impl_func(t, &member.to_string()) {
                    //     if *is_method {
                    //         return Ok((t.clone(), MemberGet::Method(v.clone())));
                    //     } else {
                    //         return Err(format!("Function '{}' is not a method", member));
                    //     }
                    // }
                }

                Err(format!("Type '{}' doesn't have a member '{}'", name, member))
            },
            types::Type::GenericNamed { path, name, args } => {
                fn replace_poly(t: types::Type, args: &Vec<types::Type>) -> types::Type {
                    match t {
                        types::Type::Polymorphic { name: _, id } => args[id].clone(),
                        types::Type::GenericNamed { path, name, args: gargs } => 
                            types::Type::GenericNamed { 
                                path,
                                name, 
                                args: gargs.iter()
                                .map(|t| replace_poly(t.clone(), args)).collect() 
                            },
                        types::Type::Function { arg, ret } =>
                            types::Type::Function {
                                arg: Box::new(replace_poly(*arg, args)),
                                ret: Box::new(replace_poly(*ret, args))
                            },
                        types::Type::Tuple { els } =>
                            types::Type::Tuple {
                                els: els.iter()
                                .map(|t| replace_poly(t.clone(), args)).collect()
                            },
                        _ => t
                    }
                }

                let spec = self.type_spec(path, name)?;

                if let types::TypeSpecData::Struct ( members) = &spec.data {
                    let memb_id = members.iter().position(|(_, n)| n == member);
                    if memb_id.is_some() {
                        let (t, _) = members.get(memb_id.unwrap()).unwrap();
                        // we have to replace any polymorphic types in 't' with the type from args at index - the id of the polymorphic type
                        return Ok((replace_poly(t.clone(), args), MemberGet::Field(memb_id.unwrap().try_into().unwrap())));
                    }
                }
                let full_args = args.iter().chain(g_args.iter()).cloned().collect::<Vec<_>>();
                // if it's not a member of a struct, we search for a namespace with the name of the type and look for a method there
                for ns in self.accessible_namespaces() {
                    if let Some((t, v, true, resolved_gargs)) = 
                        ns.resolve_namespace(path, name).and_then(|n| n.resolve_func(&[], &member.to_string(), &full_args, self))
                    {
                        let garg_map = resolved_gargs.iter().rev().filter_map(|mut t| {
                                while let types::Type::Forall { t: new_t, .. } = t {
                                    t = &*new_t;
                                }
                                if let types::Type::Polymorphic { name, .. } = t { Some(name.clone()) } else { None }
                            }).zip(full_args.iter().cloned()).collect();
        
                        let t = types::subst_polymorphic(t, &garg_map);
                        let mut v = v.deep_clone()?;
                        v.subst_named_class_funcs(&garg_map, self)?;

                        return Ok((replace_poly(t, args), MemberGet::Method(v)));
                    }
                }
                // if let Some((t, v, true)) = 
                //         ns.resolve_namespace(path, name).and_then(|n| n.resolve_func(&[], &member.to_string(), g_args))
                //     {
                //         return Ok((t.clone(), MemberGet::Method(v.clone())));
                //     }

                Err(format!("Type '{}' doesn't have a member '{}'", t, member))
            },
            types::Type::Variable { constraint, id } => {
                let c = if let Some(c) = constraint { c } else {
                    return Err(format!("Type '{}' doesn't have a member '{}'", t, member))
                };

                let (cls_path, cls_name, cls_gargs) = match c {
                    types::Class::Named { path, name } => (path, name, vec![]),
                    types::Class::Generic { path, name, args } => (path, name, args.clone()),
                    _ => return Err("Cannot get class functions for a multi class".to_string())
                };

                let full_args = cls_gargs.iter().chain(g_args.iter()).cloned().collect::<Vec<_>>();

                for ns in self.accessible_namespaces() {
                    if let Some((t, _v, true, resolved_gargs)) = 
                        ns.resolve_namespace(&cls_path, &cls_name).and_then(|n| n.resolve_func(&[], &member.to_string(), &cls_gargs, self))
                    {
                        let garg_map = resolved_gargs.iter().filter_map(
                            |t| if let types::Type::Polymorphic { name, .. } = t { Some(name.clone()) } else { None }
                        ).zip(full_args.iter().cloned()).collect();
        
                        let t = types::subst_polymorphic(t, &garg_map);
                        return Ok((t, MemberGet::Method(Value::ClassFuncV(c.clone(), member.to_string(), *id))));
                    }
                }


                unimplemented!()
            },
            _ => Err(format!("Type '{}' has no member '{}'", t, member))
        }
    }
    
    /*
    pub fn member_of_class(&self, c: &types::Class, member: &str, type_var_id: i32, g_args: &Vec<types::Type>) -> Result<(types::Type, MemberGet), String> {
        
		let spec = {
			let mut r = None;
			for ns in self.accessible_namespaces().iter().rev() {
				if let Some(t) = ns.resolve_class_spec(cls_path, cls_name) {
					r = Some(t);
				}
			}
			if let Some(r) = r {
				r
			} else {
				return Err(format!("Class '{}' not found", c));
			}
		};

        if let Some((_, _, _, true)) =  spec.funcs.get(member) {
            let md = spec.funcs.get(member).unwrap();
            return Ok((md.0.clone(), MemberGet::Method(Value::ClassFuncV(spec.basis.clone(), member.to_string(), type_var_id))));
        }
        Err(format!("Class '{}' doesn't have a member '{}'", c, member))
    }*/

    pub fn get_class_inst_func(&self, cls: &types::Class, cl_inst: &types::Type, name: &str) -> Result<(Value, HashMap<String, types::Type>), String> {
		for ns in self.accessible_namespaces().iter().rev() {
            if let Some((funcs, poly_map)) = ns.resolve_impl(cl_inst, cls, self) {
                if let Some(f) = funcs.get(name) {
                    return Ok((f.1.clone(), poly_map)); // need to make this return any generics for the implentation and the types they map to
                } else {
                    return Err(format!("Class '{}' doesn't have a function named '{}'", cls, name));
                }
            }
        }
        return Err(format!("Type '{}' is not an instance of class '{}'",cl_inst, cls));
    }

    fn add_oper_prec(&mut self, oper: &str, prec: &parser::OperPrec) -> Result<(), String> {
        if let Some(..) = self.bin_ops.get(oper) {
            return Err(format!("The precedence of '_ {} _' has already been declared", oper));
        }
        match prec {
            parser::OperPrec::New { right, prec: prec_expr } => {
                let v = expr_value(prec_expr, self)?;
                if v.1 != (types::Type::Named { path: vec![], name: format!("{}", types::INT) }) {
                    return Err(format!("The precedence of '_ {} _' must be an integer", oper));
                }
                let v = eval_whnf(&v.0)?.unwrap_or_else(|| v.0.clone());
                let prec_n = if let Value::Data(DataValue::Int(n)) = v { n } else {
                    return Err(format!("The precedence of '_ {} _' must be computable at compile time", oper));
                };

                self.bin_ops.insert(oper.to_string(), (BinOpPrec { right_assoc: *right, precedence: prec_n }, vec![]));
            },
            parser::OperPrec::Inherit { oper: from } => {
                if let Some((inherit_prec , _)) = self.bin_ops.get(*from) {
                    self.bin_ops.insert(oper.to_string(), (inherit_prec.clone(), vec![]));
                } else {
                    return Err(format!("The precedence of '_ {} _' cannot be inherited from an undeclared operator", oper));
                } 
            }
            parser::OperPrec::None => return Err("add_oper_prec was called with None as the precedence".to_string())
        }
        Ok(())
    }
}

pub struct ChildContext<'a> {
    // this is just a way to automatically keep track of the real context's stacks
    ctx: &'a mut Context
}

impl Drop for ChildContext<'_> {
    fn drop(&mut self) {
        self.ctx.drop_level();
    }
}

impl Deref for ChildContext<'_> {
    type Target = Context;
    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl DerefMut for ChildContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ctx
    }
}
// --------------------------

fn native_to_closure(n: NativeFunc, n_args: usize) -> Closure {
    Closure { arity: n_args, stored: vec![], thunk: Rc::new(RefCell::new(Thunk::Call(
            ThunkCall::Native(n), 
            (0usize..n_args).map(|x| Value::Arg(x)).collect()
        )
    ))}
}

fn apply_n_to_closure(mut f: Closure, args: &[Value], args_start: usize) -> Closure {
    // add all args to the stored of the closure starting from args_start
    // basically args_start is where we start looking for an open slot(a call placeholder - this symbol will be used for it: [-] )
    // so if we have stored [[-], 1, [-], 2] and we want to add [3,4] at place 1:
    // we look at place 1, it contains a 1, go to place 2 and it contains a [-], so we add 3 there
    // then we look at place 3, it contains a 2, the stored array has ended, add the rest of the args ([4]) to the end
    // another example : stored: [], args_start: 1, args: [1], since stored is shorter than args_start, we have to add a placeholder
    // before adding 1, so we get [[-], 1]
    // ----------
    // NOTE: the above description is how it used to work, but I'm too lazy to update it, so i'll write this addendum:
    // since now every <closure arity> args is like a new call, we need to add placeholders, so that the newly added args 
    // are on the rigth args_start *in their respective call*. In practice this involves
    // taking f.stored.len() % f.arity instead of just f.stored.len() and i % f.arity instead of just i

    while f.stored.len() % f.arity < args_start as usize {
        f.stored.push(Value::CallPlaceholder);
    }
    let mut i = args_start;
    for arg in args {
        while i < f.stored.len() {
            if i % f.arity >= f.stored.len() % f.arity {
                if let Value::CallPlaceholder = f.stored[i as usize] {
                    f.stored[i as usize] = arg.clone();
                    break;
                }
            }
            i += 1;
        }
        if i >= f.stored.len() {
            f.stored.push(arg.clone());
        }
        i += 1;
    }
    f
}

#[allow(dead_code)]
fn apply_to_closure(f: Closure, arg: &Value, arg_n: usize) -> Closure {
    apply_n_to_closure(f, core::slice::from_ref(arg), arg_n)
}


fn complete_lambda(mut v: Value, n_args: usize, n_outside_args: usize, n_patvars: usize) -> Result<Value, String> {
    let minimized_mapping = v.minimize_args(n_args - n_patvars)?;
    dbg_println!(2, "lambda_complete minimized {}", &v);

    let extra_stored = minimized_mapping.into_iter().filter_map(|i| {
        if i < n_outside_args {
            Some(Value::Arg(i))
        } else {
            None
        }
    }).collect::<Vec<_>>();
    let res_arity = n_args - n_outside_args + extra_stored.len();
    
    let cls = match v {
        Value::Arg(..) | Value::Data(..) => 
            Closure {
                arity: res_arity,
                stored: extra_stored,
                thunk: Rc::new(RefCell::new(Thunk::Ret(v))) 
            },
        Value::Closure(c) => {
            // c.arity = c.arity.max(n_args);
            // Value::Closure(c)
            Closure { arity: res_arity, stored: extra_stored, thunk: Rc::new(RefCell::new(Thunk::Ret(Value::Closure(c)))) }
        },
        Value::Thunk(t) => {
            Closure {
                arity: res_arity,
                stored: extra_stored,
                thunk: t
            }
        },
        _ => panic!("complete_lambda: ??? {}", v)
    };

    // call the "lifted" lambda with external args
    // if n_outside_args > min_arg {
    //     cls.stored.extend((min_arg..n_outside_args).map(|i| Value::Arg(i)));
    // }

    // if n_outside_args == 0 {
    //     cls
    // } else {
    //     // we need to add args 0 - n_outside_args to the stored list of cls, since we're in a block and they are automatically bound
    //     let mut cls = match cls {
    //         Value::Closure(c) => c,
    //         _ => unreachable!()
    //     };
    //     cls.stored = cls.stored.into_iter().chain((0..n_outside_args).map(|i| Value::Arg(i))).collect();
    //     Value::Closure(cls)
    // }
    Ok(Value::Closure(cls))
}

// doesn't actually evaluate, just "inserts" the values as arguments
fn call_value(mut v: Value, arg_vals: Vec<Value>) -> Result<Value, String> {
    match v {
        _ if arg_vals.len() == 0 => Ok(v), // don't do anything if there are no args
        Value::Closure(cls) => {
            // v = Value::Closure(apply_n_to_closure(cls, &arg_vals.as_slice(), (ctx.n_block_args).try_into().unwrap(), ctx.total_args));
            v = Value::Closure(apply_n_to_closure(cls, &arg_vals.as_slice(), 0));
            Ok(v)
        }
        Value::Thunk(thk) => {
            let is_call = match &*(*thk).borrow() {
                Thunk::Call(..) => true,
                _ => false
            };
            let thk = if is_call {
                let thk = (*thk).borrow().clone();
                if let Thunk::Call(call, mut args) = thk {
                    args.extend(arg_vals);
                    Rc::new(RefCell::new(Thunk::Call(call, args)))
                } else { unreachable!() }
            } else {
                Rc::new(RefCell::new(Thunk::Call(ThunkCall::Value(Value::Thunk(thk)), arg_vals)))
            };
            Ok(Value::Thunk(thk))
        }
        Value::RecThunk(..) | Value::ClassFuncV(..) | Value::Arg(..) => {
            Ok(Value::Thunk(Rc::new(RefCell::new(Thunk::Call(ThunkCall::Value(v), arg_vals)))))
        }
        // if the "function" is a data constructor, we can just replace the args in the element list with the current args
        Value::Data(DataValue::Pack(..)) => {
            let arg_vals_refs = arg_vals.iter().collect::<Vec<_>>();
            v.replace_args(&arg_vals_refs, false)?;
            Ok(v)
        }
        _ => Err(format!("Call on non-function: {}", v))
    }
}


// returns Some(..) where the "arguments" passed to the branch closures are inside the returned vector if val matches pattern, None otherwise
fn match_pat(pat: &Pattern, val: &mut Value) -> Result<Option<Vec<Value>>, String> {
    fn match_rec(pat: &Pattern, mut val: &mut Value, repl: &mut Vec<Value>) -> Result<bool, String> {
        match (pat, &mut val) {
            (Pattern::Wildcard, _) => {
                Ok(true)
            },
            (Pattern::Var(n), _) => {
                if repl.len() <= *n {
                    repl.resize(*n + 1, Value::Bottom);
                }
                repl[*n] = val.clone();
                Ok(true)
            },
            (Pattern::Const(c), Value::Data(d)) => {
                Ok(c == d)
            },
            (Pattern::Pack(pid, pats), Value::Data(DataValue::Pack(vid, vals))) => {
                if pid != vid {
                    return Ok(false);
                }
                // recurse on each pattern and value
                // and if any of them fail, fail ouselves
                for (p, v) in pats.iter().zip(vals.iter_mut()) {
                    if !match_rec(p, v, repl)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            },
            (Pattern::List(els, tail), Value::Data(DataValue::Pack(vid, vals))) => {
                if els.len() == 0 { return Ok(*vid == types::LIST_NIL); }
                if *vid == types::LIST_NIL { return Ok(false); }

                // try to match the first pattern in els with the head of the pack
                if !match_rec(&els[0], &mut vals[0], repl)? {
                    return Ok(false);
                }
                
                if els.len() == 1 {
                    // we only had one element, if there is a tail, we need to match it
                    if tail.is_some() {
                        return match_rec(tail.as_ref().unwrap(), &mut vals[1], repl)
                    } else {
                        // we have to make sure the next element is nil
                        let ev = eval_whnf(&vals[1])?;
                        if let Some(evr) = ev { 
                            vals[1] = evr;
                        } 
                        return Ok(vals[1] == Value::Data(DataValue::Pack(types::LIST_NIL, vec![])));
                    }
                } else {
                    // there is more than one element in the pattern:
                    // make a new pattern without the first element and match the tail of the value to that
                    let new_pat = Pattern::List(els[1..].to_vec(), tail.clone());
                    return match_rec(&new_pat, &mut vals[1], repl);
                }
            },
            (Pattern::Const(..), Value::Thunk(..)) |
            (Pattern::Pack(..), Value::Thunk(..)) |
            (Pattern::List(..), Value::Thunk(..)) |
            (Pattern::Const(..), Value::Closure(..)) |
            (Pattern::Pack(..), Value::Closure(..)) |
            (Pattern::List(..), Value::Closure(..)) |
            (Pattern::Const(..), Value::RecThunk(..)) |
            (Pattern::Pack(..), Value::RecThunk(..)) |
            (Pattern::List(..), Value::RecThunk(..)) 
                => {
                // we can't match a thunk, so we need to evaluate it
                // and then try to match the result
                let ev = eval_whnf(val)?;
                if let Some(evr) = ev { 
                    *val = evr;
                } else { return Ok(false); }
                match_rec(pat, val, repl)
            }
            /*
                let ev = if let Ok(ev) = eval_whnf(v) {
                    ev.unwrap_or_else(|| v.clone())
                } else { return false; };
            */
            _ => Ok(false)
        }
    }

    let mut repl = vec![];
    if match_rec(pat, val, &mut repl)? {
        Ok(Some(repl))
    } else { Ok(None) }
}

// evaluates up to weak head normal form
pub fn eval_whnf(v: &Value) -> Result<Option<Value>, String> {
    fn eval_rec<'a>(v: &'a Value, args: &Vec<&'a Value>) -> Result<Option<Value>, String>
    {   
        let mut v = v.clone();
        // actually stored in reverse order, so we can add to the front more easily
        let mut args = args.iter().rev().map(|v| (*v).clone()).collect::<Vec<Value>>();
        loop {
            dbg_println!(1, "evaluating {}{}", v,
                if args.len() == 0 { "".to_string() } else { format!(" | args: {}", args.iter().rev().map(|v| format!("{}", v)).collect::<Vec<String>>().join(", ")) }
            );
            match v {
                Value::Bottom => { return Err("Tried to evaluate bottom".to_string()); },
                Value::Arg(n) => {
                    if args.len() == 0 { return Ok(None); } else {
                        return Ok(Some(args[n].clone()));
                    }
                }
                Value::Data(..) => { return Ok(Some(v)); }, // data values are already in whnf
                Value::Closure(ref cls) => {
                    args.extend(cls.stored.iter().zip(0..cls.stored.len()).rev().map(|(v,i)| {
                        let mut v = v.clone();
                        v = if let Value::CallPlaceholder = v {
                            Value::Arg(i)
                        } else { v };
                        // v.replace_args(args);
                        v
                    }));
                    
                    if cls.arity > args.len() {
                        return Ok(Some(v));
                    }

                    let mut thk = Value::Thunk(Rc::new(RefCell::new((*cls.thunk).borrow().clone())));
                    let args_refs = args.iter().rev().collect::<Vec<&Value>>();
                    thk.replace_args(&args_refs, false)?;
                    if cls.arity == args.len() {
                        // all args used, instead of recursing, we can do tail call optimization
                        v = thk;
                        args = vec![];
                        continue;
                    } else {
                        // not all args used, recurse
                        let remaining = args_refs.into_iter().skip(cls.arity).collect::<Vec<&Value>>();
                        let mut res = if let Some(res) = eval_rec(&thk, &remaining)? { res } else { thk };
                        res.replace_args(&remaining, false)?;
                        args.drain((args.len()-cls.arity)..args.len());
                        v = res;
                        continue;
                    }
                }
                Value::Thunk(thk) => {
                    let thk = (*thk).borrow();
                    match &*thk {
                        Thunk::Ret(rv) => {
                            // tail call optimization
                            v = rv.clone();
                            continue;
                        }
                        Thunk::Call(to_call, call_args) => {
                            match to_call {
                                ThunkCall::Native(native) => {
                                    if let Some(res) = native(&call_args)? {
                                        v = res;
                                        continue;
                                    } else {
                                        return Ok(None);
                                    }
                                }
                                ThunkCall::Value(Value::Closure(cls)) => {
                                    args.extend(call_args.iter().rev().map(|v| (*v).clone()));
                                    v = Value::Closure(cls.clone());
                                    continue;
                                }
                                ThunkCall::Value(Value::Thunk(thk)) => {
                                    args = call_args.iter().rev().map(|v| (*v).clone()).collect::<Vec<Value>>();
                                    v = Value::Thunk(thk.clone());
                                    continue;
                                }
                                ThunkCall::Value(Value::RecThunk(rt, repl_args)) => {
                                    let thk = rt.upgrade().ok_or("Tried to call a dead recursive thunk")?;
                                    let mut val = Value::Thunk(thk);
                                    for args in repl_args {
                                        let args = args.iter().collect::<Vec<&Value>>();
                                        val.replace_args(&args, false)?;
                                    }
                                    let call_args_refs = call_args.iter().collect::<Vec<&Value>>();
                                    val.replace_args(&call_args_refs, false)?;

                                    args = call_args.iter().rev().map(|v| (*v).clone()).collect::<Vec<Value>>();
                                    v = val;
                                    continue;
                                }
                                ThunkCall::Value(Value::Arg(..)) => { return Err("Tried to call an argument placeholder".to_string()); },
                                ThunkCall::Value(Value::ClassFuncV(class, name, _)) |
                                ThunkCall::Value(Value::ClassFuncN(class, name, _)) => 
                                    { return Err(format!("Tried to call class function '{}::{}' placeholder", class, name)); },
                                _ => { return Err("Tried to call a non-callable value".to_string()); }
                            }
                        }
                        Thunk::If(cond, tr, fl) => {
                            let cond = eval_rec(cond, &args.iter().collect())?;
                            match cond {
                                None => { return Ok(None); },
                                Some(Value::Data(DataValue::Bool(b))) => {
                                    v = if b { tr.clone() } else { fl.clone() };
                                    continue;
                                }
                                Some(cond) => { return Err(format!("Tried to evaluate a non-boolean value in if: {}", cond)); }
                            }
                        }
                        Thunk::Match(mval, pats) => {
                            // let mut mval = if let Some(mval) = eval_rec(mval, &args.iter().collect())? {
                            //     mval
                            // } else { return Ok(None); };
                            let mut mval = mval.clone();
                            let mut matched = false;
                            v = Value::Bottom; // to avoid borrow checker, if we get a match, we'll replace this
                            for (pat, branch) in pats {
                                if let Some(pat_args) = match_pat(pat, &mut mval)? {
                                    // let pat_args_refs = pat_args.iter().chain(args.iter().map(|v| *v)).collect::<Vec<&Value>>();
                                    // return eval_rec(branch, &pat_args_refs);
                                    args.extend(pat_args.iter().rev().map(|v| (*v).clone()));
                                    v = branch.clone();
                                    matched = true;
                                    break;
                                }
                            }
                            if !matched {
                                return Err(format!("Failed to match value: {}", mval));
                            } else {
                                continue;
                            }
                        }
                        Thunk::Unpack(uv, n) => {
                            let uv = if let Some(uv) = eval_rec(uv, &args.iter().collect())? {
                                uv
                            } else { return Ok(None); };
                            if let Value::Data(DataValue::Pack(_, els)) = uv {
                                if els.len() <= *n {
                                    return Err(format!("Unpack: tried to unpack element {}, but pack had only {} elemts", n, els.len()));
                                }
                                v = els[*n].clone();
                                continue;
                            } else { return Err("Unpack: expected a pack".to_string()); }
                        }
                    };
                }
                Value::RecThunk(rt, repl_args) => {
                    let thk = rt.upgrade().ok_or("Tried to call a dead recursive thunk")?;
                    let mut val = Value::Thunk(thk);
                    for args in repl_args {
                        let args = args.iter().collect::<Vec<&Value>>();
                        val.replace_args(&args, false)?;
                    }
                    v = val;
                    continue;
                }
                Value::CallPlaceholder => { return Err("Tried to evaluate a call placeholder".to_string()); },
                Value::ClassFuncV(class, name, _) | 
                Value::ClassFuncN(class, name, _) => 
                    { return Err(format!("Tried to evaluate class function '{}::{}' placeholder", class, name)); },
            };
        }
    }
    eval_rec(v, &vec![])
}

// --------------------------

// also adds pattern variables as identifiers to the Context

pub fn pattern(e: &Expr, t: &types::Type, ctx: &mut Context, only_types: bool) -> Result<(Pattern, usize, types::Subst), String> {
    fn with_patvars(e: &Expr, t: &types::Type, ctx: &mut Context, n_patvars: &mut usize, only_types: bool)
    -> Result<(Pattern, types::Subst), String> {
        match e {
            Expr::Identifier { path, name, generic_args } => {
                if generic_args.len() > 0 {
                    return Err("Generic arguments are not allowed in patterns".to_string());
                }
                if name == &"_" {
                    Ok((Pattern::Wildcard, types::Subst::new()))
                } else {
                    let (it, iv) = if let Some(tup) = 
                    ctx.get_variant(Some(t), &types::path_expr_to_path(path)?, name) { tup } else {
                        // ctx.add_ident(name, Some(DataValue::PatVar(*n_patvars)), Some(t.clone()));
                        // *n_patvars += 1;
                        // Ok((Pattern::Var, types::Subst::new()))

                        // if it is not a variant, and it doesn't have a path or generic args it must be a variable
                        if path.is_empty() && generic_args.is_empty() {
                            ctx.add_typed_arg(name, t.clone());
                            *n_patvars += 1;
                            return Ok((Pattern::Var(*n_patvars-1), types::Subst::new()));
                        } else {
                            return Err(format!("'{}' is not a constructor", name));
                        }
                    };
                    let it = it.clone();
                    let iv = iv.clone();
                    let (it, _) = types::instantiate(it, ctx);
                    let un = types::unify(&t, &it, &ctx)?;
                    if only_types {
                        Ok((Pattern::Pack(-1, vec![]), un))
                    } else {
                        if let Value::Data(DataValue::Pack(id, _)) = iv {
                            Ok((Pattern::Pack(id, vec![]), un))
                        } else {
                            Err(format!("'{}' is not a constructor", name))
                        }
                    }
                }
            },
            Expr::Literal { kind, val } => {
                let (lv, lt) = literal(*kind, val)?;
                let un = types::unify(&t, &lt, &ctx)?;
                Ok((Pattern::Const(lv), un))
            },
            Expr::Call { func, args } => {
                let func = &**func;
                let args = &**args;

                if let Expr::Identifier { path, name, generic_args } = func {
                    if generic_args.len() > 0 {
                        return Err("Generic arguments are not allowed in patterns".to_string());
                    }
                    let path = types::path_expr_to_path(path)?;
                    let (it, iv) = if let Some(tup) = ctx.get_variant(Some(t), &path, name) { tup } else {
                        return Err(format!("'{}' is not a constructor for the matched type '{}'", name, t));
                    };
                    // should always pass
                    if let Expr::Tuple { els: arg_els } = args {
                        let iv = iv.clone();
                        let it = it.clone();

                        let (mut cur, _tvs) = types::instantiate(it, ctx);
                        // let iv = instantiate_class_funcs(iv, &tvs);

                        let mut sub = types::Subst::new();
                        let mut pack_pats = vec![];
                        for a in arg_els {
                            if let types::Type::Function{ arg: carg, ret: cret} = cur {
                                let (ap, as_) = with_patvars(a, &carg, ctx, n_patvars, only_types)?;
                                sub = types::compose_subst(&as_, &sub);
                                cur = *cret;
                                pack_pats.push(ap);
                            }
                        }
                        // check that what is left unifies with the type of the pattern
                        let un = types::unify(&t, &cur, &ctx)?;
                        sub = types::compose_subst(&un, &sub);

                        if only_types {
                            return Ok((Pattern::Pack(-1, vec![]), sub));
                        } else {
                            let vid = if let Value::Data(DataValue::Pack(id, _)) = iv {
                                id
                            } else {
                                return Err(format!("'{}' is not a constructor", name));
                            };
                            return Ok((Pattern::Pack(vid, pack_pats), sub));
                        }
                    }
                } 
                Err("Function in pattern was not an identifier".to_string())
            },
            Expr::List { els, tail } => {
                let mut el_pats = vec![];
                let mut sub = types::Subst::new();
                let mut t = t.clone();
                // if it's a list type, extract the type
                let el_type = if let types::Type::GenericNamed { path: _, name, args} = &t {
                    if args.len() == 1 && name == types::LIST {
                        args[0].clone()
                    } else {
                        return Err("Matched type was not a list".to_string());
                    }
                // if it's a variable, unify it with List<a>
                } else if let types::Type::Variable { .. } = &t {
                    let un = types::unify(&t, 
                        &types::Type::GenericNamed {
                            path: vec![],
                            name: types::LIST.to_string(), 
                            args: vec![ctx.anon_type_var()]
                        }, &ctx)?;
                    sub = types::compose_subst(&un, &sub);
                    t = types::subst(&t, &sub);
                    if let types::Type::GenericNamed { args, ..} = &t {
                        args[0].clone()
                    } else { unreachable!() }
                } else {
                    return Err("Matched type was not a list".to_string());
                };
                
                for e in els {
                    let (ep, es) = with_patvars(e, &el_type, ctx, n_patvars, only_types)?;
                    sub = types::compose_subst(&es, &sub);
                    el_pats.push(ep);
                }

                let tail_pat = if let Some(tl) = tail {
                    let (tp, ts) = with_patvars(tl, &t, ctx, n_patvars, only_types)?;
                    sub = types::compose_subst(&ts, &sub);
                    Some(tp)
                } else {
                    None
                };

                Ok((Pattern::List(el_pats, tail_pat.map(Box::new)), sub))
            },
            Expr::Tuple { els } if els.len() >= 2 => {
                let mut sub = types::Subst::new();
                let mut pack_pats = vec![];

                let un = types::unify(&t, &types::Type::Tuple { els: (0..els.len()).map(|_| ctx.anon_type_var()).collect() }, &ctx)?;
                sub = types::compose_subst(&un, &sub);
                let t = types::subst(&t, &sub);

                if let types::Type::Tuple { els: tels } = t {
                    if els.len() != tels.len() {
                        return Err(format!("Tuple in pattern had {} elements but type of pattern had {}", els.len(), tels.len()));
                    }
                    for (e, t) in els.iter().zip(tels.iter()) {
                        let (p, s) = with_patvars(e, t, ctx, n_patvars, only_types)?;
                        sub = types::compose_subst(&s, &sub);
                        pack_pats.push(p);
                    }
                } else {
                    return Err(format!("Tuple in pattern but type of pattern was not a tuple, but namely '{}'", t));
                }
                Ok((Pattern::Pack(0, if only_types {vec![]} else {pack_pats}), sub))
            },
            Expr::Tuple { els } if els.len() == 1 => {
                with_patvars(&els[0], t, ctx, n_patvars, only_types)
            },
            Expr::Tuple { els } if els.len() == 0 => {
                let un = types::unify(&t, &types::Type::Unit, &ctx)?;
                Ok((Pattern::Const(DataValue::Unit), un))
            },
            _ => Err("Invalid expression in a pattern".to_string())
        }
    }

    let mut n_patvars = 0;
    with_patvars(e, &types::instantiate(t.clone(), ctx).0, ctx, &mut n_patvars, only_types).map(|(p, s)| (p, n_patvars, s))
}


// func_signature and func_body are a way to reuse code between decl_func and decl_oper

fn func_signature<'a>(
    gparams: &Vec<(Option<parser::ClassExpr>, &str)>, 
    params: &Vec<(parser::TypeExpr, &'a str)>,
    has_self: bool,
    self_type: Option<(types::Type, &[Option<types::Class>], &[&str])>,
    ret_type: &parser::TypeExpr,
    ctx: &mut Context) 
    -> Result<(HashMap<String, i32>, Vec<types::Type>, Vec<(types::Type, &'a str)>, types::Type, types::Type, types::Type), String>
{
    let (gcs, mut gqs) : (Vec<_>, Vec<_>) = gparams.clone().into_iter().unzip();
    // convert the class expressions to classes
    let mut gcs = gcs.into_iter()
        .map(|c| -> Result<Option<types::Class>, String> {
            if let Some(ce) = c {
                Ok(Some(types::class_expr_to_class(&ce, &gqs)?))
            } else { Ok(None) }
        })
        .collect::<Result<Vec<_>, _>>()?;
    // extend with the quants and constraints passed in self_type
    if let Some((_, cs, qs)) = &self_type {
        gqs.extend(qs.iter());
        gcs.extend(cs.iter().map(|c| (*c).clone()));
    }

    // we need to go through constraints in gcs and add polymorphic types from foralls in classes to gqs
    for c in gcs.iter() {
        if let Some(mut c) = c.as_ref() {
            while let types::Class::Forall { constraint: _, quant: cq, t: ct } = c {
                gqs.push(&cq);
                c = (*ct).as_ref();
            }
        } 
    }

        
    // start with the return type
    let ret_type = type_expr_to_type(ret_type, &gqs)?;
    let mut func_type = ret_type.clone();
    
    // tparams stores the parameters' names and types in reverse order
    let mut tparams = vec![];
    // add every parameter to the type and store them to be added to the context later
    for p in params.iter().rev() {
        let p_type = type_expr_to_type(&p.0, &gqs)?;
        tparams.push((types::Type::Unit, p.1)); // we will add the type later, after instantiating
        func_type = types::Type::Function {
            arg: Box::new(p_type),
            ret: Box::new(func_type)
        };
    }
    // if we are in a type, and there is a self parameter,
    // we need to add it to the type
    if has_self {
        if let Some(self_type) = &self_type {
            tparams.push((types::Type::Unit, "self")); // we will add the type later, after instantiating
            func_type = types::Type::Function {
                arg: Box::new(self_type.0.clone()),
                ret: Box::new(func_type)
            };
        } else {
            // if there is a self parameter, but we are not in a type, return an error
            return Err("Standalone function may not have a 'self' parameter".to_string());
        }
    }
    // let tparams_len = tparams.len();
    // add the generic parameters as foralls
    for (c,q) in gcs.iter().zip(gqs.iter()) {
        if let Some(mut c) = c.clone() {
            let mut forall_depth = 0;
            while let types::Class::Forall { constraint: cc, quant: cq, t: ct } = c {
                func_type = types::Type::Forall {
                    constraint: cc.map(|cc| *cc),
                    quant: cq.to_string(),
                    t: Box::new(func_type)
                };
                c = *ct;
                forall_depth += 1;
            }
            // now we need to go back forall_depth times and add the quantifier
            let mut cur = &mut func_type;
            for _ in 0..forall_depth {
                if let types::Type::Forall { constraint: _, quant: _, t } = cur {
                    cur = t;
                } else {
                    unreachable!();
                }
            }
            *cur = types::Type::Forall {
                constraint: Some(c),
                quant: q.to_string(),
                t: Box::new(cur.clone())
            };
        } else {
            func_type = types::Type::Forall {
                constraint: None,
                quant: q.to_string(),
                t: Box::new(func_type)
            };
        }
    }
    // dbg_println!(0, "before inst func_type: {}", func_type);
    let (inst_func_type, qids) = types::instantiate(func_type.clone(), ctx);
    // dbg_println!(0, "after inst func_type: {}", inst_func_type);

    // when declaring class functions, we need to know on which generic parameter the class function depends
    // here we get the reulting ids of the generic parameters
    // and later, when we generalize, we'll create a HashMap<String, String> which will be 
    // be a map from the input generic parameters to the output generic parameters
    // qids.retain(|q, _| gqs.contains(&q.as_str()));

    // fill in the types in tparams, by walking the instantiated func_type
    let ret_type = {
        let mut cur = &inst_func_type; 
        for (t, _) in tparams.iter_mut().rev() {
            if let types::Type::Function { arg, ret } = cur {
                *t = (**arg).clone();
                cur = ret;
            } else { break; }
        }
        cur.clone()
    };

    let g_args_res = 
    gqs.into_iter()
        .map(|n| types::Type::Polymorphic { name: n.to_string(), id: 0 })
        .collect::<Vec<_>>(); 

    Ok((qids, g_args_res, tparams, inst_func_type, func_type, ret_type))
}


fn func_body<'a>(
    body: &Option<Box<parser::Expr>>,
    tparams: Vec<(types::Type, &'a str)>,
    ret_type: types::Type,
    inst_func_type: types::Type,
    rec_val: Rc<RefCell<Thunk>>,
    qids: HashMap<String, i32>,
    ctx: &mut Context
) -> Result<(Option<Value>, HashMap<i32, types::Type>, HashMap<String, String>), String> {
    // create a new context for the function body
    let (mut iv, mut it, is) = {
        if let Some(body) = body {
            let mut ctx = ctx.child();

            // add each of the parameters to the context
            for (pt, pn) in tparams.iter().rev() {
                ctx.add_typed_arg(&pn, pt.clone());
            }

            let (a,b,c) = expr_val(body, Some(&ret_type), &mut ctx)?;
            // println!("iv: {:?}", a);
            (Some(a), b, c)
        } else {
            (None, ctx.anon_type_var(), types::Subst::new())
        }
    };

    let tparams_len = tparams.len();

    // add the parameter types to the infered type
    for (t, _) in tparams.into_iter() {
        it = types::Type::Function {
            arg: Box::new(t),
            ret: Box::new(it)
        };
    }    

    let un = types::unify(&inst_func_type, &it, &ctx)?;

    let is = types::compose_subst(&is, &un);
    // fix the recursion placeholder
    if Rc::weak_count(&rec_val) > 1 /* this 1 is in the ctx's idents */ {
        if let Some(v) = iv {
            let new_thk = match v {
                Value::Thunk(thk) => thk,
                _ => Rc::new(RefCell::new(Thunk::Ret(v)))
            }; 
            std::mem::swap(&mut *(*rec_val).borrow_mut(), &mut *(*new_thk).borrow_mut());
            iv = Some(Value::Thunk(rec_val))
        }
    }

    // complete the function value
    if let Some(v) = iv {
        // since the child context with the params is dropped, we need to add the number of params 
        // to total args(which are now just the outside args)
        iv = Some(complete_lambda(v, ctx.total_args + tparams_len, ctx.total_args, 0)?);
    }
    
    ctx.apply_subst(&is);
    let mut sub = is;
    let qmap: HashMap<String, String>;
    if matches!(it, types::Type::Function { .. }) {
        let (_gen_type, gen_subst) 
            = types::generalise(
                &types::subst(&inst_func_type, &sub), 
                Some(&(qids.iter().map(|(s,i)| 
                    (match sub.get(i) { Some(types::Type::Variable { constraint: _, id }) => *id, _ => *i }, s.clone())
                ).collect::<HashMap<_,_>>()))
            );
        qmap = qids.into_iter().map(|(k, id)| {
            let rt = gen_subst.get(&id).or(Some(&types::Type::Unit)).unwrap();
            if let types::Type::Polymorphic { name, .. } = rt {
                (k, name.clone())
            } else {
                (k, "".to_string())
            }
        }).collect();
        // here we have to get what variables were replaced with what 
        // quantifiers and change the ClassFuncVars to ClassFuncPolys with those quantifiers
        sub = types::compose_subst(&gen_subst, &sub);

        
        if let Some(iv) = &mut iv {
            iv.generalise_class_funcs(&sub)?;
        }
        // it = gen_type;
    } else {
        // it = types::subst(&inst_func_type, &sub);
        qmap = HashMap::new();
    };


    return Ok((iv, sub, qmap))
}

//todo: rewrite this entire function because it's a mess
pub fn decl_func<'a>(st: &'a Sttm, ctx: &mut Context, self_type: Option<(types::Type, &[Option<types::Class>], &[&str])>) -> 
    Result<(types::Type, Option<Value>, types::Subst, &'a str, HashMap<String, String>, Vec<types::Type>), String> {
    let (name, gparams, has_self, params, ret_type, body) = match st {
        Sttm::FuncDecl { name, gparams, has_self, params, ret_type, body } => (name, gparams, has_self, params, ret_type, body),
        _ => return Err("Statement was not a function declaration".to_string())
    };
    let (qids, g_args_res, tparams, inst_func_type, func_type, ret_type) =
        func_signature(gparams, params, *has_self, self_type, ret_type, ctx)?;

    // create a dummy value for recursion purposes
    let rec_val = Rc::new(RefCell::new(Thunk::Ret(Value::Bottom)));
    ctx.add_ident(name, Some(Value::RecThunk(Rc::downgrade(&rec_val), vec![])), Some(inst_func_type.clone()));
    // let rec_val = Rc::new(RefCell::new(Thunk{args: vec![], f: ThunkFunc::Ret(DataValue::Bottom)}));
    // ctx.add_ident(name, Some(DataValue::RecThunk(Rc::downgrade(&rec_val))), Some(func_type.clone()));


    let (iv, sub, qmap) = 
        func_body(body, tparams, ret_type, inst_func_type, rec_val, qids, ctx)?;
    
    dbg_println!(1, "decl_func: {} : {} | {}", name, iv.as_ref().unwrap_or(&Value::Bottom), func_type);


    // we don't want to add it here, because we don't know the namespace,
    // and not all functions need to be added to the namespace
    // ctx.add_ident(name, iv.clone(), Some(it.clone()));

    return Ok((func_type, iv, sub, name, qmap, g_args_res));
}

pub fn decl_oper<'a>(st: &'a Sttm, ctx: &mut Context, self_type: Option<(types::Type, &[Option<types::Class>], &[&str])>) -> 
    Result<(bool, (Option<types::Type>, types::Type), types::Type, Option<Value>, types::Subst, HashMap<String, String>), String>
{
    let (oper, is_bin, prec, def) = match st {
        Sttm::OperDecl { 
            oper, left_arg, right_arg, oper_prec, def
        } => {
            if *left_arg && !right_arg {
                return Err("Postfix operators are not supported".to_string());
            }
            (oper, *left_arg && *right_arg, oper_prec, def)
        },
        _ => return Err("Statement was not an operator declaration".to_string())
    };

    if !matches!(prec, parser::OperPrec::None) {
        if !is_bin {
            return Err("Prefix operators cannot have a precedence declaration".to_string());
        }
        ctx.add_oper_prec(oper,prec)?;
    }

    let (gparams, has_self, params, ret_type, body) = if let Some(parser::OperDef {
        gparams, 
        has_self,
        params,
        ret_type,
        body
    }) = def { (gparams, has_self, params, ret_type, body) } else {
        return Err("Operator declaration must have a definition".to_string());
    };

    let has_args = params.len() + if *has_self { 1 } else { 0 };
    if is_bin && has_args != 2 {
        return Err("Binary operator must have exactly two arguments".to_string());
    } 
    if !is_bin && has_args != 1 {
        return Err("Prefix operator must have exactly one argument".to_string());
    }

    let (qids, _g_args_res, tparams, inst_func_type, func_type, ret_type) =
    func_signature(gparams, params, *has_self, self_type, ret_type, ctx)?;

    let mut lt = None;
    let rt;
    let rec_val = Rc::new(RefCell::new(Thunk::Ret(Value::Bottom)));
    if is_bin {
        let mut args_type = func_type.clone();
        let mut cur = &mut args_type;
        while let types::Type::Forall { t, .. } = cur {
            cur = t;
        }
        let (mut l, mut r) = if let types::Type::Function { arg, ret } = cur {
            // let arg = *arg.to_owned();
            // let ret = *ret.to_owned();
            // *cur = arg;
            let l = *arg.to_owned();
            if let types::Type::Function { arg, .. } = *ret.to_owned() {
                (l, *arg.to_owned())
            } else {
                return Err("Binary operator must have exactly two arguments".to_string());
            }
        } else {
            return Err("Binary operator must have exactly two arguments".to_string());
        };
        *cur = l;
        l = args_type;

        let mut new_r = l.clone();
        cur = &mut new_r;
        while let types::Type::Forall { t, .. } = cur {
            cur = t;
        }
        *cur = r;
        r = new_r;

        rt = r.clone();
        lt = Some(l.clone());
        ctx.add_bin_op(oper, l, r, func_type.clone(), Value::RecThunk(Rc::downgrade(&rec_val), vec![]))?;
    } else {
        let mut arg_type = func_type.clone();
        let mut cur = &mut arg_type;
        while let types::Type::Forall { t, .. } = cur {
            cur = t;
        }
        if let types::Type::Function { arg, .. } = cur {
            *cur = *arg.to_owned();
        } else {
            return Err("Prefix operator must have exactly one argument".to_string());
        }

        rt = arg_type.clone();
        ctx.add_un_op(oper, arg_type, func_type.clone(), Value::RecThunk(Rc::downgrade(&rec_val), vec![]))?;
    }

    let (iv, sub, qmap) = 
        func_body(body, tparams, ret_type, inst_func_type, rec_val, qids, ctx)?;

    // remove the temporary recusion operator
    if is_bin {
        ctx.pop_bin_op(oper)?;
    } else {
        ctx.pop_un_op(oper)?;
    }
    dbg_println!(1, "decl_oper: {} : {} | {}", oper, iv.as_ref().unwrap_or(&Value::Bottom), func_type);

    return Ok((is_bin,(lt, rt), func_type, iv, sub, qmap));
}

pub fn decl_class(st: &Sttm, ctx: &mut Context) -> Result<String, String> {
    let (name, gparams, inst_t, funcs) = match st {
        Sttm::ClassDecl { name, gparams, inst_t, funcs } => (name, gparams, inst_t, funcs),
        _ => return Err("Statement was not a class declaration".to_string())
    };

    let mut ctx = ctx.child();

    let (inst_tname, _) = match inst_t {
        TypeExpr::Named { name, .. } => (name, None),
        TypeExpr::Generic { name, args, .. } => (name, Some(args)),
        _ => return Err("Invalid type in class declaration".to_string()) 
    };

    // create a type for the instance of the class
    let inst_typ = match inst_t {
        TypeExpr::Named { name, .. } => types::Type::Polymorphic { name: name.to_string(), id: 0 },
        TypeExpr::Generic { name, args, .. } => {
            // map the args from type expressions to types
            let args = args.iter().map(|a| 
                if let TypeExpr::Named { name, .. } = a {
                    types::Type::Polymorphic { name: name.to_string(), id: 0 }
                } else { panic!("Invalid type in class declaration") }
            ).collect::<Vec<_>>();
            types::Type::GenericPolymorphic { name: name.to_string(), args, id: 0 }
        },
        _ => return Err("Invalid type in class declaration".to_string()) 
    };
    // let inst_typ_wo_foralls = inst_typ.clone();
    // inst_typ = types::Type::Forall {
    //     constraint: None, // later when we add constraints to classes we will change this
    //     quant: tname.to_string(),
    //     t: Box::new(inst_typ)
    // };

    // create the basis for the class spec
    let cls = if gparams.len() > 0 {
        let mut cls = types::Class::Generic { path: vec![], name: name.to_string(), 
            args: gparams.iter().map(|(_, n)| types::Type::Polymorphic { name: n.to_string(), id: 0 }).collect()
        };
        for (c,n) in gparams.iter() {
            let cnst = if let Some(c) = c {
                Some(Box::new(types::class_expr_to_class(&c, &[])?))
            } else { None };
            cls = types::Class::Forall {
                constraint: cnst,
                quant: n.to_string(),
                t: Box::new(cls)
            };
        }
        cls
    } else {
        types::Class::Named { path: vec![], name: name.to_string() }
    };

    // let (gcs, mut gqs) : (Vec<_>, Vec<_>) = gparams.clone().into_iter().unzip();
    // // convert the class expressions to classes
    // let mut gcs = gcs.into_iter()
    //     .map(|c| -> Result<Option<types::Class>, String> {
    //         if let Some(ce) = c {
    //             Ok(Some(types::class_expr_to_class(&ce, &gqs)?))
    //         } else { Ok(None) }
    //     })
    //     .collect::<Result<Vec<_>, _>>()?;

    // add the constraint that tname is an instance of the class
    // gcs.push(Some(cls.clone()));
    // gqs.push(&tname);
    let mut func_gcs = vec![Some(cls.clone())];
    let mut func_gqs = vec![*inst_tname];

    // add the args of the instance type if it is generic
    if let types::Type::GenericPolymorphic { args, .. } = &inst_typ {
        for a in args.iter() {
            if let types::Type::Polymorphic { name, .. } = a {
                func_gcs.push(None);
                func_gqs.push(name);
            } else { unreachable!() }
        }
    }

    // ctx.into_namespace(name.to_string());

    // create types and values (if they have) for each of the functions
    let spec_funcs = {
        let mut fs = HashMap::new();
        for sttm in funcs {
            match sttm {
                Sttm::FuncDecl { name, has_self, .. } => {
                    let (typ, val, _, _, qmap, _) = 
                        decl_func(sttm, &mut ctx, Some((inst_typ.clone(), &func_gcs, &func_gqs)))?;
                    fs.insert(name.to_string(), (typ, val, qmap.get(&inst_tname.to_string()).unwrap().clone(), *has_self));
                }
                Sttm::OperDecl { oper, .. } => {
                    let (is_bin, (lt, rt), typ, val, _, qmap) = 
                        decl_oper(sttm, &mut ctx, Some((inst_typ.clone(), &func_gcs, &func_gqs)))?;
                    // this is a terrible hack, but since identifiers can't start with a number, we 
                    // have a way to distinguish operators and functions without introducing a new map
                    // the number also helps us distinguish between unary and binary operators
                    let name = (if is_bin { "2" } else { "1" }).to_string() + oper;
                    fs.insert(name.clone(), (typ.clone(), val.clone(), qmap.get(&inst_tname.to_string()).unwrap().clone(), false));

                    let v = if let Some(v) = val { v } else {
                        Value::ClassFuncN(cls.clone(), name, qmap.get(&inst_tname.to_string()).unwrap().clone())
                    };

                    if is_bin {
                        ctx.add_bin_op(oper, lt.unwrap(), rt, typ, v)?;
                    } else {
                        ctx.add_un_op(oper, rt, typ.clone(), v)?;
                    }
                }
                _ => return Err("Statement in class was not a function or operator declaration".to_string())
            }
        }
        fs
    };

    let spec = types::ClassSpec {
        basis: cls.clone(),
        funcs: spec_funcs,
    };

    let spec_gargs = gparams.iter().map(|(_, n)| types::Type::Polymorphic { name: n.to_string(), id: 0 }).collect::<Vec<_>>();
    ctx.add_class_spec(name.to_string(), spec_gargs, spec);

    ctx.move_up();

    Ok(name.to_string())
}

pub fn decl_impl(st: &Sttm, ctx: &mut Context) -> Result<(), String> {
    let (gparams, class, inst, funcs) = match st {
        Sttm::Impl { gparams, class, inst, funcs } => (gparams, class, inst, funcs),
        _ => return Err("Statement was not an implementation declaration".to_string())
    };

    let (gcs, gqs) : (Vec<_>, Vec<_>) = gparams.clone().into_iter().unzip();
    // convert the class expressions to classes
    let gcs = gcs.into_iter()
        .map(|c| -> Result<Option<types::Class>, String> {
            if let Some(ce) = c {
                Ok(Some(types::class_expr_to_class(&ce, &gqs)?))
            } else { Ok(None) }
        })
        .collect::<Result<Vec<_>, _>>()?;

    // get the class for which we are implementing
    let cls = types::class_expr_to_class(class, &gqs)?;

    // get the type of the instance
    let mut inst_typ = types::type_expr_to_type(inst, &gqs)?;
    
    let inst_typ_wo_foralls = inst_typ.clone();

    // now add foralls to the instance type
    for (q, c) in gqs.iter().zip(gcs.iter()) {
        inst_typ = types::Type::Forall {
            constraint: c.clone(),
            quant: q.to_string(),
            t: Box::new(inst_typ)
        };
    }
    // and instantinate it
    let g_inst_typ = inst_typ.clone();

    (inst_typ, _) = types::instantiate(inst_typ, ctx);

    // dbg_println!(0, "decl_impl: {} : {}", cls, inst_typ);
    // add the type as a temp instance of the class now, so when unifying, it fulfils the constraint that is the current class
    ctx.add_tmp_class_instance(&cls, &inst_typ)?;

    // get the functions we need to implement
    let mut spec_funcs = ctx.get_class_funcs(&cls, &inst_typ, &gcs, &gqs)?;
    
    let mut inst_funcs = HashMap::new();
    
    // ctx.into_namespace(inst_typ.clone());
    
    // go through each declared function and make sure it matches the spec
    for sttm in funcs {
        match sttm {
            Sttm::FuncDecl { name, has_self, .. } => {
                let (typ, val, _, _fname, _, _) = decl_func(sttm, ctx, Some((inst_typ_wo_foralls.clone(), gcs.as_slice(), gqs.as_slice())))?;
                
                let val = val.ok_or_else(|| format!("Function '{}' in class implementation must have a body", name))?;
                
                let (spec_typ, _, spec_has_self) = if let Some(sp) = spec_funcs.get(*name) { sp } else {
                    return Err(format!("Class {} does not have a function named {}", cls, name));
                };
                match (has_self, spec_has_self) {
                    (true, false) => 
                        return Err(format!("Class function '{}::{}' has self parameter, but implementation for instance {} doesn't", cls, name, inst_typ)), 
                    (false, true) => 
                        return Err(format!("Class function '{}::{}' doesn't have self parameter, but implementation for instance {} does", cls, name, inst_typ)),
                    _ => ()
                }
                let (spec_typ, _) = types::instantiate(spec_typ.clone(), ctx);
                let (inst_typ, _) = types::instantiate(typ.clone(), ctx);
                // make sure the types unify
                types::unify(&spec_typ, &inst_typ, &ctx)?;
                spec_funcs.remove(*name);
                inst_funcs.insert(name.to_string(),(typ, val, *has_self));
            }
            Sttm::OperDecl { oper, .. } => {
                let (is_bin, (lt, rt), typ, val, _, _) = 
                        decl_oper(sttm, ctx, Some((inst_typ_wo_foralls.clone(), gcs.as_slice(), gqs.as_slice())))?;

                let val = val.ok_or_else(|| format!("Operator '{}' in class implementation must have a body", oper))?;

                let name = (if is_bin { "2" } else { "1" }).to_string() + oper;
                let (spec_typ, _, _) = if let Some(sp) = spec_funcs.get(&name) { sp } else {
                    return Err(format!("Class {} does not have an operator named {}", cls, name));
                };
                if is_bin {
                    ctx.add_bin_op(oper, lt.unwrap(), rt, typ.clone(), val.clone())?;
                } else {
                    ctx.add_un_op(oper, rt, typ.clone(), val.clone())?;
                }

                let (spec_typ, _) = types::instantiate(spec_typ.clone(), ctx);
                let (inst_typ, _) = types::instantiate(typ.clone(), ctx);
                // make sure the types unify
                types::unify(&spec_typ, &inst_typ, &ctx)?;
                spec_funcs.remove(&name);
                inst_funcs.insert(name.to_string(),(typ, val, false));
            }
            _ => return Err("Statement in implementation was not a function or operator declaration".to_string())
        }
    }

    // later we'll add all remaining funcs and methods if they have a default implementation
    // but for now, simply check that there are no remaining funcs or methods
    if !spec_funcs.is_empty() {
        let mut missing_str = String::new();
        for (name, _) in spec_funcs {
            missing_str.push_str(&format!("{}, ", name));
        }
        return Err(format!("Instance '{}' of class '{}' does not implement {}", inst_typ, cls, missing_str));
    }

    ctx.pop_tmp_class_instance();
    ctx.add_class_instance(cls, g_inst_typ, inst_funcs)?;

    Ok(())
}

pub fn decl_type(te: &TypeExpr, ctx: &mut Context, only_types: bool) -> Result<String, String> {
    fn with_poly<'a>(te: &TypeExpr, ctx: &mut Context, only_types: bool, cs: &Vec<Option<types::Class>>, qs: &Vec<&'a str>) -> Result<String, String>
    {
        match te {
            TypeExpr::Forall { constraint, quant, t } => {
                let mut qs = qs.clone();
                qs.push(quant);

                let mut cs = cs.clone();
                let cnst = if let Some(ce) = constraint {
                    Some(types::class_expr_to_class(ce, &qs)?)
                } else { None };
                cs.push(cnst);

                with_poly(t, ctx, only_types, &cs, &qs)
            },
            TypeExpr::Variant { name, variants, funcs } => {
                let t_name = name.map(str::to_string)
                    .unwrap_or_else(|| format!("anon_variant{}#", ctx.anon_type_id()));

                let mut ctx = ctx.child();

                let mut spec_variants = vec![];

                let mut typ = if qs.len() == 0 {
                    types::Type::Named{ path: vec![], name: t_name.to_string() }
                } else {
                    types::Type::GenericNamed { 
                        path: vec![],
                        name: t_name.to_string(), 
                        args: qs.iter()
                            .map(|n| types::Type::Polymorphic{ name: n.to_string(), id: 0 })
                            .collect()
                    }
                };

                for ((v_name, els), i) in variants.iter().zip(0..) {
                    let val = if !only_types {
                        Value::Data(DataValue::Pack(i, (0..els.len()).map(|n| Value::Arg(n)).collect() ))
                    } else { Value::Bottom };

                    let mut typ = typ.clone();
                    // make the constructor function type
                    for el in els.iter().rev() {
                        if let TypeExpr::Named { path: _, name: el_name } = el {
                            if qs.contains(el_name) {
                                typ = types::Type::Function { 
                                    arg: Box::new(types::Type::Polymorphic { name: el_name.to_string(), id: 0 }), 
                                    ret: Box::new(typ) 
                                };
                                continue;
                            }
                        }
                        typ = types::Type::Function { arg: Box::new(types::type_expr_to_type(el, &qs)?), ret: Box::new(typ) };
                    }
                    // now add foralls to the type of the constructor
                    for (c, q) in cs.iter().zip(qs.iter()) {
                        typ = types::Type::Forall { 
                            constraint: c.clone(),
                            quant: q.to_string(), 
                            t: Box::new(typ) 
                        };
                    }

                    // ctx.add_ident(v_name, Some(val), Some(typ));
                    spec_variants.push((v_name.to_string(), typ.clone(), val));
                }

                let typ_wo_foralls = typ.clone();
                // add foralls to the type of the variant
                for (c, q) in cs.iter().zip(qs.iter()) {
                    typ = types::Type::Forall { constraint: c.clone(), quant: q.to_string(), t: Box::new(typ) };
                }

                let spec = types::TypeSpec {
                    basis: typ.clone(),
                    data: types::TypeSpecData::Variant(spec_variants)
                };


                let spec_gargs = qs.iter().map(|n| types::Type::Polymorphic { name: n.to_string(), id: 0 }).collect::<Vec<_>>();
                ctx.add_type_spec(t_name.clone(), spec_gargs, spec);

                ctx.into_namespace(t_name.clone());

                // add functions and methods
                for fsttm in funcs {
                    if let Sttm::FuncDecl { has_self, name, .. } = fsttm {
                        let r = decl_func(fsttm, &mut ctx, Some((typ_wo_foralls.clone(), cs.as_slice(), qs.as_slice())))?;
                        if r.1.is_none() {
                            return Err(format!("Function '{}' in variant '{}' has no body.", name, t_name));
                        }
                        ctx.add_func_to_cur_namespace(name, r.5, r.0, r.1.unwrap(), *has_self,);
                    }
                }

                // move the new identifiers into the parent context
                ctx.move_up();

                Ok(t_name.to_string())
            },
            TypeExpr::Struct { name, fields, funcs } => {
                let t_name = name.map(str::to_string)
                    .unwrap_or_else(|| format!("anon_struct{}#", ctx.anon_type_id()));

                let mut ctx = ctx.child();

                let mut typ = if qs.len() == 0 {
                    types::Type::Named{ path: vec![], name: t_name.to_string() }
                } else {
                    types::Type::GenericNamed { 
                        path: vec![],
                        name: t_name.to_string(), 
                        args: qs.iter()
                            .map(|a| types::Type::Polymorphic{ name: a.to_string(), id: 0 })
                            .collect()
                    }
                };

                let field_names = fields.iter().map(
                    |(f_typ_e,f_name)| -> Result<_, String> {
                        let ftyp = types::type_expr_to_type(&f_typ_e, &qs)?;
                        Ok((ftyp, f_name.to_string()))
                    }
                ).collect::<Result<Vec<_>, _>>()?;

                let typ_wo_foralls = typ.clone();
                // add foralls to the type of the struct
                for (c, q) in cs.iter().zip(qs.iter()) {
                    typ = types::Type::Forall { constraint: c.clone(), quant: q.to_string(), t: Box::new(typ) };
                }

                let spec = types::TypeSpec {
                    basis: typ.clone(),
                    data: types::TypeSpecData::Struct(field_names)
                };
                let spec_gargs = qs.iter().map(|n| types::Type::Polymorphic { name: n.to_string(), id: 0 }).collect::<Vec<_>>();
                ctx.add_type_spec(t_name.clone(), spec_gargs, spec);

                ctx.into_namespace(t_name.clone());

                // add functions and methods
                for fsttm in funcs {
                    if let Sttm::FuncDecl { has_self, name,  .. } = fsttm {
                        let r = decl_func(fsttm, &mut ctx, Some((typ_wo_foralls.clone(), cs.as_slice(), qs.as_slice())))?;
                        if r.1.is_none() {
                            return Err(format!("Function '{}' in struct '{}' has no body.", name, t_name));
                        }
                        ctx.add_func_to_cur_namespace(name, r.5, r.0, r.1.unwrap(), *has_self);
                    }
                }

                // move the new identifiers into the parent context
                ctx.move_up();

                Ok(t_name.to_string())
            },
            _ => Err("Type Expression was not a declaration".to_string())
        }
    }

    with_poly(te, ctx, only_types, &vec![], &vec![])
}

// returns None if statement was not processed
pub fn do_sttm(st: &Sttm, ctx: &mut Context, self_type: Option<(types::Type, &[Option<types::Class>], &[&str])>) -> Option<Result<types::Subst, String>> {
    match st {
        Sttm::Empty => Some(Ok(types::Subst::new())),
        Sttm::Expr { expr } => {
            // even though we don't care about the expression,
            // since there aren't any side-effects (yet),
            // we still have to check for errors
            let r = expr_val(expr, None, ctx);
            if r.is_err() { return Some(Err(r.err().unwrap())); }
            Some(Ok(r.unwrap().2))
        },
        Sttm::Decl { t: _, left, right } => {
            let right = if right.is_some() { right.as_ref().unwrap() } else { 
                // for now:
                return Some(Err("Declaration must have a right hand side".to_string()));
            };
            let name = match &**left {
                Expr::Identifier { path, name ,generic_args} 
                    if path.len() == 0 && generic_args.len() == 0 => name,
                // for now:
                _ => return Some(Err("Declaration left hand side must be an identifier".to_string()))
            };
            let rec_typ = ctx.anon_type_var();
            // add the identifier to the context as a recursion placeholder
            let rec_val = Rc::new(RefCell::new(Thunk::Ret(Value::Bottom)));
            ctx.add_ident(name, Some(Value::RecThunk(Rc::downgrade(&rec_val), vec![])), Some(rec_typ.clone()));

            let r = expr_val(right, None, ctx);
            if r.is_err() { return Some(Err(r.err().unwrap())); }
            let (mut iv, mut it, is) = r.unwrap();
            
            let un = types::unify(&rec_typ, &it, &ctx);
            if un.is_err() { return Some(Err(un.err().unwrap())); }
            let un = un.unwrap();

            let is = types::compose_subst(&is, &un);
            // fix the recursion placeholder
            if Rc::weak_count(&rec_val) > 1 /* this 1 is in the ctx's idents */ {
                let new_thk = match iv {
                    Value::Thunk(thk) => thk,
                    _ => Rc::new(RefCell::new(Thunk::Ret(iv)))
                }; 
                std::mem::swap(&mut *(*rec_val).borrow_mut(), &mut *(*new_thk).borrow_mut());
                iv = Value::Thunk(rec_val)
            }

            // if let Some(t) = t {
            // 	// check that t unifies with what we inferred
            // }
            ctx.apply_subst(&is);
            let sub = is;
            if matches!(it, types::Type::Function { .. }) {
                let (gen_type, gen_subst) 
                    = types::generalise(&types::subst(&rec_typ, &sub), None);
                // sub = types::compose_subst(&gen_subst, &sub);
                it = gen_type;
                if let Err(e) = iv.generalise_class_funcs(&gen_subst) {
                    return Some(Err(e));
                }
            } else {
                it = types::subst(&rec_typ, &sub);
            };

            // sub = types::compose_subst(&is, &sub);
            ctx.add_ident(name, Some(iv), Some(it));
            Some(Ok(sub))
        },
        Sttm::TypeDecl { type_expr } => {
            let r = decl_type(&type_expr, ctx, false);
            if r.is_err() { return Some(Err(r.unwrap_err())); }
            Some(Ok(types::Subst::new()))
        },
        Sttm::FuncDecl { has_self, .. } => {   
            let r = decl_func(st, ctx, self_type);
            if r.is_err() { return Some(Err(r.err().unwrap())); }
            let (typ, val, sub, fname, _, gargs) = r.unwrap();
            if val.is_none() {
                return Some(Err("Function declaration must have a body".to_string()));
            }
            ctx.add_func_to_cur_namespace(fname, gargs, typ.clone(), val.clone().unwrap(), *has_self);
            ctx.add_ident(fname, val, Some(typ));
            Some(Ok(sub))
        },
        Sttm::OperDecl { oper, .. } => {
            let r = decl_oper(st, ctx, self_type);
            if r.is_err() { return Some(Err(r.err().unwrap())); }
            let (is_bin,(lt, rt), typ, val, sub, _) = r.unwrap();
            if val.is_none() {
                return Some(Err("Operator declaration must have a body".to_string()));
            }
            if is_bin {
                if let Err(err) = ctx.add_bin_op(oper, lt.unwrap(), rt, typ, val.unwrap()) {
                    return Some(Err(err));
                }
            } else {
                if let Err(err) = ctx.add_un_op(oper, rt, typ, val.unwrap()) {
                    return Some(Err(err));
                }
            }
            Some(Ok(sub))
        }
        Sttm::ClassDecl { .. } => {
            let r = decl_class(st, ctx);
            if r.is_err() { return Some(Err(r.err().unwrap())); }
            Some(Ok(types::Subst::new()))
        },
        Sttm::Impl { .. } => {
            let r = decl_impl(st, ctx);
            if r.is_err() { return Some(Err(r.err().unwrap())); }
            Some(Ok(types::Subst::new()))
        },
        _ => return None
    }
}

pub fn execute(input: &str, ctx: &mut Context) -> Result<(), String> {
    let input = input.trim();
    let (sts, rest) = crate::parser::sttms(input)?;
    if rest.len() > 0 {
        return Err(format!("Part of input not parsed: '{}'", rest));
    }

    for st in sts {
        let r = do_sttm(&st, ctx, None);
        if r.is_none() { Err("Could not process statement".to_string())?; }
        let r = r.unwrap();
        if r.is_err() { return Err(r.unwrap_err()); }
    }
    Ok(())
}

// --------------------------


pub fn literal(kind: LiteralKind, val: &str) -> Result<(DataValue, types::Type), String> {
    use types::{INT, BOOL, CHAR, LIST, LIST_NIL, LIST_CONS};
    match kind {
        LiteralKind::Number => {
            let v = val.parse().or(Err(format!("Could not parse '{}' as int", val)))?;
            Ok((DataValue::Int(v), types::Type::Named{ path: vec![], name: format!("{INT}")}))
        },
        LiteralKind::Boolean => {
            let v = if val == "true" { true } else if val == "false" { false } else {
                return Err(format!("Could not parse '{}' as bool", val));
            };
            Ok((DataValue::Bool(v), types::Type::Named{ path: vec![], name: format!("{BOOL}")}))
        },
        LiteralKind::Character => {
            let v = val.chars().next().unwrap(); // this doesn't handle backslash escapes
            Ok((DataValue::Char(v), types::Type::Named{ path: vec![], name: format!("{CHAR}")}))
        }
        LiteralKind::String => {
            let mut v = DataValue::Pack(LIST_NIL, vec![]);
            for c in val.chars().rev() {
                v = DataValue::Pack(LIST_CONS, vec![Value::Data(DataValue::Char(c)), Value::Data(v)]);
            }
            Ok((v, 
                types::Type::GenericNamed { path: vec![], name: LIST.to_string(), args: vec![
                    types::Type::Named{ path: vec![], name: format!("{CHAR}")}
                ] }
            ))
        },
        // _ => Err(format!("Literal kind not implemented: '{}'", val))
    }
}

// internal implementation of expr_value, without generalizing and cannonizing the resulting type
fn expr_val(e: &Expr, extyp: Option<&types::Type>, ctx: &mut Context) -> Result<(Value, types::Type, types::Subst), String> {
    match e {
        Expr::Literal { kind, val } => {
            literal(*kind, val).map(|(v, t)| (Value::Data(v), t, types::Subst::new()))
        },
        Expr::Identifier { path, name, generic_args} => {
            let path = types::path_expr_to_path(path)?;
            let g_args = 
                generic_args.iter()
                .map(|t| type_expr_to_type(t, &[]))
                .collect::<Result<Vec<_>, _>>()?;
            let (mut v, t) = ctx.ident(&path, name, &g_args, extyp)?;
            dbg_println!(1, "Got ident {}: {} | {}", name, v, t);
            let t = t.clone();
            let (new_t, tvs) = types::instantiate(t, ctx);
            v.instantiate_class_funcs(&tvs)?;
            Ok((v, new_t, types::Subst::new()))
        },
        Expr::Access { expr, generic_args, member } => {
            let (v, t, s) = expr_val(expr, None, ctx)?;
            let g_args = 
                generic_args.iter()
                .map(|t| type_expr_to_type(t, &[]))
                .collect::<Result<Vec<_>, _>>()?;

            let (mt, mg) = ctx.member_of(&t, &member, &g_args)?; 

            if let MemberGet::Field(mid) = mg {
                Ok((Value::Thunk(Rc::new(RefCell::new(Thunk::Unpack(v, mid)))), mt, s))
            } else 
            if let MemberGet::Method(mthd) = mg {
                dbg_println!(1, "Got member method {}: {} | {}", member, mthd, mt);
                let (new_mt, new_s) = types::call_type(mt, &t, s, ctx)?;
                let new_v = call_value(mthd, vec![v])?;
                return Ok((new_v, new_mt, new_s));
            } else {
                Err("???".to_string())
            }
        },
        Expr::Unary { op, expr } => {
            let op = *op;
            let e = &**expr;
            
            let (ev, et, es) = expr_val(e, None, ctx)?;

            let (mut ov, ot) = if op.starts_with('`') {
                Some(ctx.ident(&vec![], &op[1..], &vec![], None)?)
            } else {
                if let Some((ov, ot)) = ctx.un_op(op, &et) {
                    dbg_println!(1, "Got unary oper {}: {} | {}", op, ov, ot);
                    Some((ot, ov))
                } else { None }
            }.ok_or(format!("Undefined unary operator '{}'", op))?.clone();

            
            let (res_t, res_sub) = types::call_type(ot.clone(), &et, es, ctx)?;
            dbg_println!(2, "unary before apply: {} | {}", &ov, &ev);
            // let v = apply_to_closure(ov, &ev, 0);

            ov.resolve_class_funcs(&res_sub, ctx)?;

            let v = call_value(ov, vec![ev])?;
            
            
            Ok((v, res_t, res_sub))
        },
        Expr::Binary { .. } => {
            fn handle_binary<'a>(e: &'a Expr, extyp: Option<&types::Type>, ctx: &mut Context, precedence: i32) -> 
                Result<(Value, types::Type, types::Subst, Option<(Option<(types::Type, Value)>, &'a str, i32, &'a Expr<'a>)>), String> 
            {
                if let Expr::Binary { left, op, right } = e {
                    let left = &**left;
                    let op = *op;
                    let right = &**right;

                    
                    // get the operator
                    let (prec, optv) = if op.starts_with('`') {
                        let (iv, it) = ctx.ident(&vec![], &op[1..], &vec![], None)?;
                        Some((BinOpPrec { precedence: 0, right_assoc: true }, Some((it, iv))))
                    } else {
                        ctx.bin_op_prec(op).map(|p| (p, None))
                    }.ok_or(format!("Undefined binary operator '{}'", op))?;
                    
                    // get the left expression
                    let (lv, lt, ls) = expr_val(left, 
                        // types::next_param(&op.t).as_ref(), 
                        optv.as_ref().and_then(|(t,_)| types::next_param(t)).as_ref(),
                        ctx)?;
                    
                    if prec.precedence > precedence || (prec.precedence == precedence && prec.right_assoc)
                    {
                        let mut lv = lv;
                        let mut lt = lt;
                        let mut sub = ls;
                        let mut optv = optv;
                        let mut rest = right;
                        let mut prec = prec.precedence;
                        let mut oper_name = op;

                        loop {
                            let op_rest = if let Some((ot, ov)) = optv {
                                let (new_lt, mut new_sub) = types::call_type(ot, &lt, sub, ctx)?;
                                
                                let (rv, rt, rs, op_rest) 
                                    = handle_binary(rest, types::next_param(&new_lt).as_ref(), ctx, prec)?;
                                new_sub = types::compose_subst(&rs, &new_sub);
                                
                                let (new_lt, new_sub) = types::call_type(new_lt, &rt, new_sub, ctx)?;
                                lt = new_lt;
                                sub = new_sub;
                                
                                // lv = Value::Closure(apply_n_to_closure(ov, &[lv, rv], 0));
                                lv = call_value(ov, vec![lv, rv])?;

                                op_rest
                            } else {
                                let (rv, rt, rs, op_rest) 
                                    = handle_binary(rest, None, ctx, prec)?;
                                let (ot, mut ov) = if let Some((_,t,v)) = ctx.bin_op(oper_name, &lt, &rt) { (t,v) } else {
                                    return Err(format!("Undefined binary operator '{}' for types {} and {}", oper_name, lt, rt));
                                };
                                dbg_println!(1, "Got binary oper {}: {} | {}", op, ov, ot);

                                let (new_lt, mut new_sub) = types::call_type(ot, &lt, sub, ctx)?;
                                new_sub = types::compose_subst(&rs, &new_sub);

                                let (new_lt, new_sub) = types::call_type(new_lt, &rt, new_sub, ctx)?;
                                lt = new_lt;
                                sub = new_sub;

                                ov.resolve_class_funcs(&sub, ctx)?;

                                // lv = Value::Closure(apply_n_to_closure(ov, &[lv, rv], 0));
                                lv = call_value(ov, vec![lv, rv])?;

                                op_rest
                            };

                            if let Some((new_tv, new_oper_name, new_prec, new_rest)) = op_rest {
                                optv = new_tv;
                                oper_name = new_oper_name;
                                prec = new_prec;
                                rest = new_rest;
                            } else {
                                break;
                            }
                        }

                        return Ok((lv, lt, sub, None));
                    } else {
                        return Ok((lv, lt, ls, Some((optv, op, prec.precedence, right))));
                    }

                } else {
                    let evr = expr_val(e, extyp, ctx)?;
                    return Ok((evr.0, evr.1, evr.2, None));
                }
            }

            let (v, t,s, ..) = handle_binary(e, extyp, ctx, -1)?;
            Ok((v, t, s))
        },
        Expr::Conditional { cond, true_branch, false_branch} => {
            let cond = &**cond;
            let true_brach = &**true_branch;
            let false_branch = false_branch.as_ref().map(|x| &**x);

            let (cond_v, cond_t, cond_s) = 
                expr_val(cond, Some(&types::Type::Named { path: vec![], name: types::BOOL.to_string() }), ctx)?;

            let (true_v, true_t, true_s) = 
                expr_val(true_brach, extyp, ctx)?;

            let fls = if let Some(false_branch) = false_branch {
                Some(expr_val(false_branch, Some(&true_t), ctx)?)
            } else {
                None
            };

            let (false_v, fb) = if let Some((v, t, s)) = fls {
                (v, Some((t,s)))
            } else {
                (Value::Data(DataValue::Unit), None)
            };

            let (ret_t, ret_sub) 
                = types::if_type(
                    (cond_t, cond_s),
                    (true_t, true_s), 
                    fb, &ctx
                )?;

            let ret_v = Value::Thunk(Rc::new(RefCell::new(Thunk::If(cond_v, true_v, false_v))));
            
            Ok((ret_v, ret_t, ret_sub))
        },
        Expr::Lambda { params, body } => {
            let params = &**params;
            let body = &**body;

            let mut tmp_vec: Vec<Expr> = vec![];
            let param_iter = match params {
                Expr::Tuple { els } => els.iter(),
                a => {tmp_vec.push(a.clone()); tmp_vec.iter() }
            };

            let mut ctx = ctx.child();
            
            let mut param_types = Vec::new();
            let mut args = Vec::new();

            //(x->{let f = y->y*x; =f(2);})(3)
            for e in param_iter {
                let name = match e {
                    Expr::Identifier { path, name, generic_args } 
                        if path.len() == 0 && generic_args.len() == 0 => name,
                    _ => return Err("Lambda parameters must be identifiers".to_string())
                };
                let (v, t) = ctx.add_arg(name);
                args.push(v);
                param_types.push(t);
            }
            
            let (mut v, mut typ, mut sub) = expr_val(body, None, &mut ctx)?;
            
            // dbg_println!(2, "lambda before make type: {} | params: {:?}", &typ, &param_types);
            for t in param_types.iter().rev() {
                let (new_typ, new_sub) = types::make_lambda(t, typ, sub)?;
                typ = new_typ;
                sub = new_sub;
            }
            
            // dbg_println!(2, "lambda before complete: {} | type: {}", &v, &typ);
            dbg_println!(2, "lambda before complete: {}", &v);
            v = complete_lambda(v, ctx.total_args, ctx.total_args - args.len(), 0)?;
            dbg_println!(2, "lambda after complete: {}", &v);

            Ok((v, typ, sub))
        },
        Expr::Call { func, args } => {
            let func = &**func;
            let args = &**args;

            let mut tmp_vec: Vec<Expr> = vec![];
            let args_iter = match args {
                Expr::Tuple { els } => els.iter(),
                a => {tmp_vec.push(a.clone()); tmp_vec.iter()}
            };

            // dbg_println!(2, "Call before expr_val: {:?}", &func);
            let (mut v, mut typ, mut sub) = expr_val(func, extyp, ctx)?;
            dbg_println!(2, "Call after expr_val: {} | type: {}", &v, &typ);
            ctx.apply_subst(&sub);
            let mut arg_vals = Vec::new();
            for a in args_iter {
                // since this is a subcontext, we start again from 0 args 
                let nargs = ctx.reset_args();
                let (av, at, as_) = expr_val(a, types::next_param(&typ).as_ref(), ctx)?;
                // restore the number of args for this context
                ctx.restore_args(nargs);

                sub = types::compose_subst(&as_, &sub);
                let (new_typ, new_sub) = 
                    types::call_type(typ, &at, sub, ctx)?;
                typ = new_typ;
                sub = types::compose_subst(&new_sub, &new_sub);
                // v = call_val(v, &av, 0);
                arg_vals.push(av);
            }

            v.resolve_class_funcs(&sub, ctx)?;
            arg_vals.iter_mut().try_for_each(|x| x.resolve_class_funcs(&sub, ctx))?;

            dbg_println!(2, "Call before call: {} | type: {}", &v, typ);
            let result = call_value(v, arg_vals)?;
            
            dbg_println!(2, "Call after call: {}", &result);
            Ok((result, typ, sub))
        },
        Expr::Block { sttms } => {
            let mut ctx = ctx.child();
            let mut sub = types::Subst::new();


            for sttm in sttms {
                match sttm {
                    Sttm::BlockReturn { expr } => {
                        let (v, t, s) = expr_val(expr, extyp, &mut ctx)?;
                        ctx.apply_subst(&s);
                        sub = types::compose_subst(&s, &sub);
                        dbg_println!(2, "BlockReturn: {} | type: {}", &v, &t);
                        return Ok((v, t, sub));
                    }
                    x => {
                        let s = do_sttm(x, &mut ctx, None);
                        if s.is_none() { return Err("Statement type not supported".to_string()); }
                        let s = s.unwrap()?;
                        sub = types::compose_subst(&s, &sub);
                    }
                }
            }

            Ok((Value::Data(DataValue::Unit), types::Type::Unit, sub))
        },
        Expr::Match { expr, cases } => {
            let (ev, mut et, mut es) = expr_val(expr, None, ctx)?;
            ctx.apply_subst(&es);

            
            let mut res_typ = extyp.cloned().unwrap_or_else(|| ctx.anon_type_var());
            let mut res_cases = Vec::new();
            for (patr, re) in cases {
                let mut ctx = ctx.child();
                let (p, n_vars, ps) = pattern(patr, &et, &mut ctx, false)?;
                ctx.apply_subst(&ps);
                es = types::compose_subst(&ps, &es);

                let (rv, rt, rs) = expr_val(re, Some(&res_typ), &mut ctx)?;
                // let rv = Value::Closure(Closure { arity: n_vars, stored: vec![], thunk: 
                //     Rc::new(RefCell::new(Thunk::Ret(rv)))
                // });
                // since each case is treated like a closure, we need to complete it 
                let rv = complete_lambda(rv, ctx.total_args, ctx.total_args - n_vars, n_vars)?;

                // do the types busywork
                ctx.apply_subst(&rs);
                es = types::compose_subst(&rs, &es);
                let (rt, _tvs1) = types::instantiate(types::subst(&rt, &es), &mut ctx);
                // let rv = instantiate_class_funcs(rv, &tvs1);

                let un = types::unify(&res_typ, &rt, &ctx)?;
                es = types::compose_subst(&un, &es);
                res_typ = types::instantiate(types::subst(&res_typ, &es), &mut ctx).0;
                et = types::subst(&et, &es);

                res_cases.push((p, rv));
            }
            ctx.apply_subst(&es);
            Ok((Value::Thunk(Rc::new(RefCell::new(Thunk::Match(ev, res_cases)))), types::subst(&res_typ, &es), es))
        },   
        Expr::List { els, tail } => {
            let mut sub = types::Subst::new();
            let mut typ = ctx.anon_type_var();
            let mut vals = Vec::new();
            // recurse on every element, unify their types and keep their values
            for el in els {
                let (v, it, s) = expr_val(el, Some(&typ), ctx)?;
                sub = types::compose_subst(&s, &sub);
                let un = types::unify(&typ, &it, &ctx)?;
                sub = types::compose_subst(&un, &sub);
                typ = types::subst(&typ, &sub);
                vals.push(v);
            }

            // now unify the tail type with the list type

            let tail_val = if let Some(t) = tail {
                let list_of_typ = types::Type::GenericNamed { path: vec![], name: types::LIST.to_string(), args: vec![typ] };
                
                let (v, it, s) = expr_val(t, Some(&list_of_typ), ctx)?;
                sub = types::compose_subst(&s, &sub);

                let un = types::unify(&list_of_typ, &it, &ctx)?;
                sub = types::compose_subst(&un, &sub);
                typ = types::subst(&list_of_typ, &sub);
                v
            } else {
                typ = types::Type::GenericNamed { path: vec![], name: types::LIST.to_string() , args: vec![typ] };
                Value::Data(DataValue::Pack(types::LIST_NIL, vec![]))
            };

            let mut res_val = tail_val;
            for v in vals.into_iter().rev() {
                res_val = Value::Data(DataValue::Pack(types::LIST_CONS, vec![v, res_val]));
            }

            Ok((res_val, typ, sub))
        },
        Expr::Struct { struct_type, fields } => {
            let (st_path, st_name) = match struct_type {
                TypeExpr::Named { path, name }
                    => (path, name),
                TypeExpr::Generic { path, name, .. }
                    => (path, name),
                _ => return Err("Anonymous struct types not supported yet".to_string())
            };
            let st_path = types::path_expr_to_path(&st_path)?;
            let (spec, _) = types::instantiate_spec(ctx.type_spec(&st_path, &st_name.to_string())?.clone(), ctx);
	
            let mut sub = types::Subst::new();
            let mut vals = vec![];
            if let types::TypeSpecData::Struct(needed) = spec.data {
                let mut seen = HashSet::new();
                let mut needed = needed.iter().zip(0..).map(|((t,n), i)| (n.as_str(), (t,i))).collect::<HashMap<_, _>>();
                vals.resize(needed.len(), Value::Bottom);
                for (f_name, vl) in fields {
                    if !needed.contains_key(f_name) {
                        if seen.contains(f_name) {
                            return Err(format!("Duplicate field name: '{}'", f_name));
                        } else {
                            return Err(format!("Struct '{}' does not have field '{}'", spec.basis, f_name));
                        }
                    }
                    let (exp_f_type, f_id) = needed.get(f_name).unwrap();
                    let exp_f_type = types::subst(&exp_f_type, &sub);
                    let (v, it, s) = expr_val(vl, Some(&exp_f_type), ctx)?;
                    sub = types::compose_subst(&s, &sub);
                    let un = types::unify(&exp_f_type, &it, &ctx)?;
                    sub = types::compose_subst(&un, &sub);
                    vals[*f_id] = v;
                    needed.remove(f_name);
                    seen.insert(f_name);
                }

                if !needed.is_empty() {
                    return Err(format!("Struct '{}' is missing field(s): {}", &spec.basis, needed.keys().map(|x| format!("'{}'", x)).collect::<Vec<_>>().join(", ")));
                }

                Ok((Value::Data(DataValue::Pack(0, vals)), types::subst(&spec.basis, &sub), sub))
            } else {
                return Err(format!("Type '{}' is not a struct", &spec.basis));
            }
        },
        Expr::Tuple { els } if els.len() == 0 => {
            Ok((Value::Data(DataValue::Unit), types::Type::Unit, types::Subst::new()))
        },
        Expr::Tuple { els } if els.len() == 1 => {
            expr_val(&els[0], extyp, ctx)
        },
        Expr::Tuple { els } => {
            let mut sub = types::Subst::new();
            let mut types: Vec<types::Type> = vec![];
            let mut vals: Vec<Value> = vec![];

            if let Some(types::Type::Tuple { els: expels }) = extyp {
                for (e, exp) in els.iter().zip(expels.iter()) {
                    let (v, t, s) = expr_val(e, Some(exp), ctx)?;
                    sub = types::compose_subst(&s, &sub);
                    types.push(t);
                    vals.push(v);
                }
            }
            else {
                for e in els {
                    let (v, t, s) = expr_val(e, None, ctx)?;
                    sub = types::compose_subst(&s, &sub);
                    types.push(t);
                    vals.push(v);
                }
            }   

            Ok((Value::Data(DataValue::Pack(0, vals)), types::Type::Tuple { els: types }, sub))
        },
        Expr::CompilerPrimitive { name } => {
            let f = if let Some(f) = PRIMITIVES.iter().find_map(|(n, n_args, f)| {
                if n != name { return None; }
                Some(Value::Closure(native_to_closure(*f, *n_args)))
            }) { f } else {
                return Err(format!("Unknown primitive: '{}'", name));
            };

            // let t = if let Some(t) = extyp { t.clone() } else {
            //     ctx.anon_type_var()
            // };
            let t = ctx.anon_type_var();

            Ok((f, t, types::Subst::new()))
        }
        // _ => Err(format!("Expr_val not implemented for expression: {:?}", e))
    }
}

// return a lazy value and type, corresponding to the given expression
pub fn expr_value(e: &Expr, ctx: &Context) -> Result<(Value, types::Type, types::Subst), String> {
    // this is a hack since things in a block can affect the context
    let mut ctx = ctx.clone();
    
    let (v, t, s) = expr_val(e, None, &mut ctx)?;
	let (gen_t, gen_s) = types::generalise(&t, None);
	types::compose_subst(&gen_s, &s);
	Ok((v, types::canonize(gen_t), gen_s))
}


// --------------------------

// evaluates each element to weak head normal form checking that they are charaters and then concatenates them into a string
pub fn eval_string(v: &Value, f: &mut dyn std::io::Write) -> Result<(), String> {
    let mut v = eval_whnf(v)?.unwrap_or_else(|| v.clone());
    loop {
        match v {
            Value::Data(DataValue::Pack(types::LIST_NIL, _)) => {
                return Ok(());
            }
            Value::Data(DataValue::Pack(types::LIST_CONS, mut s)) => {
                let (head, tail) = if s.len() == 2 {
                    let t = s.pop().unwrap();
                    let h = s.pop().unwrap();
                    (h, t)
                } else {
                    return Err(format!("Expected list with two elements, got: {:?}", s));
                };
                let c = eval_whnf(&head)?.unwrap_or(head);
                if let Value::Data(DataValue::Char(c)) = c {
                    write!(f, "{}", c).map_err(|e| format!("Format Error: {}", e))?;
                    f.flush().map_err(|e| format!("Format Error: {}", e))?;
                } else {
                    return Err(format!("Expected char, got: {:?}", c));
                }
                v = eval_whnf(&tail)?.unwrap_or(tail);
            }
            _ => {
                return Err(format!("Expected list, got: {:?}", v));
            }
        }
    }
}

// --------------------------

/*
cool things:
-an infinite list using the fixpoint combinator:
    {let fix = f->{let x = f(x); =x;}; =fix(Cons(3));}

Variants:

a variant is what in haskell is made like this:   data X = A a b c | B d | C
or like in rust:    enum X { A(int, String,bool), B(int), C }
or equivalently in C/C++:   struct X { int disc; union { struct A { a f1; b f2; c f3; }; ... }; };
it is a sum type of all of its variants

it is declared like this:
variant X {
    A(a, b, c); 
    B(d);
    C;
}

an anonymous variant can also be used to declare a variable like this:
variant {
    Just(int);
    Nothing;
} x = Just(5);

or to declare a field in a struct like this:
struct Car {
    int age;
    variant {
        Red;
        White;
        RGB(int, int, int);
    } color;
}

----------------
Generic Types:

generic types take type (and sometimes value) parameters like so:
suppose G is a generic type with arity 1, defined like so
struct|variant|type|newtype G<T> { ... }
then G<T> is a type for any type T
in compile time contexts, where types can be variables,
G(T) will be a type value (contains compile-time information for the type), with a type 'Type'
in contrast, G<T> will always be a type and can only be used in places where a type is expected

-----------------
Next:

variant List<a> {
    Cons(a, List<a>);
    Nil;
}
// this will automatically create func:
func List::Cons<a>(a head, List<a> tail) -> List<a> { ... }
// and value:
List<a> List::Nil = ...;

// this will be equal to (once compile time programming is implemented):

#func List(Type a) -> Type = // the # means that this is a compile-time function
    Variant { // automatically detects that Variant is (ironically) a variant of Type
        "Cons" = (a, List(a)),
        "Nil" = ()
    };

-----------------
Classes...

if we have a function:
func f<Show T1, Show T2>(T1 x, T2 y) -> String { ... }
suppose it gets interpreted as something like:
type:   forall Show T1: forall Show T2: T1 -> T2 -> String
value:  smth..smth.. { ClassFunc, ClassFunc }

how do we know which ClassFunc to instantiate when we call f?
when we call it, it will be instantiated, so the type will become
type:   V1{Show} -> V2{Show} -> String

*/