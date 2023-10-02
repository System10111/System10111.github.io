use crate::types;

type ParseRes<'a,T> = Result<(T, &'a str), String>;

const OPERATOR_CHARACTERS: &str = "!@%$^&*-=+/|<:>~.";

const KEYWORDS: &[&str] = &[
    "match",
    "struct",
    "variant",
    "class",
    "if",
    "else",
    "let",
    "forall",
    "func",
    "self",
    "true",
    "false",
    "impl",
    "for",
    "namespace",
    "operator"
];

// ----------------------
macro_rules! chain {
    ($s: ident, $f: ident, $( $x: expr),*) => {
        {
            let a = $f($s, $($x)*)?;
            $s = a.1;
            a.0
        }
    };
    ($s: ident, $f: ident) => {
        {
            let a = $f($s)?;
            $s = a.1;
            a.0
        }
    };
}
// ----------------------

fn sat(s: &str, p: fn(char) -> bool) -> ParseRes<char> {
    let c = s.chars().nth(0);
    if c.is_none() {
        return Err(String::from("Expected a character, but got end of input"));
    }
    let c = c.unwrap();
    if !p(c) {
        return Err(format!("Unexpected character: {}", c));
    } else {
        return Ok((c, &s[1..]));
    }
}

// any/some return a vector of results, and consume as much input as possible
// anyc/somec are a specialisation when the passed function returns a char, so that a string slice is returned
// anyd/somed consume as much input as possible, but discard the results

fn any<'a, T, F : Fn(&'a str) -> ParseRes<'a, T>>(s: &'a str, f: F) -> ParseRes<Vec<T>> {
    let mut v = Vec::new();
    let mut cur = s;
    loop {
        match f(cur) {
            Ok((t, rs)) => {
                v.push(t);
                cur = rs;
            },
            Err(_) => return Ok((v, cur))
        }
    }
}

fn anyc(s: &str, f: fn(s: &str) -> ParseRes<char>) -> ParseRes<&str> {
    let mut cur = s;
    let mut i = 0;
    loop {
        match f(cur) {
            Ok((c, rs)) => {
                let real_char = s.chars().nth(i).unwrap();
                if real_char != c {
                    return Err(format!("Anyc f returned '{}', expected '{}'", c, real_char));
                }
                cur = rs;
                i += 1;
            },
            Err(_) => return Ok((&s[0..i], cur))
        }
    }
}

fn anyd<'a, T, F : Fn(&'a str) -> ParseRes<'a, T>>(s: &'a str, f: F) -> ParseRes<()> {
    let mut cur = s;
    loop {
        match f(cur) {
            Ok((_, rs)) => {
                cur = rs;
            },
            Err(_) => return Ok(((), cur))
        }
    }
}

fn some<'a, T, F : Fn(&'a str) -> ParseRes<'a, T>>(s: &'a str, f: F) -> ParseRes<Vec<T>>
{
    let mut s = s;
    let first = chain!(s, f);
    let mut rest = chain!(s, any, f);
    rest.insert(0, first);
    Ok((rest, s))
}

fn somec(s: &str, f: fn(s: &str) -> ParseRes<char>) -> ParseRes<&str>
{
    let mut cur = s;
    chain!(cur, f);
    let rest = chain!(cur, anyc, f);
    Ok((&s[0..rest.len()+1], cur))
}

fn somed<'a, T, F : Fn(&'a str) -> ParseRes<'a, T>>(s: &'a str, f: F) -> ParseRes<()>
{
    let mut s = s;
    chain!(s, f);
    chain!(s, anyd, f);
    Ok(((), s))
}

fn zeroorone<'a, T, F : Fn(&'a str) -> ParseRes<'a, T>>(s: &'a str, f: F) -> ParseRes<Option<T>>
{
    match f(s) {
        Ok((t, rs)) => Ok((Some(t), rs)),
        Err(_) => Ok((None, s))
    }
}

// ----------------------

fn seq<'a>(s: &'a str, sequence: &'static str) -> ParseRes<'a, &'a str> {
    if s.starts_with(sequence) {
        Ok((&s[0..sequence.len()], &s[sequence.len()..]))
    } else {
        Err(format!("Expected '{}', but got '{}'", sequence, s))
    }
}

fn wspc(s: &str) -> ParseRes<char> {
    return sat(s, char::is_whitespace);
}

// ----------------------

fn ident_str(s :&str) -> ParseRes<&str> {
    fn ident_start(c: char) -> bool { 
        char::is_alphabetic(c) |
        (c == '_') 
    }
    fn ident_char(c: char) -> bool {
        char::is_alphabetic(c) |
        char::is_ascii_digit(&c) |
        (c == '_')
    }

    let mut cur = s;
    let mut i = 0;
    chain!(cur, sat, ident_start);
    i += 1;
    let rest = chain!(cur, anyc, |s| sat(s, ident_char));
    i += rest.len();

    if KEYWORDS.contains(&&s[0..i]) {
        return Err(format!("Keyword '{}' cannot be used as an identifier", &s[0..i]));
    }
    return Ok((&s[0..i], cur));
}

fn self_ident(s: &str) -> ParseRes<()> {
    fn ident_start(c: char) -> bool { 
        char::is_alphabetic(c) |
        (c == '_') 
    }
    fn ident_char(c: char) -> bool {
        char::is_alphabetic(c) |
        char::is_ascii_digit(&c) |
        (c == '_')
    }

    let mut cur = s;
    let mut i = 0;
    chain!(cur, sat, ident_start);
    i += 1;
    let rest = chain!(cur, anyc, |s| sat(s, ident_char));
    i += rest.len();

    if &&s[0..i] == &"self" {
        return Ok(((), cur));
    }
    Err("Expected 'self'".to_string())
}

// ----------------------
#[derive(Debug, Copy, Clone)]
pub enum LiteralKind { Number, String, Character, Boolean }

pub type PathExpr<'a> = Vec<(&'a str, Vec<TypeExpr<'a>>)>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Literal {
        kind: LiteralKind,
        val: &'a str
    },
    Identifier {
        path: PathExpr<'a>,
        name: &'a str,
        generic_args: Vec<TypeExpr<'a>>
    },
    Access {
        expr: Box<Expr<'a>>,
        member: &'a str,
        generic_args: Vec<TypeExpr<'a>>
    },
    Tuple {
        els: Vec<Expr<'a>>
    },
    Conditional {
        cond: Box<Expr<'a>>,
        true_branch: Box<Expr<'a>>,
        false_branch: Option<Box<Expr<'a>>>
    },
    Unary {
        op: &'a str,
        expr: Box<Expr<'a>>
    },
    Binary { 
        // Binary expressions are always right-associative in the AST
        // and operator precedence is handled when type checking / interpreting
        left: Box<Expr<'a>>,
        op: &'a str,
        right: Box<Expr<'a>>
    },
    Call {
        func: Box<Expr<'a>>,
        args: Box<Expr<'a>>
    },
    Lambda {
        params: Box<Expr<'a>>,
        body: Box<Expr<'a>>
    },
    Block {
        sttms: Vec<Sttm<'a>>
    },
    Match {
        expr: Box<Expr<'a>>,
        cases: Vec<(Expr<'a>, Expr<'a>)>
    },
    List {
        els: Vec<Expr<'a>>,
        tail: Option<Box<Expr<'a>>>
    },
    Struct {
        struct_type: TypeExpr<'a>,
        fields: Vec<(&'a str, Expr<'a>)>
    },
    // this is temporary, in the future compiler_primitive will just be a compile time function, instead of a special expression
    CompilerPrimitive {
        name: &'a str
    }
}

#[derive(Debug, Clone)]
pub enum OperPrec<'a> {
    None,
    New { right: bool, prec: Expr<'a> },
    Inherit { oper: &'a str }
}

#[derive(Debug, Clone)]
pub struct OperDef<'a> {
    pub gparams: Vec<(Option<ClassExpr<'a>>, &'a str)>,
    pub has_self: bool,
    pub params: Vec<(TypeExpr<'a>, &'a str)>,
    pub ret_type: TypeExpr<'a>,
    pub body: Option<Box<Expr<'a>>>
}

#[derive(Debug, Clone)]
pub enum Sttm<'a> {
    Empty,
    Expr {
        expr: Box<Expr<'a>>
    },
    Decl {
        t: Option<TypeExpr<'a>>,
        left: Box<Expr<'a>>,
        right: Option<Box<Expr<'a>>>
    },
    Asgn {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>
    },
    BlockReturn {
        expr: Box<Expr<'a>>
    },
    Return {
        expr: Option<Box<Expr<'a>>>
    },
    TypeDecl {
        type_expr: TypeExpr<'a>
    },
    FuncDecl {
        name: &'a str,
        gparams: Vec<(Option<ClassExpr<'a>>, &'a str)>,
        has_self: bool,
        params: Vec<(TypeExpr<'a>, &'a str)>,
        ret_type: TypeExpr<'a>,
        body: Option<Box<Expr<'a>>>
    },
    OperDecl {
        oper: &'a str,
        left_arg: bool,
        right_arg: bool,
        oper_prec: OperPrec<'a>,
        def: Option<OperDef<'a>>
    },
    ClassDecl {
        name: &'a str,
        gparams: Vec<(Option<ClassExpr<'a>>, &'a str)>,
        inst_t: TypeExpr<'a>,
        funcs: Vec<Sttm<'a>>
    },
    Impl {
        gparams: Vec<(Option<ClassExpr<'a>>, &'a str)>,
        class: ClassExpr<'a>,
        inst: TypeExpr<'a>,
        funcs: Vec<Sttm<'a>>
    },
    Namespace {
        name: &'a str,
        sttms: Vec<Sttm<'a>>
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr<'a> {
    Named {
        path: PathExpr<'a>,
        name: &'a str
    },
    Generic {
        path: PathExpr<'a>,
        name: &'a str,
        args: Vec<TypeExpr<'a>>
    },
    Variant {
        name: Option<&'a str>,
        variants: Vec<(&'a str, Vec<TypeExpr<'a>>)>,
        funcs: Vec<Sttm<'a>> // only FuncDecls
    },
    Struct {
        name: Option<&'a str>,
        fields: Vec<(TypeExpr<'a>, &'a str)>,
        funcs: Vec<Sttm<'a>> // only FuncDecls
    },
    Tuple {
        els: Vec<TypeExpr<'a>>
    },
    Function {
        arg: Box<TypeExpr<'a>>,
        ret: Box<TypeExpr<'a>>
    },
    Forall {
        constraint: Option<ClassExpr<'a>>,
        quant: &'a str,
        t: Box<TypeExpr<'a>>
    }
}

#[derive(Debug, Clone)]
pub enum ClassExpr<'a> {
    Named {
        path: PathExpr<'a>,
        name: &'a str
    },
    Generic {
        path: PathExpr<'a>,
        name: &'a str,
        args: Vec<TypeExpr<'a>>
    },
    Multi {
        classes: Vec<ClassExpr<'a>>
    }
}


//--------

fn comment(s: &str) -> ParseRes<()> {
    let mut s = s;
    if s.starts_with("//") {
        chain!(s, seq, "//");
        chain!(s, anyc, |s| sat(s, |c| c != '\n'));
        Ok(((), s))
    } else if s.starts_with("/*") {
        // TODO: nested comments like this:
        /* /* nested! */ */
        chain!(s, seq, "/*");
        while !s.starts_with("/") {
            chain!(s, anyc, |s| sat(s, |c| c != '*'));
            chain!(s, seq, "*");
        }
        chain!(s, seq, "/");
        Ok(((), s))
    } else {
        Err("Expected comment".to_string())
    }
}

fn wspc_or_cmnt(s: &str) -> ParseRes<()> {
    let mut s = s;
    let begin_space = chain!(s, anyc, wspc);
    let has_comment = if let Ok((_, new_s)) = comment(s) {
        s = new_s;
        true
    } else { false };

    if begin_space.len() > 0 || has_comment {
        Ok(((), s))
    } else {
        Err("Expected whitespace or comment".to_string())
    }
}

//--------

fn path_expr(mut s: &str) -> ParseRes<PathExpr> {
    // a namespace 'path', e.g. "foo::bar::baz" where "foo::bar::" will be consumed and "baz" will be left for the next parser
    // optionally begins with a '::'

    fn path_part(s: &str) -> ParseRes<(&str, Vec<TypeExpr>)> {
        if let Ok(((id, args), s)) = (|| -> Result<((&str, Vec<TypeExpr>), &str), String> {
            let mut s = s;
            let id = chain!(s, ident_str);
            chain!(s, anyd, wspc_or_cmnt);
            let args = if let Ok((args, new_s)) = generic_call(s) {
                s = new_s;
                args
            } else {
                vec![]
            };
            Ok(((id, args), s))
        })() {
            Ok(((id, args), s))
        } else {
            if let Ok((l, s)) = list_type_expr(s) {
                if let TypeExpr::Generic { path: _, name, args } = l {
                    Ok(((name, args), s))
                } else { unreachable!() }
            } else {
                Err("Expected path part".to_string())
            }
        }
    }

    if s.starts_with("::") {
        chain!(s, seq, "::");
        chain!(s, anyd, wspc_or_cmnt);
    }
    let mut path = vec![];
    loop {
        if let Ok((part, new_s)) = (|| -> ParseRes<(&str, Vec<TypeExpr>)> {
            let mut s = s;
            chain!(s, anyd, wspc_or_cmnt);
            let part = chain!(s, path_part);
            chain!(s, anyd, wspc_or_cmnt);
            chain!(s, seq, "::");
            Ok((part, s))
        })() {
            s = new_s;
            path.push(part);
        } else {
            break;
        }
    }
    Ok((path, s))
}

//--------


fn variant_type(s: &str) -> ParseRes<TypeExpr> {
    /*
    variant X<a,b> {
        A(a, b, c);
        B(d);
        C;
    }
    */
    let mut s = s;
    chain!(s, seq, "variant");
    chain!(s, anyd, wspc_or_cmnt);

    let name = if let Ok(tup) = (|| -> Result<(&str, &str), String> {
        let mut s = s;
        let name = chain!(s, ident_str);
        chain!(s, anyd, wspc_or_cmnt);
        Ok((name, s))	
    })() {
        s = tup.1;
        Some(tup.0)
    } else { None };
    // check for generic type parameters
    let params = chain!(s, generic_arguments);
    chain!(s, anyd, wspc_or_cmnt);

    chain!(s, seq, "{");
    chain!(s, anyd, wspc_or_cmnt);
    let mut variants = vec![];
    let mut funcs = vec![];

    loop {
        // is it a function/method?
        if let Ok((fnc, new_s)) = func_decl(s, false) {
            funcs.push(fnc);
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
        } else
        // is it a variant?
        if let Ok((vrnt, new_s)) = (|mut s| -> ParseRes<(&str, Vec<TypeExpr>)> {
            let name = chain!(s, ident_str);
            // optionally get arguments in tuple-style
            if let Ok((tps, new_s)) = (|| -> ParseRes<Vec<TypeExpr>> {
                let mut s = s;
                chain!(s, anyd, wspc_or_cmnt);
                chain!(s, seq, "(");
                chain!(s, anyd, wspc_or_cmnt);
                let mut els = Vec::new();
                loop {
                    if (els.len() > 0) & s.starts_with(",")
                    {
                        chain!(s, seq, ",");
                        chain!(s, anyd, wspc_or_cmnt);
                    } else if s.starts_with(")") {
                        chain!(s, seq, ")");
                        break;
                    }
                    let (e, new_s) = type_expr(s)?;
                    els.push(e);
                    s = new_s;
                    chain!(s, anyd, wspc_or_cmnt);
                }
                chain!(s, anyd, wspc_or_cmnt);
                chain!(s, seq, ";");
                chain!(s, anyd, wspc_or_cmnt);
                Ok((els, s))
            })() {
                // found arguments
                Ok(((name, tps), new_s))
            } else {
                // no arguments
                chain!(s, anyd, wspc_or_cmnt);
                chain!(s, seq, ";");
                chain!(s, anyd, wspc_or_cmnt);
                Ok(((name, Vec::new()), s))
            }
        })(s) {
            variants.push(vrnt);
            s = new_s;
        } else { break; }
    }
    chain!(s, seq, "}");

    let mut res = TypeExpr::Variant { name, variants, funcs };
    for (constraint, quant) in params.into_iter().rev() {
        res = TypeExpr::Forall { quant, constraint, t: Box::new(res) };
    }

    Ok((res, s))
}

fn struct_type(s: &str) -> ParseRes<TypeExpr> {
    // struct X<A,B> {
    //    A foo;
    //    B bar;
    // }
    let mut s = s;
    chain!(s, seq, "struct");
    chain!(s, anyd, wspc_or_cmnt);

    let name = if let Ok(tup) = (|| -> Result<(&str, &str), String> {
        let mut s = s;
        let name = chain!(s, ident_str);
        chain!(s, anyd, wspc_or_cmnt);
        Ok((name, s))	
    })() {
        s = tup.1;
        Some(tup.0)
    } else { None };

    // check for generic type parameters
    let params = chain!(s, generic_arguments);
    chain!(s, anyd, wspc_or_cmnt);

    chain!(s, seq, "{");
    chain!(s, anyd, wspc_or_cmnt);
    let mut fields = vec![];
    let mut funcs = vec![];

    loop {
        // is it a function/method?
        if let Ok((fnc, new_s)) = func_decl(s, false) {
            funcs.push(fnc);
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
        } else
        // is it a field?
        if let Ok((fld, new_s)) = (|mut s| -> ParseRes<_> {
            let typ = chain!(s, type_expr);
            chain!(s, anyd, wspc_or_cmnt);
            let name = chain!(s, ident_str);
            chain!(s, anyd, wspc_or_cmnt);
            chain!(s, seq, ";");
            chain!(s, anyd, wspc_or_cmnt);
            Ok(((typ, name), s))
        })(s) {
            fields.push(fld);
            s = new_s;
        } else { break; }
    }

    chain!(s, seq, "}");

    let mut res = TypeExpr::Struct { name, fields, funcs };
    for (constraint, quant) in params.into_iter().rev() {
        res = TypeExpr::Forall { quant, constraint, t: Box::new(res) };
    }
    Ok((res, s))
}

fn list_type_expr(mut s: &str) -> ParseRes<TypeExpr> {
    chain!(s, seq, "[");
    chain!(s, anyd, wspc_or_cmnt);
    let e = chain!(s, type_expr);
    chain!(s, anyd, wspc_or_cmnt);
    chain!(s, seq, "]");
    Ok((TypeExpr::Generic { path: vec![], name: crate::types::LIST, args: vec![e] }, s))
}

pub fn type_expr(s: &str) -> ParseRes<TypeExpr> {
    let mut s = s;
    let (mut t, mut s) = 
        if s.starts_with("forall") {
            chain!(s, seq, "forall");
            chain!(s, somed, wspc_or_cmnt);

            let (constraint, quant) =
                if let Ok(((c,q), new_s)) = (|| -> ParseRes<(ClassExpr, &str)> {
                    let mut s = s;
                    let constraint = chain!(s, class_expr);
                    chain!(s, anyd, wspc_or_cmnt);
                    let quant = chain!(s, ident_str);
                    Ok(((constraint, quant), s))
                })() {
                    s = new_s;
                    (Some(c), q)
                } else if let Ok((q, new_s)) = (|| -> ParseRes<&str> {
                    let mut s = s;
                    let quant = chain!(s, ident_str);
                    Ok((quant, s))
                })() {
                    s = new_s;
                    (None, q)
                } else {
                    return Err(format!("Expected class constraint or quantifier after 'forall'"));
                };

            chain!(s, anyd, wspc_or_cmnt);
            chain!(s, seq, ":");
            chain!(s, anyd, wspc_or_cmnt);
            let (inner, new_s) = type_expr(s)?;
            Ok((TypeExpr::Forall { quant, constraint, t: Box::new(inner) }, new_s))
        } else 
        if s.starts_with("(") {
            chain!(s, seq, "(");
            chain!(s, anyd, wspc_or_cmnt);
            let mut els = Vec::new();
            loop {
                if (els.len() > 0) & s.starts_with(",")
                {
                    chain!(s, seq, ",");
                    chain!(s, anyd, wspc_or_cmnt);
                } else if s.starts_with(")") {
                    chain!(s, seq, ")");
                    break;
                }
                let (e, new_s) = type_expr(s)?;
                els.push(e);
                s = new_s;
                chain!(s, anyd, wspc_or_cmnt);
            }
            Ok((TypeExpr::Tuple { els: els }, s))
        } else
        if s.starts_with("[") {
            list_type_expr(s)
        } else
        if let Ok((t, new_s)) = variant_type(s) {
            s = new_s;
            Ok((t, s))
        } else
        if let Ok((t, new_s)) = struct_type(s) {
            s = new_s;
            Ok((t, s))
        } else
        if let Ok((id, new_s)) = ident(s) {
            match id {
                Expr::Identifier { path, name, generic_args } if generic_args.len() == 0
                    => Ok((TypeExpr::Named { path, name }, new_s)),
                Expr::Identifier { path, name, generic_args } 
                    => Ok((TypeExpr::Generic { path, name, args: generic_args }, new_s)),
                _ => unreachable!()
            }
        } else {
            Err("Could not parse type".to_string())
        }?;
    
    chain!(s, anyd, wspc_or_cmnt);

    loop {
        if s.starts_with("->") {
            chain!(s, seq, "->");
            chain!(s, anyd, wspc_or_cmnt);
            let (t2, new_s) = type_expr(s)?;
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
            t = TypeExpr::Function{ arg: Box::new(t), ret: Box::new(t2) };
        } else {
            return Ok((t, s))
        }
    }
}

fn type_decl(s: &str) -> ParseRes<Sttm> {
    let mut s = s;
    if let Ok((t, new_s)) = variant_type(s) {
        s = new_s;
        return Ok((Sttm::TypeDecl { type_expr: t }, s));
    } else
    if let Ok((t, new_s)) = struct_type(s) {
        s = new_s;
        return Ok((Sttm::TypeDecl { type_expr: t }, s));
    }
    Err("Expected a type declaration".to_string())
}

pub fn class_expr(s: &str) -> ParseRes<ClassExpr> {
    // Either:
    // _name_
    // _name_ < _type_expr_, _type_expr_, ... >
    // _class_expr_ & _class_expr_ & ...
    let mut s = s;

    let mut res = None;

    loop {
        // a basic class expression is just an identifier
        let id = chain!(s, ident);

        let cur = match id {
            Expr::Identifier { path, name, generic_args } if generic_args.len() == 0
                => ClassExpr::Named { path, name },
            Expr::Identifier { path, name, generic_args } 
                => ClassExpr::Generic { path, name, args: generic_args },
            _ => unreachable!()
            
        };

        if let Some(cur_res) = res {
            // if it's already a multi, add to it
            if let ClassExpr::Multi { mut classes } = cur_res {
                classes.push(cur);
                res = Some(ClassExpr::Multi { classes: classes });
            } else {
                // else make it a multi with the last one
                res = Some(ClassExpr::Multi { classes: vec![cur_res, cur] });
            }
        } else {
            // if there wasn't a previous one, just set it
            res = Some(cur);
        }

        chain!(s, anyd, wspc_or_cmnt);

        // if there is a '&', get the next class expression
        if s.starts_with("&") {
            chain!(s, seq, "&");
            chain!(s, anyd, wspc_or_cmnt);
            continue;
        } else {
            // at this point, if we haven't failed any of the chain!s, there should definately be something in res
            return Ok((res.unwrap(), s))
        }
    }
}

pub fn expr(s: &str) -> ParseRes<Expr> {
    let mut s = s;

    // is there a unary operator?
    let un_op = if let Ok(tup) = (|| -> Result<(&str, &str), String> {
        let mut s = s;
        let op = chain!(s, oper);
        chain!(s, anyd, wspc_or_cmnt);
        Ok((op, s))
        // let expr = chain!(s, expr);
        // Ok((Expr::Unary { op: op, expr: Box::new(expr) }, s))
    })() {
        s = tup.1;
        Some(tup.0)
    } else {
        None
    };

    // find if there is any valid expression
    // and later we'll check if it's part of a binary expression
    let mut res: Option<Expr> = None;
    // check if it's a compiler primitive
    // this is temporary, and we're hijacking the future compile time function call syntax
    // but in the end it should look like this when it's properly implemented:
    if s.starts_with("#compiler_primitive(\"") {
        chain!(s, seq, "#compiler_primitive(\"");
        let name = chain!(s, anyc, |s| sat(s, |c| c != '"'));
        chain!(s, seq, "\")");
        res = Some(Expr::CompilerPrimitive { name: name });
    } else 
    if let Ok(((), new_s)) = self_ident(s) {
        res = Some(Expr::Identifier { path: vec![], name: &s[0..4] /* should always be "self" */, generic_args: vec![] });
        s = new_s;
    } else
    if let Ok((e, new_s)) = struct_expr(s) {
        res = Some(e);
        s = new_s;
    } else 
    if let Ok((e, new_s)) = match_expr(s) {
        res = Some(e);
        s = new_s;
    } else
    if let Ok((e, new_s)) = block(s) {
        res = Some(e);
        s = new_s;
    } else
    if let Ok((e, new_s)) = lambda(s) {
        res = Some(e);
        s = new_s;
    } else
    if let Ok((e, new_s)) = conditional(s) {
        res = Some(e);
        s = new_s;
    } else
    if let Ok((e, new_s)) = ident(s) {
        res = Some(e);
        s = new_s;
    } else
    if let Ok((e, new_s)) = list(s) {
        res = Some(e);
        s = new_s;
    } else
    if let Ok((e, new_s)) = literal(s) {
        res = Some(e);
        s = new_s;
    } else 
    if let Ok((e, new_s)) = tuple(s, expr) {
        res = Some(e);
        s = new_s;
    } 

    if res.is_none() {
        // if there was a unary operator, we can try a new expression after it
        if un_op.is_some() {
            if let Ok((e, new_s)) = expr(s) {
                res = Some(e);
                s = new_s;
            } else {
                return Err(String::from("Expected expression"));
            }
        } else {
            return Err(String::from("Expected expression"));
        }
    }
    let mut res = res.unwrap();

    // if there are any tuples or accesses immediately after the expression
    // add them as the parameters to a call or as an access
    loop {
        // call
        if let Ok((args, new_s)) = (|| -> ParseRes<Expr> {
            let mut s = s;
            chain!(s, anyd, wspc_or_cmnt);
            let args = chain!(s, tuple, expr);
            Ok((args, s))
        })() {
            res = Expr::Call {
                func: Box::new(res),
                args: Box::new(args)
            };
            s = new_s;
        } else
        // access
        if let Ok(((member, generics), new_s)) = (|| -> ParseRes<(&str, Vec<TypeExpr>)> {
            let mut s = s;
            chain!(s, anyd, wspc_or_cmnt);
            chain!(s, seq, ".");
            chain!(s, anyd, wspc_or_cmnt);
            let member = chain!(s, ident_str);
            let generics = if let Ok((g, new_s)) = generic_call(s) { 
                s = new_s;
                g 
            } else { 
                vec![]
            };
            Ok(((member, generics), s))
        })() {
            res = Expr::Access {
                expr: Box::new(res),
                generic_args: generics,
                member
            };
            s = new_s;
        }
        // not either of the above
        else {
            break;
        }
    }
    

    // if there is a unary operator, make a new expression with it
    if let Some(op) = un_op {
        res = Expr::Unary { op: op, expr: Box::new(res) };
    }

    // if there is a binary operator and another expression afterwards
    // this becomes a binary expression
    if let Ok(((op, right), new_s)) = (|| -> ParseRes<(&str, Expr)> {
        let mut s = s;
        chain!(s, anyd, wspc_or_cmnt);
        let op = chain!(s, oper);
        chain!(s, anyd, wspc_or_cmnt);
        let right = chain!(s, expr);
        Ok(((op, right), s))
    })() {
        return Ok((Expr::Binary { op: op, left: Box::new(res), right: Box::new(right) }, new_s));
    } else {
        return Ok((res, s));
    }

}

fn struct_expr(s: &str) -> ParseRes<Expr> {
    // struct_name { field1 = expr1; field2 = expr2; ... }
    let mut s = s;
    let tp = chain!(s, type_expr);
    match &tp {
        TypeExpr::Named { .. } | TypeExpr::Struct { .. } => {},
        TypeExpr::Generic { name, .. } if name != &types::LIST => {},
        _ => return Err(String::from("Expected struct type"))
    }
    chain!(s, anyd, wspc_or_cmnt);
    chain!(s, seq, "{");
    chain!(s, anyd, wspc_or_cmnt);
    let mut fields = Vec::new();
    loop {
        if s.starts_with("}") {
            chain!(s, seq, "}");
            break;
        }
        // in the future we will support nested fields, but for now we only support one level
        let fname = chain!(s, ident_str);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, seq, "=");
        chain!(s, anyd, wspc_or_cmnt);
        let val = chain!(s, expr);
        fields.push((fname, val));
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, seq, ";");
        chain!(s, anyd, wspc_or_cmnt);
    }
    Ok((Expr::Struct { struct_type: tp, fields }, s))
}

fn match_expr(s: &str) -> ParseRes<Expr> {
    /*
    match x {
        Cons(x, xs) => xs,
        Nil => Nil
    }
    */
    let mut s = s;
    chain!(s, seq, "match");
    chain!(s, anyd, wspc_or_cmnt);
    let ex = chain!(s, expr);
    chain!(s, anyd, wspc_or_cmnt);
    chain!(s, seq, "{");
    chain!(s, anyd, wspc_or_cmnt);
    let mut cases = Vec::new();
    loop {
        if (cases.len() > 0) & s.starts_with(",")
        {
            chain!(s, seq, ",");
            chain!(s, anyd, wspc_or_cmnt);
        } else if s.starts_with("}") {
            chain!(s, seq, "}");
            break;
        }
        let pat = chain!(s, expr);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, seq, "=>");
        chain!(s, anyd, wspc_or_cmnt);
        let r = chain!(s, expr);
        cases.push((pat, r));
        chain!(s, anyd, wspc_or_cmnt);
    }
    Ok((Expr::Match { expr: Box::new(ex), cases: cases }, s))
}

fn lambda(s: &str) -> ParseRes<Expr> {
    let mut s = s;
    let p = 
        if let Ok((id, new_s)) = name_ident(s) {
            s = new_s;
            id
        } else 
        if let Ok((tup, new_s)) = tuple(s, name_ident) {
            s = new_s;
            tup
        } else {
            return Err(String::from("Expected identifier or tuple"));
        };

    chain!(s, anyd, wspc_or_cmnt);
    chain!(s, seq, "->");
    chain!(s, anyd, wspc_or_cmnt);

    let body = chain!(s, expr);
    return Ok((Expr::Lambda { params: Box::new(p), body: Box::new(body) }, s));
}

fn conditional(s: &str) -> ParseRes<Expr> {
    let mut s = s;
    chain!(s, seq, "if");
    if (s.len() > 0) & !"{([".contains(s.chars().nth(0).unwrap())
    {
        chain!(s, somed, wspc_or_cmnt);
    }
    let cond = chain!(s, expr);
    chain!(s, anyd, wspc_or_cmnt);

    let mut has_colon = false;
    if (s.len() > 0) & (s.chars().nth(0).unwrap() == ':') {
        has_colon = true;
        chain!(s, sat, |c| c == ':');
        chain!(s, anyd, wspc_or_cmnt);
    }

    let true_branch = chain!(s, expr);
    if !matches!(true_branch, Expr::Block { .. }) & !has_colon {
        return Err(String::from("Expected ':'"));
    }

    if let Ok((false_branch, new_s)) = (|| -> ParseRes<Expr> {
        let mut s = s;
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, seq, "else");
        if (s.len() > 0) & !"{([".contains(s.chars().nth(0).unwrap())
        {
            chain!(s, somed, wspc_or_cmnt);
        }
        let false_branch = chain!(s, expr);
        Ok((false_branch, s))
    })() {
        return Ok((Expr::Conditional { 
            cond: Box::new(cond), 
            true_branch: Box::new(true_branch), 
            false_branch: Some(Box::new(false_branch)) }, 
        new_s));
    } else {
        return Ok((Expr::Conditional { 
            cond: Box::new(cond), 
            true_branch: Box::new(true_branch), 
            false_branch: None }, 
        s));
    }
}

fn name_ident(s: &str) -> ParseRes<Expr> {
    let mut s = s;
    let id = chain!(s, ident_str);
    Ok((Expr::Identifier { path: vec![], name: id, generic_args: vec![] }, s))
}

fn ident(mut s: &str) -> ParseRes<Expr> {
    // _optional path_ identifier _optional generic agruments_
    // example: Math::Vector::from_angle<float>
    // note: this produces different syntax tree from Math::Vector<float>::from_angle
    // but should be equivalent in the end
    // in the latter case the generic agruments are part of the path so they are not included in the identifier expression itself
    let path = chain!(s, path_expr);
    chain!(s, anyd, wspc_or_cmnt);
    let id = chain!(s, ident_str);
    chain!(s, anyd, wspc_or_cmnt);
    let generics = if let Ok((args, new_s)) = generic_call(s) {
        s = new_s;
        args
    } else {
        vec![]
    };
    Ok((Expr::Identifier { path: path, name: id, generic_args: generics }, s))
}

fn num_literal(mut s: &str) -> ParseRes<Expr> {
    let start = s;
    let whole = chain!(s, somec, |s| sat(s, |c| c.is_ascii_digit()));
    let mut len = whole.len();
    if let Ok((frac, new_s)) = (|| -> Result<(&str, &str), String> {
        let mut s = s;
        chain!(s, sat, |c| c == '.');
        let frac = chain!(s, somec, |s| sat(s, |c| c.is_ascii_digit()));
        Ok((frac, s))
    })() {
        s = new_s;
        len += frac.len() + 1;
    }
    Ok((Expr::Literal { kind: LiteralKind::Number, val: &start[0..len] }, s))	
}

fn literal(s: &str) -> ParseRes<Expr> {
    fn char_allowed(c: char) -> bool { c != '\'' && c != '\\' && c != '\n' }
    fn string_allowed(c: char) -> bool { c != '\"' && c != '\\' && c != '\n' }
    // character literal
    if let Ok(tup) = (|| -> Result<(Expr, &str), String> {
        let mut s = s;
        chain!(s, sat, |c| c == '\'');
        let res_char = &s[0..1];
        chain!(s, sat, char_allowed);
        chain!(s, sat, |c| c == '\'');
        Ok((Expr::Literal { kind: LiteralKind::Character, val: res_char }, s))	
    })() {
        return Ok(tup);
    } else 
    // string literal
    if let Ok(tup) = (|| -> Result<(Expr, &str), String> {
        let mut s = s;
        chain!(s, sat, |c| c == '\"');
        let res_str = chain!(s, anyc, |s| sat(s, string_allowed));
        chain!(s, sat, |c| c == '\"');
        Ok((Expr::Literal { kind: LiteralKind::String, val: res_str }, s))	
    })() {
        return Ok(tup);
    } else
    // number literal
    if let Ok(tup) = num_literal(s) {
        return Ok(tup);
    } else
    // true literal
    if let Ok(tup) = (|| -> Result<(Expr, &str), String> {
        let mut s = s;
        let val = chain!(s, seq, "true");
        Ok((Expr::Literal { kind: LiteralKind::Boolean, val: val }, s))	
    })() {
        return Ok(tup);
    } else
    // false literal
    if let Ok(tup) = (|| -> Result<(Expr, &str), String> {
        let mut s = s;
        let val = chain!(s, seq, "false");
        Ok((Expr::Literal { kind: LiteralKind::Boolean, val: val }, s))	
    })() {
        return Ok(tup);
    } else {
        return Err(String::from("Expected literal"));
    }
}

fn tuple(s: &str, f: fn(s: &str) -> ParseRes<Expr>) -> ParseRes<Expr> {    
    let mut s = s;
    chain!(s, sat, |c| c == '(');
    chain!(s, anyd, wspc_or_cmnt);

    let t = Expr::Tuple { els: 
        if let Ok((vec, new_s)) = (|| -> Result<(Vec<Expr>, &str), String> {
            let mut s = s;
            let first_el = chain!(s, f);
            let mut rest = chain!(s, any, |s| -> ParseRes<Expr> {
                let mut s = s;
                chain!(s, any, wspc);
                chain!(s, sat, |c| c == ',');
                chain!(s, any, wspc);
                let x = chain!(s, f);
                Ok((x, s))
            });
            rest.insert(0, first_el);
            Ok((rest, s))
        })() { s = new_s; vec } else { vec![] }
    };
    chain!(s, anyd, wspc_or_cmnt);
    chain!(s, sat, |c| c == ')');
    return Ok((t, s));
}

fn oper(s: &str) -> ParseRes<&str> {
    if let Ok(tup) = (|| -> Result<(&str, &str), String> {
        let mut s = s;
        let res = s;
        // putting a ` in front of a function name turns it into an operator
        chain!(s, sat, |c| c == '`');
        let f = chain!(s, ident_str);
        Ok((&res[0..f.len() + 1], s))
    })() {
        return Ok(tup);
    } else
    if let Ok(tup) = (|| -> Result<(&str, &str), String> {
        let orig_s = s;
        let mut s = s;
        let mut op = chain!(s, somec, |s| sat(s, |c| OPERATOR_CHARACTERS.contains(c)));
        // since comments are comments even if they are next to operators,
        // if the operator contains a //, /* or */, it should be cut off to before that
        // and s should be restored so it
        if op.contains("//") {
            // get the position of the //
            let pos = op.find("//").unwrap();
            // cut off the rest of the operator
            op = &op[0..pos];
            // restore s
            s = &orig_s[op.len()..];
        }
        if op.contains("/*") {
            let pos = op.find("/*").unwrap();
            op = &op[0..pos];
            s = &orig_s[op.len()..];
        }
        if op.contains("*/") {
            let pos = op.find("*/").unwrap();
            op = &op[0..pos];
            s = &orig_s[op.len()..];
        }
        // some combinations of characters are reserved and don't count as operators
        if ["=", ":", ".", "->", "=>", "/*", "*/", "//", "::"].contains(&op) || op.is_empty()
        {
            return Err(String::from("Expected operator"));
        }
        Ok((op, s))
    })() {
        return Ok(tup);
    } else {
        return Err(String::from("Expected operator"));
    }
}

pub fn list(s: &str) -> ParseRes<Expr> {
    // [] = empty list
    // [x] = list with one element
    // [x, y, z] = list with multiple elements
    // [x:xs] = list with one element and a tail
    // [x, y:ys] = list with multiple elements and a tail
    let mut s = s;
    chain!(s, sat, |c| c == '[');
    chain!(s, anyd, wspc_or_cmnt);
    if s.starts_with("]") {
        chain!(s, sat, |c| c == ']');
        return Ok((Expr::List { els: vec![], tail: None }, s));
    }

    let first_el = chain!(s, expr);
    chain!(s, any, wspc);

    let mut els = chain!(s, any, |s| -> ParseRes<Expr> {
        let mut s = s;
        chain!(s, sat, |c| c == ',');
        chain!(s, any, wspc);
        let x = chain!(s, expr);
        chain!(s, any, wspc);
        Ok((x, s))
    });
    els.insert(0, first_el);

    let tail = if s.starts_with(":") {
        chain!(s, sat, |c| c == ':');
        chain!(s, any, wspc);
        let tail = chain!(s, expr);
        Some(tail)
    } else {
        None
    };
    chain!(s, any, wspc);
    chain!(s, sat, |c| c == ']');
    return Ok((Expr::List { els: els, tail: tail.map(|x| Box::new(x)) }, s));
}

pub fn block(s: &str) -> ParseRes<Expr> {
    let mut s = s;
    chain!(s, sat, |c| c == '{');
    chain!(s, anyd, wspc_or_cmnt);
    let mut sttms = Vec::new();
    while let Ok((x, new_s)) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        let x = chain!(s, sttm);
        chain!(s, anyd, wspc_or_cmnt);
        Ok((x, s))
    })() {
        s = new_s;
        sttms.push(x);
    }
    chain!(s, sat, |c| c == '}');
    return Ok((Expr::Block { sttms: sttms }, s));
}

pub fn sttm(s: &str) -> ParseRes<Sttm> {
    // is this just an empty statement?
    if let Ok(new_s) = (|| -> Result<&str, String> {
        let mut s = s;
        chain!(s, sat, |c| c == ';');
        Ok(s)
    })() {
        return Ok((Sttm::Empty, new_s));
    }

    // is this a namespace?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        chain!(s, seq, "namespace");
        chain!(s, anyd, wspc_or_cmnt);
        let name = chain!(s, ident_str);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == '{');
        chain!(s, anyd, wspc_or_cmnt);
        let sttms = chain!(s, sttms);
        chain!(s, sat, |c| c == '}');
        Ok((Sttm::Namespace { name: name, sttms: sttms }, s))
    })() {
        return Ok(tup);
    }

    // is this a return statement?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        chain!(s, seq, "return");
        chain!(s, anyd, wspc_or_cmnt);

        // value is optional (iterpreter will default it to '()' if it is not specified)
        let e = if let Ok((e, new_s)) = (|| -> Result<(Expr, &str), String> {
            let mut s = s;
            let e = chain!(s, expr);
            Ok((e, s))
        })() {
            s = new_s;
            Some(Box::new(e))
        } else {
            None
        };

        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == ';');
        Ok((Sttm::Return { expr: e }, s))
    })() {
        return Ok(tup);
    }

    // is this a block return? ( of the form " = <expr>; ")
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        chain!(s, seq, "=");
        chain!(s, anyd, wspc_or_cmnt);
        let e = chain!(s, expr);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == ';');
        Ok((Sttm::BlockReturn { expr: Box::new(e) }, s))
    })() {
        return Ok(tup);
    }

    // is this a declaration?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        // later a types will be able to take place of let
        chain!(s, seq, "let");
        chain!(s, anyd, wspc_or_cmnt);

        // for now, just allow an identifier, but patterns will also be supported later
        let left = chain!(s, name_ident);
        chain!(s, anyd, wspc_or_cmnt);

        // assignment is optional
        let right = if let Ok((right, new_s)) = (|| -> Result<(Expr, &str), String> {
            let mut s = s;
            chain!(s, sat, |c| c == '=');
            chain!(s, anyd, wspc_or_cmnt);
            let right = chain!(s, expr);
            Ok((right, s))
        })() {
            s = new_s;
            Some(Box::new(right))
        } else {
            None
        };

        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == ';');

        Ok((Sttm::Decl { t: None, left: Box::new(left), right: right }, s))
    })() {
        return Ok(tup);
    }

    // is this a type declaration?
    if let Ok(tup) = type_decl(s) {
        return Ok(tup);
    }

    // is this an assignment?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;

        // for now, just allow an identifier, but patterns will also be supported later
        let left = chain!(s, name_ident);
        chain!(s, anyd, wspc_or_cmnt);
        
        chain!(s, sat, |c| c == '=');
        chain!(s, anyd, wspc_or_cmnt);
        
        let right = chain!(s, expr);
        chain!(s, anyd, wspc_or_cmnt);
        
        chain!(s, sat, |c| c == ';');
        
        Ok((Sttm::Asgn { left: Box::new(left), right: Box::new(right) }, s))
    })() {
        return Ok(tup);
    }

    // is this just a simple expression?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        let e = chain!(s, expr);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == ';');
        Ok((Sttm::Expr { expr: Box::new(e) }, s))
    })() {
        return Ok(tup);
    }

    // is this a function declaration?
    if let Ok(tup) = func_decl(s, false) {
        return Ok(tup);
    }

    // is this an operator declaration?
    if let Ok(tup) = oper_decl(s, false) {
        return Ok(tup);
    }

    // is this a class declaration?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        let cls = chain!(s, class_decl);
        Ok((cls, s))
    })() {
        return Ok(tup);
    }

    // is this an impl statement?
    if let Ok(tup) = (|| -> Result<(Sttm, &str), String> {
        let mut s = s;
        let impl_ = chain!(s, impl_decl);
        Ok((impl_, s))
    })() {
        return Ok(tup);
    }

    Err(String::from("Expected statement"))
}

pub fn sttms(mut s: &str) -> ParseRes<Vec<Sttm>> {
    chain!(s, anyd, wspc_or_cmnt);
    some(s, |mut s| {
        let x = chain!(s, sttm);
        chain!(s, anyd, wspc_or_cmnt);
        Ok((x, s))
    })
}

fn generic_call(mut s: &str) -> ParseRes<Vec<TypeExpr>> {
    // <_type_, _type_, ...>
    chain!(s, seq, "<");
    chain!(s, anyd, wspc_or_cmnt);

    let mut params = vec![];

    loop {
        if params.len() > 0 && s.starts_with(",") {
            chain!(s, seq, ",");
            chain!(s, anyd, wspc_or_cmnt);
        } else if s.starts_with(">") {
            chain!(s, seq, ">");
            break;
        } else if params.len() > 0 {
            return Err(String::from("Expected ',' or '>'"));
        }

        let param = chain!(s, type_expr);
        params.push(param);
        chain!(s, anyd, wspc_or_cmnt);
    }

    Ok((params, s))
}

fn generic_arguments(s: &str) -> ParseRes<Vec<(Option<ClassExpr>, &str)>> {
    // < _class_expr_ _ident_, _class_expr_ _ident_, ...>
    let mut s = s;
    let open = seq(s, "<");
    if open.is_err() {
        return Ok((vec![], s)); // if we don't have a generic type, return an empty vector
        // all errors from now on result in a fail
    }        
    s = open.unwrap().1;
    let mut params = vec![];

    chain!(s, anyd, wspc_or_cmnt);
    loop {
        if (params.len() > 0) & s.starts_with(",")
        {
            chain!(s, seq, ",");
            chain!(s, anyd, wspc_or_cmnt);
        } else if s.starts_with(">") {
            chain!(s, seq, ">");
            break;
        }

        let (constraint, quant) =
            if let Ok(((c,q), new_s)) = (|| -> ParseRes<(ClassExpr, &str)> {
                let mut s = s;
                let constraint = chain!(s, class_expr);
                chain!(s, anyd, wspc_or_cmnt);
                let quant = chain!(s, ident_str);
                Ok(((constraint, quant), s))
            })() {
                s = new_s;
                (Some(c), q)
            } else if let Ok((q, new_s)) = (|| -> ParseRes<&str> {
                let mut s = s;
                let quant = chain!(s, ident_str);
                Ok((quant, s))
            })() {
                s = new_s;
                (None, q)
            } else {
                return Err(format!("Expected constraint or type parameter in generic parameter list"));
            };

        if params.iter().any(|(_, q)| q == &quant) {
            return Err(format!("Duplicate type parameter '{}'", quant));
        } 
        params.push((constraint, quant));
        chain!(s, anyd, wspc_or_cmnt);
    }
    Ok((params, s))
}

fn func_arguments(mut s: &str) -> ParseRes<(bool, Vec<(TypeExpr, &str)>)> {
    chain!(s, sat, |c| c == '(');
    chain!(s, anyd, wspc_or_cmnt);

    // see if the first param is 'self'
    let has_self = if let Ok(((), new_s)) = self_ident(s) {
        s = new_s;
        chain!(s, anyd, wspc_or_cmnt);
        true
    } else {
        false
    };

    let mut params = vec![];
    loop {
        if ((params.len() > 0) | has_self) & s.starts_with(",")
        {
            chain!(s, seq, ",");
            chain!(s, anyd, wspc_or_cmnt);
        } else if s.starts_with(")") {
            break;
        }
        let pt = chain!(s, type_expr);
        chain!(s, anyd, wspc_or_cmnt);
        let pn = chain!(s, ident_str);
        if params.iter().any(|(_, n)| *n == pn) {
            return Err(format!("Duplicate parameter '{}'", pn));
        }
        params.push((pt, pn));
        chain!(s, anyd, wspc_or_cmnt);
    }
    
    chain!(s, seq, ")");

    Ok(((has_self, params), s))
}

// if allow_sign_only is true, then we allow a function signature without a body
fn func_decl(s: &str, allow_sign_only: bool) -> ParseRes<Sttm> {
    /*
    func _name_<G1, _class_expr_ G2>(Type1 arg1, Type2 arg2) -> Ret_Type
    Either: = _expr_;
    Or: a block
    Or: just ';' for signature only
    */
    let mut s = s;
    chain!(s, seq, "func");
    chain!(s, anyd, wspc_or_cmnt);
    let name = chain!(s, ident_str);
    chain!(s, anyd, wspc_or_cmnt);
    
    // check for generic type parameters
    let gparams = chain!(s, generic_arguments);
    chain!(s, anyd, wspc_or_cmnt);

    // get the parameters
    let (has_self, params) = chain!(s, func_arguments);
    chain!(s, anyd, wspc_or_cmnt);

    // return type is not optional
    chain!(s, seq, "->");
    chain!(s, anyd, wspc_or_cmnt);
    let ret_type = chain!(s, type_expr);
    chain!(s, anyd, wspc_or_cmnt);

    // if there is an '=', allow any expression, if not, allow only a block
    let body = if s.starts_with("=") {
        chain!(s, seq, "=");
        chain!(s, anyd, wspc_or_cmnt);
        let expr = chain!(s, expr);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == ';');
        Some(expr)
    } else if allow_sign_only && s.starts_with(";") {
        chain!(s, seq, ";");
        None
    } else {
        let block = chain!(s, block);
        // the ";" is optional for a block
        chain!(s, zeroorone, |mut s| {
            chain!(s, anyd, wspc_or_cmnt);
            chain!(s, seq, ";");
            Ok(((), s))
        });
        Some(block)
    };

    let res = Sttm::FuncDecl {
        name: name,
        gparams: gparams,
        has_self: has_self,
        params: params,
        ret_type: ret_type,
        body: body.map(Box::new),
    };

    Ok((res, s))

}

fn oper_decl(mut s: &str, allow_sign_only: bool) -> ParseRes<Sttm> {
    /*
    the declaration starts with the "operator" keyword
    immediately afterwards are the generic type parameters, if any
    (note this is different from the normal function syntax, where the generic type parameters are after the name)

    the operator, made of symbols(defined in fn operator), has a "_" either to the left (prefix), right (postfix), or both (infix)
    postfix operators are not supported by the rest of the language, but parsing them helps with error messages

    after a binary operator, there is an optional declaration of associativity and precedence
    there are 2 options for how to do this:
        - with ~ [left|right] [precedence expression] (the precedence expr will later be required to be a compile time expression)
            an extra "~" is also necessary to separate the precedence expression from the function signature (if there is one)
        - with ~= operator     this will copy the associativity and precedence from the given operator

    after that is the function signature, which is the same as a normal function signature
    
    afterwards is the body, again like a normal function, which is required unless allow_sign_only is true 

    example: operator<T> _++_ ([T] xs, [T] ys) -> [T] = xs `concat ys;
        
    */

    chain!(s, seq, "operator");
    chain!(s, anyd, wspc_or_cmnt);

    // check for generic type parameters
    let gparams = chain!(s, generic_arguments);
    // space is required to separate the operator from the generic type parameters or the operator keyword
    if gparams.len() > 0 { chain!(s, somed, wspc_or_cmnt);  }
    

    // check the left "_"
    let left = if s.starts_with("_") {
        chain!(s, seq, "_");
        chain!(s, anyd, wspc_or_cmnt);
        true
    } else {
        false
    };

    // get the operator
    let operator = chain!(s, oper);
    chain!(s, anyd, wspc_or_cmnt);

    // check the right "_"
    let right = if s.starts_with("_") {
        chain!(s, seq, "_");
        chain!(s, anyd, wspc_or_cmnt);
        true
    } else {
        false
    };

    if !left && !right {
        return Err("Operator must have at least one application side '_'".to_string());
    }

    // check for associativity and precedence
    let prec = if left && right {
        if s.starts_with("~=") {
            chain!(s, seq, "~=");
            chain!(s, somed, wspc_or_cmnt);
            let inherit_oper = chain!(s, oper);
            OperPrec::Inherit { oper: inherit_oper }
        } else if s.starts_with("~") {
            chain!(s, seq, "~");
            chain!(s, anyd, wspc_or_cmnt);
            let right = if s.starts_with("left") {
                chain!(s, seq, "left");
                false
            } else if s.starts_with("right") {
                chain!(s, seq, "right");
                true
            } else {
                return Err("Expected 'left' or 'right'".to_string());
            };
            chain!(s, somed, wspc_or_cmnt);
            let prec = if let Ok(tup) = block(s) {
                s = tup.1;
                tup.0
            } else if let Ok(tup) = num_literal(s) {
                s = tup.1;
                tup.0
            } else {
                return Err("Expected a block or a number literal as precedence".to_string());
            };
            OperPrec::New { right, prec }
        } else {
            OperPrec::None
        }
    } else { OperPrec::None };
    chain!(s, anyd, wspc_or_cmnt);

    // if there was a precedence declaration, we can check for a finishing ';', skipping the rest of the definition if we find it
    if !matches!(prec, OperPrec::None) {
        if s.starts_with(";") {
            chain!(s, seq, ";");
            return Ok((Sttm::OperDecl { 
                oper: operator, 
                oper_prec: prec,
                left_arg: left,
                right_arg: right, 
                def: None
            }, s));
        }
    }

    // if we're not finished, next is the function signature, but it must be separated if we had a new precedence declaration
    if matches!(prec, OperPrec::New { .. }) {
        chain!(s, seq, "~");
        chain!(s, anyd, wspc_or_cmnt);
    }



    // get the parameters
    let (has_self, params) = chain!(s, func_arguments);
    chain!(s, anyd, wspc_or_cmnt);

    // return type is not optional
    chain!(s, seq, "->");
    chain!(s, anyd, wspc_or_cmnt);
    let ret_type = chain!(s, type_expr);
    chain!(s, anyd, wspc_or_cmnt);

    // if there is an '=', allow any expression, if not, allow only a block
    let body = if s.starts_with("=") {
        chain!(s, seq, "=");
        chain!(s, anyd, wspc_or_cmnt);
        let expr = chain!(s, expr);
        chain!(s, anyd, wspc_or_cmnt);
        chain!(s, sat, |c| c == ';');
        Some(expr)
    } else if (allow_sign_only) && s.starts_with(";") {
        chain!(s, seq, ";");
        None
    } else {
        let block = chain!(s, block);
        // the ";" is optional for a block
        chain!(s, zeroorone, |mut s| {
            chain!(s, anyd, wspc_or_cmnt);
            chain!(s, seq, ";");
            Ok(((), s))
        });
        Some(block)
    };

    let res = Sttm::OperDecl {
        oper: operator,
        
        left_arg: left,
        right_arg: right,
        
        oper_prec: prec,
        
        def: Some( OperDef {
            gparams: gparams,
            has_self: has_self,
            params: params,
            ret_type: ret_type,
            body: body.map(Box::new),
        })
    };

    Ok((res, s))
}

fn class_decl(s: &str) -> ParseRes<Sttm> {
    /*
    class _name_<G1, _class_expr_ G2> for _texpr_ {
        func signatures...
    }
    */

    let mut s = s;
    chain!(s, seq, "class");
    chain!(s, anyd, wspc_or_cmnt);
    let name = chain!(s, ident_str);
    chain!(s, anyd, wspc_or_cmnt);

    // check for generic type parameters
    let gparams = chain!(s, generic_arguments);
    chain!(s, anyd, wspc_or_cmnt);

    chain!(s, seq, "for");
    chain!(s, anyd, wspc_or_cmnt);

    let inst_t = chain!(s, type_expr);
    match &inst_t {
        TypeExpr::Named { path, .. } | TypeExpr::Generic { path,  .. }
            if path.len() == 0  => {},
        _ => return Err(format!("Expected a named or generic type expression for class instance type")),
    }
    chain!(s, anyd, wspc_or_cmnt);

    chain!(s, seq, "{");
    chain!(s, anyd, wspc_or_cmnt);

    let mut funcs = vec![];
    loop {
        // is it a function/method?
        if let Ok((fnc, new_s)) = func_decl(s, true) {
            if let Sttm::FuncDecl { body, .. } = &fnc {
                if body.is_some() {
                    return Err(format!("Functions in a class cannot have a body")); // todo: allow this for default implementations
                }
            }
            funcs.push(fnc);
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
        // is it an operator declaration?
        } else if let Ok((fnc, new_s)) = oper_decl(s, true) {
            if let Sttm::OperDecl { def: Some(OperDef { body: Some(_), .. }), .. } = &fnc {
                return Err(format!("Operators in a class cannot have a body"));
            }
            funcs.push(fnc);
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
        } else {
            break;
        }
    }
    chain!(s, seq, "}");

    let res = Sttm::ClassDecl { name, gparams, inst_t, funcs };
    Ok((res, s))
}

fn impl_decl(s: &str) -> ParseRes<Sttm> {
    /*
    impl<G1, _class_expr_ G2> _class_expr_ for _type_expr_ {
        func implementations...
    }
    */
    let mut s = s;
    chain!(s, seq, "impl");
    chain!(s, anyd, wspc_or_cmnt);
    let gparams = chain!(s, generic_arguments);
    chain!(s, anyd, wspc_or_cmnt);
    let cls = chain!(s, class_expr);
    // don't allow multi class_exprs
    if let ClassExpr::Multi { .. } = &cls {
        return Err(format!("Multiple classes can't be implemented at once"));
    }
    chain!(s, anyd, wspc_or_cmnt);
    chain!(s, seq, "for");
    chain!(s, anyd, wspc_or_cmnt);
    let tname = chain!(s, type_expr);

    chain!(s, seq, "{");
    chain!(s, anyd, wspc_or_cmnt);

    let mut funcs = vec![];
    loop {
        // is it a function/method?
        if let Ok((fnc, new_s)) = func_decl(s, true) {
            funcs.push(fnc);
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
        // is it an operator declaration?
        } else if let Ok((fnc, new_s)) = oper_decl(s, true) {
            funcs.push(fnc);
            s = new_s;
            chain!(s, anyd, wspc_or_cmnt);
        } else { break; }
    }
    chain!(s, seq, "}");

    let res = Sttm::Impl {
        gparams: gparams,
        class: cls,
        inst: tname,
        funcs: funcs,
    };

    Ok((res, s))
}

