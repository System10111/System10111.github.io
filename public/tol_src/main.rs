mod interpreter;
mod parser;
mod types;
mod tests;
use std::io;

// const MAKE_STRING_START: &str = "Str::str(";
// const MAKE_STRING_END: &str = ")";
// the above style crashes for some examples like "[[]]", so as a patch, we use the following:
const MAKE_STRING_START: &str = "(";
const MAKE_STRING_END: &str = ").str";

const SETUP: &str = "
let map = (f,l)-> match l { 
    [] => [], 
    [x:xs] => [f(x) : map(f, xs)]
};
let iota = [0,1,2,3,4,5,6,7,8,9: map(x->x+10, iota)];

let zip_w = (f, xs, ys) -> match (xs, ys) {
    ([], _) => [],
    (_, []) => [],
    ([x:xs], [y:ys]) => [f(x,y) : zip_w(f, xs, ys)]
};
let fib = {
    let tail = l -> match l { [x:xs] => xs };
    let fib = [0, 1 : zip_w( (x,y)->x+y, fib, tail(fib) )];
    = fib;
};
";
// { let iota = [0: map(x->x+1, iota)]; =iota.str; }
// a bit more efficient iota: { let iota = [0,1,2,3,4,5: map(x->x+6, iota)]; =iota.str; }

/*
custom operators:
-----------------
// declares(but does not define) a unary operator 
// for now it does not have precedence, in fact, this statement does nothing at all
// but this is just future proofing the system
operator +_; 

// declares(but does not define) a left associative binary operator with precedence 6 
operator _+_ ~ left 6; 

// declares(but does not define) an operator that inherits its associativity and precedence from another operator
// this is useful as trying to import two modules with the same operator, but with different associativity or precedence
// will cause a conflict. this way you can import one of them and let the other copy its associativity and precedence
operator _-_ ~= +;

// defines(but does not declare associativity or precedence for) a binary operator
// if the operator was not declared before, an error will be thrown
operator _++_ ([char] xs, [char] ys) -> [char] = xs `concat ys;

// defines and declares associativity and precedence for a binary operator
// note an extra '~' between the precedence and function signature
operator _++_ ~ left 6 ~ ([char] xs, [char] ys) -> [char] = xs `concat ys;

// defines and inherits associativity and precedence for a binary operator
operator _++_ ~= + ([char] xs, [char] ys) -> [char] = xs `concat ys;

// defines a generic binary operator
operator<T> _++_ ([T] xs, [T] ys) -> [T] = xs `concat ys;

// defines a binary operator without a body - it can only happen in a class
class Foo for T {
    operator _&%@_ ~ left 5 ~ (T xs, T ys) -> T;
}



// the most complicated operator definition I could think of
operator<Num T> _+~*_ ~ left 6 ~ ([T] xs, [T] ys) -> T = fold( (acc,x)->acc+s, 1, zip_w((x,y)->x*y, xs, ys) );
// it defines a generic operator '+~*' which takes two lists of the same type (which must be a Num) and returns a value of that type
// which (not relevant to the operator syntax) is just the dot product of the two lists


example:
file num.tol:
    ...
    operator _+_ ~ left 6;
    operator _-_ ~= +;
    ...

file vec2.tol:
    ...
    #import num; // not final import syntax
    ...
    struct Vec2 { int x; int y; 
        operator _++_ ~= + (Vec2 a, Vec2 b) -> Vec2 = Vec2 { x: a.x + b.x, y: a.y + b.y };
        // note: this operator is defined inside the struct, but that is just a matter of style and aids organization and readability
        // defining it outside the struct would be equivalent
    }
    ... 
file list_helpers.tol:
    ...
    #import num;
    ...
    operator _++_ ~= + ([T] xs, [T] ys) -> [T] = xs `concat ys;
    ...
file main.tol:
    ...
    #import vec2;
    #import list_helpers;
    // if _++_ was declared differently in vec2 and list_helpers,
    // an error would be thrown here
    // fortunately, it was defined to have the same associativity and precedence as _+_,
    // so even if we change the declaration for _+_, it will still work
*/


/*  
I just want to point out how 'map' is like the first major boss of developing this language.
It is a lambda which matches on one of the arguments and possibly recurses with the other and a match variable.
It uses pretty much all the features the language currently has.
Gonna start keeping a list of the bosses and making Elden Ring puns about them.

> The great category theory was shattered - 
    and the correct implementation of the lambda calculus was nowhere to be found
    Soon its offspring, demibugs all, claimed the shards of category theory.
    What followed was a great labour - the refactiong.
> Mapgit, the fell fomen
> Iotrick, the recursively grafted
> Classan, the conqueror of functional polymorphism
> Namespacestel, naturalborn of encapsulation
> Usability princess Replanni

*/
fn main() {
    let mut ctx = interpreter::Context::new();
    if SETUP.len() > 0 {
        interpreter::execute(SETUP, &mut ctx).unwrap();
    }
    let mut ast_on = false;
    let mut type_check_on = true;
    let mut eval_on = true;
    let mut auto_string = true;
    let mut print_non_strings = false;
    loop {
        let mut input = String::new();
        let str_input;
        io::stdin().read_line(&mut input).unwrap();
        match input.trim() {
            "!q" => break,
            "!a" => {
                ast_on = !ast_on; 
                println!("ast {}\n", if ast_on { "on" } else { "off" }); 
                continue;
            },
            "!t" => {
                type_check_on = !type_check_on; 
                println!("type check {}\n", if type_check_on { "on" } else { "off" }); 
                continue;
            },
            "!e" => {
                eval_on = !eval_on; 
                println!("eval {}\n", if eval_on { "on" } else { "off" }); 
                continue;
            },
            "!ps" => {
                print_non_strings = !print_non_strings; 
                println!("print non-strings {}\n", if print_non_strings { "on" } else { "off" }); 
                continue;
            },
            "!s" => {
                auto_string = !auto_string; 
                println!("auto string {}\n", if auto_string { "on" } else { "off" }); 
                continue;
            },
            "!h" => {
                println!("
                    !q: quit\n
                    !a: toggle ast printing\n
                    !t: toggle type checking\n
                    !e: toggle evaluation\n
                    !ps: toggle printing non-strings\n
                    !s: toggle auto-stringing\n
                    !h: print this help message\n"
                );
                continue;
            },
            "" => continue,
            _ => {}
        };

        // parse the input
        let mut e = match parser::expr(&input) {
            Ok((_, rest)) if rest.trim().len() > 0 => {
                println!("Part of input not parsed: {}", rest);
                continue;
            },
            Ok((e, _)) => e,
            Err(err) => {
                println!("Syntax Error: {}", err);
                continue;
            }
        };
        
        if auto_string {
            let t = match types::infer(&e, &mut ctx) {
                Ok((t, _)) => t,
                Err(err) => {
                    println!("Type Error when converting to string: {}", err);
                    continue;
                }
            };
            if !types::is_string(&t) {
                str_input = MAKE_STRING_START.to_string() + &input + MAKE_STRING_END;
                e = match parser::expr(&str_input) {
                    Ok((e, _)) => e,
                    Err(err) => {
                        println!("Syntax Error when converting to string: {}", err);
                        continue;
                    }
                };
            }
            if type_check_on {
                println!("Type: {}", types::canonize(t));
            }
        }

        let type_correct = if type_check_on && !auto_string  {
            match types::infer(&e, &mut ctx) {
                Ok((t, _s)) => {
                    println!("Type: {}", types::canonize(t));
                    true
                },
                Err(err) => {
                    println!("Type Error: {}", err);
                    false
                }
            }
        } else { true };

        if type_correct && eval_on {
            let res = interpreter::expr_value(&e, &mut ctx);
            let (v, t) = match res {
                Ok((v, t, _)) => (v, t),
                Err(err) => {
                    println!("Error: {}\n", err);
                    continue;
                }
            };
            if types::is_string(&t) {
                print!("= ");
                match interpreter::eval_string(&v, &mut std::io::stdout().lock()) {
                    Ok(()) => {},
                    Err(err) => {
                        println!("Error: {}\n", err);
                        continue;
                    }
                };
                println!("");
            } else if print_non_strings {
                let eval_res = match interpreter::eval_whnf(&v) {
                    Ok(r) => r,
                    Err(err) => {
                        println!("Error: {}\n", err);
                        continue;
                    }
                }.unwrap_or(v);
                println!("= {}", eval_res);
            } else {
                println!("= <non-string>");
            }
            // if type_check_on {
            //     println!("Eval type: {}", types::canonize(t));
            // }
        }
        if ast_on {
            println!("AST: {:#?}", e);
        }
        println!("");
    }
}
