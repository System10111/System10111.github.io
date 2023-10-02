#[cfg(test)]
mod tests {
    use crate::{parser, interpreter, types, dbg_println};
    use interpreter::{Context, execute, expr_value, eval_whnf};
    fn check_eval(s: &str, expected: &str) -> Result<(), String> {
		check_eval_with_setup(&[], s, expected)
	}

    fn check_eval_with_setup(setup: &[&str], s: &str, expected: &str) -> Result<(), String> {
		let mut ctx = Context::new();
        dbg_println!(1, "===========");
        if setup.len() > 0 {
            execute(setup.join("\n").as_str(), &mut ctx)?;
            dbg_println!(1, "-----------");
        }
		let parse1 = parser::expr(s)?;
        dbg_println!(1, "Parsed: {:?}", parse1.0);
		let res1 = expr_value(&parse1.0, &mut ctx.child())?;
        let eval1 = if let Some(res) = eval_whnf(&res1.0)? { res } else { res1.0 };

        dbg_println!(1, "-----------");

        let parse2 = parser::expr(expected)?;
        let res2 = expr_value(&parse2.0, &mut ctx.child())?;
        let eval2 = if let Some(res) = eval_whnf(&res2.0)? { res } else { res2.0 };
		// let actual = format!("{}", canonize(infer_res.0));
		// assert_eq!(actual, expected);
		let t1 = types::canonize(res1.1);
        let t2 = types::canonize(res2.1);
        let un = types::unify(&types::instantiate(t1.clone(), &mut ctx).0, &types::instantiate(t2.clone(), &mut ctx).0, &ctx);

		if !(t1 == t2) & un.is_err() {
			return Err(format!("\nType of s: '{}' and expected: '{}' don't unify: {}\n", t1, t2, un.unwrap_err()));
		} 
        if eval1 != eval2 {
            return Err(format!("\nExpected '{}' but got '{}'\n", eval2, eval1));
        }
        Ok(())
    }

    fn check_eval_str(s: &str, expected: &str) -> Result<(), String> {
        check_eval_str_with_setup(&[], s, expected)
    }

    fn check_eval_str_with_setup(setup: &[&str], s: &str, expected: &str) -> Result<(), String> {
        let mut ctx = Context::new();
        dbg_println!(1, "===========");
        if setup.len() > 0 {
            execute(setup.join("\n").as_str(), &mut ctx)?;
            dbg_println!(1, "-----------");
        }
        let parse = parser::expr(s)?;
        dbg_println!(1, "Parsed: {:?}", parse.0);
		let res = expr_value(&parse.0, &mut ctx.child())?;
        if !types::is_string(&res.1) {
            return Err(format!("Expected [char] but got type: {}", res.1));
        }

        let mut str_vec = Vec::new();

        interpreter::eval_string(&res.0, &mut str_vec)?; 

        let res_string = String::from_utf8(str_vec)
            .map_err(|non_utf8| String::from_utf8_lossy(non_utf8.as_bytes()).into_owned())
            .unwrap();

        if res_string != expected {
            return Err(format!("\nExpected '{}' but got '{}'\n", expected, res_string));
        }
        Ok(())
    }

    #[test] fn eval_sanity() { check_eval("5", "5").unwrap() }
    #[test] fn eval_parnt() { check_eval("(5)", "5").unwrap() }
    #[test] fn eval_id1() { check_eval("(x->x)(5)", "5").unwrap() }
    #[test] fn eval_id2() { check_eval("(x->x)(x->x)(5)", "5").unwrap() }
    #[test] fn eval_id3() { check_eval("(x->y->x(y))(x->x, 2)", "2").unwrap() }
    #[test] fn eval_id4() { check_eval("((x->y->x(y))(x->x, x->x))(1)", "1").unwrap() }
    #[test] fn eval_cnst1() { check_eval("(x->3)(5)", "3").unwrap() }
    #[test] fn eval_cnst2() { check_eval("(x->6)(x->x)", "6").unwrap() }
    #[test] fn eval_inner_lmb() { check_eval("(x->(y->y)(3))(2)", "3").unwrap() }
    #[test] fn eval_indr() { check_eval("((x,y,z)->x(y,z))(x->y->x, 9, 5)", "9").unwrap() }
    #[test] fn eval_clft() { check_eval("(x->y->x)(7, 2)", "7").unwrap() } // (x-> ((x,y)->x)(x))(7, 2)
    #[test] fn eval_crgt() { check_eval("(x->y->y)(7, 2)", "2").unwrap() } // (x-> (x->x))(7, 2)
    #[test] fn eval_clft_indr() { check_eval("(f->f(3, 5))(x->y->x)", "3").unwrap() }
    #[test] fn eval_cll_lmd1() { check_eval("(x->x(5))(x->1)", "1").unwrap() }
    #[test] fn eval_cmp() { check_eval("( (x,y,z)->x(y(z)) )(x->x, x->2, 9)", "2").unwrap() }
    #[test] fn eval_un1() { check_eval("-3", "-3").unwrap() }
    #[test] fn eval_un2() { check_eval("(x->-x)(5)", "-5").unwrap() }
    #[test] fn eval_un3() { check_eval("- -7", "7").unwrap() }
    #[test] fn eval_un4() { check_eval("(x->- -x)(5)", "5").unwrap() }
    #[test] fn eval_un5() { check_eval("(x->y->-x)(3,2)", "-3").unwrap() }
    #[test] fn eval_bin1() { check_eval("3+5", "8").unwrap() }

    #[test] fn eval_bin2() { check_eval("( (x,y,z)->x(y(z),z) ) ( (x,y)->x*y, x->1+x, 12 )", "156").unwrap() } // 12 * (12 + 1) using the S combinator
    #[test] fn eval_bin3() { check_eval("2*4+9*2", "26").unwrap() }
    #[test] fn eval_bin4() { check_eval("(x->-x+1)(3)", "-2").unwrap() }
    #[test] fn eval_bin5() { check_eval("(x->x*3+1)(2)", "7").unwrap() }
    #[test] fn eval_bin6() { check_eval("(f->f(3, 5))((x,y)->x+y)", "8").unwrap() }
    #[test] fn eval_bin7() { check_eval("(x->y->y*x)(3,2)", "6").unwrap() }
    #[test] fn eval_if1() { check_eval("if 1==1: 2 else 3", "2").unwrap() }
    #[test] fn eval_if2() { check_eval("if 10==14-4: 3 else 8", "3").unwrap() }
    #[test] fn eval_if3() { check_eval("(x->if x: 3 else 5)(3==2)", "5").unwrap() }
    #[test] fn eval_if4() { check_eval("(x->if x==5: 13 else x-3)(2+8)", "7").unwrap() }
    #[test] fn eval_ski() { check_eval("( (x,y,z)->x(z, y(z)) ) ( (x,y)->x )(x->x)(10)", "10").unwrap() }
    #[test] fn eval_block1() { check_eval("{ let f = x->y->x; = f(f); }(1,2,3)", "2").unwrap() }
    // what's interresting about this next set of tests is that x is an outer variable to the block, which leads to some pretty difficult cases
    #[test] fn eval_block2() { check_eval("(x->{let f = y->y; =f(f(3));})(5)", "3").unwrap() }
    #[test] fn eval_block3() { check_eval("(x->{let f = y->y*2; =f(3);})(5)", "6").unwrap() }
    #[test] fn eval_block4() { check_eval("(x->{let f = y->y; =f(f(x));})(3)", "3").unwrap() }
    #[test] fn eval_block5() { check_eval("(x->{let f = y->-y; =f(x);})(3)", "-3").unwrap() }
    #[test] fn eval_block6() { check_eval("(x->{let f = y->-y; =f(f(x));})(3)", "3").unwrap() } 
    #[test] fn eval_block7() { check_eval("(x->{let f = y->x; =f(f(2));})(3)", "3").unwrap() }
    #[test] fn eval_block8() { check_eval("(x->{let f = y->y*x; =f(f(2));})(3)", "18").unwrap() }
    #[test] fn eval_block9() { check_eval("(x-> {let f1 = y->y*2; let f2 = y->y*y; =f1(f2(f1(x))); })(3)", "72").unwrap() }
    #[test] fn eval_man_rec1() { check_eval(
        "{let f = (h,n)-> n*h(n-1); = n->f(f(x->x), n);}(4)", 
        "24"
    ).unwrap() }
    #[test] fn eval_man_rec2() { check_eval(
        // even though real recursion is supported, 
        // this test should still be left as is, to test manual recursion
        // "{let f = (h,n)-> if n==0: 1 else n*h(n-1); = n->f(f(f(x->1)), n);}(4)", 
        "{let f = (h,n)-> n*h(n-1); = n->f(f(f(x->1)), n);}(4)", 
        "24"
    ).unwrap() }
    #[test] fn eval_rec1() { check_eval(
        "{let f = n-> if n==0: 0 else f(n-1); =f;}(5)", 
        "0"
    ).unwrap() }
    #[test] fn eval_rec2() { check_eval(
        "{let fact = n-> if n==0: 1 else n*fact(n-1); =fact;}(5)", 
        "120"
    ).unwrap() }
    #[test] fn eval_rec3() { check_eval(
        "{let fib = n-> if n==0: 0 else if n==1: 1 else fib(n-1)+fib(n-2); =fib;}(10)", 
        "55"
    ).unwrap() }
    #[test] fn eval_fix1() { check_eval(
        "{let fix = f->{let x = f(x); =x;}; =fix(x->3);}", 
        "3"
    ).unwrap() }
    #[test] fn eval_fix2() { check_eval(
        "{let fix = f->{let x = f(x); =x;}; =fix( (f, n)-> if n==0: 0 else f(n-1) );}(1)", 
        "0"
    ).unwrap() }
    #[test] fn eval_fix3() { check_eval(
        // this is yet another way to construct the factorial function, this time using the fixpoint combinator
        "{let fix = f->{let x = f(x); =x;}; =fix( (fact,n)-> if n==0: 1 else n*fact(n-1) );}(5)", 
        "120"
    ).unwrap() }
    #[test] fn eval_fix4() { check_eval(
        "{
                let fix = f->{let x = f(x); =x;}; 
                =fix ( 
                    (fib,n)-> if n==0: 0 else if n==1: 1 else fib(n-1)+fib(n-2) 
                );
            }(7)", 
        "13"
    ).unwrap() }
    #[test] fn eval_tuple1() { check_eval("(x->(x,x))(5)", "(5, 5)").unwrap() }
    // this test fails because we're evaluating up to WHNF
    // when a strict evaluation operator(?) is implemented, this test should be made to use it
    // or since convering to string is a strict operation, we could just use that when it is implemented
    // #[test] fn eval_tuple_ls() { check_eval(
    //     "{let ls = (l,r)->(l,r); =ls(0,ls(1,ls(2,3)));}", 
    //     "(0,(1,(2,3)))"
    // ).unwrap() }

    #[test] fn eval_compose() { check_eval_with_setup(&["let compose = (f, g)-> x-> f(g(x));"], "compose(x->x+1, x->x*2)(3)", "7").unwrap() }
    //{ let twice = (f,x)->f(f(x)); =(twice(twice))(x->-x,3);}
    #[test] fn eval_quad1() { check_eval("{ let twice = (f,x)->f(f(x)); =(twice(twice))(x->-x,3);}", "3").unwrap() }
    #[test] fn eval_quad2() { check_eval_with_setup(&["let twice = f->x->f(f(x)); let quad = twice(twice);"], "quad(x->x*2, 3)", "48").unwrap() }
    #[test] fn eval_match1() { check_eval("match 1 { 1 => 5, _ => 42 }", "5").unwrap() }
    #[test] fn eval_match2() { check_eval("(x-> match x { 1 => 5, _ => 42 })(3)", "42").unwrap() }
    #[test] fn eval_match3() { check_eval("(x-> match x { 1 => 5, x => x })(3)", "3").unwrap() }
    #[test] fn eval_match4() { check_eval("(x-> match x { 1 => 5, _ => x })(3)", "3").unwrap() }
    #[test] fn eval_match5() { check_eval("(x->y-> match 1 { _ => x+y })(3, 4)", "7").unwrap() }
    #[test] fn eval_match6() { check_eval("(x->y-> match x { 0 => y, _ => x })(0, 4)", "4").unwrap() }
    #[test] fn eval_match_tuple() { check_eval_with_setup(&["let f = tup-> match tup {(1,r) => r, (l,r) => l + r};"], "f((3,4))", "7").unwrap() }
    #[test] fn eval_match_tuple2() { check_eval("match (1, 99) { (x, y) => y }", "99").unwrap() }


    #[test] fn eval_comment1() { check_eval("5 + 5 // should be 10", "10").unwrap() }
    #[test] fn eval_comment2() { check_eval("3 + // this comment ends here:\n5 ", "8").unwrap() }
    #[test] fn eval_comment3() { check_eval("2 /*inline comment!*/ + 5", "7").unwrap() }

    #[test] fn eval_func_un1() {check_eval_with_setup(&["let f = x->x+3;"], "`f 5", "8").unwrap()}
    #[test] fn eval_func_un2() {check_eval_with_setup(&["let f = x->x+3;"], "`f `f 5", "11").unwrap()}
    #[test] fn eval_func_bin1() {check_eval_with_setup(&["let f = (x,y)->x+y;"], "5 `f 3", "8").unwrap()}
    #[test] fn eval_func_bin2() {check_eval_with_setup(&["let f = (x,y)->x+y;"], "5 `f 3 `f 2", "10").unwrap()}

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
		Cons(x,xs) => Cons(f(x), map(f, xs)) 
	};
	let iota = List::Cons(0, map(x->x+1, iota));
	";
    #[test] fn eval_vlist1() { check_eval_with_setup(
        &[VLISTS], 
        "head(Cons(5, Nil))",
        "5"
    ).unwrap() }
    #[test] fn eval_vlist2() { check_eval_with_setup(
        &[VLISTS], 
        "tail(Cons(5, Nil))",
        "List::Nil"
    ).unwrap() }
    #[test] fn eval_vlist3() { check_eval_with_setup(
        &[VLISTS], 
        "lget(Cons(5, Nil), 0)",
        "5"
    ).unwrap() }
    #[test] fn eval_vlist4() { check_eval_with_setup(
        &[VLISTS], 
        // "lget(map(x->x+5, Cons(0, Nil) ), 0)",
        "map(x->x, Nil)",
        "List::Nil"
    ).unwrap() }
    #[test] fn eval_vlist5() { check_eval_with_setup(
        &[VLISTS], 
        "lget(map(x->x*2, Cons(0, Cons(1, Cons(2, Cons(3, Nil)))) ), 3)",
        "6"
    ).unwrap() }
    #[test] fn eval_vlist6() { check_eval_with_setup(
        &[VLISTS], 
        "lget(iota, 5)",
        "5"
    ).unwrap() }
    #[test] fn eval_vlist7() { check_eval_with_setup(
        &[VLISTS], 
        "lget(map(x->x*x, iota), 5)",
        "25"
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
    let fold = (f, acc, l)-> match l {
        [] => acc,
        [x:xs] => fold(f, f(acc, x), xs)
    };
	";
    #[test] fn eval_list_sanity() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "[]",
        "[]"
    ).unwrap() }
    #[test] fn eval_list1() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "[1,2,3]",
        "[1,2,3]"
    ).unwrap() }
    #[test] fn eval_list2() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "[1,2:[3,4,5]]",
        "[1,2,3,4,5]"
    ).unwrap() }
    #[test] fn eval_list3() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "head([1,2,3])",
        "1"
    ).unwrap() }
    #[test] fn eval_list4() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "tail([1,2,3])",
        "[2,3]"
    ).unwrap() }
    // #[test] fn eval_list5() { check_eval_with_setup(
    //     &[LIST_HELPERS], 
    //     "map(x->x*2, [1,2,3])",
    //     "[2,4,6]"
    // ).unwrap() } // doesn't work because of whnf
    #[test] fn eval_list6() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "lget(map(x->x*x, iota), 5)",
        "25"
    ).unwrap() }
    // #[test] fn eval_list7() { check_eval_with_setup(
    //     &[LIST_HELPERS], 
    //     "(xs-> [head(xs):xs])([1,2,3])",
    //     "[1,1,2,3]"
    // ).unwrap() }
    // #[test] fn eval_list8() { check_eval_with_setup(
    //     &[LIST_HELPERS], 
    //     "map(xs->head(xs), [[1,2,3],[4,5,6],[7,8,9]])",
    //     "[1,4,7]"
    // ).unwrap() }
    
    // eval_list 5,7 and 8 are don't work yet, because a thunk evaluating to 1 and a value of 1 are treated as different
    // they should work when the check is performed on the string representation of the values (which will force the thunk to be evaluated)
    #[test] fn eval_list_fold() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "fold((acc,x)->acc+x, 0, [1,2,3])",
        "6"
    ).unwrap() }

    const GVARIANT: &str = "
	variant EOrN<L, R> {
		Left(L);
		Right(R);
		None;
	}
	";
    #[test] fn eval_gvariant1() { check_eval_with_setup(
        &[GVARIANT], 
        "EOrN::Left(3)",
        "EOrN::Left(3)"
    ).unwrap() }
    #[test] fn eval_gvariant2() { check_eval_with_setup(
        &[GVARIANT], 
        "EOrN::Right(3)",
        "EOrN::Right(3)"
    ).unwrap() }
    #[test] fn eval_gvariant3() { check_eval_with_setup(
		&[GVARIANT],
		"(x-> if x == 0: EOrN::Left(5) else Right([1]))(20)", 
		"EOrN::Right([1])"
	).unwrap() }
    #[test] fn eval_gvariant4() { check_eval_with_setup(
        &[GVARIANT],
        "EOrN::None",
        "EOrN::None"
    ).unwrap() }
    
    const STRUCTEG: &str = "
	struct S {
		int x;
		[int] ys;
	}
	";
    #[test] fn eval_struct1() { check_eval_with_setup(
        &[STRUCTEG], 
        "S{ x = 3; ys = [1,2,3]; }.x",
        "3"
    ).unwrap() }
    #[test] fn eval_struct2() { check_eval_with_setup(
        &[STRUCTEG], 
        "S{ x = 3; ys = [1,2,3]; }.ys",
        "[1,2,3]"
    ).unwrap() }
    #[test] fn eval_struct3() { check_eval_with_setup(
        &[STRUCTEG, LIST_HELPERS], 
        "lget(S{ x = 3; ys = [1,2,3]; }.ys, 1)",
        "2"
    ).unwrap() }

    const GSTRUCT1: &str = "
	struct S<T> {
		T x;
		[T] ys;
	}
	";
    #[test] fn eval_gstruct1() { check_eval_with_setup(
        &[GSTRUCT1], 
        "S{ x = 3; ys = [1,2,3]; }.x",
        "3"
    ).unwrap() }
    #[test] fn eval_gstruct2() { check_eval_with_setup(
        &[GSTRUCT1, LIST_HELPERS], 
        "lget(S{ x = [1,2]; ys = [[], [1]]; }.ys, 1)",
        "[1]"
    ).unwrap() }

    const GSTRUCT2: &str = "
	struct S<X, Y> {
		X x;
		Y y;
	}
	";
    #[test] fn eval_gstruct3() { check_eval_with_setup(
        &[GSTRUCT2], 
        "S{ x = 3; y = 4; }.x",
        "3"
    ).unwrap() }
    #[test] fn eval_gstruct4() { check_eval_with_setup(
        &[GSTRUCT2], 
        "S{ x = 3; y = [4]; }.y",
        "[4]"
    ).unwrap() }
    #[test] fn eval_gstruct5() { check_eval_with_setup(
        &[GSTRUCT2, LIST_HELPERS], 
        "lget(S{ x = 3; y = [4,5]; }.y, 1)",
        "5"
    ).unwrap() }
    #[test] fn eval_gstruct6() { check_eval_with_setup(
        &[GSTRUCT2, LIST_HELPERS], 
        "S{ x = []; y = [4,5]; }.x",
        "[]"
    ).unwrap() }

    #[test] fn eval_variant_func() { check_eval_with_setup(
        &["
        variant Either<L, R> {
            Left(L);
            Right(R);
            func is_left(self) -> bool = match self {
                Left(_) => true,
                Right(_) => false
            };
        }"], 
        "Either::Left(3).is_left()",
        "true"
    ).unwrap() }

    #[test] fn eval_struct_func() { check_eval_with_setup(
        &["
        struct S {
            int x;
            func get_x_plus_5(self) -> int = self.x + 5;
        }"], 
        "S{ x = 3; }.get_x_plus_5()",
        "8"
    ).unwrap() }

    #[test] fn eval_variant_gfunc() { check_eval_with_setup(
        &["
        variant Option<T> {
            Some(T);
            None;
            func replace<U>(self, U u) -> Option<U> = match self {
                Some(_) => Some(u),
                None => None
            };
        }"], 
        "Option::Some(true).replace(3)",
        "Option::Some(3)"
    ).unwrap() } 

    #[test] fn eval_struct_gfunc() { check_eval_with_setup(
        &["
        struct S<T> {
            int x;
            T data;
            func keep_x<U>(self, U u) -> S<U> = S{ x = self.x; data = u; };
        }"], 
        "S{ x = 3; data = true; }.keep_x(8).data",
        "8"
    ).unwrap() }

    #[test] fn eval_func_decl() { check_eval_with_setup(
        &["func f(int x) -> int = x + 5;"], 
        "f(2)",
        "7"
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

    #[test] fn eval_basic_class1() { check_eval_with_setup(
        &[BASIC_CLASS], 
        "calc(5)",
        "8"
    ).unwrap() }
    #[test] fn eval_basic_class2() { check_eval_with_setup(
        &[BASIC_CLASS], 
        "calc(Foo{ x = 3; y = [1,2,3]; })",
        "6"
    ).unwrap() }
    #[test] fn eval_basic_class3() { check_eval_with_setup(
        &[BASIC_CLASS], 
        "(x->calc(x) + calc(x))(Foo { x = 3; y=[];})",
        "12"
    ).unwrap() }
    #[test] fn eval_basic_class4() { check_eval_with_setup(
        &[BASIC_CLASS], 
        "((x,y)->calc(x) + calc(y))(Foo { x = 3; y=[];}, 5)",
        "14"
    ).unwrap() }
    #[test] fn eval_basic_class5() { check_eval_with_setup(
        &[BASIC_CLASS], 
        "(x-> Intable::as_int(x) + 7)(Foo { x = 3; y=[];})",
        "10"
    ).unwrap() }
    #[test] fn eval_basic_class6() { check_eval_with_setup(
        &[BASIC_CLASS], 
        "{ let f = x -> Intable::as_int(x); =f(5); }",
        "5"
    ).unwrap() }
    #[test] fn eval_addable_class() { check_eval_with_setup(
        &["
        class Addable for T {
            func add(self, T other) -> T;
        }
        impl Addable for int {
            func add(self, int other) -> int = self + other;
        }
        func test<Addable T>([T] xs, [T] ys) -> T = 
            match (xs, ys) {
                ([x : _], [y : _]) => x.add(y)
            };
        "], 
        "test([1,2,3], [4,5,6])",
        "5"
    ).unwrap() }
    #[test] fn eval_class_impl_adds_member_method() { check_eval_with_setup(
        &["
        class Addable for T {
            func add(self, T other) -> T;
        }
        impl Addable for int {
            func add(self, int other) -> int = self + other;
        }
        "], 
        "5.add(7) + int::add(2, 4)",
        "18"
    ).unwrap() }


    const GCLASS : &str = "
    class To<T> for F {
        func to(self) -> T;
    }
    impl To<int> for int {
        func to(self) -> int = self;
    }
    impl To<[int]> for int {
        func to(self) -> [int] = [self];
    }
    impl To<int> for [int] {
        func to(self) -> int = match self {
            [x : _] => x,
            _ => 0
        };
    }
    impl To<[int]> for [int] {
        func to(self) -> [int] = self;
    } 
    ";
    #[test] fn eval_gclass1() { check_eval_with_setup(
        &[GCLASS], 
        "5.to<int>()",
        "5"
    ).unwrap() }
    #[test] fn eval_gclass2() { check_eval_with_setup(
        &[GCLASS], 
        "5.to<[int]>()",
        "[5]"
    ).unwrap() }
    #[test] fn eval_gclass3() { check_eval_with_setup(
        &[GCLASS], 
        "[5].to<int>()",
        "5"
    ).unwrap() }
    #[test] fn eval_gclass4() { check_eval_with_setup(
        &[GCLASS], 
        "[5].to<[int]>()",
        "[5]"
    ).unwrap() }
    #[test] fn eval_gclass5() { check_eval_with_setup(
        &[GCLASS], 
        "[int]::to<int>([5])",
        "5"
    ).unwrap() }
    #[test] fn eval_gclass6() { check_eval_with_setup(
        &[GCLASS], 
        "(x-> To::to<int>(x))(5)",
        "5"
    ).unwrap() }
    #[test] fn eval_gclass7() { check_eval_with_setup(
        &[GCLASS], 
        "(x-> To::to<[int]>(x))(5)",
        "[5]"
    ).unwrap() }
    #[test] fn eval_gclass8() { check_eval_with_setup(
        &[GCLASS], 
        "(x-> To::to<int>(x))([5])",
        "5"
    ).unwrap() }
    #[test] fn eval_gclass9() { check_eval_with_setup(
        &[GCLASS, "func calc<To<int> T>(T x) -> int = x.to() + 3;"], 
        // we can skip the <int> in x.to<int>() because the 'to' type is already known
        "calc([5])",
        "8"
    ).unwrap() }

    const GIMPL: &str = "
    class Intable for T {
        func as_int(self) -> int;
    }
    impl Intable for int {
        func as_int(self) -> int = self;
    }
    impl Intable for bool {
        func as_int(self) -> int = if self: 1 else 0;
    }
    impl<Intable T> Intable for [T] {
        func as_int(self) -> int = match self {
            [x : _] => x.as_int(),
            _ => 0
        };
    }
    ";
    #[test] fn eval_gimpl1() { check_eval_with_setup(
        &[GIMPL], 
        "[5].as_int()",
        "5"
    ).unwrap() }
    #[test] fn eval_gimpl2() { check_eval_with_setup(
        &[GIMPL], 
        "[true].as_int()",
        "1"
    ).unwrap() }
    #[test] fn eval_gimpl3() { check_eval_with_setup(
        &[GIMPL], 
        "[].as_int()",
        "0"
    ).unwrap() }
    #[test] fn eval_gimpl4() { check_eval_with_setup(
        &[GIMPL], 
        "(f->f(1))(x->Intable::as_int(x))",
        "1"
    ).unwrap() }
    #[test] fn eval_gimpl5() { check_eval_with_setup(
        &[GIMPL], 
        "((f,x)->f(x))(x->Intable::as_int(x), 1)",
        "1"
    ).unwrap() }

    #[test] fn eval_string_sanity() { check_eval_str(
        "\"hello\"",
        "hello"
    ).unwrap() }
    #[test] fn eval_string_get() { check_eval_with_setup(
        &[LIST_HELPERS], 
        "lget(\"hello\", 3)",
        "'l'"
    ).unwrap() }
    const CONCAT: &str = "
    func concat<T>([T] xs, [T] ys) -> [T] = 
        match (xs, ys) {
            ([], ys) => ys,
            ([x : xs], ys) => [x : concat(xs, ys)]
        };";
    #[test] fn eval_string_concat1() { check_eval_str_with_setup(
        &[CONCAT], "concat(\"\", \"\")",
        ""
    ).unwrap() }
    #[test] fn eval_string_concat2() { check_eval_str_with_setup(
        &[CONCAT], "concat(\"x\", \"\")",
        "x"
    ).unwrap() }
    #[test] fn eval_string_concat3() { check_eval_str_with_setup(
        &[CONCAT], "concat(\"\", \"x\")",
        "x"
    ).unwrap() }
    #[test] fn eval_string_concat4() { check_eval_str_with_setup(
        &[CONCAT], "concat(\"h\", \"i\")",
        "hi"
    ).unwrap() }
    #[test] fn eval_string_concat5() { check_eval_str_with_setup(
        &[CONCAT], "concat(\"hello \", \"world\")",
        "hello world"
    ).unwrap() }
    #[test] fn eval_string_concat6() { check_eval_str_with_setup(
        &[CONCAT], "\"hi\" `concat \" there\" `concat \" you\"",
        "hi there you"
    ).unwrap() }
    const INTERSPERSE : &str = "
    func intersperse<T>(T sep, [T] l) -> [T] = match l {
        [] => [],
        [x] => [x],
        [x : xs] => [x, sep : intersperse(sep, xs)]
    };";
    #[test] fn eval_string_intersperse() { check_eval_str_with_setup(
        &[INTERSPERSE], "intersperse(' ', \"hello\")",
        "h e l l o"
    ).unwrap() }

    const INTERCALATE : &str = "
    let fold = (f, acc, l)-> match l {
        [] => acc,
        [x:xs] => fold(f, f(acc, x), xs)
    };
    let intercalate = (sep, l) -> fold(concat, [], intersperse(sep, l));";
    #[test] fn eval_string_intercalate1() { check_eval_str_with_setup(
        &[CONCAT, INTERSPERSE, INTERCALATE], 
        "intercalate(\",\", [])",
        ""
    ).unwrap() }
    #[test] fn eval_string_intercalate2() { check_eval_str_with_setup(
        &[CONCAT, INTERSPERSE, INTERCALATE], 
        "intercalate(\",\", [\"a\"])",
        "a"
    ).unwrap() }
    #[test] fn eval_string_intercalate3() { check_eval_str_with_setup(
        &[CONCAT, INTERSPERSE, INTERCALATE], 
        "intercalate(\" there \", [\"hello\", \"world\"])",
        "hello there world"
    ).unwrap() }
    #[test] fn eval_string_intercalate4() { check_eval_str_with_setup(
        &[CONCAT, INTERSPERSE, INTERCALATE], 
        "intercalate(\",\", [\"1\", \"2\", \"3\"])",
        "1,2,3"
    ).unwrap() }

    // const STR_CLASS: &str = "
    // func concat<T>([T] xs, [T] ys) -> [T] = 
    //     match (xs, ys) {
    //         ([], ys) => ys,
    //         ([x : xs], ys) => [x : concat(xs, ys)]
    //     };
	// let map = (f,l)-> match l { 
	// 	[] => [], 
	// 	[x:xs] => [f(x) : map(f, xs)]
	// };
    // class Str for T {
    //     func str(self) -> [char];
    // }
    // impl Str for int {
    //     func str(self) -> [char] = match self {
    //         0 => ['0'],
    //         1 => ['1'],
    //         2 => ['2'],
    //         3 => ['3'],
    //         4 => ['4'],
    //         5 => ['5'],
    //         6 => ['6'],
    //         7 => ['7'],
    //         8 => ['8'],
    //         9 => ['9'],
    //         _ => if self < 0: ['-' : str(-self)]
    //             else str(self / 10) `concat str(self - self / 10 * 10)
    //     };
    // }
    // impl<Str T> Str for [T] {
    //     func str(self) -> [char] {
    //         let strl = l -> match l {
    //             [] => [']'],
    //             [x:xs] => [',', ' ' : Str::str(x) `concat strl(xs)] 
    //         };
    //         = match self {
    //             [] => \"[]\",
    //             [x:xs] => ['[' : Str::str(x) `concat strl(xs)] 
    //         };
    //     }
    // } 
    // ";
    #[test] fn eval_str_class1() { check_eval_str(
        "5.str()",
        "5"
    ).unwrap() }
    #[test] fn eval_str_class2() { check_eval_str(
        "123.str()",
        "123"
    ).unwrap() }
    #[test] fn eval_str_class3() { check_eval_str(
        "(-321).str()",
        "-321"
    ).unwrap() }
    #[test] fn eval_str_class4() { check_eval_str_with_setup(
        &["let map = (f,l)-> match l { [] => [], [x:xs] => [f(x) : map(f, xs)] };"], 
        "match map(x->Str::str(x), [1]) { [x] => x, _ => \"no\" }",
        "1"
    ).unwrap() }
    #[test] fn eval_str_class5() { check_eval_str(
        "[].str()",
        "[]"
    ).unwrap() }
    #[test] fn eval_str_class6() { check_eval_str(
        "[1].str()",
        "[1]"
    ).unwrap() }
    #[test] fn eval_str_class7() { check_eval_str(
        "[1,2,3].str()",
        "[1, 2, 3]"
    ).unwrap() }
    #[test] fn eval_str_class8() { check_eval_str(
        "[[]].str()",
        "[[]]"
    ).unwrap() }
    #[test] fn eval_str_class8_2() { check_eval_str(
        "Str::str([[]])",
        "[[]]"
    ).unwrap() }
    #[test] fn eval_str_class9() { check_eval_str(
        "[[1]].str()",
        "[[1]]"
    ).unwrap() }
    #[test] fn eval_str_class10() { check_eval_str(
        "[[1], [2,3]].str()",
        "[[1], [2, 3]]"
    ).unwrap() }
    #[test] fn eval_str_class11() { check_eval_str(
        "[[[[1]]]].str()",
        "[[[[1]]]]"
    ).unwrap() }
    #[test] fn eval_str_class12() { check_eval_str_with_setup(
        &["let map = (f,l)-> match l { [] => [], [x:xs] => [f(x) : map(f, xs)] };"], 
        "map(x->x*x, [3,4,5]).str()",
        "[9, 16, 25]"
    ).unwrap() }
    #[test] fn eval_str_class13() { check_eval_str(
        "[[int]]::str([[1]])",
        "[[1]]"
    ).unwrap() }
    #[test] fn eval_str_class_speed() { check_eval_str(
        "[1,2,3,4,5,6,7,8].str",
        "[1, 2, 3, 4, 5, 6, 7, 8]"
    ).unwrap() } // this is easy enough to get right, but it was extremely slow




    #[test] fn eval_oper1() { check_eval_str_with_setup(
        &["operator @$ _ (int a) -> [int] = [a, a-1];"],
        "(@$1).str",
        "[1, 0]"
    ).unwrap() }
    #[test] fn eval_oper2() { check_eval_with_setup(
        &["operator _+@+_ ~= + (int a, int b) -> int = (a + b) * b;"],
        "1+@+2",
        "6"
    ).unwrap() }
    #[test] fn eval_oper_prec1() { check_eval_with_setup(
        &["operator _+@+_ ~= + (int a, int b) -> int = (a + b) * b;"],
        "1 +@+ 2*2",
        "20"
    ).unwrap() }
    #[test] fn eval_oper_prec2() { check_eval_with_setup(
        &["operator _+@+_ ~= * (int a, int b) -> int = (a + b) * b;"],
        "1+@+2 * 2",
        "12"
    ).unwrap() }
    #[test] fn eval_oper_overload1() { check_eval_with_setup(
        &["operator _+@+_ ~= * (int a, int b) -> int = (a + b) * b;
              operator _+@+_ ([int] a, [int] b) -> int = match (a,b) { ([x:_], [y:_]) => x+y, _ => 0 };"],
        "1+@+2 + [2]+@+[5]",
        "13"
    ).unwrap() }

    #[test] fn eval_goper1() { check_eval_str_with_setup(
        &["operator<T> @$ _  (T a) -> [T] = [a, a];"],
        "@$'h'",
        "hh"
    ).unwrap() }
    #[test] fn eval_goper2() { check_eval_str_with_setup(
        &["operator<T> @$ _  (T a) -> [T] = [a, a];"],
        "(@$(@$1)).str",
        "[[1, 1], [1, 1]]"
    ).unwrap() }
    #[test] fn eval_goper3() { check_eval_str_with_setup(
        &["operator<A, B> _ @-> _ ~= + ([A] a, B b) -> [B] = match a { [x:xs] => [b : xs @-> b], [] => [] };"],
        "[1,2,3] @-> 'h' @-> 'g'",
        "ggg"
    ).unwrap() }

    const OPER_CLASS: &'static str = "
        class Similar for T {
            operator ~@ _ (T a) -> bool;
            operator _ @~= _ ~= == (T a, T b) -> bool;
        }
        impl Similar for int {
            operator ~@ _ (int a) -> bool = if a > 0: a < 2 else a > -2;
            operator _ @~= _ (int a, int b) -> bool = if a > b: a - b < 2 else b - a < 2;
        }
        impl<Similar T> Similar for [T] {
            operator ~@ _ ([T] a) -> bool = match a { [x:xs] => if ~@x: ~@xs else false, [] => true };
            operator _ @~= _ ([T] a, [T] b) -> bool = match (a,b) { ([x:xs], [y:ys]) => if x @~= y: xs @~= ys else false, ([], []) => true, _ => false };
        }    
        ";
    #[test] fn eval_oper_class_un1() { check_eval_with_setup(
        &[OPER_CLASS], 
        "~@1",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_un2() { check_eval_with_setup(
        &[OPER_CLASS], 
        "~@ -1",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_un3() { check_eval_with_setup(
        &[OPER_CLASS], 
        "~@[1]",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_un4() { check_eval_with_setup(
        &[OPER_CLASS], 
        "~@[]",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_un5() { check_eval_with_setup(
        &[OPER_CLASS], 
        "~@[[1]]",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_un6() { check_eval_with_setup(
        &[OPER_CLASS], 
        "(x-> ~@x)(1)",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_un7() { check_eval_with_setup(
        &[OPER_CLASS], 
        "(x-> ~@x)([1])",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_bin1() { check_eval_with_setup(
        &[OPER_CLASS], 
        "1 @~= 2",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_bin2() { check_eval_with_setup(
        &[OPER_CLASS], 
        "[1] @~= [2]",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_bin3() { check_eval_with_setup(
        &[OPER_CLASS], 
        "[[1]] @~= [[2]]",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_bin4() { check_eval_with_setup(
        &[OPER_CLASS], 
        "[[1],[2,3]] @~= [[2],[3,2]]",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_bin5() { check_eval_with_setup(
        &[OPER_CLASS], 
        "((x,y)->x @~= y)(4,5)",
        "true"
    ).unwrap() }
    #[test] fn eval_oper_class_bin6() { check_eval_with_setup(
        &[OPER_CLASS], 
        "((x,y)->x @~= y)([6],[7])",
        "true"
    ).unwrap() }


    const MONAD: &'static str = "
        class Monad for T<A> {
            func ret (A a) -> T<A>;
            operator<B> _ >>= _ ~= * (T<A> a, A -> T<B> f) -> T<B>;
        }
        variant Maybe<A> {
            Just(A);
            Nothing;
        }
        impl<A> Monad for Maybe<A> {
            func ret (A a) -> Maybe<A> = Just(a);
            operator<B> _ >>= _ (Maybe<A> a, A -> Maybe<B> f) -> Maybe<B> = match a { Just(x) => f(x), Nothing => Nothing };
        }
        ";
    #[test] fn eval_monad1() { check_eval_with_setup(
        &[MONAD], 
        "Maybe::ret(3)",
        "Maybe::Just(3)"
    ).unwrap() }
    #[test] fn eval_monad2() { check_eval_with_setup(
        &[MONAD], 
        "Maybe::ret(3) >>= x->Maybe::ret(x*x)",
        "Maybe::Just(9)"
    ).unwrap() }
}