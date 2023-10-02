import init, * as tol from './r-tol/r_tol.js';
import React from 'react';
import { Header } from './App.js';
import logo from './logo.svg';

function tol_syntax_highlight(text) {
    const keywords = [
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
    let tokens = [["", null]];
    while (text.length > 0) {
        // add whitespaces to last token
        while (text.length > 0 && /\s/g.test(text[0])) {
            tokens[tokens.length - 1][0] += text[0];
            text = text.slice(1);
        }
        if (text.length === 0) break;

        // get identifiers and keywords
        if (/[A-z]/g.test(text[0])) {
            // identifier or keyword
            let id = "";
            while (text.length > 0 && /[A-z0-9]/g.test(text[0])) {
                id += text[0];
                text = text.slice(1);
            }
            if (keywords.includes(id)) {
                tokens.push([id, "keyword"]);
            } else {
                tokens.push([id, "identifier"]);
            }
        }
        // get numbers
        else if (/[0-9]/g.test(text[0])) {
            let num = "";
            let found_dot = false;
            while (text.length > 0 && (/[0-9]/g.test(text[0]) || (text[0] === "." && !found_dot))) {
                if (text[0] === ".") found_dot = true;
                num += text[0];
                text = text.slice(1);
            }
            tokens.push([num, "number"]);
        }
        // get strings
        else if (text[0] === "\"") {
            let str = "\"";
            text = text.slice(1);
            while (text.length > 0 && text[0] !== "\"") {
                str += text[0];
                text = text.slice(1);
            }
            str += "\"";
            tokens.push([str, "string"]);
            text = text.slice(1);
        }
        // everything else
        else {
            // add to last if the last's type is null
            if (tokens[tokens.length - 1][1] === null) {
                tokens[tokens.length - 1][0] += text[0];
            }
            // otherwise create a new token
            else {
                tokens.push([text[0], null]);
            }
            text = text.slice(1);
        }
    }
    return (
        <span>
            {tokens.map(([text, type], id) => {
                if (type === null) {
                    return text;
                } else {
                    return <span key={id} className={`tol-syntax-${type}`}>{text}</span>
                }
            })}
        </span>
    )
}

function TolSnippet(props) {
    const [setup, setSetup] = React.useState(props.setup.replaceAll("\r", "").replaceAll("\\n", "\n").replaceAll("\\t", "\t") + "");
    const [setupScroll, setSetupScroll] = React.useState(false);
    const [exprScroll, setExprScroll] = React.useState(false);
    const [expr, setExpr] = React.useState(props.expr + "");
    const [result, setResult] = React.useState(props.pre_result || "");
    const [resType, setResType] = React.useState(props.pre_type || "");
    const [error, setError] = React.useState("");

    function run() {
        // with { <setup> =<expr>; }, we use the block expression feature of tol
        // the =...; is the block's return 'statement'
        // we're not actually setting setup to anything, as setup should end with a semicolon
        try {
            let res_str = tol.tol_eval("{" + setup + "=" + expr + ";}");
            let res = JSON.parse(res_str);
            // console.log(res);
            if (res.err) {
                setError(res.err.trim());
                setResult("");
                setResType("");
            } else {
                setError("");
                setResult(res.eval.trim());
                setResType(res.type.trim());
            }
        } catch (error) {
            setError("Fatal error: out of memory?/stack overflow?");
            setResult("");
            setResType("");
            // reset the interpreter since it probably crashed
            tol.default();
        }
    }

    return (
        <div className='tol-snippet'>
            {
                (props.setup_lines === undefined || props.setup_lines > 0) ?
                    (<div className="tol-setup" style={{ height: `${props.setup_lines || 10}em` }}>
                        <pre className='stack-inner tol-text' style={{ textAlign: "left", overflow: "hidden" }}>
                            <span style={{ position: "relative", top: -setupScroll }}>{tol_syntax_highlight(setup)}</span>
                        </pre>
                        <textarea
                            className='stack-inner tol-text'
                            style={{ color: "transparent", cursor: "auto" }}
                            name="setup"
                            value={setup}
                            spellCheck="false"
                            onChange={(event) => { setSetup(event.target.value); }}
                            onKeyDown={(event) => {
                                // tab changes field, so we need to capture the tab and add it ourselves
                                if (event.key === "Tab") {
                                    let trgt = event.currentTarget;
                                    event.preventDefault();
                                    var start = trgt.selectionStart;
                                    var end = trgt.selectionEnd;
                                    let newSetup = setup.substring(0, start) + "\t" + setup.substring(end);
                                    setSetup(newSetup);
                                    trgt.value = newSetup;
                                    trgt.selectionStart = start + 1;
                                    trgt.selectionEnd = start + 1;
                                }
                            }}
                            onScroll={(event) => { setSetupScroll(event.target.scrollTop); }}
                        ></textarea>
                    </div>)
                    : null
            }
            <div className='tol-repl'>
                <div className="tol-expr">
                    <pre className='stack-inner tol-text' style={{
                        textAlign: "left",
                        overflow: "hidden",
                        paddingTop: "2px",
                    }}>
                        <span style={{ position: "relative", left: -exprScroll }}>{tol_syntax_highlight(expr)}</span>
                    </pre>
                    <input
                        className='stack-inner tol-text'
                        style={{ color: "transparent", cursor: "auto" }}
                        type="text"
                        value={expr}
                        spellCheck="false"
                        onChange={(event) => { setExpr(event.target.value); }}
                        onKeyDown={(event) => {
                            // enter runs the code
                            if (event.key === "Enter") {
                                event.preventDefault();
                                run();
                            }
                        }}
                        onScroll={(event) => { setExprScroll(event.target.scrollLeft); }}
                    ></input>
                </div>
                <button
                    className='tol-submit'
                    onClick={run}
                >&gt;</button>
                {error !== "" ?
                    (<div className="tol-error">{error}</div>) :
                    (<React.Fragment>
                        <div className='tol-result'>{result}</div>
                        <div className='tol-type' style={resType.length > 25 ? { fontSize: "0.7em" } : {}}>{resType}</div>
                    </React.Fragment>
                    )
                }

            </div>
        </div>
    )
}

function TolPage() {
    return (
        <div className="App">
            <Header />
            <div className="tol-intro">
                <h1>ToL</h1>
                <p>
                    is a general purpose functional programming language inspired by C, Rust and Haskell.
                    <br />
                    While currently, only a purely functional subset of the language is implemented,
                    in the future, it will support writing procedural code, backed by the elegance and rubustness
                    of functional programming.
                </p>
                <h3>
                    Below you will find example snippets, gently introducing you to the features of the language.
                    <br />
                    The big text field contains setup code - definitions, decalrations, etc.
                    <br />
                    The small text field contains the expression to be evaluated.
                    <br />
                    You can change any of the code, and press the green button to run it and see the effect live.
                </h3>
            </div>
            <div className="tol-snippets">
                <p>ToL currently supports integer numbers and the <code>+, -, *</code> operations.
                    The decision to delay floating point numbers is that they may be properly implemented with the type class system.</p>
                <TolSnippet setup="" expr="2+2*3" pre_result="8" pre_type="int" setup_lines="0" />

                <p>Constants are declared with <code>let [name] = [value];</code></p>
                <TolSnippet setup="let x = 2;\nlet y = 3;" expr="x + y" pre_result="5" pre_type="int" setup_lines="3" />

                <p>One of the most important parts of any functional language are lambda functions.
                    <br />
                    In ToL, they take the form <code>(x,y,...) -&gt; [body]</code>.
                    <br />
                    If only one argument is passed, the parentheses can be omitted, like so: <code>x -&gt; [body]</code>.
                    <br />
                    Functions are called by passing the arguments after the function in parentheses, like so: <code>f(x, y, ...)</code>.
                </p>
                <TolSnippet setup="let square = x->x*x;" expr="square((x->x+1)(3))" pre_result="16" pre_type="int" setup_lines="3" />
                <p style={{ paddingTop: 0 }}>
                    Notice how the lambda function <code>(x-&gt;x+1)</code> is used directly in the expression, without having to declare it separately.
                    <br />
                    This is in contrast to the <code>x-&gt;x*x</code> function, which is bound to the name <code>square</code>.
                </p>

                <p><code>if</code> expressions are also supported and they look like so:
                    <br />
                    <code>if [condition]: [true value] else [false value]</code>.
                    <br />
                    Parentheses are not needed around the condition and the colon after it can be skipped if the true value is a block - <code>&#123;...&#125;</code>
                </p>
                <TolSnippet setup="let x = 5;" expr="if x+3 == 8: 1 else -1" pre_result="1" pre_type="int" setup_lines="2" />

                <p>
                    Functions, which are bound to names can recursively call themselves.
                    <br />
                    For example, this is how one might write the factorial function in ToL:
                </p>
                <TolSnippet setup="let fact = n->if n == 0: 1 else n * fact(n-1);" expr="fact(5)" pre_result="120" pre_type="int" setup_lines="3" />

                <p>
                    Functions may be passed to other functions, and may also be returned from functions.
                    <br />
                    These are what are called <i>higher order functions</i>.
                    <br />
                    A simple example of a higher order function is the <code>twice</code> function,
                    <br />which takes a function <code>f</code>,
                    and produces a function which is equivalent to calling <code>f</code> twice.
                </p>
                <TolSnippet setup="let twice = f->x->f(f(x));\nlet square_twice = twice(x->x*x);" expr="square_twice(2)" pre_result="16" pre_type="int" setup_lines="4" />

                {/* this snippet produces <thunk> instead of 16, so it's commented out for now */}
                <p>
                    A classic example of a higher order function is the <code>compose</code> function, which takes
                    two functions <code>f</code> and <code>g</code> and returns a new function <code>h</code> such that <code>h(x) == f(g(x))</code>.
                </p>
                <TolSnippet setup="let square = x->x*x;\nlet compose = (f, g) -> x -> f(g(x));" expr="compose(square, (x->x+1))(3)" pre_result="16" pre_type="int" setup_lines="4"/>

                <p>
                    It is worth mentioning that you may use this repl to inspect the type of any ToL expression,
                    even functions, by simply evaluating them. The shown value will be <code>&lt;thunk&gt;</code>,
                    and the correct type will be displayed to it's right.
                    <br />
                    For example <code>twice</code>'s type is:
                </p>
                <TolSnippet setup="let twice = f->x->f(f(x));" expr="twice" pre_result="<thunk>" pre_type="forall a: (a -> a) -> a -> a" setup_lines="2" />
                <p>
                    You may think it would be <code>(int -&gt; int) -&gt; int -&gt; int</code>,
                    meaning "take a function from int to int, and an int and return an int" (or "take int -&gt; int and return int -&gt; int"),
                    but ToL can deduce that this function is applicable to any type <code>a</code>, so it becomes
                    <code>forall a: (a -&gt; a) -&gt; a -&gt; a</code>
                    <br />
                    After all, <code>twice</code> doesn't care what function it has to apply twice, as long as said function takes the same thing as it returns.
                </p>

                {/* this snippet produces 6 instead of 48, so it's commented out for now */}
                <p>
                    An interesting consequence of this is that <code>twice</code>, being a function that takes something and returns the same thing,
                    can be passed to <code>twice</code> itself. The resulting function <code>quad</code> applies a function four times in total.
                </p>
                <TolSnippet setup="let twice = f->x->f(f(x));\nlet quad = twice(twice);" expr="quad(x->x*2, 3)" pre_result="48" pre_type="int" setup_lines="4"/>
                
                <br></br>
                <p>
                    A <i>block</i> is a way to declare and use multiple expressions in a single expression or declaration.
                    It sounds complicated but it's actually what you've been using all along.
                    The setup code is implicitly at the beginning of a block and the expression is returned at the end.
                    <br />
                    To use a block in place of an expression, you can use curly braces <code>&#123;...&#125;</code> and write any declarations you want inside them.
                    To return from a block "assign" to nothing like so <code>&#123;let ... = ...; =return_value;&#125;</code>
                    You can think of it as assigning a value to the block itself.
                </p>
                <TolSnippet
                    setup="let f = x-> {\n\tlet x_sqrd = x*x;\n\tlet x_doubl = x*2;\n\t= x_sqrd + x_doubl;\n};"
                    expr="f(3)"
                    pre_result="15"
                    pre_type="int"
                    setup_lines="7"
                />

                <p>
                    ToL supports <i>pattern matching</i> trough the use of the <code>match</code> keyword.
                    The syntax is similar to Rust's <code>match</code>:
                    <br />
                    <code>match [expr] &#123; [pattern] =&gt; [result], [pattern] =&gt; [result], ... &#125;</code>
                    <br />
                    <code>_</code> can be used in place of a pattern to match anything.
                </p>

                <p>
                    The simplest way to use <code>match</code> is like an if-else chain:
                </p>
                <TolSnippet setup="let f = x-> match x {\n\t1 => 123,\n\t2 => 321\n\t_ => 0\n};" expr="f(2)" pre_result="321" pre_type="int" setup_lines="7" />

                <p>
                    Where <code>match</code> really shines is when used with <i>algebraic data types</i> like tuples and variants.
                    <br />
                    Tuples in ToL are constructed by putting the elements in parentheses, separated by commas, like so:
                    <code>(1, 2, 3)</code>.
                    <br />
                    <code>match</code> can then be used to destructure tuples.
                    For example, this snippet returns the second element if the first is 1, and otherwise the first element, if the second is 2.
                    If neither is the case, it returns their sum.
                </p>
                <TolSnippet setup="let f = (x,y)-> match (x,y) {\n\t(1,r) => r,\n\t(l,2) => l,\n\t(l,r) => l + r \n};" expr="f(3,4)" pre_result="7" pre_type="int" setup_lines="7" />
                {/* let f = tup-> match tup {(1,r) => r, (l,2) => l, (l,r) => l + r} should work but doesn't */}

                <p>
                    A <code>variant</code> in ToL is a type that can take one of several different values,
                    much like an enum in C++ or Java. What's more, each value may contain other information,
                    like 'data' in Haskell or 'enum' in Rust.
                    <br />
                    To declare a variant, use the following syntax:
                    <br />
                    <code>variant [name] &#123; [value](optional data); [value](optional data); ... &#125;</code>
                </p>
                <TolSnippet
                    setup="variant Color { Red; Green; Blue; }\nlet marker_price = col-> match col {\n\tColor::Red => 2,\n\tColor::Green => 3,\n\tColor::Blue => 5,\n\t_ => 0\n};"
                    expr="marker_price(Red)"
                    pre_result="2"
                    pre_type="int"
                    setup_lines="9"
                />
                <p>
                    Notice how in the <code>marker_price</code> function we must specify that <code>Red</code>, <code>Green</code> and <code>Blue</code> are in the <code>Color</code> namespace.
                    (Actually, only <code>Color::Red</code> is neccessary, since after that the compiler knows that <code>Green</code> and <code>Blue</code> must be of type <code>Color</code>).
                    This is because, by default, lambda functions can take any type, and we must tell it that we're looking for a <code>Color</code>.
                    After that, ToL knows that <code>marker_price</code> accepts only a <code>Color</code> so we don't have to specify it when calling it.
                    <br />
                    (You can check that the type of <code>marker_price</code> is <code>Color -&gt; int</code>)
                </p>

                <p>
                    As mentioned before, each value in a variant may also contain data.
                    For example, if we had a <code>Shape</code> variant, we could have a <code>Circle</code> value that contains a radius,
                    a <code>Square</code> value that contains a side length, and so on.
                    The values and their data can then be destructured using <code>match</code>.
                </p>
                {/*TODO: change the 3 to 3.14 when floats exist */}
                <TolSnippet
                    setup="variant Shape { Circle(int); Square(int); Rect(int, int); }\nlet area = shape-> match shape {\n\tShape::Circle(r) => 3 * r * r,\n\tShape::Square(s) => s * s,\n\tShape::Rect(x,y) => x * y\n\t_ => 0\n};"
                    expr="area(Rect(3,5))"
                    pre_result="15"
                    pre_type="int"
                    setup_lines="10"
                />

                <p>
                    ToL data types can be recursive without by default, with no extra boilerplate.
                    For example we, even though ToL has built-in lists (more on them later), we can define our own like so:
                    We'll have a <code>List</code> variant that contains either a <code>Nil</code> value, or a <code>Cons</code> value that contains a head and a tail.
                </p>
                <TolSnippet
                    setup="variant List { Cons(int, List); Nil; }\nlet sum = list-> match list {\n\tList::Cons(h, t) => h + sum(t),\n\tList::Nil => 0\n};\nlet long_list = List::Cons(1, Cons(2, Cons(3, Cons(4, Nil))));"
                    expr="sum(long_list)"
                    pre_result="10"
                    pre_type="int"
                    setup_lines="9"
                />

                <p>
                    This is all well and good, but our list only supports integers.
                    Do we have to define a new list for every type we want to use?
                    Fortunately, no. ToL has a special syntax for defining generic types, variants included.
                    It is very similar to other languages, as it uses the well known <code>&lt;T&gt;</code> syntax.
                </p>
                <TolSnippet
                    setup="variant List<T> { Cons(T, List<T>); Nil; }\nlet get = (list, index)-> match list {\n\tList::Cons(h, t) => if index == 0: h else get(t, index - 1)\n};\nlet list_of_funcs = List::Cons(x->x, Cons(x->x*2, Cons(x->x*x, Nil)));"
                    expr="get(list_of_funcs, 1)(5)"
                    pre_result="10"
                    pre_type="int"
                    setup_lines="9"
                />

                <p>
                    Amazing! Now our list can hold any type we want, and we can even use it to store functions.
                    <br />
                    Another thing worth noting is that the expression could have been written as <code>get(list_of_funcs, 1, 5)</code>,
                    which at first glance seems illogical, since <code>get</code> only takes two arguments.
                    This is because of <i>currying</i>. Basically, in ToL, (like in other functional languages) functions only ever take one argument.
                    If you call a function with more than one argument, it will return a new function that takes another argument, returning a function... and so on, one by one.
                    <br />
                    In fact the you could also write the example like this: <code>get(list_of_funcs)(1)(5)</code>, and ToL would not know the difference.
                    In principle, you should use whatever conveys the intention most clearly - in this case <code>get(list_of_funcs, 1)(5)</code>
                </p>

                <p>
                    Structs are a way to group together data of different types. Their definition looks almost
                    exactly like a C struct, and is similar to the way variants are defined:
                    <code>struct [name] &#123; [type] [field]; [type] [field]; ... &#125;</code>
                    <br />
                    To create a struct instance, the syntax is <code>struct_name &#123; field = value; field = value; ... &#125;</code>.
                    <br />
                    The fields of a struct can be accessed using the dot operator, like so: <code>struct.field</code>.
                </p>
                <TolSnippet
                    setup="variant Color { Red; Green; Blue; }\nstruct Car { int price; Color color; }\nlet my_first_car = Car { price = 1500; color = Red; };"
                    expr="my_first_car.price"
                    pre_result="1500"
                    pre_type="int"
                    setup_lines="5"
                />

                <p>
                    They can also be generic, like variants.
                </p>
                <TolSnippet
                    setup="struct Pair<L, R> { L l; R r; }\nlet my_pair = Pair { l = 1; r = Pair { l = 2; r = 3; }; };"
                    expr="my_pair.r.l"
                    pre_result="2"
                    pre_type="int"
                    setup_lines="3"
                />

                <p>
                    Now about those built-in lists. They are basically like the ones we defined earlier,
                    but with their own syntax and a few extra quality-of-life features.
                    <br />
                    You can make a list by putting the elements in square brackets, separated by commas.
                    You can also create a new list with an existing one as it's tail by putting a colon at the end <code>:</code>
                    and the list after it in the square brackets. Ex: <code>[1, 2, 3 : the_rest_of_the_numbers]</code>
                    <br />
                    You can also use the same notation to pattern match any number of elements at the head and then the tail.
                    If you don't put a tail in the match, it will only match when the number of elements exactly matches that of the pattern.
                </p>
                <p>
                    This next snippet sums the two elements after a 1 in a list, and if there is no 1, or there aren't two elements after it, it returns 0.
                </p>
                <TolSnippet
                    setup="let f = list -> match list {\n\t[1, x, y : rest] => x + y,\n\t[_ : rest] => f(rest),\n\t_ => 0\n};"
                    expr="f([3,5,6,2, 1,6,7])"
                    pre_result="13"
                    pre_type="int"
                    setup_lines="7"
                />
                <p>
                    The type of a list is written as <code>[T]</code>, where <code>T</code> is the type of the elements.
                </p>
                <TolSnippet
                    setup=""
                    expr="[[1,2], [3,4]]"
                    pre_result="[[1, 2], [3, 4]]"
                    pre_type="[[int]]"
                    setup_lines="0"
                />

                <p>
                    <i>Type classes</i> are a one of the most powerful features of ToL.
                    They function similarly to interfaces in OOP languages, or traits in Rust, or classes in Haskell.
                    <br />
                    Basically, they are like a common ground for multiple types that share some common functionality.
                    By using them you can write functions that work on multiple types, as long as they implement the type class.
                </p>

                <p>
                    To define a type class, you use the following syntax:
                    <br />
                    <code>class [name] for [type variable] &#123; [function declarations...] &#125;</code>
                    <br />
                    It is important to note, that although we use the <code>class</code> keyword,
                    type classes are not actually classes in the object-oriented sense of the word.
                    <br />
                    Function declarations themselves look like this: <code>func [name]([optional "self"], [type] [arg], [type] [arg], ...) -&gt; [return type];</code>
                    <br />
                    Function definitions(which we'll use when we implement the class for a specific type) are the same
                    as declarations, but before the <code>;</code> we put an <code>=</code> and the function body.
                    If the function body is a block, the <code>=</code> can be skipped.
                    <br />
                    You can also use them to define standalone functions, but that lambdas are usually preffered, since they automatically deduce types.
                    <br />
                </p>

                <p>
                    For example we can define a type class for types that can be added together,
                    and any type that implements it must have a function called <code>add</code>, which adds two values of that type together.
                    We'll make one of the arguments <code>self</code>, which allows us to use the <code>noun.verb</code> notation.
                    Again, even though it looks simalar to OOP, this is not a class, and this is
                    not a method call, since it cannot modify the object - it is just a more convenient way to call a function. In fact
                    the <code>self</code> argument is just a normal argument, and you can still call the function in the usual functional style.
                    <br />
                    (Such a class may utilize a custom operator <code>+</code> to make itself easier to use,
                    but I have not written the explanation for operator overloading yet.)
                </p>
                <TolSnippet
                    setup="class Addable for T {\n\tfunc add(self, T other) -> T;\n}"
                    expr=""
                    pre_result=""
                    pre_type=""
                    setup_lines="5"
                />

                <p>
                    Now we can implement the class for a specific type.
                    To do so, we use the <code>impl</code> keyword in the following way:
                    <br />
                    <code>impl [class] for [type] &#123; [function definitions...] &#125;</code>
                    <br />
                    First we'll implement it for <code>int</code>, which is trivial.
                </p>

                <TolSnippet
                    setup="class Addable for T {\n\tfunc add(self, T other) -> T;\n}
                  \nimpl Addable for int {\n\tfunc add(self, int other) -> int = self + other;\n}
                  \nfunc element_add<Addable T>([T] xs, [T] ys) -> [T] = 
                    \n\tmatch (xs, ys) {\n\t\t([x : xs], [y : ys]) => [x.add(y) : element_add(xs, ys)],\n\t\t_ => []\n\t};
                  \nlet get = (xs, i)-> match xs {\n\t[x : xs] => if i == 0: x else get(xs, i - 1),\n\t_ => 0\n};"
                    expr="get(element_add([1,2,3], [4,5,6]), 1)"
                    pre_result="7"
                    pre_type="int"
                    setup_lines="15"
                />


            </div>
            <div className="App-placeholder">
                <img src={logo} className="App-logo" alt="logo" />
                <p>
                    Edit <code>src/App.js</code> and save to reload.
                </p>
                <a
                    className="App-link"
                    href="https://reactjs.org"
                    target="_blank"
                    rel="noopener noreferrer"
                >
                    Learn React
                </a>
            </div>
        </div>
    );
}

async function init_tol() {
    await init();
}

export default TolPage;
export { init_tol };