import init, * as tol from './r-tol/r_tol.js';

import './App.css';
import Showcase from './Showcase.js';
import AboutMe from './AboutMe.js';

import logo from './logo.svg';
import React from 'react';


function Header() {
  return (
    <header className="header">
      <div className="header-left"> 
        <h1 className="header-text">My Cool Site</h1>
      </div>
      <div className="header-right">
        <h3 className="header-small-text">a cool link</h3>
        <h3 className="header-small-text">another cool link</h3>
        <h3 className="header-small-text">a third cool link</h3>
        <a href='/tol'>tol</a>
      </div>
    </header>
  );
}

function Transition(props) {
  return (
    <div className="transition-section">
      <svg xmlns="http://www.w3.org/2000/svg" 
          width="100%" height="175"
      >
        <pattern id={`main-pattern-${props.id}`} height="100%" width="200" 
          patternUnits="userSpaceOnUse" patternTransform="scale(1.5 1.5)">
          {/* background */}
          <rect x="0" y="0" width="100%" height="100%" fill={props.col2} />
          {/* middle zig-zag line */}
          <path 
            d="M -10 -10 L 0 50 l 25 25 l 25 -25 l 25 25 l 25 -25 l 25 25 l 25 -25 l 25 25 l 25 -25 V -10 Z" 
            fill={props.col1}
          />
          {/* squares next to line */}
          {[-2,-1,0,1,2,3,4,5,6].map((n) => {
            return (
              <path key={n} d={`M ${38 + n * 25} ${n%2===0 ? 69 : 55} l 12 12 l 12 -12 l -12 -12 Z`} 
                fill={n%2===0 ? props.col1 : props.col2}
              />
            );
          })}
          {/* big squares */}
          {[[75, 30, false], [10, 15, false], [150, 20, false],
            [20, 90, true], [150, 100, true], [100, 95, true]
            ].map(([x,y, post], id) => {
            return (
              <path key={id} d={`M ${x} ${y} l 12 12 l 12 -12 l -12 -12 Z`} 
                fill={post ? props.col1 : props.col2}
              />
            );
          })}
          {/* small squares */}
          {[[100, 25, false], [35, 30, false], [135, 40, false], [0, 35, false],
            [55, 90, true], [125, 85, true], [5, 100, true], [175, 80, true]
            ].map(([x,y, post], id) => {
            return (
              <path key={id} d={`M ${x} ${y} l 5 5 l 5 -5 l -5 -5 Z`} 
                fill={post ? props.col1 : props.col2}
              />
            );
          })}
        </pattern>
        <g>
          <rect x="0" y="-10" width="100%" height="110%" fill={`url(#main-pattern-${props.id})`}/>
        </g>
      </svg>
    </div>
  )
}

function Langs() {
  return (
    <div className="langs-section">
      {
        ["x86 Assembly" ,"C", "C++", "Rust", "Haskell", "C#", "Java", "Python", "JavaScript", 
          "ReactJS", "PHP", "Lua", "SQL"].map((lang, id) => {
            let phi = 2 * Math.PI * id / 13;
            let len = Math.random() * 30 + 10;
            let [x, y] = [Math.cos(phi) * len, Math.sin(phi) * len];
            let scale = Math.random() * 0.5 + 0.5;
            return (
              <h3 key={id}
                style={{
                  left: `${50+x}%`,
                  top: `${50+y}%`,
                  scale: `${100*scale}%`,
                  opacity: Math.max(1.7-scale, 0.5),
                }}
              >{lang}</h3>
            );
        })
      }
    </div>
  );
}


function MainPage() {
  return (
    <div className="App">
      <Header />
      <Showcase />
      <Transition 
        col1="var(--dark-background)" col2="var(--light-background)"
        id="show-to-about"
      />
      <AboutMe />
      <Transition 
        col1="var(--light-background)" col2="var(--dark-background)"
        id="about-to-langs"
      />
      <Langs />
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

function tol_syntax_highlight(text) {
  const keywords = [
    "match",
    "struct",
    "variant",
    "if",
    "else",
    "let",
    "forall",
    "func",
    "self"
  ];
  let tokens = [["", null]];
  while(text.length > 0) {
    // add whitespaces to last token
    while(text.length > 0 && /\s/g.test(text[0])) {
      tokens[tokens.length-1][0] += text[0];
      text = text.slice(1);
    }
    if(text.length === 0) break;
    
    // get identifiers and keywords
    if(/[A-z]/g.test(text[0])) {
      // identifier or keyword
      let id = "";
      while(text.length > 0 && /[A-z0-9]/g.test(text[0])) {
        id += text[0];
        text = text.slice(1);
      }
      if(keywords.includes(id)) {
        tokens.push([id, "keyword"]);
      } else {
        tokens.push([id, "identifier"]);
      }
    } 
    // get numbers
    else if(/[0-9]/g.test(text[0])) {
      let num = "";
      let found_dot = false;
      while(text.length > 0 && (/[0-9]/g.test(text[0]) || (text[0] === "." && !found_dot))) {
        if(text[0] === ".") found_dot = true;
        num += text[0];
        text = text.slice(1);
      }
      tokens.push([num, "number"]);
    }
    // get strings
    else if(text[0] === "\"") {
      let str = "\"";
      text = text.slice(1);
      while(text.length > 0 && text[0] !== "\"") {
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
      if(tokens[tokens.length-1][1] === null) {
        tokens[tokens.length-1][0] += text[0];
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
        if(type === null) {
          return text;
        } else {
          return <span key={id} className={`tol-syntax-${type}`}>{text}</span>
        }
      })}
    </span>
  )
}

function TolSnippet(props) {
  const [setup, setSetup] = React.useState(props.setup.replace("\\n", "\n") + "");
  const [setupScroll, setSetupScroll] = React.useState(false);
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
      if(res.err) {
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
        (<div className="tol-setup" style={{height: `${props.setup_lines || 10}em`}}>
          <pre className='stack-inner tol-text' style={{textAlign: "left", overflow: "hidden"}}>
            <span style={{position: "relative", top: -setupScroll}}>{tol_syntax_highlight(setup)}</span>
          </pre>
          <textarea
            className='stack-inner tol-text'
            style={{color: "transparent", cursor: "auto"}}
            name="setup" 
            value={setup}
            spellCheck="false"
            onChange={(event) => { setSetup(event.target.value); }}
            onKeyDown={(event) => {
              // tab changes field, so we need to capture the tab and add it ourselves
              if(event.key === "Tab") {
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
            {tol_syntax_highlight(expr)}
          </pre>
          <input 
            className='stack-inner tol-text'
            style={{color: "transparent", cursor: "auto"}}
            type="text" 
            value={expr} 
            spellCheck="false"
            onChange={(event) => { setExpr(event.target.value); }}
            onKeyDown={(event) => {
              // enter runs the code
              if(event.key === "Enter") {
                event.preventDefault();
                run();
              }
            }}
          ></input>
        </div>
        <button
          className='tol-submit' 
          onClick={run}
        >&gt;</button>
        { error !== "" ? 
          (<div className="tol-error">{error}</div>) :
          (<React.Fragment>
            <div className='tol-result'>{result}</div>
            <div className='tol-type'>{resType}</div>
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
          <br/>
          While currently, only a purely functional subset of the language is implemented,
          in the future, it will support writing procedural code, backed by the elegance and rubustness 
          of functional programming. 
        </p>
        <h3>
          Below you will find example snippets, gently introducing you to the features of the language.
          <br/>
          The big text field contains setup code - definitions, decalrations, etc.
          <br/>
          The small text field contains the expression to be evaluated.
          <br/>
          You can change any of the code, and press the green button to run it and see the effect live.
        </h3>
      </div>
      <div className="tol-snippets">
        <p>ToL currently supports integer numbers and the <code>+, -, *</code> operations. 
          The decision to delay floating point numbers is that they may be properly implemented with the type class system.</p>
        <TolSnippet setup="" expr="2+2*3" pre_result="8" pre_type="int" setup_lines="0"/>
        
        <p>Constants are declared with <code>let [name] = [value];</code></p>
        <TolSnippet setup="let x = 2;\nlet y = 3;" expr="x + y" pre_result="5" pre_type="int" setup_lines="3"/>
        
        <p>One of the most important parts of any functional language are lambda functions.
          In ToL, they take the form <code>(x,y,...) -&gt; [body]</code>.
          If only one argument is passed, the parentheses can be omitted, like so: <code>x -&gt; [body]</code>.
          Functions are called by passing the arguments after the function in parentheses, like so: <code>f(x, y, ...)</code>.
        </p>
        <TolSnippet setup="let square = x->x*x;" expr="square((x->x+1)(3))" pre_result="16" pre_type="int" setup_lines="3"/>
        <p>Notice how the lambda function <code>(x-&gt;x+1)</code> is used directly in the expression, without having to declare it separately.</p>
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

async function run() {
  await init();
}


function App() {
  let path = window.location.pathname;
  if (path === "/tol") {
    run();
    return (
      <TolPage />
    )
  } else {
    return (
      <MainPage />
    );
  }
}

export default App;
