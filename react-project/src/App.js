import './App.css';
import Showcase from './Showcase.js';
import AboutMe from './AboutMe.js';

import logo from './logo.svg';

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

function App() {
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

export default App;
