import './App.css';
import Showcase from './Showcase.js';
import AboutMe from './AboutMe.js';
import TolPage, { init_tol } from './TolPage';
import ProjectPage from './ProjectPage.js';
import Skills from './Skills.js';

import React from 'react';


import logo from './logo.svg';


function Header() {
  return (
    <header className="header">
      <div className="header-left"
        onClick={
          () => {window.location.href = '/';}
        }
        style={{cursor: 'pointer'}}
      > 
        <h1 className="header-text">Kristian R</h1>
        <img src={logo} className="header-logo" alt="logo" />
      </div>
      <div className="header-right">
        <a href='/project/type_one'>Type One</a>
        <h3 className="header-small-text">another cool link</h3>
        <h3 className="header-small-text">a third cool link</h3>
        <a href='/tol'>ToL</a>
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

function Footer() {
  return (
    <footer className="footer">
      <div className="footer-left">
        <h3 className="footer-text">Kristian R</h3>
        <img src={logo} className="header-logo" alt="logo" />
      </div>
      <div className="footer-right">
        <h3 className="footer-text">krisi2001@gmail.com</h3>
        <h3 className="footer-text">+359 88 615 1112</h3>
      </div>
    </footer>
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
      <Skills />
      <Footer />
    </div>
  );
}


function App() {
  let path = window.location.pathname;
  if (path === "/tol") {
    init_tol();
    return (
      <TolPage />
    )
  } else if (path.startsWith("/project/")) {
    let name = path.substring(9);
    return (
      <ProjectPage name={name} />
    );
  } else {
    return (
      <MainPage />
    );
  }
}

export default App;
export { Header, Footer };
