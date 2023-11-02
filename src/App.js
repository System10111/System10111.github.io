import './App.css';
import Showcase from './Showcase.js';
import AboutMe from './AboutMe.js';
import TolPage, { init_tol } from './TolPage';

import React from 'react';

import langs from './content/langs.json'

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

function Langs() {
  // return (
  //   <div className="langs-section">
  //     {
  //       ["x86 Assembly" ,"C", "C++", "Rust", "Haskell", "C#", "Java", "Python", "JavaScript", 
  //         "ReactJS", "PHP", "Lua", "SQL"].map((lang, id) => {
  //           let phi = 2 * Math.PI * id / 13;
  //           let len = Math.random() * 30 + 10;
  //           let [x, y] = [Math.cos(phi) * len, Math.sin(phi) * len];
  //           let scale = Math.random() * 0.5 + 0.5;
  //           return (
  //             <h3 key={id}
  //               style={{
  //                 left: `${50+x}%`,
  //                 top: `${50+y}%`,
  //                 scale: `${100*scale}%`,
  //                 opacity: Math.max(1.7-scale, 0.5),
  //               }}
  //             >{lang}</h3>
  //           );
  //       })
  //     }
  //   </div>
  // );

  const radius_base = 20;

  // create a canvas
  const canvasRef = React.useRef(null);
  const [selected_lang, set_selected_lang] = React.useState("");
  const [lang_spheres, set_lang_spheres] = React.useState(
    Object.entries(langs).map( ([lang, data], id) => 
    [lang, data["scale"], 1.0 - 2 * Math.random(), 1.0 - 2 * Math.random()] 
  )); // [lang, cur_scale, x, y]

  React.useEffect(() => {
    const canvas = canvasRef.current;
    const context = canvas.getContext('2d');

    setInterval(() => {
      // clear canvas
      context.clearRect(0, 0, canvas.width, canvas.height);
      let next_lang_spheres = [...lang_spheres];

      let [width, height] = [canvas.width, canvas.height];

      // move spheres so they don't overlap, but stay near the center
      for(let i = 0; i < lang_spheres.length; i++) {
        let l1 = lang_spheres[i];
        let l1_rad = radius_base * langs[l1[0]]["radius"] * l1[1];

        // move away from overlapping spheres
        for(let j = i+1; j < lang_spheres.length; j++) {
          let l2 = lang_spheres[j];
          let l2_rad = radius_base * langs[l2[0]]["radius"] * l2[1];
          let dx = l1[2] - l2[2];
          let dy = l1[3] - l2[3];
          let dist_sqr = dx*dx + dy*dy;
          let rad_sum = l1_rad + l2_rad;
          if (dist_sqr < rad_sum*rad_sum) {
            let dist = Math.sqrt(dist_sqr);
            let overlap = rad_sum - dist;
            let dx_norm = dx / dist;
            let dy_norm = dy / dist;
            const speed = 0.1;
            next_lang_spheres[i][2] += dx_norm * overlap * speed / 2;
            next_lang_spheres[i][3] += dy_norm * overlap * speed / 2;
            next_lang_spheres[j][2] -= dx_norm * overlap * speed / 2;
            next_lang_spheres[j][3] -= dy_norm * overlap * speed / 2;
          }

          // draw line between every pair of spheres
          context.beginPath();
          context.moveTo(l1[2] + width/2, l1[3] + height/2);
          context.lineTo(l2[2] + width/2, l2[3] + height/2);
          context.strokeStyle = "rgba(255,255,255,0.5)";
          context.stroke();
        }
        const wall_push_speed = 5.0;
        // move away from walls
        if(l1[3] < -height/2 + l1_rad) {
          next_lang_spheres[i][3] = Math.min(next_lang_spheres[i][3] + wall_push_speed, -height/2 + l1_rad);
        }
        if(l1[3] > height/2 - l1_rad) {
          next_lang_spheres[i][3] = Math.max(next_lang_spheres[i][3] - wall_push_speed, height/2 - l1_rad);
        }
        if(l1[2] < -width/2 + l1_rad) {
          next_lang_spheres[i][2] = Math.min(next_lang_spheres[i][2] + wall_push_speed, -width/2 + l1_rad);
        }
        if(l1[2] > width/2 - l1_rad) {
          next_lang_spheres[i][2] = Math.max(next_lang_spheres[i][2] - wall_push_speed, width/2 - l1_rad);
        }

        const attract_speed = 0.5;
        // move towards center
        let dx = -l1[2];
        let dy = -l1[3];
        let dist = Math.sqrt(dx*dx + dy*dy);
        let dx_norm = dx / dist;
        let dy_norm = dy / dist;
        next_lang_spheres[i][2] += dx_norm * attract_speed;
        next_lang_spheres[i][3] += dy_norm * attract_speed;


        // draw the sphere for debug purposes
        // context.beginPath();
        // context.arc(l1[2]+ width/2, l1[3] + height/2, l1_rad, 0, 2 * Math.PI);
        // context.strokeStyle = "rgba(0,0,0,1.0)";
        // context.stroke();
      }

      set_lang_spheres(next_lang_spheres);
    }, 1000/20);

    const handleResize = e => {
      canvas.height = window.innerHeight;
      canvas.width = window.innerWidth;
    };

    
    handleResize();
    window.addEventListener("resize", handleResize);

    const observer = new ResizeObserver(entries => {
      for (let entry of entries) {
        const cr = entry.contentRect;
        canvas.width = cr.width;
      }
    });
    observer.observe(document.querySelector(".langs-canvas-holder"));

    const width = canvas.width;
    const height = canvas.height;
    
    // draw a line on the canvas as a test
    context.beginPath();
    context.moveTo(0, 0);
    context.lineTo(width, height);
    context.stroke();

    // we only want to run this once when the canvas loads
    // eslint-disable-next-line react-hooks/exhaustive-deps 
  }, []);

  return ( 
    <div className="langs-section">
      <div className="langs-canvas-holder">
        <canvas ref={canvasRef} width="1280" height="720" 
          onMouseMove= {e => { 
            // console.log(e.clientX, e.clientY);
            let new_lang_spheres = [...lang_spheres];
            for(let i = 0; i < new_lang_spheres.length; i++) {
              let l = lang_spheres[i];
              let dx = e.clientX - l[2] - canvasRef.current.width/2;
              let dy = e.clientY - l[3] - canvasRef.current.height/2;
              let dist_sqr = dx*dx + dy*dy;
              let dist = Math.sqrt(dist_sqr);
              let scale_factor = 1 + 1/(1+Math.exp((dist-100)/100)); // scaled and shifted sigmoid
              new_lang_spheres[i][1] = langs[l[0]]["scale"] * scale_factor;
            }
          }}
        />
        {
        // the text for the languages
        lang_spheres.map( ([lang, cur_scale, x, y]) => {
          let [width, height] = canvasRef.current ? [canvasRef.current.width, canvasRef.current.height] : [1280, 720];
          return (
            <h3 
              key={lang}
              style={{
                left: `${x + width/2}px`,
                top: `calc(${y*100/height + 50}% - 5vh)`,
                scale: `${100}%`,
                opacity: Math.max(1.0, 0.5),
                fontSize: `${0.8 * cur_scale}em`,
                color: `${selected_lang === lang ? "var(--accent-color)" : "var(--text-color)"}`,	
              }}
              onClick={() => {
                if(selected_lang === lang) {
                  set_selected_lang("");
                } else {
                  set_selected_lang(lang);
                }
              }}
            >{lang}</h3>
          );
        })
      }
      </div>
      <div className="langs-info">
      </div>
    </div>
  );
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
      <Langs />
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
  } else {
    return (
      <MainPage />
    );
  }
}

export default App;
export { Header, Footer };
