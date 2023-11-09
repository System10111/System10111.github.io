import React from 'react';
import './AboutMe.css';

// eslint-disable-next-line
function AboutMeOld() {
  return (
    <div className="aboutme-section">
      <div className='aboutme-pic'/>
      <div className='aboutme-text-section'>
        <div className='aboutme-text'>
          <h1>About Me</h1>
          <p> 
          My name is Kristian Rachev. 
          I have always been fascinated by the world of computers and technology, 
          and I have been programming since middle school. 
          However, my passions extend beyond just programming, 
          as I am also deeply interested in creating art through a variety of mediums such as 
          music, games, drawings, and writing. 
          I believe that creativity and technology go hand in hand and I enjoy pushing the 
          boundaries of my knowledge and learning new concepts and ideas. 
          I am a person who is constantly looking to grow and improve, 
          and I am always eager to take on new challenges and opportunities. 
          My drive to learn and create is what motivates me to work hard and pursue my passions, 
          whether it be in programming or any other field.
          </p>
        </div>
        <div className='aboutme-text'>
          <h1>Programming</h1>
          <p>
          As a programmer, I have a wealth of experience and skills that I have acquired over the years. 
          I have worked on various projects, from front-end and back-end web development to 
          game programming in C# and Unity. 
          I am comfortable working independently or as part of a team and am always eager to 
          stay up-to-date on the latest developments in my field. 
          I am currently working on developing a <a href='/project/type-one'>3D space game</a> with OpenGL and C, 
          as well as my own programming language - <a href='/tol'>ToL</a>.
          My favorite language is C, 
          but I am also proficient in C++, C#, Java, Python, Javascript, Haskell, Rust and others. 
          I also have experience using a variety of tools and software, such as Unity, Godot, Blender, 
          Photoshop, and Audacity. I am confident in my ability to quickly learn new languages and 
          technologies, and to use my knowledge and experience to efficiently solve problems and 
          expand my abilities.
          </p>
        </div>
      </div>  
    </div>
  )
}

function AboutMeProgrammer()
{
  return (
    <div className="aboutme-panel-div">
      <div className="aboutme-pic"/>
      <h1>A Rockstar Programmer</h1>
      <p>
          As a programmer, I have a wealth of experience and skills that I have acquired over the years. 
          I have worked on various projects, from front-end and back-end web development to 
          game programming in C# and Unity. 
          I am comfortable working independently or as part of a team and am always eager to 
          stay up-to-date on the latest developments in my field. 
          I am currently working on developing a <a href='/project/type_one'>3D space game</a> with OpenGL and C, 
          as well as my own programming language - <a href='/tol'>ToL</a>.
          My favorite language is C, 
          but I am also proficient in C++, C#, Java, Python, Javascript, Haskell, Rust and others. 
          I also have experience using a variety of tools and software, such as Unity, Godot, Blender, 
          Photoshop, and Audacity. I am confident in my ability to quickly learn new languages and 
          technologies, and to use my knowledge and experience to efficiently solve problems and 
          expand my abilities.
      </p>
    </div>
  )
}

function AboutMeMusician()
{
  return (
    <div className="aboutme-panel-div">
      <div className="aboutme-pic"
        style={{float: "right"}}
      />
      <h1>A Programming Rockstar</h1>
      <p>
        Music has always been a passion of mine, and I have been playing various instruments since I was a child. 
        I am proficient in playing the guitar, piano, and drums, and I have experience composing and producing music using software such as Cubase and FL Studio. 
        I have also worked on sound design for video games, using tools such as Wwise to create immersive audio experiences. 
        I believe that music and sound are essential components of any creative project, and I enjoy exploring the ways in which they can enhance and the experience.
      </p>
    </div>
  )
}

function AboutMeArtist()
{
  return (
    <div className="aboutme-panel-div">
      <div className="aboutme-pic"/>
      <h1>A Creative Artist</h1>
      <p>
        Art has always been a passion of mine, and I have been creating various forms of art since I was a child. 
        I am proficient in drawing, painting, and digital art, and I have experience using software such as Photoshop and Illustrator to create stunning visuals. 
        I have also worked on graphic design for websites and games, in different areas, such as UI, 3D modeling, texturing, VFX and more.
        I am also very experienced in graphical shader programming, and I can use it to create stunning visual effects and animations.
      </p>
    </div>
  )
}

function AboutMe() {
  const [index, setIndex] = React.useState(0);

  return (
    <div className="aboutme-section">
      <div className="aboutme-port">
        <div className="aboutme-scene">
          <div className="aboutme-plane"/>
          <div className="aboutme-logo" alt="logo"/>
          <div className="aboutme-slider"
            style={{ 
              transform: `perspective(5000px) translate3d(0, 0, -2000px) rotate3d(0, 1, 0, ${180 - index * 120}deg)`
            }}
          >
            {
            // [[148, 30, 28], [28, 148, 30], [30, 28, 148]] // dilute rgb
            [[238, 143, 141], [141, 238, 143], [143, 141, 238]] // pastel rgb
            // [[225, 185, 101], [101, 225, 185], [185, 101, 225]] // dilute y/o lb/lg mg/pr
            // first create border color - 20% darker, then convert to string with rgb
            .map((color) => ([
              `rgb(${color[0]}, ${color[1]}, ${color[2]}, 0.98)`,
              `rgb(${color[0] * 0.8}, ${color[1] * 0.8}, ${color[2] * 0.8})`]))
            .map((color, i) => (
              <div className="aboutme-slide" key={i}
              style={{
                transform: `rotate3d(0, 1, 0, ${180 - i * 120}deg) translate3d(0, 0, 1000px)`,
              }}>
                <div className="aboutme-panel" 
                  style={{backgroundColor: color[0], borderColor: color[1]}}
                >
                  {[AboutMeProgrammer, AboutMeMusician, AboutMeArtist][i]()}
                  <div className="aboutme-panel-icon" style={{
                    backgroundImage: ["url('/img/programming.png')", "url('/img/music.png')", "url('/img/art.png')"][i]
                  }}/>
                </div>

              </div>
            ))}
          </div>
        </div>

        <div className="aboutme-nav">
          <div style={{cursor:"pointer", pointerEvents: "all"}} onClick={() => setIndex((prevIndex) => prevIndex - 1)}></div>
          <div style={{flex: "2"}}></div>
          <div style={{cursor:"pointer", pointerEvents: "all"}} onClick={() => setIndex((prevIndex) => prevIndex + 1)}></div>
        </div>

      </div>
    </div>
  )
}

export default AboutMe;