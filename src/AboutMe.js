import React from 'react';
import './AboutMe.css';

function AboutMe() {
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

export default AboutMe;