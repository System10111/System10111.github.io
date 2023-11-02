import React from 'react';
import './Showcase.css';

import showcase_slides from './content/showcase.json'

function Showcase() {
  const [index, setIndex] = React.useState(0);
  const [isHovering, setIsHovering] = React.useState(false);
  const timeout_ref = React.useRef(null);
  const delay = 5000;

  function reset_timeout() {
    if (timeout_ref.current) {
      clearTimeout(timeout_ref.current);
    }
  }

  function begin_timeout() {
    reset_timeout();
    timeout_ref.current = setTimeout(
      () =>
        setIndex((prevIndex) =>
          prevIndex === showcase_slides.length - 1 ? 0 : prevIndex + 1
        ),
      delay
    );

    return () => { reset_timeout() };
  }

  React.useEffect(begin_timeout, [index]);

  return (
    <div className="showcase-section">
      <div className="slideshow-port">
        <div className="slideshow-slider"
          style={{ 
            transform: `translate3d(${-index * 100}%, 0, 0)`
          }}
          onMouseEnter={() => { reset_timeout(); setIsHovering(true);}}
          onMouseLeave={() => { begin_timeout(); setIsHovering(false);}}
        >
          {showcase_slides.map((slide, index) => (
            <div className="slideshow-slide" 
              key={index} 
            >
              <div className="slideshow-slide-image"
                style={{backgroundImage: `url(${slide.img})`}} 
                onClick={() => {
                  if(slide.link) {
                    window.location.href = slide.link;
                  }
                }}
              />
              <div className="slideshow-slide-text" 
                style={{opacity: isHovering ? 1.0 : 0.0}}
              >
                <h1>{slide.title}</h1>
                <p>{slide.description}</p>
              </div>
            </div>
            ))}
        </div>


        <div className="slideshow-dots">
          {showcase_slides.map((_, idx) => (
            <div key={idx} className={`slideshow-dot${index === idx ? " slideshow-dot-active" : ""}`} 
              onClick={() => setIndex(idx)} 
            /> 
          ))}
        </div>
      </div>
    </div>
  )
}

export default Showcase;