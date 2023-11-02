import React from 'react';
import { Header, Footer } from './App.js';
import projects from './content/projects.json';

function ProjectSection(props) {
    if(props.section.type === "image_text") {
        let image_style = {
            backgroundImage: `url(${props.section.image})`
        };
        if(props.section.flip_image) { image_style.transform = "scaleX(-1)"; }

        let direction_classes = [" left", " right"];
        if(props.section.direction === "right") { direction_classes.reverse(); }

        return (
            <div className="project-section">
                <div className={"project-section-panel" + direction_classes[0]}>
                    <div className={"project-image" + direction_classes[props.section.flip_image ? 1 : 0]}
                        style={image_style} 
                    >
                    </div>
                    <div className="project-section-text">
                        {props.section.h1 && <h1>{props.section.h1}</h1>}
                        {props.section.h2 && <h2>{props.section.h2}</h2>}
                    </div>
                </div>
            </div>
        )
    }

    if(props.section.type === "download") {
        return (
            <div className="project-section">
                <div className={"project-section-panel left"} style={{height:"90%"}}>
                    <div className={"project-download-image"}
                        style={{backgroundImage: `url(${props.section.image})`}} 
                    >
                    </div>
                    <div className="project-download-text">
                        {props.section.h1 && <h1>{props.section.h1}</h1>}
                        <button className="project-download-button"
                            onClick={() => {
                                // open props.section.link in a new tab
                                var win = window.open(props.section.link, '_blank');
                                win.focus();

                            }}
                        ><b>{props.section.button_text}</b></button>
                        <div style={{height:"3em"}}/>
                        {props.section.h2 && <h2>{props.section.h2}</h2>}
                    </div>
                </div>
            </div>
        )
    }
}

function ProjectPage(props) {
    let proj = projects[props.name];
    if(proj === undefined) {
        return (
            <div className="App">
            <Header />
            <h1>404</h1>
            <h2>Project not found</h2>
            <Footer />
            </div>
        );
    }

    return (
        <div className="App">
        <Header />
        {proj.sections.map((section, index) => (
            <ProjectSection key={index} section={section}/>
        ))}
        <Footer />
        </div>
    );
}

export default ProjectPage;