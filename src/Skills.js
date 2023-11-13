import React from 'react';

import skills from './content/skills.json'


const star_svg = <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="100" height="100" fill="#000000">
<path d="M12 2l2.4 7.2h7.6l-6 4.8 2.4 7.2-6-4.8-6 4.8 2.4-7.2-6-4.8h7.6z"/>
</svg>

// eslint-disable-next-line
function OldSkills() {
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
      Object.entries(skills).map( ([lang, data], id) => 
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
          let l1_rad = radius_base * skills[l1[0]]["radius"] * l1[1];
  
          // move away from overlapping spheres
          for(let j = i+1; j < lang_spheres.length; j++) {
            let l2 = lang_spheres[j];
            let l2_rad = radius_base * skills[l2[0]]["radius"] * l2[1];
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
                new_lang_spheres[i][1] = skills[l[0]]["scale"] * scale_factor;
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

function Skills() {
    const canvasRef = React.useRef(null);
    
    function norm_to_canvas([x, y]) {
        // if the canvas hasn't loaded, use view width/height
        let [width, height] = canvasRef.current ? 
            [canvasRef.current.width, canvasRef.current.height] :
            [window.innerWidth, window.innerHeight];
        return [x * width, y * height];
    }
    function rand_around([x, y]) {
        let [cx, cy] = norm_to_canvas([x, y]);

        let angle = Math.random() * 2 * Math.PI;
        let radius = Math.max(Math.random(), 0.2) * 200;
        let adj_x = Math.cos(angle) * radius;
        let adj_y = Math.sin(angle) * radius;

        let canvas_pos = [cx + adj_x, cy + adj_y];
        return canvas_pos;
    }
    function cur_radius(skill_data)
    {
        const radius_base = 15;
        return skill_data["radius"] * radius_base * cur_scale(skill_data);
    }
    function cur_scale(skill_data)
    {
        return skill_data["scale"] * (1 + 1/(1+Math.exp((skill_data["mouse_dist"]-100)/100)));
    }

    /* skills.json format:
        {
            "<cloud 1 name>": {
                "pos": [x, y],
                "color[optional]": [r, g, b],
                "skill1": {
                    "level": "<confidence level>",
                    "scale": "<scale factor>",
                    "radius": "<radius of 'margin'-sphere>"
                    "connection[optional]": "<name of cloud>/<name of skill>"
                }
                "skill2": ...
            },
            "<cloud 2 name>": ...
            ...
        }
    */
    const [clouds, set_clouds] = React.useState(
        Object.fromEntries(Object.entries(skills).map( ([cloud_name, cloud_data], id) => {
            let cloud = {
                "pos": cloud_data["pos"],
                "color": cloud_data["color"] ? cloud_data["color"] : [255, 255, 255],
                "skills": {}
            };
            Object.entries(cloud_data).forEach( ([skill_name, skill_data], id) => {
                if (skill_name === "pos" || skill_name === "color") return;
                cloud["skills"][skill_name] = {
                    "pos": rand_around(cloud_data["pos"]),
                    "level": skill_data["level"],
                    "scale": skill_data["scale"],
                    "radius": skill_data["radius"],
                    "connection": skill_data["connection"],
                    "description": skill_data["description"],
                    "mouse_dist": 9999.0,
                };
            });
            return [cloud_name, cloud];
        }))
    ); // { cloud: { pos, color, skills: { skill: { pos, level, scale, radius, connection, mouse_dist } } }
    const [selected_skill, set_selected_skill] = React.useState(undefined);

    function update_clouds(canvas, context) {
        const DEBUG = false;

        // clear canvas
        context.clearRect(0, 0, canvas.width, canvas.height);

        let next_clouds = {...clouds};

        // move, unoverlap, and draw spheres and lines
        for(let [, cloud_data] of Object.entries(next_clouds)) {
            let skill_list = Object.entries(cloud_data["skills"]);

            for(let i = 0; i < skill_list.length; i++) {
                let s1 = skill_list[i][1];
                let s1_rad = cur_radius(s1);
                let [x,y] = s1["pos"];

                // move away from overlapping spheres and draw inner connections
                for(let j = i+1; j < skill_list.length; j++) {
                    let s2 = skill_list[j][1];
                    let s2_rad = cur_radius(s2);
                    let [x2, y2] = s2["pos"];

                    // let dx = l1[2] - l2[2];
                    // let dy = l1[3] - l2[3];
                    // let dist_sqr = dx*dx + dy*dy;
                    // let rad_sum = l1_rad + l2_rad;
                    // if (dist_sqr < rad_sum*rad_sum) {
                    //   let dist = Math.sqrt(dist_sqr);
                    //   let overlap = rad_sum - dist;
                    //   let dx_norm = dx / dist;
                    //   let dy_norm = dy / dist;
                    //   const speed = 0.1;
                    //   next_lang_spheres[i][2] += dx_norm * overlap * speed / 2;
                    //   next_lang_spheres[i][3] += dy_norm * overlap * speed / 2;
                    //   next_lang_spheres[j][2] -= dx_norm * overlap * speed / 2;
                    //   next_lang_spheres[j][3] -= dy_norm * overlap * speed / 2;
                    // }

                    let [dx, dy] = [x - x2, y - y2];
                    let dist_sqr = dx*dx + dy*dy;
                    let rad_sum = s1_rad + s2_rad;
                    if (dist_sqr < rad_sum*rad_sum) {
                        let dist = Math.sqrt(dist_sqr);
                        let overlap = rad_sum - dist;
                        let dx_norm = dx / dist;
                        let dy_norm = dy / dist;
                        const speed = 0.1;
                        s1["pos"][0] += dx_norm * overlap * speed / 2;
                        s1["pos"][1] += dy_norm * overlap * speed / 2;
                        s2["pos"][0] -= dx_norm * overlap * speed / 2;
                        s2["pos"][1] -= dy_norm * overlap * speed / 2;
                    }

                    // draw line between every pair of spheres
                    context.beginPath();
                    context.moveTo(x, y);
                    context.lineTo(x2, y2);
                    // context.strokeStyle = "rgba(255,255,255,0.5)";
                    context.strokeStyle = `rgba(${cloud_data["color"][0]},${cloud_data["color"][1]},${cloud_data["color"][2]},0.5)`;
                    context.stroke();

                    skill_list[j][1] = s2;
                }

                const wall_push_speed = 10.0;
                const wall_rad_mult = 0.5;
                // move away from walls
                if(x < 0 + s1_rad * wall_rad_mult) {
                    s1["pos"][0] = Math.min(s1["pos"][0] + wall_push_speed, 0 + s1_rad * wall_rad_mult);
                }
                if(x > canvas.width - s1_rad * wall_rad_mult) {
                    s1["pos"][0] = Math.max(s1["pos"][0] - wall_push_speed, canvas.width - s1_rad * wall_rad_mult);
                }
                if(y < 0 + s1_rad * wall_rad_mult) {
                    s1["pos"][1] = Math.min(s1["pos"][1] + wall_push_speed, 0 + s1_rad * wall_rad_mult);
                }
                if(y > canvas.height - s1_rad * wall_rad_mult) {
                    s1["pos"][1] = Math.max(s1["pos"][1] - wall_push_speed, canvas.height - s1_rad * wall_rad_mult);
                }

                const attract_speed = 0.75;
                // move towards center
                let [cx, cy] = norm_to_canvas(cloud_data["pos"]);
                let [dx, dy] = [cx - x, cy - y];
                let dist = Math.sqrt(dx*dx + dy*dy);
                let dx_norm = dx / dist;
                let dy_norm = dy / dist;
                s1["pos"][0] += dx_norm * attract_speed;
                s1["pos"][1] += dy_norm * attract_speed;

                if(s1["connection"]) {
                    let [cloud_name, skill_name] = s1["connection"].split("/");
                    let s2 = next_clouds[cloud_name]["skills"][skill_name];
                    let [x2, y2] = s2["pos"];
                    context.beginPath();
                    context.moveTo(x, y);
                    context.lineTo(x2, y2);
                    // context.strokeStyle = "rgba(255,255,255,0.5)";
                    // context.strokeStyle = `rgba(${cloud_data["color"][0]},${cloud_data["color"][1]},${cloud_data["color"][2]},0.5)`;
                    // have a gradient from the color of the cloud to the color of the connected cloud
                    let grad = context.createLinearGradient(x, y, x2, y2);
                    grad.addColorStop(0, `rgba(${cloud_data["color"][0]},${cloud_data["color"][1]},${cloud_data["color"][2]},0.5)`);
                    grad.addColorStop(1, `rgba(${next_clouds[cloud_name]["color"][0]},${next_clouds[cloud_name]["color"][1]},${next_clouds[cloud_name]["color"][2]},0.5)`);
                    context.strokeStyle = grad;
                    context.stroke();
                }

                // draw the sphere for debug purposes
                if(DEBUG) {
                    context.beginPath();
                    context.arc(x, y, s1_rad, 0, 2 * Math.PI);
                    context.strokeStyle = "rgba(0,0,0,1.0)";
                    context.stroke();
                }
                skill_list[i][1] = s1;
            }
            
            // draw the center of the cloud for debug purposes
            if(DEBUG) {
                let [x,y] = norm_to_canvas(cloud_data["pos"]);
                context.beginPath();
                context.arc(x, y, 20, 0, 2 * Math.PI);
                context.strokeStyle = "rgba(255,0,0,1.0)";
                context.stroke();
            }
            set_clouds(next_clouds);
        }
    }

    React.useEffect(() => {
        const canvas = canvasRef.current;
        const context = canvas.getContext('2d');


        setInterval(() => { update_clouds(canvas, context); }, 1000/20);

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
        observer.observe(document.querySelector(".skills-canvas-holder"));

        // we only want to run this once when the canvas loads
        // eslint-disable-next-line react-hooks/exhaustive-deps 
    }, []);


    return ( 
        <div className="skills-section"
            onClick={() => { console.log(clouds); }}
        >
          <div className="skills-canvas-holder"
            style={{
              filter: selected_skill ? "blur(5px)" : "none",
            }}
            onMouseMove= {e => { 
                let t_rect = document.getElementById('skills_canvas').getBoundingClientRect();
                let [mx, my] = [e.clientX - t_rect.left, e.clientY - t_rect.top];
                // console.log(mx, my);
                
                let next_clouds = {...clouds};
                for(let [, cloud_data] of Object.entries(next_clouds)) {
                    let skill_list = Object.entries(cloud_data["skills"]);
                    for(let i = 0; i < skill_list.length; i++) {
                        let s1 = skill_list[i][1];
                        let [x,y] = s1["pos"];
                        let dx = mx - x;
                        let dy = my - y;
                        let dist_sqr = dx*dx + dy*dy;
                        s1["mouse_dist"] = Math.sqrt(dist_sqr);
                        skill_list[i][1] = s1;
                    }
                    // next_clouds[cloud_name]["skills"] = Object.fromEntries(skill_list);
                }
            }}
            onMouseLeave={e => {
                // reset all mouse_dist values to 9999
                let next_clouds = {...clouds};
                for(let [, cloud_data] of Object.entries(next_clouds)) {
                    let skill_list = Object.entries(cloud_data["skills"]);
                    for(let i = 0; i < skill_list.length; i++) {
                        let s1 = skill_list[i][1];
                        s1["mouse_dist"] = 9999.0;
                        skill_list[i][1] = s1;
                    }
                    // next_clouds[cloud_name]["skills"] = Object.fromEntries(skill_list);
                }
            }}
          >
            <canvas id='skills_canvas' ref={canvasRef}/>
            {
                Object.entries(clouds).flatMap( ([cloud_name, cloud_data], id) => {
                    return Object.entries(cloud_data["skills"]).map( ([skill_name, skill_data], id) => {
                        let [x, y] = skill_data["pos"];
                        return (
                            <h3 
                              key={skill_name}
                              style={{
                                left: `${x}px`,
                                top: `${y}px`,
                                // scale: `${cur_scale(skill_data)}`,
                                transform: `translate(-50%, -50%) scale(${cur_scale(skill_data)})`,
                                opacity: Math.max(1.0, 0.5),
                                fontSize: `${0.8 *  skill_data["scale"]}em`,
                                // color: "var(--text-color)",	
                                color: `rgba(${cloud_data["color"][0]},${cloud_data["color"][1]},${cloud_data["color"][2]},1.0)`,
                              }}
                              onClick={() => {
                                if(selected_skill === skill_name) {
                                  set_selected_skill([]);
                                } else {
                                  set_selected_skill([cloud_name, skill_name]);
                                }
                              }}
                            >{skill_name}</h3>
                          );
                    });
                })
            }
          </div>
          <div className="skills-info"
            style={{
                // this is a fun hack - since we want a transition on the opacity, we can't just set display to none
                // so we just yeet the entire div offscreen instead
                left: selected_skill ? "0" : "-100%"
            }}
            onClick={() => { set_selected_skill(""); }}
          >
            <div className="skills-info-panel"
              style={{
                opacity: selected_skill ? "1" : "0",
              }}
            >{ selected_skill ? <div>
                <h2>{selected_skill[1]}</h2>
                <br/>
                <div className="skills-info-stars">
                  <div className="skills-info-stars-half"
                    style={{
                      width: `${clouds[selected_skill[0]]["skills"][selected_skill[1]]["level"]/5 * 100}%`,
                      backgroundImage: `url(/img/star_full.svg)`, 
                    }}
                  />
                  <div className="skills-info-stars-half"
                    style={{
                      width: "100%",
                      backgroundImage: `url(/img/star.svg)`, 
                    }}
                  />
                </div>
                {
                  clouds[selected_skill[0]]["skills"][selected_skill[1]]["description"] ?
                  <p dangerouslySetInnerHTML={{ __html: clouds[selected_skill[0]]["skills"][selected_skill[1]]["description"]}}/> :
                  <p/>
                }
            </div> : <div/>}</div>
          </div>
        </div>
    );
}

export default Skills;