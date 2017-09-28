
let context = Canvas.getContext2d "canvas";
let width = 1900.0;
let height = 1060.0;



let gravity = Vector2.mul Vector2.down 0.2;
let theOG = (width /. 2.0, height /. 2.0);

module Particle = {
    type t = {
        position: Vector2.t,
        velocity: Vector2.t,
        colour: (int, int, int)
    };

    let make position velocity colour => {
        { position, velocity, colour };
    };

    let draw { position, colour } => {
        Canvas.drawCircle colour::colour context position 3.0
    };

    let update pos { position, velocity, colour } => {
        let gravity = Vector2.mul (Vector2.sub pos position) 0.01;
        let v = Vector2.add velocity gravity;
        let p = Vector2.add position v;
        { position: p, velocity: v, colour }
    };
};

let particle = Particle.make theOG (10.0, 0.0);

type state = {
    particles: list Particle.t,
    pos: Vector2.t
};
let state = ref {
    particles: [],
    pos: Vector2.zero
};
 
type message = 
  | Tick
  | SpawnParticle Vector2.t;

let update state message => {
    switch message {
        | Tick => {
            {...state, particles: List.map (Particle.update state.pos) state.particles};
        }
        | SpawnParticle pos => {
            let colour = (Random.int 256, Random.int 256, Random.int 256);
            let velocity = Vector2.mul (Vector2.randomUnit ()) 5.0;
            {pos, particles: [Particle.make pos velocity colour, ...state.particles]};
        }
        | _ => state;
    };
};

let render state => {
    Canvas.clearCanvas context;
    List.iter Particle.draw state.particles;
};

let dispatch message => {
    state := update !state message;
    render !state;
};

external setInterval : (unit => unit) => int => unit = "setInterval" [@@bs.val];
external requestAnimationFrame : (unit => unit) => unit = "requestAnimationFrame" [@@bs.val];

external documentOn : string => (Js.t {..} => unit) => unit = "document.addEventListener" [@@bs.val];

setInterval (fun () => dispatch Tick) 17;


documentOn "mousemove" (fun e => requestAnimationFrame (fun () => dispatch @@ SpawnParticle (e##x, e##y)));