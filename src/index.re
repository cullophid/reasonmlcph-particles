
let context = Canvas.getContext2d "canvas";
let width = 1900.0;
let height = 1060.0;

let initialVelicity = 3.0;
let dampening = 0.99;

let gravity = Vector2.down |> Vector2.mul 0.05;

let theOG = (width /. 2.0, height /. 2.0);
module BlackHole = {
    type t = Vector2.t;
    let draw pos =>
        Canvas.drawCircle colour::(0, 0, 0) context pos 20.0;
};

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
        Canvas.drawCircle ::colour context position 4.0
    };

    let update planets { position, velocity, colour } => {
        let gravity planet => {
            let dist = planet |> Vector2.sub position;

            dist |> Vector2.mul (0.05 /. (max 5.0 @@ Vector2.length dist)) |> Vector2.mul 0.5;
        };
        let v = List.fold_left (fun v planet => gravity planet |> Vector2.add v) velocity planets;
        let p = Vector2.add position v;
        { position: p, velocity: v, colour }
    };
};


type state = {
    blackHoles: list BlackHole.t,
    particles: list Particle.t,
    pos: Vector2.t
};
let state = ref {
    blackHoles: [],
    particles: [],
    pos: Vector2.zero
};
 
type message = 
  | Tick float
  | MouseMove Vector2.t
  | MouseClick Vector2.t
  | SpawnParticle;

let update state message => {
    switch message {
        | Tick _ => {
            {...state, particles: List.map (Particle.update [state.pos, ...state.blackHoles]) state.particles};
        }
        | MouseMove pos => {
            {...state, pos}
        }
        | MouseClick pos => {
            {...state, blackHoles: [pos, ...state.blackHoles]}
        }
        | SpawnParticle =>
            if (state.pos == (0.0, 0.0)) state 
            else {
            let colour = (Random.int 256, Random.int 256, Random.int 256);
            let velocity = Vector2.randomUnit () |> Vector2.mul initialVelicity;
            {...state, particles: [Particle.make state.pos velocity colour, ...state.particles]};
        }
    };
};

let render state => {
    Canvas.clearCanvas context;
    let numberOfParticles = state.particles |> List.length |> string_of_int; 
    List.iter Particle.draw state.particles;
    List.iter BlackHole.draw state.blackHoles;
    Canvas.strokeText context numberOfParticles 10.0 10.0;
};

let dispatch message => {
    state := update !state message;
    render !state;
};

external animationFrame : (float => unit) => unit = "requestAnimationFrame" [@@bs.val];

external documentOn : string => (Js.t {..} => unit) => unit = "document.addEventListener" [@@bs.val];

documentOn "mousemove" (fun e => dispatch @@ MouseMove (e##x, e##y));
documentOn "click" (fun e => dispatch @@ MouseClick (e##x, e##y));

let i = ref 0;
let rec loop time => {
    i := !i + 1;
    animationFrame loop;
    dispatch @@ Tick time;
    if (!i mod 5 == 0) {
        dispatch SpawnParticle;
    }
};

animationFrame loop;
