module Program

open Engine
open Joust

[<EntryPoint>]
let main argv =
    let _ =
        Engine.play_game Game.windowTitle Game.windowWidth Game.windowHeight Game.init Game.update Game.display

    0
