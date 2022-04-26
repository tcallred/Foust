namespace Joust

open Platform
open System.Numerics

module Game =

    open SDL
    open Engine

    let windowTitle = "Foust!"
    let windowWidth = 1000
    let windowHeight = 800

    let background =
        (Pixel.Color.fromRGBA (uint8 0, uint8 0, uint8 0, uint8 1))

    type Model =
        { Platforms: Platform list
          Player: Bird.Bird }

    let init =
        { Platforms =
            [ { Position = new Vector2(0.0F, 700.0F)
                Size = new Vector2(1000.0F, 50.0F) }
              { Position = new Vector2(400.0F, 400.0F)
                Size = new Vector2(200.0F, 50.0F) } ]
          Player =
            { Bird.default_bird with
                Player = true
                Position = new Vector2(500.0F, 100.0F) } }

    let update (state: Model) (input: Engine.UserInput) : Model =
        // Printf.printfn "%A" state.Player
        { state with
            Player =
                state.Player
                |> (fun player ->
                    if input.[ScanCode.Z] then
                        Bird.flap player
                    else
                        player)
                |> (fun player ->
                    if input.[ScanCode.Right] then
                        Bird.increase_speed player Bird.MoveDir.Right
                    else
                        player)
                |> (fun player ->
                    if input.[ScanCode.Left] then
                        Bird.increase_speed player Bird.MoveDir.Left
                    else
                        player)
                |> Bird.update_physics state.Platforms (windowWidth, windowHeight) }

    let display (state: Model) (renderer: Render.Renderer) : unit =
        let _ =
            renderer |> Render.setDrawColor background

        renderer |>* Render.clear

        let renderer = Bird.draw renderer state.Player

        List.fold Platform.draw renderer state.Platforms
        |>* Render.present
