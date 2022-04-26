namespace Engine

module Engine =
    open SDL
    open System.Diagnostics

    type UserInput = Map<ScanCode, bool>

    let rec game_tick
        (state: 'model)
        (update: 'model -> UserInput -> 'model)
        (display: 'model -> Render.Renderer -> unit)
        (renderer: Render.Renderer)
        (stopwatch: Stopwatch)
        : 'model =
        stopwatch.Start()
        let input = getState ()
        let next_state = update state input
        display next_state renderer
        stopwatch.Stop()
        let elapsed = stopwatch.Elapsed.TotalMilliseconds

        if elapsed < 16.0 then
            System.Threading.Thread.Sleep(int (16.0 - elapsed))
        else
            ()

        stopwatch.Reset()

        match Event.poll () with
        | Some (Event.KeyDown kd) ->
            if kd.Keysym.Scancode = ScanCode.Escape then
                next_state
            else
                game_tick next_state update display renderer stopwatch
        | Some (Event.Quit _) -> next_state
        | _ -> game_tick next_state update display renderer stopwatch

    let play_game
        windowTitle
        windowWidth
        windowHeight
        (init: 'model)
        (update: 'model -> UserInput -> 'model)
        (display: 'model -> Render.Renderer -> unit)
        =
        use system =
            new System([ Flags.Video; Flags.Events ])

        use window =
            Window.create (windowTitle, Window.Position.Centered, windowWidth, windowHeight, Window.Flags.None)

        use renderer =
            Render.create window None Render.Flags.Accelerated

        game_tick init update display renderer (new Stopwatch())
