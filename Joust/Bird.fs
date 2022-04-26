namespace Joust

open System.Numerics
open Platform
open SDL

module GameUtils =
    type Position = float32 * float32
    type Size = float32 * float32

    let colided (pos1: Position) (size1: Size) (pos2: Position) (size2: Size) : bool =
        let (x1, y1) = pos1
        let (x2, y2) = pos2
        let (w1, h1) = size1
        let (w2, h2) = size2
        let x_diff = x1 - x2
        let y_diff = y1 - y2

        (y_diff < float32 h2
         && y_diff > 0.0F - (float32 h1))
        && (x_diff < float32 w2
            && x_diff > 0.0F - (float32 w1))

module Bird =
    type MoveDir =
        | Left
        | Right

    type PlatformColision =
        | Landing of Platform
        | BumpedHead of Platform
        | BumpedLeft of Platform
        | BumpedRight of Platform
        | NoCollision

    type Bird =
        { Velocity: Vector2
          Position: Vector2
          Grounded: bool
          Player: bool }

    let width = 25.0F
    let height = 25.0F

    let player_color =
        Pixel.Color.fromRGBA (0x7cuy, 0xd8uy, 0xdeuy, 0xFFuy)

    let max_speed = 15.0F
    let max_speed_pos = new Vector2(max_speed, max_speed)
    let max_speed_neg = new Vector2(-max_speed, -max_speed)

    let gravity = new Vector2(0.0F, 0.1F)
    let speed_inc_grounded = new Vector2(0.5F, 0.0F)
    let speed_inc_flying = new Vector2(0.1F, 0.0F)
    let flap_incr = new Vector2(0.0F, -0.5F)

    let default_bird =
        { Position = new Vector2(0.0F, 0.0F)
          Velocity = new Vector2(0.0F, 0.0F)
          Grounded = false
          Player = false }

    let coliding_with_plat (projected_pos: Vector2) (platform: Platform) : bool =
        GameUtils.colided
            (projected_pos.X, projected_pos.Y)
            (width, height)
            (platform.Position.X, platform.Position.Y)
            (platform.Size.X, platform.Size.Y)

    let detect_collision (bird: Bird) (projected_pos: Vector2) (platforms: Platform list) : PlatformColision =
        let platform =
            List.tryFind (fun plat -> coliding_with_plat projected_pos plat) platforms

        match platform with
        | Some plat ->
            if ((bird.Position.Y + height) < plat.Position.Y)
               && ((bird.Position.X + width > plat.Position.X)
                   && (bird.Position.X < plat.Position.X + plat.Size.X)) then
                // Printf.printfn "Landing"
                Landing plat
            else if ((bird.Position.Y) > plat.Position.Y + plat.Size.Y)
                    && ((bird.Position.X + width > plat.Position.X)
                        && (bird.Position.X < plat.Position.X + plat.Size.X)) then
                // Printf.printfn "Bumping: %A %A %A" bird projected_pos plat
                BumpedHead plat
            else if (bird.Position.X + width < plat.Position.X) then
                BumpedLeft plat
            else if (bird.Position.X > plat.Position.X + plat.Size.X) then
                BumpedRight plat
            else
                NoCollision

        | None -> NoCollision


    let standing_on_platform (pos: Vector2) (plaforms: Platform list) : bool =
        match
            List.tryFind
                (fun (plat: Platform) ->
                    plat.Position.Y = pos.Y + height
                    && pos.X + width > plat.Position.X
                    && pos.X < plat.Position.X + plat.Size.X)
                plaforms
            with
        | Some _ -> true
        | None -> false

    let update_physics (platforms: Platform list) (screen_dims: int * int) (bird: Bird) : Bird =
        let projected_pos =
            let pos = bird.Position + bird.Velocity
            let (w, _) = screen_dims

            if pos.X + width < 0.0F then
                new Vector2(float32 w + (pos.X + width), pos.Y)
            else if pos.X > float32 w then
                new Vector2(-width + (pos.X - float32 w), pos.Y)
            else
                pos

        let (next_pos, next_vel, is_grounded) =
            match detect_collision bird projected_pos platforms with
            | Landing platform ->
                (new Vector2(projected_pos.X, platform.Position.Y - height), new Vector2(bird.Velocity.X, 0.0F), true)
            | BumpedHead platform ->
                (new Vector2(projected_pos.X, platform.Position.Y + platform.Size.Y),
                 new Vector2(bird.Velocity.X, -bird.Velocity.Y),
                 bird.Grounded)
            | BumpedLeft platform ->
                (new Vector2(platform.Position.X - width, projected_pos.Y),
                 new Vector2(-bird.Velocity.X, bird.Velocity.Y),
                 bird.Grounded)
            | BumpedRight platform ->
                (new Vector2(platform.Position.X + platform.Size.X, projected_pos.Y),
                 new Vector2(-bird.Velocity.X, bird.Velocity.Y),
                 bird.Grounded)
            | NoCollision -> (projected_pos, bird.Velocity, standing_on_platform projected_pos platforms)

        { bird with
            Grounded = is_grounded
            Velocity =
                if bird.Grounded || is_grounded then
                    next_vel
                else
                    next_vel + gravity
            Position = next_pos }

    let increase_speed (bird: Bird) (dir: MoveDir) : Bird =
        let inc_ammnt =
            (if bird.Grounded then
                 speed_inc_grounded
             else
                 speed_inc_flying)

        let speed_increase =
            match dir with
            | Left -> -inc_ammnt
            | Right -> inc_ammnt

        let projected_vel = bird.Velocity + speed_increase

        let final_vel =
            if abs projected_vel.X < 0.01F then
                new Vector2(0.0F, projected_vel.Y)
            else if projected_vel.X > max_speed then
                new Vector2(max_speed, projected_vel.Y)
            else if projected_vel.X < -max_speed then
                new Vector2(-max_speed, projected_vel.Y)
            else
                projected_vel

        { bird with Velocity = final_vel }

    let flap (bird: Bird) : Bird =
        { bird with
            Velocity = bird.Velocity + flap_incr
            Grounded = false }


    let draw (renderer: Render.Renderer) (bird: Bird) : (Render.Renderer) =
        let (x, y) =
            (bird.Position.X |> round |> int, bird.Position.Y |> round |> int)

        let (w, h) = (int width, int height)

        let _ =
            renderer |> Render.setDrawColor player_color

        let _ =
            renderer
            |> Render.fillRectangle (Some <| Rectangle.create (x, y, w, h))

        renderer
