namespace Joust

open SDL
open System.Numerics

module Platform =
    type Platform = { Position: Vector2; Size: Vector2 }

    let plat_color =
        Pixel.Color.fromRGBA (0xb1uy, 0x67uy, 0x3buy, uint8 1)

    let draw (renderer: Render.Renderer) (platform: Platform) : (Render.Renderer) =
        let (x, y) =
            (platform.Position.X |> round |> int, platform.Position.Y |> round |> int)

        let (w, h) =
            (platform.Size.X |> round |> int, platform.Size.Y |> round |> int)

        let _ =
            renderer |> Render.setDrawColor plat_color

        let _ =
            renderer
            |> Render.fillRectangle (Some <| Rectangle.create (x, y, w, h))

        renderer
