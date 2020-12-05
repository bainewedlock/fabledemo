module App.App

open Fable.Core
open Browser.Types

let window = Browser.Dom.window
let getById (id:string) = window.document.getElementById id
let getButton x = getById x :?> HTMLButtonElement
let canvas = getById "canvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
canvas.width <- 400.
canvas.height <- 400.

ctx.scale(10., 10.)
ctx.translate(-490., 10.)

let drawPixel (x, y) = ctx.rect(float x, float y, 0.5, 0.5)

let drawPixels (color:string) ps =
    ctx.strokeStyle <- U3.Case1 color
    ps |> Seq.iter drawPixel

let mutable g = Solution.Grid.empty10

let clear () =
    ctx.save()
    ctx.setTransform(1.,0.,0.,1.,0.,0.)
    ctx.clearRect(0., 0., canvas.width, canvas.height)
    ctx.restore()
    printfn "clear!"

let draw () =
    clear()
    drawPixels "#000000" g.sand
    //drawPixels "#000000" g.visited
    ctx.stroke()

let reset () =
    g <- Solution.demogrid
    draw ()

let iter () =
    g <- Solution.fillstep (500, 0) g
    draw ()

(getButton "stepButton").onclick <- fun e -> iter ()
(getButton "resetButton").onclick <- fun e -> reset ()

reset ()
