module App.App

open Fable.Core
open Browser.Types

let window = Browser.Dom.window
let getById (id:string) = window.document.getElementById id
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

let mutable g = Solution.demogrid

let draw () =
    drawPixels "#000000" g.sand
    //drawPixels "#000000" g.visited
    ctx.stroke()

let iter () =
    g <- Solution.fillstep g
    draw ()

let button1 = getById "button1" :?> HTMLButtonElement
button1.onclick <- fun e -> iter ()

draw ()
