module App

open Browser.Types

let window = Browser.Dom.window
let canvas = window.document.getElementById "myCanvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()

canvas.width <- 400.
canvas.height <- 400.

ctx.moveTo(0., 0.)
ctx.lineTo(100., 100.)
ctx.stroke()

ctx.textAlign <- "center"
ctx.fillText("Hello world!", 200., 200.)

printfn "done!"


