module App

open Fable.Core
open Browser.Types

let window = Browser.Dom.window
let getById (id:string) = window.document.getElementById id
let canvas = getById "canvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()

canvas.width <- 400.
canvas.height <- 400.

ctx.moveTo(0., 0.)
ctx.lineTo(100., 100.)
ctx.stroke()

ctx.textAlign <- "center"
ctx.fillText("Hello world!", 200., 200.)

let button1 = getById "button1" :?> HTMLButtonElement
button1.onclick <- fun e ->  printfn "button clicky: %A" e

printfn "done!"


