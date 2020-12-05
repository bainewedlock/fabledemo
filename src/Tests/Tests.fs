module Tests

open Swensen.Unquote
let utest = test
open Expecto
open Solution


[<Tests>]
let tests =
    testList "fill" [
        //test "fill demo" {
        //    let g = demogrid
        //    let filled = fill g (fun _ -> ())
        //    utest <@ filled = 57 @> }
        test "fillstep demo" {
            let g = demogrid
            let spring = 500, 0
            let g' = fillstep spring g
            utest <@ g.sand.Count = 34 @>
            utest <@ g'.sand.Count = 39 @>
            utest <@ g'.visited.Count = 10 @>
            let g'' = fillstep spring g'
            utest <@ g''.visited.Count > 10 @> }
        test "demo grid" {
            let g = demogrid
            utest <@ g.maxy = 13 @>
            utest <@ g.sand.Contains (495, 2) @>
            utest <@ g.sand.Contains (495, 3) @>
            utest <@ g.sand.Contains (500, 13) @>
            utest <@ g.sand.Contains (495, 1) |> not @>
            utest <@ g.sand.Contains (495, 8) |> not @> }
        test "fillstep: bowl size 5" {
                           //#     #
                           // #####
            let bowl = Set [0,1; 1,2; 2,2; 3,2; 4,2; 5,2; 6,1]
            let spring = 3, -1
            let g = { Grid.empty10 with sand=bowl }
            let g' = fillstep spring g
            g.sand.Count =! 7
            g'.visited =! Set [ 3,0; 1,1; 2,1; 3,1; 4,1; 5,1]
            let sand' = g'.sand
            utest <@ sand'.Count = 12 @> }
        test "fillstep: bowl size 1" {
                           //# #
                           // #
            let bowl = Set [0,2; 1,3; 2,2]
            let spring = 1, 0
            let g = fillstep spring { Grid.empty10 with sand = bowl }
            let sand = g.sand
            utest <@ g.visited = Set [1,1; 1,2] @>
            utest <@ sand.Contains (1,2) @>
            let sand' = (fillstep spring g).sand
            utest <@ sand'.Contains (1,1) @>
        }
        test "fillstep: free fall" {
            let spring = 0, 0
            let grid = { Grid.empty10 with maxy = 3; }
            let result = fillstep spring grid
            result.visited =! Set [0,1; 0,2; 0,3] }
    ]

[<Tests>]
let parseTests = 
    testList "parse" [
        test "parse input" {
            let res = parse "x=495, y=2..7
                             y=7, x=495..501"
            res =! [
                VLine ((495, 2), 6)
                HLine ((495, 7), 7) ] }
        test "regexNumbers" {
            regexNumbers "(\d+)x(\d+)" "1x23" =! Some [1; 23] } ]
