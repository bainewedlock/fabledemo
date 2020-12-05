module Tests

open Swensen.Unquote
let utest = test
open Expecto
open Solution


[<Tests>]
let tests =
    testList "all tests" [
        //test "fill demo" {
        //    let g = demogrid
        //    let filled = fill g (fun _ -> ())
        //    utest <@ filled = 57 @> }
        test "fillstep demo" {
            let g = demogrid
            let g' = fillstep g
            utest <@ g.sand.Count = 34 @>
            utest <@ g'.sand.Count = 39 @>
            utest <@ g'.visited.Count = 10 @> }
        test "demo grid" {
            let g = demogrid
            utest <@ g.maxy = 13 @>
            utest <@ g.track = [500, 0] @>
            utest <@ g.sand.Contains (495, 2) @>
            utest <@ g.sand.Contains (495, 3) @>
            utest <@ g.sand.Contains (500, 13) @>
            utest <@ g.sand.Contains (495, 1) |> not @>
            utest <@ g.sand.Contains (495, 8) |> not @> }
        test "fillstep: bowl size 5" {
                           //#     #
                           // #####
            let bowl = Set [0,1; 1,2; 2,2; 3,2; 4,2; 5,2; 6,1]
            let start = 3, -1
            let g = { Grid.empty10 with track = [start]; sand=bowl }
            let g' = fillstep g
            g.sand.Count =! 7
            g'.visited =! Set [ 3,0; 1,1; 2,1; 3,1; 4,1; 5,1]
            let sand' = g'.sand
            utest <@ sand'.Count = 12 @> }
        test "fillstep: bowl size 1" {
                           //# #
                           // #
            let bowl = Set [0,1; 1,2; 2,1]
            let start = 1, -1
            let g = fillstep { Grid.empty10 with track=[start]; sand = bowl }
            let sand = g.sand
            utest <@ g.visited = Set [1,0; 1,1] @>
            utest <@ sand.Contains (1,1) @>
        }
        test "fillstep: free fall" {
            let grid = { Grid.empty10 with maxy = 3; track = [0, 0] }
            let result = fillstep grid
            result.visited =! Set [0,1; 0,2; 0,3] }
        test "parse input" {
            let res = parse "x=495, y=2..7
                             y=7, x=495..501"
            res =! [
                VLine ((495, 2), 6)
                HLine ((495, 7), 7) ] }
        test "regexNumbers" {
            regexNumbers "(\d+)x(\d+)" "1x23" =! Some [1; 23] }
    ]

