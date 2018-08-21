namespace ProjectEulerConsole

open System
open System.IO

open Shared
open Shared.Matrix
open Shared.Numbers
open Shared.Primes


module ProjectEuler400 =


    /// The moon has been opened up, and land can be obtained for free, but there is a catch. You have to build a wall 
    /// around the land that you stake out, and building a wall on the moon is expensive. Every country has been allotted 
    /// a 500 m by 500 m square area, but they will possess only that area which they wall in. 251001 posts have been placed 
    /// in a rectangular grid with 1 meter spacing. The wall must be a closed series of straight lines, each line running 
    /// from post to post.
    /// 
    /// The bigger countries of course have built a 2000 m wall enclosing the entire 250 000 m2 area. The Duchy of Grand Fenwick, 
    /// has a tighter budget, and has asked you (their Royal Programmer) to compute what shape would get best maximum enclosed-area/wall-length ratio.
    /// 
    /// You have done some preliminary calculations on a sheet of paper. For a 2000 meter wall enclosing the 250 000 m2 area the 
    /// enclosed-area/wall-length ratio is 125. 
    /// 
    /// Although not allowed, but to get an idea if this is anything better: if you place a circle inside the square area touching 
    /// the four sides the area will be equal to π*2502 m2 and the perimeter will be π*500 m, so the enclosed-area/wall-length ratio 
    /// will also be 125.
    /// 
    /// However, if you cut off from the square four triangles with sides 75 m, 75 m and 75√2 m the total area becomes 238750 m2 and the 
    /// perimeter becomes 1400+300√2 m. So this gives an enclosed-area/wall-length ratio of 130.87, which is significantly better.
    ///
    /// Find the maximum enclosed-area/wall-length ratio.
    /// Give your answer rounded to 8 places behind the decimal point in the form abc.defghijk.
    let Problem314() =

        let sqrt2 = Math.Sqrt(2.0)

        let triangleArea a b = a * b / 2.0
        let enclosedArea side = 250000.0 - (triangleArea side side * 4.0)
        let wallLength side = (250.0 - side) * 8.0 + (4.0 * side * sqrt2)
        let getRatio n = enclosedArea n / wallLength n


        let testFn n = 
            let ea side = 250000.0 - (triangleArea side side * 4.0) + (triangleArea 1.0 1.0 * 4.0) 

            let wl side = (250.0 - side) * 8.0 + (4.0 * side * sqrt2) - (4.0 * sqrt2 + 4.0 * 2.0)

            ea n / wl n

        let a = getRatio 3.0
        let b = testFn 3.0


        let values = seq { 0.0 .. 250.0 } |> Seq.toArray

        let ratios = values |> Array.map (fun n -> n, (getRatio n))

        let max = ratios |> Array.maxBy (fun p -> snd(p))

        ratios |> Seq.iter (fun p -> printfn "%A" p)

        0