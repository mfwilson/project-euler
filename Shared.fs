namespace ProjectEulerConsole

open System
open System.Collections

module Shared =

    module Fibonacci =

        let Generate =
            Seq.unfold (fun (a,b) -> Some( a+b, (b, a+b) ) ) (0,1)    



    module Primes =

        let IsPrime n =
            let rec testDivisors d =
                if n = d then true
                else if n % d = 0L then false
                else testDivisors (d + 1L)
            testDivisors 2L

        let DivideWork f t nd =
            let range = (t + 1L) - f
            if range < nd then failwith "Range is less than the number of divisions"
            let delta = range / nd
            [| 0L .. nd - 1L |] |> Array.map (fun i -> let ti = ((i + 1L) * delta) - 1L
                                                       ((i * delta) + f),(if i >= (nd - 1L) then t else ti+f))        
        
        let BruteForce start max =
            let getResultsTask(f, t) =
                let rec testEachPrime(f, t) = 
                    seq {
                        if f <= t then
                            if (IsPrime f) then yield f
                            yield! testEachPrime ((f + 1L),t)
                    }
                testEachPrime(f, t) |> Array.ofSeq

            let ntasks : Int64 = int64 System.Environment.ProcessorCount * 2L

            if max < (ntasks * 10L) then
                getResultsTask(start, max)
            else
                let taskArgs = DivideWork start max ntasks
                let results = 
                    Async.Parallel [ for taskNo in 0L .. (ntasks - 1L) -> 
                                         async { return getResultsTask taskArgs.[int taskNo] } ]
                    |> Async.RunSynchronously
                results |> Array.concat

        let Sieve max =             
            let array = BitArray(max, true)
            let lastp = Math.Sqrt(float max) |> int
            for p in 2 .. lastp + 1 do
                if array.Get(p) then
                    for pm in p * 2 .. p .. max - 1 do
                        array.Set(pm, false)
            seq { for i in 2 .. max - 1 do if array.Get(i) then yield i }
