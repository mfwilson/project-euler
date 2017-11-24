namespace ProjectEulerConsole

open System
open System.Collections

open Microsoft.FSharp.Collections

module Shared =
    open System.Globalization

    type Direction = 
        | North
        | NorthEast
        | East
        | SouthEast
        | South
        | SouthWest
        | West
        | NorthWest

    module Numbers =
        
        let Triangles() =                         
            Seq.unfold (fun (n, sum) -> Some(n + sum, (n + 1, n + sum))) (1, 0)            

        let DivisorsCount n = 
            if n = 1 then 
                1    
            else
                let root = int (Math.Sqrt(float n))
                let counter = 
                    fun count i ->
                        match n % i = 0, i = root with
                        | true, true -> count + 1
                        | true, false -> count + 2
                        | _ -> count                        
                seq { 1 .. root } |> Seq.fold counter 0
            
        let Factors n = 
            if n = 1 then 
                Seq.singleton 1
            else
                let factors = seq { 1 .. n / 2 } |> Seq.choose (fun i -> if n % i = 0 then Some(i) else None)
                Seq.append factors [ n ]            

        let Power x y =
            Seq.init y (fun i -> int x) |> Seq.fold (fun total i -> total * bigint i) 1I

        let Factorial n = 
            [ 2 .. n ] |> List.fold (fun total i -> bigint i * total) 1I

        let SumDigits n = 
            n.ToString().ToCharArray() 
            |> Array.toSeq 
            |> Seq.map ( fun c -> Int32.Parse(c.ToString()) )
            |> Seq.toList
            |> Seq.sum

    module Process =

        let Parallel size (source : unit -> seq<int>) (test : int -> int option) =

            let processors = System.Environment.ProcessorCount 

            let processTask items =
                async { return items |> Array.tryPick test }

            
            let rec doWork(batch) =
                let results = 
                    Async.Parallel [ for task in 0 .. processors -> 
                                         let workSet = source() 
                                                       |> Seq.skip (task * size * batch) 
                                                       |> Seq.take size 
                                                       |> Seq.toArray
                                         processTask workSet ]
                    |> Async.RunSynchronously
                    |> Array.choose (fun n -> n)
                Console.WriteLine("Completed batch {0}", batch)
                if Array.length results > 0 then results else doWork(batch + 1)
            
            let result = doWork(1)

            0

            
    module Fibonacci =

        let Generate =
            Seq.unfold (fun (a,b) -> Some( a + b, (b, a + b) ) ) (0,1)    

    module Palindromes =
       
        let IsValid (s : string) =            
            let length = s.Length
            seq { for i in 0 .. length / 2 - 1 do yield i, length - i - 1 }
            |> Seq.forall (fun (x, y) -> s.[x] = s.[y])    

        let TestInt32 n =
            IsValid( n.ToString() )


    module Primes =

        let IsPrime n =
            let rec testDivisors d =
                if n = d then true
                else if n % d = 0L then false
                else testDivisors (d + 1L)
            testDivisors 2L

        let Factors n =
            seq { 2L .. n } |> Seq.choose (fun i -> if n % i = 0L && IsPrime i then Some(i) else None)

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
            
    module Dates =
        
        let generateRange (startDate : DateTime) (endDate : DateTime) =
            Seq.unfold (fun d -> if (d < endDate) then Some(d, d.AddDays(1.0)) else None) startDate
      
    module Matrix =

        let getTransformation (direction : Direction) =
            match direction with
            | Direction.North     -> Some(fun x y -> x, y - 1)
            | Direction.NorthEast -> Some(fun x y -> x + 1, y - 1)
            | Direction.East      -> Some(fun x y -> x + 1, y)
            | Direction.SouthEast -> Some(fun x y -> x + 1, y + 1)
            | Direction.South     -> Some(fun x y -> x, y + 1)
            | Direction.SouthWest -> Some(fun x y -> x - 1, y + 1)
            | Direction.West      -> Some(fun x y -> x - 1, y)
            | Direction.NorthWest -> Some(fun x y -> x - 1, y - 1)
            | _ -> None

        let getValues (values : int[,]) (direction : Direction) x y count = 
            
            let xDim = Array2D.length1 values
            let yDim = Array2D.length1 values

            if y + count > yDim || x + count > xDim then 
                None
            else
                let transform = (getTransformation direction).Value

                let expander (x', y', c) =
                    match c with
                    | 1 -> Some(values.[x', y'], (x', y', c + 1))
                    | count -> None
       //             | _ -> Some(transform x' y', c + 1)

//                let f = Seq.unfold (fun (a, b, d) -> if (d < endDate) then Some(d, d.AddDays(1.0)) else None) (x, y, 1)
                let f = Seq.unfold (expander) (x, y, 1)
                

                Some(0)
                

