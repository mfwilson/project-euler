namespace ProjectEulerConsole

open System
open System.Collections
open System.Text

open Microsoft.FSharp.Collections

module Shared =

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
        open System.Numerics
        
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
            
        let ProperDivisors n =
            if n = 1 then 
                Seq.singleton 1
            else
                seq { 1 .. n / 2 } |> Seq.choose (fun i -> if n % i = 0 then Some(i) else None)

        let Factors n = 
            let divisors = ProperDivisors n
            if n = 1 then divisors else Seq.append divisors [ n ]            

        let Power x y =
            Seq.init y (fun i -> int x) |> Seq.fold (fun total i -> total * bigint i) 1I

        let Factorial n = 
            [ 2 .. n ] |> List.fold (fun total i -> bigint i * total) 1I

        let SumDigits n = 
            n.ToString().ToCharArray() 
            |> Array.map ( fun c -> (int c - 48) )
            |> Array.sum

        let SquareDigits n =
            n.ToString().ToCharArray() 
            |> Array.map (fun c -> (int c - 48) * (int c - 48))
            |> Array.sum

        let AddDigits n (convert : int -> BigInteger) = 
            n.ToString().ToCharArray() 
            |> Array.map ( fun c -> Int32.Parse(c.ToString()) )
            |> Array.map convert
            |> Array.sum

        let IsAmicable n =
            let first = ProperDivisors n
            let second = first |> Seq.sum
            let value = ProperDivisors second |> Seq.sum
            n = value && n <> second

        let IsAbundant n = ProperDivisors n |> Seq.sum > n

        let ToEnglish (n : int) =
            let smallNumbers = [| "Zero"; "One"; "Two"; "Three"; "Four"; "Five"; "Six"; "Seven"; "Eight"; "Nine"; "Ten"; "Eleven"; "Twelve"; "Thirteen"; "Fourteen"; "Fifteen"; "Sixteen"; "Seventeen"; "Eighteen"; "Nineteen" |]
            let tensNumbers = [| ""; ""; "Twenty"; "Thirty"; "Forty"; "Fifty"; "Sixty"; "Seventy"; "Eighty"; "Ninety" |]                        
            let scaleNumbers = [| ""; "Thousand"; "Million"; "Billion" |]

            let threeDigitGroupToWords threeDigits =
                let groupText = new StringBuilder()

                // Determine the hundreds and the remainder
                let hundreds = threeDigits / 100
                let tensUnits = threeDigits % 100
                let tens = tensUnits / 10
                let units = tensUnits % 10

                // Hundreds rules
                if hundreds <> 0 then
                    groupText.Append( smallNumbers.[hundreds] + " Hundred" ) |> ignore
                    if tensUnits <> 0 then groupText.Append(" and ") |> ignore

                // Tens rules
                if tens >= 2 then
                    groupText.Append(tensNumbers.[tens]) |> ignore
                    if units <> 0 then groupText.Append(" " + smallNumbers.[units]) |> ignore
                else if tensUnits <> 0 then
                    groupText.Append(smallNumbers.[tensUnits]) |> ignore
                groupText.ToString()

            // Ensure a positive number to extract from
            let mutable positive = n

            // Extract the three-digit groups
            let digitGroups = [| for i = 0 to 3 do yield positive % 1000; positive <- positive / 1000 |]

            // Convert each three-digit group to words
            let groupText = [| for i = 0 to 3 do yield threeDigitGroupToWords digitGroups.[i] |]

            // Recombine the three-digit groups
            let combined = new StringBuilder(groupText.[0])
            
            // Determine whether an 'and' is needed
            let mutable appendAnd = (digitGroups.[0] > 0) && (digitGroups.[0] < 100)
            
            // Process the remaining groups in turn, smallest to largest
            for i = 1 to 3 do            
                // Only add non-zero items
                if digitGroups.[i] <> 0 then
                    // Build the string to add as a prefix
                    let prefix = new StringBuilder(groupText.[i] + " " + scaleNumbers.[i])
                    if combined.Length <> 0 then prefix.Append(if appendAnd then " and " else ", ") |> ignore

                    // Opportunity to add 'and' is ended
                    appendAnd <- false;

                    // Add the three-digit group to the combined string
                    combined.Insert(0, prefix) |> ignore
            combined.ToString()


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

        let GenerateBig =
            Seq.unfold (fun (a,b) -> Some( a + b, (b, a + b) ) ) (0I,1I)    

    module Palindromes =
       
        let IsValid (s : string) =            
            let length = s.Length
            seq { for i in 0 .. length / 2 - 1 do yield i, length - i - 1 }
            |> Seq.forall (fun (x, y) -> s.[x] = s.[y])    

        let TestInt32 n =
            IsValid( n.ToString() )

        let TestBase2 (n : int) =
            IsValid(Convert.ToString(n, 2))

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
            | Direction.North     -> fun x y -> x - 1, y
            | Direction.NorthEast -> fun x y -> x - 1, y + 1
            | Direction.East      -> fun x y -> x, y + 1
            | Direction.SouthEast -> fun x y -> x + 1, y + 1
            | Direction.South     -> fun x y -> x + 1, y
            | Direction.SouthWest -> fun x y -> x + 1, y - 1
            | Direction.West      -> fun x y -> x, y - 1
            | Direction.NorthWest -> fun x y -> x - 1, y - 1        

        let createSpiral N M =
            let matrix = Array2D.zeroCreate<int> N M

            let changeDirection direction = 
                match direction with
                | Direction.North -> Direction.West
                | Direction.East  -> Direction.North
                | Direction.South -> Direction.East
                | Direction.West  -> Direction.South

            let rec getNextPoint (x, y, direction) =
                let x', y' = getTransformation direction x y
                if x' < N && x' >= 0 && y' >= 0 && y' < M && matrix.[x', y'] = 0 then 
                    x', y', direction
                else
                    let newDirection = changeDirection direction
                    getNextPoint(x, y, newDirection)
                                  
            let expander (value, x, y, direction) =
                matrix.[x, y] <- value                
                let x', y', dir = getNextPoint(x, y, direction)
                match value - 1 with                               
                | 1 -> matrix.[x', y'] <- 1; None
                | _ -> Some(value, (value - 1, x', y', dir))

            Seq.unfold (expander) (N * M, 0, M - 1, Direction.West) |> Seq.toArray |> ignore
            matrix


        let getValues (values : int[,]) (direction : Direction) x y count =
            let xDim = Array2D.length1 values
            let yDim = Array2D.length1 values
            let transform = getTransformation direction

            let next a b c = 
                let xpoint, ypoint = transform a b
                if xpoint >= xDim || xpoint < 0 || ypoint < 0 || ypoint >= yDim then 
                    None
                else
                    Some(values.[xpoint, ypoint], (xpoint, ypoint, c + 1))

            let expander (x', y', c) =
                match c with
                | 1 -> Some(values.[x', y'], (x', y', c + 1))
                | v when v <= count -> next x' y' c
                | _ -> None

            let sequence = Seq.unfold (expander) (x, y, 1)
            if Seq.length sequence = count then Some(sequence) else None
            
        let search (values : int[,]) (directions : Direction seq) count =
            let xDim = Array2D.length1 values
            let yDim = Array2D.length1 values
        
            let points = Seq.init (xDim * yDim) (fun i -> i / xDim, i % yDim) 
                         |> Seq.map (fun p -> directions |> Seq.map (fun d -> d, p) ) 
                         |> Seq.concat
                         |> Seq.toArray

            let sequences = points 
                            |> Array.choose (fun (d, p) -> getValues values d (fst(p)) (snd(p)) count) 
                            |> Array.map (fun x -> Seq.toList x)
                            |> Array.map (fun x -> Seq.reduce (*) x)
                            
            sequences |> Array.max
            
                

