﻿namespace ProjectEulerConsole

module ProjectEuler =

    let Problem1 =
        Seq.init 1000 (fun n -> n)
        |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0)
        |> Seq.sum
    
    let Problem1_1 =
        Seq.fold (fun total n -> if n % 3 = 0 || n % 5 = 0 then total + n else total) 0 (seq { 1 .. 999 })

