namespace ProjectEulerConsole

module Shared =

    module Fibonacci =

        let Generate =
            Seq.unfold (fun (a,b) -> Some( a+b, (b, a+b) ) ) (0,1)    

