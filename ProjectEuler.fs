namespace ProjectEulerConsole

open System
open System.IO
open Shared.Numbers

module ProjectEuler =
    open System.Numerics

    let Problem1() =
        Seq.init 1000 (fun n -> n)
        |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0)
        |> Seq.sum
    
    let Problem1_1() =
        Seq.fold (fun total n -> if n % 3 = 0 || n % 5 = 0 then total + n else total) 0 (seq { 1 .. 999 })

    let Problem2() =
        Shared.Fibonacci.Generate
        |> Seq.takeWhile (fun n -> n < 4000000)
        |> Seq.fold (fun total n -> if n % 2 = 0 then total + n else total) 0 

    let Problem3() =
        let value = 600851475143L
        let max = Math.Sqrt(float value) |> int
        let folder = fun total n -> if value % n = 0L then List.append [n] total else total
        Shared.Primes.Sieve max
        |> Seq.map (fun n -> int64 n)
        |> Seq.fold folder [ ] 
        |> Seq.max

    let Problem4() =        
        let x = List.rev [ 100 .. 999 ]
        let test = Shared.Palindromes.TestInt32        
        let search = fun n s -> List.choose (fun i -> if test(n * i) then Some(n * i) else None) s
        List.collect (fun i -> search i x) x
        |> List.sort
        |> List.rev
        |> List.head        

    /// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    /// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    let Problem5() = 
        let folder = 
            fun total n -> 
                if total % n = 0L then
                    total
                else
                    let n' = int64 n               
                    let factors = Shared.Primes.Factors n' |> Seq.toList 
                    let res = factors |> Seq.fold (fun p i -> if total % i <> 0L || p % i <> 0L then p * i else p) 1L
                    total * res                

        [ 1L .. 20L ] |> List.fold folder 1L
        

    /// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
    let Problem6() =
        let sumOfSquares = Seq.initInfinite (fun n -> (n + 1) * (n + 1)) |> Seq.take 100 |> Seq.sum
        let sum = Seq.initInfinite (fun n -> (n + 1)) |> Seq.take 100 |> Seq.sum
        (sum * sum) - sumOfSquares
                
    /// Find the 10001st prime.
    let Problem7() =
        Shared.Primes.Sieve 1000000 |> Seq.item 10000

    /// Find the greatest product of five consecutive digits in the 1000-digit number.
    let Problem8() =
        let value = "73167176531330624919225119674426574742355349194934" +
                    "96983520312774506326239578318016984801869478851843" +
                    "85861560789112949495459501737958331952853208805511" +
                    "12540698747158523863050715693290963295227443043557" +
                    "66896648950445244523161731856403098711121722383113" +
                    "62229893423380308135336276614282806444486645238749" +
                    "30358907296290491560440772390713810515859307960866" +
                    "70172427121883998797908792274921901699720888093776" +
                    "65727333001053367881220235421809751254540594752243" +
                    "52584907711670556013604839586446706324415722155397" +
                    "53697817977846174064955149290862569321978468622482" +
                    "83972241375657056057490261407972968652414535100474" +
                    "82166370484403199890008895243450658541227588666881" +
                    "16427171479924442928230863465674813919123162824586" +
                    "17866458359124566529476545682848912883142607690042" +
                    "24219022671055626321111109370544217506941658960408" +
                    "07198403850962455444362981230987879927244284909188" +
                    "84580156166097919133875499200524063689912560717606" +
                    "05886116467109405077541002256983155200055935729725" +
                    "71636269561882670428252483600823257530420752963450"         
        let toInt = fun c -> Int32.Parse(c.ToString())
        let mult = fun (array : char[]) -> Array.fold (fun total c -> total * toInt(c)) 1 array
        value.ToCharArray() 
        |> Array.toSeq 
        |> Seq.windowed 5 
        |> Seq.map mult
        |> Seq.max

    /// There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
    let Problem9() =
        let integers = Seq.initInfinite (fun n -> n + 1) |> Seq.take 1000 |> Seq.toList
        let test = 
            fun a b -> 
                if a > b then
                    false
                else
                    let sum = (a * a) + (b * b)
                    let c = Math.Sqrt(float sum)
                    if c = Math.Round(c, 0) && a + b + int c = 1000 then true else false
                    //Console.WriteLine("a:{0} b:{1} c:{2}", a, b, c)
        let search = fun n s -> List.choose (fun i -> if test n i then Some(n, i, 1000 - n - i) else None) s
        let a, b, c = List.collect (fun i -> search i integers) integers |> List.head
        a * b * c
        
    /// Find the sum of all the primes below two million.
    let Problem10() =
        Shared.Primes.Sieve 2000000 |> Seq.map (fun n -> uint64 n) |> Seq.sum

    /// What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
    let Problem11() =        
        let values = array2D [
                        [ 08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08  ];
                        [ 49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00  ];
                        [ 81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65  ];
                        [ 52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91  ];
                        [ 22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80  ];
                        [ 24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50  ];
                        [ 32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70  ];
                        [ 67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21  ];
                        [ 24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72  ];
                        [ 21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95  ];
                        [ 78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92  ];
                        [ 16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57  ];
                        [ 86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58  ];
                        [ 19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40  ];
                        [ 04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66  ];
                        [ 88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69  ];
                        [ 04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36  ];
                        [ 20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16  ];
                        [ 20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54  ];
                        [ 01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48  ];
                    ]
        
        let directions = [| Shared.Direction.East; Shared.Direction.SouthEast; Shared.Direction.South; Shared.Direction.SouthWest; |]
        Shared.Matrix.search values directions 4

    /// What is the value of the first triangle number to have over five hundred divisors?
    let Problem12() =
        let picker = 
            fun n ->         
                let length = Shared.Numbers.DivisorsCount n
                if length > 500 then Some(n) else None
        let result = Shared.Numbers.Triangles() |> Seq.tryPick picker
        result.Value        

    let Problem13() = 
        let result = [ 37107287533902102798797998220837590246510135740250I;
                       46376937677490009712648124896970078050417018260538I;
                       74324986199524741059474233309513058123726617309629I;
                       91942213363574161572522430563301811072406154908250I;
                       23067588207539346171171980310421047513778063246676I;
                       89261670696623633820136378418383684178734361726757I;
                       28112879812849979408065481931592621691275889832738I;
                       44274228917432520321923589422876796487670272189318I;
                       47451445736001306439091167216856844588711603153276I;
                       70386486105843025439939619828917593665686757934951I;
                       62176457141856560629502157223196586755079324193331I;
                       64906352462741904929101432445813822663347944758178I;
                       92575867718337217661963751590579239728245598838407I;
                       58203565325359399008402633568948830189458628227828I;
                       80181199384826282014278194139940567587151170094390I;
                       35398664372827112653829987240784473053190104293586I;
                       86515506006295864861532075273371959191420517255829I;
                       71693888707715466499115593487603532921714970056938I;
                       54370070576826684624621495650076471787294438377604I;
                       53282654108756828443191190634694037855217779295145I;
                       36123272525000296071075082563815656710885258350721I;
                       45876576172410976447339110607218265236877223636045I;
                       17423706905851860660448207621209813287860733969412I;
                       81142660418086830619328460811191061556940512689692I;
                       51934325451728388641918047049293215058642563049483I;
                       62467221648435076201727918039944693004732956340691I;
                       15732444386908125794514089057706229429197107928209I;
                       55037687525678773091862540744969844508330393682126I;
                       18336384825330154686196124348767681297534375946515I;
                       80386287592878490201521685554828717201219257766954I;
                       78182833757993103614740356856449095527097864797581I;
                       16726320100436897842553539920931837441497806860984I;
                       48403098129077791799088218795327364475675590848030I;
                       87086987551392711854517078544161852424320693150332I;
                       59959406895756536782107074926966537676326235447210I;
                       69793950679652694742597709739166693763042633987085I;
                       41052684708299085211399427365734116182760315001271I;
                       65378607361501080857009149939512557028198746004375I;
                       35829035317434717326932123578154982629742552737307I;
                       94953759765105305946966067683156574377167401875275I;
                       88902802571733229619176668713819931811048770190271I;
                       25267680276078003013678680992525463401061632866526I;
                       36270218540497705585629946580636237993140746255962I;
                       24074486908231174977792365466257246923322810917141I;
                       91430288197103288597806669760892938638285025333403I;
                       34413065578016127815921815005561868836468420090470I;
                       23053081172816430487623791969842487255036638784583I;
                       11487696932154902810424020138335124462181441773470I;
                       63783299490636259666498587618221225225512486764533I;
                       67720186971698544312419572409913959008952310058822I;
                       95548255300263520781532296796249481641953868218774I;
                       76085327132285723110424803456124867697064507995236I;
                       37774242535411291684276865538926205024910326572967I;
                       23701913275725675285653248258265463092207058596522I;
                       29798860272258331913126375147341994889534765745501I;
                       18495701454879288984856827726077713721403798879715I;
                       38298203783031473527721580348144513491373226651381I;
                       34829543829199918180278916522431027392251122869539I;
                       40957953066405232632538044100059654939159879593635I;
                       29746152185502371307642255121183693803580388584903I;
                       41698116222072977186158236678424689157993532961922I;
                       62467957194401269043877107275048102390895523597457I;
                       23189706772547915061505504953922979530901129967519I;
                       86188088225875314529584099251203829009407770775672I;
                       11306739708304724483816533873502340845647058077308I;
                       82959174767140363198008187129011875491310547126581I;
                       97623331044818386269515456334926366572897563400500I;
                       42846280183517070527831839425882145521227251250327I;
                       55121603546981200581762165212827652751691296897789I;
                       32238195734329339946437501907836945765883352399886I;
                       75506164965184775180738168837861091527357929701337I;
                       62177842752192623401942399639168044983993173312731I;
                       32924185707147349566916674687634660915035914677504I;
                       99518671430235219628894890102423325116913619626622I;
                       73267460800591547471830798392868535206946944540724I;
                       76841822524674417161514036427982273348055556214818I;
                       97142617910342598647204516893989422179826088076852I;
                       87783646182799346313767754307809363333018982642090I;
                       10848802521674670883215120185883543223812876952786I;
                       71329612474782464538636993009049310363619763878039I;
                       62184073572399794223406235393808339651327408011116I;
                       66627891981488087797941876876144230030984490851411I;
                       60661826293682836764744779239180335110989069790714I;
                       85786944089552990653640447425576083659976645795096I;
                       66024396409905389607120198219976047599490197230297I;
                       64913982680032973156037120041377903785566085089252I;
                       16730939319872750275468906903707539413042652315011I;
                       94809377245048795150954100921645863754710598436791I;
                       78639167021187492431995700641917969777599028300699I;
                       15368713711936614952811305876380278410754449733078I;
                       40789923115535562561142322423255033685442488917353I;
                       44889911501440648020369068063960672322193204149535I;
                       41503128880339536053299340368006977710650566631954I;
                       81234880673210146739058568557934581403627822703280I;
                       82616570773948327592232845941706525094512325230608I;
                       22918802058777319719839450180888072429661980811197I;
                       77158542502016545090413245809786882778948721859617I;
                       72107838435069186155435662884062257473692284509516I;
                       20849603980134001723930671666823555245252804609722I;
                       53503534226472524250874054075591789781264330331690I; ] |> List.sum
        result.ToString().Substring(0, 10)

    /// Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
    /// How many such routes are there through a 20×20 grid?
    let Problem15() =
        // central binomial coefficients
        // (2n)! / n!^2
        let centralBinomialCoefficient n =
            let denominator = !!n
            (!!(2 * n)) / (denominator * denominator)            
        centralBinomialCoefficient 20

    /// What is the sum of the digits of the number 2^1000?
    let Problem16() =        
        let n = Shared.Numbers.Power 2 1000        
        Shared.Numbers.SumDigits n               

    /// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
    ///
    /// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
    /// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) 
    /// contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
    let Problem17() =        
        let result = seq { 1 .. 1000 }
                     |> Seq.map (fun n -> Shared.Numbers.ToEnglish n)
                     |> Seq.map (fun s -> s.Replace(" ", "").Length)
                     |> Seq.sum       
        result

    /// You are given the following information, but you may prefer to do some research for yourself.
    ///    1 Jan 1900 was a Monday.
    ///    Thirty days has September,
    ///    April, June and November.
    ///    All the rest have thirty-one,
    ///    Saving February alone,
    ///    Which has twenty-eight, rain or shine.
    ///    And on leap years, twenty-nine.
    /// A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
    /// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?       
    let Problem19() =
        let startDate = DateTime(1901, 1, 1)
        let endDate = DateTime(2000, 12, 31)
        let isSundayTheFirst(date : DateTime) = date.DayOfWeek = DayOfWeek.Sunday && date.Day = 1
        Shared.Dates.generateRange startDate endDate |> Seq.where isSundayTheFirst |> Seq.length
              
    /// Find the sum of the digits in the number 100!
    let Problem20() =
        let result = Shared.Numbers.Factorial 100
        Shared.Numbers.SumDigits result

    /// Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
    /// If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
    /// For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
    /// Evaluate the sum of all the amicable numbers under 10000.
    let Problem21() =
        let result = seq { 1 .. 10000 } |> Seq.where (fun n -> Shared.Numbers.IsAmicable n) |> Seq.toArray
        result |> Array.sum
        
    /// Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it  
    /// into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list 
    /// to obtain a name score.
    ///
    /// For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, 
    /// COLIN would obtain a score of 938 × 53 = 49714.
    /// 
    /// What is the total of all the name scores in the file?
    let Problem22() =
        let scoreName (name : String) =            
            name.ToCharArray() |> Array.sumBy (fun c -> int c - int 'A' + 1)

        let allNames = File.ReadAllText(@"Data\p022_names.txt").Split([| ','|])
                       |> Array.map (fun (n : String) -> n.Replace("\"", ""))
        allNames |> Array.sortInPlace

        let result = allNames |> Array.mapi (fun i name -> scoreName name * (i + 1)) |> Array.sum
        result

    /// A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 
    /// 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
    ///
    /// A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
    ///
    /// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By 
    /// mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper 
    // limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant 
    // numbers is less than this limit.
    ///
    /// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
    let Problem23() =
        let abundantNumbers = seq { 1 .. 28123 } |> Seq.where (fun n -> Shared.Numbers.IsAbundant n) |> Seq.toArray
        let numberSet = abundantNumbers |> Set.ofArray

        // binary search?
        
        let hasSum (n : int) (slice : int[]) =            
            let value = slice |> Array.tryFindBack (fun r -> n - r <> r && numberSet.Contains(n - r))
            value.IsSome
            
        let result = seq { 1 .. 28123 }
                     |> Seq.toArray
                     |> Array.skip 1
                     |> Array.mapi (fun i n -> if hasSum n (abundantNumbers.[0 .. (i - 1)]) then None else Some(n)) 
                     |> Array.choose id
                     |> Array.sum
        result + 1

    /// The Fibonacci sequence is defined by the recurrence relation:
    /// Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
    /// Hence the first 12 terms will be:
    ///    F1 = 1
    ///    F2 = 1
    ///    F3 = 2
    ///    F4 = 3
    ///    F5 = 5
    ///    F6 = 8
    ///    F7 = 13
    ///    F8 = 21
    ///    F9 = 34
    ///    F10 = 55
    ///    F11 = 89
    ///    F12 = 144
    /// The 12th term, F12, is the first term to contain three digits.
    ///    
    /// What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
    let Problem25() =
        let index = Shared.Fibonacci.GenerateBig |> Seq.tryFindIndex (fun n -> n.ToString().Length >= 1000)
        index.Value + 2

    /// Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
    ///
    ///    21 22 23 24 25
    ///    20  7  8  9 10
    ///    19  6  1  2 11
    ///    18  5  4  3 12
    ///    17 16 15 14 13
    ///
    /// It can be verified that the sum of the numbers on the diagonals is 101.
    ///
    /// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
    let Problem28() =
        let n = 1001
        let matrix = Shared.Matrix.createSpiral n n 
        
        let diagonal1 = Shared.Matrix.getValues matrix (Shared.Direction.SouthEast) 0 0 n
        let diagonal2 = Shared.Matrix.getValues matrix (Shared.Direction.NorthEast) (n - 1) 0 n

        let result = (diagonal1.Value |> Seq.sum) + (diagonal2.Value |> Seq.sum) - 1
        result

    /// How many distinct terms are in the sequence generated by a^b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100? 
    let Problem29() =
        let powers = seq { 2 .. 100 } |> Seq.toArray
        let values = seq { 2 .. 100 } 
                     |> Seq.toArray 
                     |> Array.map (fun a -> powers |> Array.map (fun b -> Shared.Numbers.Power a b)) 
                     |> Array.concat
                     |> Array.distinct
        values.Length
        
    /// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145. 
    /// Find the sum of all numbers which are equal to the sum of the factorial of their digits.
    /// Note: as 1! = 1 and 2! = 2 are not sums they are not included.
    let Problem34() =
        let factorials = seq { 0 .. 9 } |> Seq.map (fun n -> Shared.Numbers.Factorial n) |> Seq.toArray
        
        let space = seq { 10 .. 1000000 } 
                    |> Seq.choose (fun n -> if bigint n = Shared.Numbers.AddDigits n (fun n -> factorials.[n]) then Some(n) else None)
                    |> Seq.toArray
        space |> Array.sum

    /// The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
    /// Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
    /// (Please note that the palindromic number, in either base, may not include leading zeros.)
    let Problem36() =
        let palindromes = seq { 1 .. 1000000 } 
                          |> Seq.where (fun n -> Shared.Palindromes.TestInt32 n && Shared.Palindromes.TestBase2 n)
                          |> Seq.toArray
        palindromes |> Array.sum       

    /// Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten pentagonal numbers are:
    /// 
    /// 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
    /// It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 − 22 = 48, is not pentagonal.
    ///
    /// Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised; what is the value of D?
    let Problem44() =
        let pentagonals = Seq.init 10000 (fun n -> n * (3 * n - 1) / 2) |> Seq.skip 1 |> Seq.toArray
        let pentagonSet = pentagonals |> Set.ofSeq
        let length = pentagonals.Length - 1        
        
        let rec seek j k =
            if k >= length then 
                None
            else
                let Pj = pentagonals.[j]
                let Pk = pentagonals.[k]
                if pentagonSet.Contains(Pj + Pk) && pentagonSet.Contains(Pk - Pj) then  
                    Some(Pk - Pj)
                else
                    seek j (k + 1)

        let result = seq { 0 .. length } |> Seq.pick (fun n -> seek n (n+1)) 
        result

    /// Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
    let Problem48() =        
        let result = seq { 1 .. 1000 } |> Seq.map (fun i -> Shared.Numbers.Power i i) |> Seq.sum
        let s = result.ToString()
        s.Substring(s.Length - 10, 10)
        
    /// The prime 41, can be written as the sum of six consecutive primes:
    ///     41 = 2 + 3 + 5 + 7 + 11 + 13
    /// This is the longest sum of consecutive primes that adds to a prime below one-hundred.
    /// The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
    ///
    /// Which prime, below one-million, can be written as the sum of the most consecutive primes?
    let Problem50() =
        let primes = Shared.Primes.Sieve 1000000 |> Seq.toArray
        let primeSet = primes |> Set.ofArray
        // TODO
        0

    /// If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
    /// Not all numbers produce palindromes so quickly. For example,
    /// 
    /// 349 + 943 = 1292,
    /// 1292 + 2921 = 4213
    /// 4213 + 3124 = 7337
    ///
    /// That is, 349 took three iterations to arrive at a palindrome.
    ///
    /// Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that 
    /// never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of 
    /// these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition 
    /// you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, 
    /// (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first 
    /// number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
    ///
    /// Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
    /// 
    /// How many Lychrel numbers are there below ten-thousand?
    /// 
    /// NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.
    let Problem55() =
        let sumAndReverse n = n + BigInteger.Parse(new String(n.ToString().ToCharArray() |> Array.rev))
                    
        let rec isLychrel n retry =
            if retry <= 0 then 
                false
            else
                let value = sumAndReverse n
                if Shared.Palindromes.TestInt32 value then 
                    true
                else
                    isLychrel value (retry - 1)

        let values = seq { 1I .. 9999I } |> Seq.choose (fun n -> if isLychrel n 50 then None else Some n) |> Seq.toArray
        values |> Seq.length

    /// A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
    /// 
    /// For example,
    /// 
    ///     44 → 32 → 13 → 10 → 1 → 1
    ///     85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
    /// 
    /// Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
    /// 
    /// How many starting numbers below ten million will arrive at 89?
    let Problem92() =
        let rec findNumberChain n =
            let result = Shared.Numbers.SquareDigits n
            match n with
            | 1 -> None
            | 89 -> Some(1)
            | _ -> findNumberChain result
        let result = seq { 2 .. 10000000 } |> Seq.choose (fun n -> findNumberChain n) |> Seq.sum
        result

    /// By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a square number: 1296 = 362. What 
    /// is remarkable is that, by using the same digital substitutions, the anagram, RACE, also forms a square number: 9216 = 962. We 
    /// shall call CARE (and RACE) a square anagram word pair and specify further that leading zeroes are not permitted, neither may 
    /// a different letter have the same digital value as another letter.
    ///
    /// Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, 
    /// find all the square anagram word pairs (a palindromic word is NOT considered to be an anagram of itself).
    /// 
    /// What is the largest square number formed by any member of such a pair?
    let Problem98() =
        let allWords = File.ReadAllText(@"Data\p098_words.txt").Split([| ','|])
                       |> Array.map (fun (n : String) -> n.Replace("\"", ""))

        let sortedWords = allWords 
                          |> Array.map (fun w -> Array.sort (w.ToCharArray()))
                          |> Array.mapi (fun i c -> new String(c), i)
                          |> Array.sortBy (fun w -> fst(w))

        let anagrams = sortedWords 
                       |> Array.pairwise 
                       |> Array.where (fun (a, b) -> fst(a) = fst(b))
                       |> Array.map (fun (a, b) -> allWords.[snd(a)], allWords.[snd(b)])
                       |> Array.sortByDescending (fun (a, b) -> a.Length)

        let min = anagrams |> Array.minBy (fun (a, b) -> Seq.length a) 
        let max = anagrams |> Array.maxBy (fun (a, b) -> Seq.length a) 
        let minLength = fst(min).Length
        let maxLength = fst(max).Length

        let squaresMap = Seq.init 100000 (fun n -> int64 n * int64 n) 
                         |> Seq.where (fun n -> let len = n.ToString().Length
                                                len >= minLength && len <= maxLength) 
                         |> Seq.groupBy (fun n -> n.ToString().Length)
                         |> Seq.map (fun (len, coll) -> len, coll |> Seq.map (fun s -> s.ToString()) |> Set.ofSeq)
                         |> Map.ofSeq

        let matchSquare (a : char[], b : char[]) =
            let squares = squaresMap.[a.Length]

            let pick (square : String) =
                let map = Array.zip a (square.ToCharArray()) |> Map.ofArray
                let reverseMap = Array.zip (square.ToCharArray()) a |> Map.ofArray
                if map.Count <> reverseMap.Count then
                    None
                else
                    let lookup = new String(b |> Array.map (fun c -> map.[c]))
                    if squares.Contains(lookup) then Some([| square; lookup |]) else None
                    
            let results = squares |> Seq.choose (fun s -> pick s) |> Seq.concat |> Seq.map (fun s -> Int64.Parse s) 
            if Seq.length results = 0 then None else Some(results |> Seq.max)

        let result = anagrams |> Array.tryPick (fun (a, b) -> matchSquare(a.ToCharArray(), b.ToCharArray()))
        result.Value

    let Problem99() =
        
        let allPairs = File.ReadAllText(@"Data\p099_base_exp.txt").Split([| '\n' |])
                       |> Array.map (fun (pair : String) -> pair.Split([| ',' |]))
                       |> Array.map (fun (pair : string []) -> (bigint.Parse(pair.[0]), Int32.Parse(pair.[1])))
        
        let results = allPairs |> Array.map (fun n -> fst(n), snd(n), Shared.Numbers.Factors (snd n) |> Seq.toArray)


//        let results = allPairs |> Array.map (fun n -> Math.Pow(fst(n) / 10000.0, snd(n) / 1000.0))
  //      let max = results |> Array.max
  //      (results |> Array.findIndex (fun n -> n = max)) + 1
        0    