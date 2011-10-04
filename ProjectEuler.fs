﻿namespace ProjectEulerConsole

open System

module ProjectEuler =

    let Problem1 =
        Seq.init 1000 (fun n -> n)
        |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0)
        |> Seq.sum
    
    let Problem1_1 =
        Seq.fold (fun total n -> if n % 3 = 0 || n % 5 = 0 then total + n else total) 0 (seq { 1 .. 999 })

    let Problem2 =
        Shared.Fibonacci.Generate
        |> Seq.takeWhile (fun n -> n < 4000000)
        |> Seq.fold (fun total n -> if n % 2 = 0 then total + n else total) 0 

    let Problem3 =
        let value = 600851475143L
        let max = Math.Sqrt(float value) |> int
        let folder = fun total n -> if value % n = 0L then List.append [n] total else total
        Shared.Primes.Sieve max
        |> Seq.map (fun n -> int64 n)
        |> Seq.fold folder [ ] 
        |> Seq.max

    let Problem4 =        
        let x = List.rev [ 100 .. 999 ]
        let test = Shared.Palindromes.TestInt32        
        let search = fun n s -> List.choose (fun i -> if test(n * i) then Some(n * i) else None) s
        List.collect (fun i -> search i x) x
        |> List.sort
        |> List.rev
        |> List.head        

    /// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    /// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    let Problem5 = 
        let x = seq { 3 .. 10 }                
        let factors = Seq.collect (fun i -> Shared.Primes.Sieve i) x
        let g = Seq.toList factors
        0

    /// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
    let Problem6 =
        let sumOfSquares = Seq.initInfinite (fun n -> (n + 1) * (n + 1)) |> Seq.take 100 |> Seq.sum
        let sum = Seq.initInfinite (fun n -> (n + 1)) |> Seq.take 100 |> Seq.sum
        (sum * sum) - sumOfSquares
                
    /// Find the 10001st prime.
    let Problem7 =
        Shared.Primes.Sieve 1000000 |> Seq.nth 10000

    /// Find the greatest product of five consecutive digits in the 1000-digit number.
    let Problem8 =
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
    let Problem9 =
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
    let Problem10 =
        Shared.Primes.Sieve 2000000 |> Seq.map (fun n -> uint64 n) |> Seq.sum

    let Problem13 = 
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


        