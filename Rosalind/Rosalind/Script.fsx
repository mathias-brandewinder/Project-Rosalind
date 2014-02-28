#load "Rosalind.fs"
open Rosalind.Tools

// http://rosalind.info/problems/dna/
let problem_1 () =

    let update (a,c,g,t) x =
        match x with
        | 'A' -> (a+1,c,g,t)
        | 'C' -> (a,c+1,g,t)
        | 'G' -> (a,c,g+1,t)
        | 'T' -> (a,c,g,t+1)
        | _   -> failwith "Unknown character"

    let input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    let output = input |> Seq.fold (fun st ch -> update st ch) (0,0,0,0)

    output = (20, 12, 17, 21)

// http://rosalind.info/problems/rna/
let problem_2 () =

    let input = "GATGGAACTTGACTACGTAAATT"
    let output = input |> String.map (fun x -> if x = 'T' then 'U' else x)
    output = "GAUGGAACUUGACUACGUAAAUU"

// http://rosalind.info/problems/revc/
let problem_3 () =

    let dnaComplement x =
        match x with
        | 'A' -> 'T'
        | 'C' -> 'G'
        | 'G' -> 'C'
        | 'T' -> 'A'
        | _   -> failwith "Unknown character"
    
    let input = "AAAACCCGGT"   
    let output = new System.String (input |> String.map dnaComplement |> Seq.toArray |> Array.rev)
    output = "ACCGGGTTTT"

// http://rosalind.info/problems/fib/
let problem_4 () =
    
    let n,k = 5,3
    let next (o,n) = (o+n,o*k)
    let pop = Seq.unfold (fun (o,n) -> Some(o+n,next(o,n))) (0,1)
    (pop |> Seq.nth (n-1)) = 19

// http://rosalind.info/problems/gc/
let problem_5 () =

    let gc x =
        match x with
        | 'G' | 'C' -> true
        | 'A' | 'T' -> false
        | _ -> failwith "Unknown case"

    let gcContent (dna:string) = 
        dna |> Seq.averageBy (fun c -> if gc c then 1. else 0.)

    let parseFASTA (data:string) =
        data.Split('>') 
        |> Seq.filter (fun x -> x.StartsWith "Rosalind")
        |> Seq.map (fun x -> x.[..12], (x.[13..]).Replace("\010",""))

    let input = ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT"

    let output = 
        input 
        |> parseFASTA 
        |> Seq.map (fun (id,dna) -> id, 100. * gcContent dna) 
        |> Seq.maxBy snd 
    
    fst output = "Rosalind_0808" && abs (snd output - 60.919540) < 0.001

let problem_6 () =

    let dna1 = "GAGCCTACTAACGGGAT"
    let dna2 = "CATCGTAATGACGGCCT"

    let hamming = 
        (dna1, dna2) 
        ||> Seq.map2 (fun c1 c2 -> if c1 = c2 then 0 else 1) 
        |> Seq.sum

    hamming = 7

// http://rosalind.info/problems/iprb/
//type Allele = 
//    | Dominant 
//    | Recessive
//type Individual = 
//    | Homozigous of Allele
//    | Heterozigous
//
//let problem_7 () =
//    
//    let cross individual1 individual2 =
//        match individual1 with
//        | Homozigous(all1) -> 
//            match all1 with
//            | Dominant -> 1.
//            | Recessive ->
//                match individual2 with
//                | Homozigous(all2) ->
//                    match all2 with
//                    | Dominant -> 1.
//                    | Recessive -> 0.
//                | Heterozigous -> 0.5                    
//        | Heterozigous     -> 
//            match individual2 with
//            | Homozigous(all2) ->
//                match all2 with
//                | Dominant -> 1.
//                | Recessive -> 0.5
//            | Heterozigous -> 0.75
//
//    let k,m,n = 2, 2, 2
//    let pop = [ (Homozigous(Dominant)),  (float k); Heterozigous,            (float m); (Homozigous(Recessive)), (float n) ] |> Map.ofList


// http://rosalind.info/problems/subs/
let problem_8 () =

    let codons = "UUU F      CUU L      AUU I      GUU V
UUC F      CUC L      AUC I      GUC V
UUA L      CUA L      AUA I      GUA V
UUG L      CUG L      AUG M      GUG V
UCU S      CCU P      ACU T      GCU A
UCC S      CCC P      ACC T      GCC A
UCA S      CCA P      ACA T      GCA A
UCG S      CCG P      ACG T      GCG A
UAU Y      CAU H      AAU N      GAU D
UAC Y      CAC H      AAC N      GAC D
UAA Stop   CAA Q      AAA K      GAA E
UAG Stop   CAG Q      AAG K      GAG E
UGU C      CGU R      AGU S      GGU G
UGC C      CGC R      AGC S      GGC G
UGA Stop   CGA R      AGA R      GGA G
UGG W      CGG R      AGG R      GGG G "

    let map = 
        codons 
        |> parseWords 
        |> Seq.toArray 
        |> pairs 
        |> Map.ofSeq

    let translate (dna:string) =
        dna |> chunks 3 |> Seq.map (fun x -> map.[x]) |> Seq.takeWhile (fun t -> t <> "Stop") |> String.concat ""

    let input = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    let output = "MAMAPRTEINSTRING"
    translate input = output



// http://rosalind.info/problems/fibd/
type Pop = bigint list

let problem_9 () =

    let newborn (pop:Pop) (k:bigint) =
        let rec birth c l =
            match l with
            | [] -> failwith "uh?"
            | [_] -> c
            | hd::tl -> 
                let b = hd * k
                birth (c + b) tl
        birth 0I pop 
    
    let next (pop:Pop) k =
        let n = newborn pop k
        List.append (pop.Tail) [n]
    
    let sim n k =
        let pop = [ for i in 1 .. n -> if i = n then 1I else 0I ]
        pop |> Seq.unfold (fun p -> Some(p |> List.sum, next p k))
    
    let (N,M) = (6,3)
    let k = 1I
    let output = sim M k |> Seq.nth (N-1)
    
    output = 4I
    
