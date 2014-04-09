//http://rosalind.info/problems/prot/

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
UGG W      CGG R      AGG R      GGG G"

let byPair ls = 
    ls 
    |> Seq.unfold (fun state -> 
        match state with 
        | [] -> None 
        | x1::x2::rest -> Some((x1,x2),rest))

let codonsTable =
    codons.Split([|' '; '\n' |]) 
    |> Array.filter (fun x -> x <> "")
    |> Array.toList 
    |> byPair 
    |> Map.ofSeq

let translate (dna:string) = 
    seq {
        for i in 0..((dna.Length-1)/3) do
            let token = dna.[i*3..i*3+2]
            yield codonsTable.[token]
    } 
    |> Seq.takeWhile (fun value -> value <> "Stop")
    |> String.concat ""
   
let input = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
let output = "MAMAPRTEINSTRING"

translate input = output