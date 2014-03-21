//http://rosalind.info/problems/hamm/

let dna1 = "GAGCCTACTAACGGGAT"
let dna2 = "CATCGTAATGACGGCCT"

let hamming = 
    (dna1, dna2) 
    ||> Seq.map2 (fun c1 c2 -> if c1 = c2 then 0 else 1) 
    |> Seq.sum

hamming = 7