// http://rosalind.info/problems/dna/

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