// http://rosalind.info/problems/revc/

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