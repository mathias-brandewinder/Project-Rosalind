// http://rosalind.info/problems/rna/

let input = "GATGGAACTTGACTACGTAAATT"
let output = input |> String.map (fun x -> if x = 'T' then 'U' else x)
output = "GAUGGAACUUGACUACGUAAAUU"