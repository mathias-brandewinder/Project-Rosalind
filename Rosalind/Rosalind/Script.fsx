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