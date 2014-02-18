let problem_1 () =

    let update (a,c,g,t) x =
        match x with
        | 'A' -> (a+1,c,g,t)
        | 'C' -> (a,c+1,g,t)
        | 'G' -> (a,c,g+1,t)
        | 'T' -> (a,c,g,t+1)
        | _   -> failwith "Unknown character"

    let dna = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    let result = dna |> Seq.fold (fun st ch -> update st ch) (0,0,0,0)

    result = (20, 12, 17, 21)

let problem_2 () =

    let input = "GATGGAACTTGACTACGTAAATT"
    let output = input |> String.map (fun x -> if x = 'T' then 'U' else x)
    output = "GAUGGAACUUGACUACGUAAAUU"