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

// TODO PROBLEM 4

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
        |> Seq.map (fun x -> x.[..12], x.[13..].Trim())

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
        |> Seq.map (fun (id,dna) -> id, gcContent dna) 
        |> Seq.maxBy snd 
    
    printfn "%s, %f" (fst output) (snd output)
