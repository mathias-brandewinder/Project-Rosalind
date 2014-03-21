// http://rosalind.info/problems/gc/

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
let test = parseFASTA input
let output = 
    input 
    |> parseFASTA 
    |> Seq.map (fun (id,dna) -> id, 100. * gcContent dna) 
    |> Seq.maxBy snd 
    
fst output = "Rosalind_0808" && abs (snd output - 60.919540) < 0.001