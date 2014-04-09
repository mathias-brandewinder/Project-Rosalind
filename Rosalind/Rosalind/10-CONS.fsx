//http://rosalind.info/problems/cons/

#load "Rosalind.fs"
open Rosalind.Tools

let input = @">Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGCAACC
>Rosalind_5
TTGGAACT
>Rosalind_6
ATGCCATT
>Rosalind_7
ATGGCACT"

let output = @"ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6"

let count (dna:string) =
    dna 
    |> Seq.map (fun x ->
        match x with
        | 'A' -> [| 1;0;0;0 |]
        | 'C' -> [| 0;1;0;0 |]
        | 'G' -> [| 0;0;1;0 |]
        | 'T' -> [| 0;0;0;1 |]
        | _   -> failwith "Oops")
    |> Seq.toArray

let accumulate count1 count2 =
    (count1, count2) 
    ||> Array.map2 (fun x y -> (x,y) ||> Array.map2 (fun x1 y1 -> x1+y1))

let mostFrequent (count:int[]) =
    ([|'A';'C';'G';'T'|],count) 
    ||> Array.zip 
    |> Array.maxBy snd |> fst

let majority (input:string) =
    let parsed = 
        input 
        |> parseFASTA 
        |> Array.map snd
    let len = parsed.[0].Length
    let init = Array.init len (fun x -> [|0;0;0;0|])

    let matrix = 
        parsed 
        |> Array.map count
        |> Array.fold (fun state x -> accumulate state x) init
        
    let maj = matrix |> Seq.map mostFrequent |> Seq.map string |> String.concat ""
    let stuff = 
        [|'A';'C';'G';'T'|] 
        |> Array.mapi (fun i c -> 
            let counts = matrix |> Array.map (fun block -> block.[i] |> string) |> String.concat " "
            sprintf "%s: %s" (string c) counts)
        |> String.concat "\n"
            
    maj + "\n" + stuff

output = majority input