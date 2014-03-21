// http://rosalind.info/problems/fibd/

type Pop = bigint list

let newborn (pop:Pop) (k:bigint) =
    let rec birth c l =
        match l with
        | [] -> failwith "uh?"
        | [_] -> c
        | hd::tl -> 
            let b = hd * k
            birth (c + b) tl
    birth 0I pop 
    
let next (pop:Pop) k =
    let n = newborn pop k
    List.append (pop.Tail) [n]
    
let sim n k =
    let pop = [ for i in 1 .. n -> if i = n then 1I else 0I ]
    pop |> Seq.unfold (fun p -> Some(p |> List.sum, next p k))
    
let (N,M) = (6,3)
let k = 1I
let output = sim M k |> Seq.nth (N-1)
    
output = 4I
