// http://rosalind.info/problems/fib/

let n,k = 5,3
let next (o,n) = (o+n,o*k)
let pop = Seq.unfold (fun (o,n) -> Some(o+n,next(o,n))) (0,1)
(pop |> Seq.nth (n-1)) = 19