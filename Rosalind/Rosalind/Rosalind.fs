namespace Rosalind

module Tools =

    open System
    open System.Text.RegularExpressions

    let chunks (n:int) (dna:string) =
        let l = dna.Length
        Seq.unfold (fun i -> if (i < l) then Some(dna.[i..i+n-1],i+n) else None) 0

    let pairs (x:_ []) =
        let l = x.Length
        Seq.unfold (fun i -> if (i < l) then Some((x.[i],x.[i+1]),i+2) else None) 0
    
    let parseWords (text:string) =
        let matchWords = Regex(@"\w+")
        text
        |> matchWords.Matches
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
