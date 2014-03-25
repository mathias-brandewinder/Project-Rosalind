//http://rosalind.info/problems/iprb/

let input = "27 17 25" //"2 2 2"
let output = "0.767050298380222"

type Allele = 
    | Dominant
    | Recessive

type Genotype =
    | Heterozygous
    | Homozygous of Allele

type Pop = Map<Genotype,int>

let pick (t:Genotype) (pop:Pop) =
    let tot = pop |> Map.toSeq |> Seq.sumBy snd   
    let p = float pop.[t] / float tot
    p, pop |> Map.add t (pop.[t] - 1)

// fully spelled out, heavy version
//let probaDominant (g1:Genotype) (g2:Genotype) =
//    match g1 with
//    | Homozygous(x) ->
//        match x with
//        | Dominant -> 1.
//        | Recessive ->
//            match g2 with
//            | Homozygous(y) ->
//                match y with
//                | Recessive -> 0.
//                | Dominant ->  1.
//            | Heterozygous ->  0.5
//    | Heterozygous ->
//        match g2 with
//        | Homozygous(y) ->
//            match y with
//            | Dominant ->  1.
//            | Recessive -> 0.5
//        | Heterozygous ->  0.75

let probaDominant (g1:Genotype) (g2:Genotype) =
    match (g1,g2) with
    | Homozygous(Dominant),  _  
    | _,                     Homozygous(Dominant)  -> 1.0
    | Heterozygous,          Heterozygous          -> 0.75
    | Homozygous(Recessive), Heterozygous 
    | Heterozygous,          Homozygous(Recessive) -> 0.5
    | Homozygous(Recessive), Homozygous(Recessive) -> 0.0

let solve (input:string) =

    let genotypes = [| Homozygous(Dominant); Heterozygous; Homozygous(Recessive); |]
    let population = 
        input.Split(' ') 
        |> Array.map int 
        |> Array.zip genotypes
        |> Map.ofArray

    [ 
        for g1 in population do
            let (proba1,rest) = pick (g1.Key) population
            for g2 in rest do
                let (proba2,_) = pick (g2.Key) rest
                yield proba1 * proba2 * probaDominant g1.Key g2.Key
    ]
    |> List.sum
    |> string

solve input = output

//"27 17 25"
//"0.767050298380222"