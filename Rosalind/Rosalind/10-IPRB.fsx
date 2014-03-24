//http://rosalind.info/problems/iprb/

let input = "2 2 2"
let output = "0.78333"

type Allele = 
    | Dominant
    | Recessive

type Genotype = (Allele * Allele)
type Pop = Map<Genotype,int>

let pick (t:Genotype) (pop:Pop) =
    let tot = pop |> Map.toSeq |> Seq.sumBy snd   
    let p = float pop.[t] / float tot
    p, pop |> Map.add t (pop.[t] - 1)

let solve (input:string) =

    let genotypes = [| (Dominant,Dominant); (Dominant,Recessive); (Recessive,Recessive); |]
    let population = 
        input.Split(' ') 
        |> Array.map int 
        |> Array.zip genotypes
        |> Map.ofArray

    let combos = 
        [ 
            for g1 in population do
                let (proba1,rest) = pick (g1.Key) population
                for g2 in rest do
                    let (proba2,_) = pick (g2.Key) rest
                    yield (proba1*proba2), (g1.Key,g2.Key)
        ]

    "TODO"

solve input = output