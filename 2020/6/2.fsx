open System.IO

printfn "%i"
        ((File.ReadAllText "input.txt").Split "\n\n"
         |> Seq.sumBy (
             fun g -> g.Split "\n"
                   |> Seq.map (Seq.fold (fun s v -> Set.add v s) Set.empty)
                   |> Set.intersectMany
                   |> Set.count
            )
        )