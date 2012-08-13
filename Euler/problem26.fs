


[<EntryPoint>]
let main argv = 
    let cycleLengthOfExpansionOf n =
        let y = 1m / n

        let s = y.ToString("F50")

        let t = s.ToCharArray() |> Array.toList
    
        let u = t |> List.rev |> List.findIndex (fun x -> x <> '0')

        let trimmedString = s.Substring(0, 52 - u - 1) // - 1 because the last digit may be rounded

        if trimmedString.Length = 0 then
            0
        else
            let expansion = 
                trimmedString.Substring(2).ToCharArray()
                |> Array.rev
                |> Array.mapi (fun x y -> (x, y))
        
            if expansion.Length <= 1 then
                0
            else
                let isConsistentWithCycleLength possibleCycleLength (expansion: (int * char) []) = 
                    expansion
                    |> Seq.forall 
                        (fun (digitIndex, digit) -> 
                            (digitIndex % possibleCycleLength <> 0) || digit = snd(expansion.[digitIndex % possibleCycleLength])
                        )

                let cycleLength =
                    [1..expansion.Length] // possible cycle lengths are all shorter than the actual expansion's length
                    |> Seq.find (fun possibleCycleLength -> isConsistentWithCycleLength possibleCycleLength expansion)

                if cycleLength < expansion.Length then
                    cycleLength
                else
                    0

    let maxCycleLength =
        [2..1000]
        |> Seq.map (fun x -> (x, cycleLengthOfExpansionOf (decimal x)))
        |> Seq.maxBy (fun (x, y) -> y)

    printfn "%A %A" (1m / (decimal(fst(maxCycleLength)))) maxCycleLength

    0