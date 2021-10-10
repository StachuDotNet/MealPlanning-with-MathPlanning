module DataEntry.Measures

// taken from https://github.com/putridparrot/FSharp.Units

module Mass = 
    type [<Measure>] g = // grams
        static member create(value : float) = LanguagePrimitives.FloatWithMeasure<g> value
        static member toMilligrams(value : float<g>) = mg.create(float value * 1000.0)
    and [<Measure>] mg = // milligrams
        static member create(value : float) = LanguagePrimitives.FloatWithMeasure<mg> value
    
    and [<Measure>] mcg = // micrograms
        static member create(value : float) = LanguagePrimitives.FloatWithMeasure<g> value
        static member toMilligrams(value : float<g>) = mg.create(float value * 1000000.0)
    let parse metric amount =
        match metric with
        | "g" -> g.create amount |> g.toMilligrams |> Some
        | "mg" -> mg.create amount |> Some
        | "mcg" -> mcg.create amount |> mcg.toMilligrams |> Some
        | _ ->
            printfn $"Warning: Could not parse metric {metric}"
            None
