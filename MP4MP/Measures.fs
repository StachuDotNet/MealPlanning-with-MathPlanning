module MP4MP.Measures

// taken from https://github.com/putridparrot/FSharp.Units

module Mass =
    /// grams
    type [<Measure>] g = 
        static member create(value : float) = LanguagePrimitives.FloatWithMeasure<g> value
        static member toMilligrams(value : float<g>) = mg.create(float value * 1000.0)
    
    /// milligrams
    and [<Measure>] mg = 
        static member create(value : float) = LanguagePrimitives.FloatWithMeasure<mg> value
    
     /// micrograms
    and [<Measure>] mcg =
        static member create(value : float) = LanguagePrimitives.FloatWithMeasure<mcg> value
        static member toMilligrams(value : float<mcg>) = mg.create(float value / 1000.0)
    
    let parse metric amount =
        match metric with
        | "g" -> g.create amount |> g.toMilligrams |> Some
        | "mg" -> mg.create amount |> Some
        | "mcg" -> mcg.create amount |> mcg.toMilligrams |> Some
        | _ ->
            //printfn $"Warning: Could not parse metric {metric}"
            None
