open System.IO
open System.Text.Json
open DataEntry
open DataEntry.KnownFoods
open DataEntry.NutrientRequirements
open DataEntry.Nutrients
open DataEntry.RawInputTypes

let fileLocation = "/home/stachu-locai/Data/healthy-foods.json"

let foods =
    File.ReadAllText fileLocation
    |> JsonSerializer.Deserialize<RawInputFood array>

let printList heading list =
    printfn "%s: %A" heading (String.concat ", " list)

let allCategories = foods |> Seq.map(fun f -> f.CategoryName) |> Seq.distinct
printList "Categories" allCategories

let parsedFoods = foods |> Seq.map parseFood

open Flips
open Flips.Types

let items = parsedFoods |> Seq.map (fun z -> z.Name) |> Seq.toList
let foodMap =
    items
    |> List.map(fun name ->
        name,
        parsedFoods |> Seq.find(fun z -> z.Name = name)
    )
    |> Map.ofList

let numberOfItem = // this is what we're deciding on
    [for item in items do
        item, Decision.createContinuous (sprintf "NumberOf%s" item) 0.0 infinity]
    |> Map.ofList

let objective =
    let caloriesForFood = items |> List.map(fun item -> item, foodMap.[item].ServingSizeData.Calories |> double) |> Map.ofList
    let objectiveExpression = List.sum [for item in items -> caloriesForFood.[item] * numberOfItem.[item]]
    Objective.create "MinimizeCalories" Minimize objectiveExpression

let constraints =
    let createNutrientExpr nutrient = 
        [ for item in items ->
            let q = numberOfItem.[item]
            
            let amountInFood =
                foodMap.[item].FoodNutrition
                |> Seq.tryFind(fun f -> f.Nutrient = nutrient)
                |> Option.bind(fun z -> z.Amount)
                |> Option.defaultValue 0.0<Measures.Mass.mg>
                |> double
            
            amountInFood * q
        ]
        |> List.sum

    let createNutrientConstraint nutrient =
        let expr = createNutrientExpr nutrient
        let req = nutritionRequirements nutrient |> Option.defaultValue 0.0<Measures.Mass.mg> |> float
        let name = Nutrient.getName nutrient
        Constraint.create $"Min {req}mg {name}" (expr >== req)
        
    allNutrients |> List.map createNutrientConstraint

let model =
    Model.create objective
    |> Model.addConstraints constraints

let result = Solver.solve Settings.basic model

match result with
| Optimal solution ->
    printfn $"Minimum Calories: {Objective.evaluate solution objective}"

    solution.DecisionResults
    |> Map.toList
    |> Seq.filter(fun (_decision, value) -> value > 0.0)
    |> Seq.iter(fun (decision, value) -> 
        let (DecisionName name) = decision.Name
        printfn $"{name}\tValue: %f{value}"
    )
| _ -> printfn $"Unable to solve. Error: %A{result}"