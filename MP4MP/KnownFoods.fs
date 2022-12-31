module MP4MP.KnownFoods

open MP4MP.Measures
open MP4MP.Nutrients

[<CLIMutable>]
type RawServingSizeData =
  { Volume: string
    Mass: string
    Calories: int }

[<CLIMutable>]
type RawInputFood =
  { Name: string
    CategoryName: string
    ServingSizeData: RawServingSizeData
    FoodNutrition: string array }

type NutrientInFood =
    { Nutrient: Nutrient
      Amount: float<Mass.mg> option }
    
type ServingSize =
    { MassInGrams: double
      Calories: int }

type Food =
    { Name: string
      CategoryName: string
      ServingSizeData: ServingSize
      FoodNutrition: NutrientInFood list }
    
let parseFood (rawFood: RawInputFood) =
    let nutrientsInFood =
        rawFood.FoodNutrition
        |> Array.choose(fun s ->
            let parts = s.Split " "
            
            if parts.Length < 3 then
                //printfn $"Couldn't parse {s}"
                None
            else
                { Nutrient = parts.[2..] |> String.concat " " |> Nutrient.parseNutrient
                  Amount = Mass.parse parts.[1] (float parts.[0]) }
                |> Some
        )
        |> Array.toList

    let servingSize: ServingSize =
        { MassInGrams = rawFood.ServingSizeData.Mass.Split(" ").[0] |> double
          Calories = rawFood.ServingSizeData.Calories }
             
    { Name = rawFood.Name
      CategoryName = rawFood.CategoryName
      ServingSizeData = servingSize
      FoodNutrition = nutrientsInFood }
    
    
    
let fileLocation = "/home/stachu/healthy-foods.json"

open System.IO
open System.Text.Json

let foods =
    File.ReadAllText fileLocation
    |> JsonSerializer.Deserialize<RawInputFood array>
    |> Seq.map parseFood