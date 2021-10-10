module DataEntry.RawInputTypes

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