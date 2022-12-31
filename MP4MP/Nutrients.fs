module MP4MP.Nutrients

type Vitamin = 
    | Vitamin_A
    | Vitamin_C
    | Vitamin_E
    | Vitamin_K
    | B1_Thiamin
    | B2_Riboflavin
    | B3_Niacin
    | B5_Pantothenic_Acid
    | B6_Pyridoxine
    | Folate
//with this.ToString() =
//    match this with
//    | Vitamin_A -> "Vitamin A"
//    | Vitamin_C -> "Vitamin C"
//    | Vitamin_E -> "Vitamin E"
//    | Vitamin_K -> "Vitamin K"
//    | B1_Thiamin -> "B1 Thiamin"
//    | B2_Riboflavin -> "B2 Riboflavin"
//    | B3_Niacin -> "B3 Niacin"
//    | B5_Pantothenic_Acid -> "B5 Pantothenic Acid"
//    | B6_Pyridoxine -> "B6 Pyridoxine"
//    | Folate -> "Folate"
    
module Vitamin =
    let getName vitamin =
        match vitamin with
        | Vitamin_A -> "Vitamin A"
        | Vitamin_C -> "Vitamin C"
        | Vitamin_E -> "Vitamin E"
        | Vitamin_K -> "Vitamin K"
        | B1_Thiamin -> "B1 Thiamin"
        | B2_Riboflavin -> "B2 Riboflavin"
        | B3_Niacin -> "B3 Niacin"
        | B5_Pantothenic_Acid -> "B5 Pantothenic Acid"
        | B6_Pyridoxine -> "B6 Pyridoxine"
        | Folate -> "Folate"
type Mineral =
    | Calcium
    | Chromium
    | Sodium
    | Potassium
    | Zinc
    | Copper
    | Iodine
    | Iron
    | Magnesium
    | Phosphorus
    | Molybdenum
    | Manganese
    | Selenium
module Mineral =
    let getName mineral =
        match mineral with
        | Calcium -> "Calcium"
        | Chromium -> "Chromium"
        | Sodium -> "Sodium"
        | Potassium -> "Potassium"
        | Zinc -> "Zinc"
        | Copper -> "Copper"
        | Iodine -> "Iodine"
        | Iron -> "Iron"
        | Magnesium -> "Magnesium"
        | Molybdenum -> "Molybdenum"
        | Manganese -> "Manganese"
        | Selenium -> "Selenium"
        | Phosphorus -> "Phosphorus"

type Fat =
    | Monounsaturated_Fat
    | Polyunsaturated_Fat
    | Saturated_Fat
module Fat =
    let getName fat =
        match fat with
        | Polyunsaturated_Fat -> "Polyunsaturated Fat"
        | Monounsaturated_Fat -> "Monounsaturated Fat"
        | Saturated_Fat -> "Saturated Fat"

type Nutrient =
    | Vitamin of Vitamin
    | Mineral of Mineral
    | Fat of Fat
    | Protein
    | Dietary_Fiber
    | Omega3FattyAcids
    | Tryptophan
    //| Calories (?)
    //| Water (?)
module Nutrient = 
    let getName (nutrient: Nutrient) =
        match nutrient with
        | Vitamin v -> Vitamin.getName v
        | Mineral m -> Mineral.getName m
        | Fat f -> Fat.getName f
        | Dietary_Fiber -> "Dietary Fiber"
        | Omega3FattyAcids -> "Omega-3 Fatty Acids"
        | Protein -> "Protein"
        | Tryptophan -> "Tryptophan" 

    let parseNutrient nutrientStr =
        match nutrientStr with
        | "Vitamin A" -> Vitamin Vitamin_A
        | "Vitamin C" -> Vitamin Vitamin_C
        | "Vitamin E" -> Vitamin Vitamin_E
        | "Vitamin K" -> Vitamin Vitamin_K
        | "B1 Thiamin" -> Vitamin B1_Thiamin
        | "B2 Riboflavin" -> Vitamin B2_Riboflavin
        | "B3 Niacin" -> Vitamin B3_Niacin
        | "B5 Pantothenic Acid" -> Vitamin B5_Pantothenic_Acid
        | "B6 Pyridoxine" -> Vitamin B6_Pyridoxine
        | "Folate" -> Vitamin Folate
        | "Calcium" -> Mineral Calcium
        | "Chromium" -> Mineral Chromium
        | "Sodium" -> Mineral Sodium
        | "Potassium" -> Mineral Potassium
        | "Zinc" -> Mineral Zinc
        | "Copper" -> Mineral Copper
        | "Iodine" -> Mineral Iodine
        | "Iron" -> Mineral Iron
        | "Magnesium" -> Mineral Magnesium
        | "Molybdenum" -> Mineral Molybdenum
        | "Manganese" -> Mineral Manganese
        | "Selenium" -> Mineral Selenium
        | "Phosphorus" -> Mineral Phosphorus
        | "Polyunsaturated Fat" -> Fat Polyunsaturated_Fat
        | "Monounsaturated Fat" -> Fat Monounsaturated_Fat
        | "Saturated Fat" -> Fat Saturated_Fat
        | "Dietary Fiber" -> Dietary_Fiber
        | "Omega-3 Fatty Acids" -> Omega3FattyAcids
        | "Protein" -> Protein
        | "Tryptophan"  -> Tryptophan
        | other -> failwith $"Couldn't parse {other}"


let allNutrients =
  [ Vitamin Vitamin_A
    Vitamin Vitamin_C
    Vitamin Vitamin_E
    Vitamin Vitamin_K
    Vitamin B1_Thiamin
    Vitamin B2_Riboflavin
    Vitamin B3_Niacin
    Vitamin B5_Pantothenic_Acid
    Vitamin B6_Pyridoxine
    Vitamin Folate
    Mineral Calcium
    Mineral Chromium
    Mineral Sodium
    Mineral Potassium
    Mineral Zinc
    Mineral Copper
    Mineral Iodine
    Mineral Iron
    Mineral Magnesium
    Mineral Molybdenum
    Mineral Manganese
    Mineral Selenium
    Mineral Phosphorus
    Fat Polyunsaturated_Fat
    Fat Monounsaturated_Fat
    Fat Saturated_Fat
    Dietary_Fiber
    Omega3FattyAcids
    Protein
    Tryptophan ]