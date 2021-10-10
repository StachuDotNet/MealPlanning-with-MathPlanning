module DataEntry.NutrientRequirements

open DataEntry.Measures
open DataEntry.Nutrients

type GenderStatus =
    | Man
    | Woman
    | Woman_Pregnant
    | Woman_Lactating

let nutritionRequirements (nutrient: Nutrient) = // (age: decimal) (genderStatus: GenderStatus) =
//    if age <> 29M || genderStatus <> Man then
//        failwith "Profile not yet supported..."

    match nutrient with
    | Vitamin Vitamin_A -> None
    | Vitamin Vitamin_C -> None
    | Vitamin Vitamin_E -> None
    | Vitamin Vitamin_K -> None
    | Vitamin B1_Thiamin -> None
    | Vitamin B2_Riboflavin -> None
    | Vitamin B3_Niacin -> None
    | Vitamin B5_Pantothenic_Acid -> None
    | Vitamin B6_Pyridoxine -> None
    | Vitamin Folate -> None
    | Mineral Calcium -> 1000.0<Mass.mg> |> Some
    | Mineral Chromium -> None
    | Mineral Sodium -> None
    | Mineral Potassium -> None
    | Mineral Zinc -> None
    | Mineral Copper -> 900.0<Mass.mg> |> Some
    | Mineral Iodine -> None
    | Mineral Iron -> 8.0<Mass.mg> |> Some
    | Mineral Magnesium -> None
    | Mineral Molybdenum -> None
    | Mineral Manganese -> None
    | Mineral Selenium -> None
    | Mineral Phosphorus -> None
    | Fat Polyunsaturated_Fat -> None
    | Fat Monounsaturated_Fat -> None
    | Fat Saturated_Fat -> None
    | Dietary_Fiber -> 38.0<Mass.g> |> Mass.g.toMilligrams |> Some
    | Omega3FattyAcids -> None
    | Protein -> 56.0<Mass.g> |> Mass.g.toMilligrams |> Some
    | Tryptophan -> None

// todo: dangers/limits