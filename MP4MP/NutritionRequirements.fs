module MP4MP.NutrientRequirements

open MP4MP.Measures
open MP4MP.Nutrients

type GenderStatus =
    | Man
    | Woman
    | Woman_Pregnant
    | Woman_Lactating

let nutritionRequirements (nutrient: Nutrient) (age: int) (genderStatus: GenderStatus) (weightInKg: float) =
    if age <> 30 || genderStatus <> Man then
        failwith "Profile not yet supported..."

    match nutrient with
    | Vitamin Vitamin_A -> None
    | Vitamin Vitamin_C -> 90.0<Mass.mg> |> Some
    | Vitamin Vitamin_E -> 15.0<Mass.mg> |> Some
    | Vitamin Vitamin_K -> 120.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Vitamin B1_Thiamin -> 1.2<Mass.mg> |> Some
    | Vitamin B2_Riboflavin -> 1.3<Mass.mg> |> Some
    | Vitamin B3_Niacin -> 16.0<Mass.mg> |> Some
    | Vitamin B5_Pantothenic_Acid -> 5.0<Mass.mg> |> Some
    | Vitamin B6_Pyridoxine -> 1.3<Mass.mg> |> Some
    | Vitamin B7_Biotin -> 30.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Vitamin Folate -> 400.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Mineral Calcium -> 1300.0<Mass.mg> |> Some
    | Mineral Chromium -> 35.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Mineral Sodium -> None
    | Mineral Potassium -> 4.7<Mass.mg> |> Some
    | Mineral Zinc -> 11.0<Mass.mg> |> Some
    | Mineral Copper -> 900.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Mineral Iodine -> 150.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Mineral Iron -> 8.0<Mass.mg> |> Some
    | Mineral Magnesium -> 400.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Mineral Molybdenum -> None
    | Mineral Manganese -> 2.3<Mass.mg> |> Some
    | Mineral Selenium -> 55.0<Mass.mcg> |> Mass.mcg.toMilligrams |> Some
    | Mineral Phosphorus -> None
    | Fat Polyunsaturated_Fat -> None
    | Fat Monounsaturated_Fat -> None
    | Fat Saturated_Fat -> None
    | Dietary_Fiber -> 38.0<Mass.g> |> Mass.g.toMilligrams |> Some
    | Omega3FattyAcids -> None
    | Protein -> 56.0<Mass.g> |> Mass.g.toMilligrams |> Some
    | Tryptophan -> 3.5<Mass.mg> * weightInKg |> Some

// todo: dangers/limits