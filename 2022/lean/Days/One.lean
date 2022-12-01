open List Int String

namespace One

def sum : List Int → Int :=
  foldl Int.add 0

def ins : Int → List Int → List Int
  | int, [] => [int]
  | int, (hd :: tl) => if int ≤ hd then int :: hd :: tl else hd :: ins int tl

def sort : List Int → List Int
  | [] => []
  | (hd :: tl) => ins hd (sort tl)

def splitCalories : String → List (List String) :=
  map (fun str => splitOn str "\n") ∘ (fun str => splitOn str "\n\n")

def filterCalories : List (List String) → List (List String) :=
  filter (not ∘ isEmpty) ∘ map (filter (not ∘ isEmpty)) 

def parseCalories : List (List String) → List Int :=
  map (sum ∘ map toInt!)

def solve (part : Int) (input : String) : String :=
  match part with
    | 1 => Id.run do
      let sol := (head! ∘ reverse ∘ sort ∘ parseCalories ∘ filterCalories ∘ splitCalories) input
      s! "{sol}"
    | 2 => Id.run do
      let sol := (sum ∘ take 3 ∘ reverse ∘ sort ∘ parseCalories ∘ filterCalories ∘ splitCalories) input
      s! "{sol}"
    | _ => "Invalid part"

#eval sort [3, 2, 1]