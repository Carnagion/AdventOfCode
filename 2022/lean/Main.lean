import Days.One

open IO.FS

def getPuzzleInput (day : Int) : IO String :=
  readFile s!"Inputs/{day}.txt"

def getSolution (day part : Int) (input : String) : String :=
  match day with
    | 1 => One.solve part input
    | _ => "Invalid day"

def main (args : List String) : IO Unit :=
  match args with
    | [day, part] => do
      let day := day.toInt!
      let input <- getPuzzleInput day
      println! (getSolution day part.toInt! input)
    | [day] => do
      let day := day.toInt!
      let input <- getPuzzleInput day
      println! (getSolution day 1 input ++ "\n" ++ getSolution day 2 input)
    | _ => println! "Provide day and part"