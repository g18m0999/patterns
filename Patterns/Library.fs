#if !INTERACTIVE
module Patterns
#endif
#if !INTERACTIVE
module Patterns
open System

type Cell =
| Pos
| Neg 
| Unk

type Pattern = 
| BlackP
| WhiteP 
| UnknownP
|OneOrMore of Pattern
|Exactly of int * Pattern
|FewerThan of int * Pattern
|Either of Pattern
|Anything
|EndOfCells
|Sequence of Cell List
|ZeroOrMore of Pattern

let _charToCell ch =
  match ch with 
  | 'b'| 'B' -> BlackP
  | 'w'| 'W'-> WhiteP
  | _ -> UnknownP

let _cellToChar cell =
  match cell with 
  | Pos -> Some 'b' 
  | Neg -> Some 'w'
  | Unk -> Some '.'
  | _ -> None 

let toCells (str:string) =
  let rec helper index cellList = 
    match index < str.Length with
    | false -> cellList
    | true -> 
      let cell = _charToCell str.[index]
      helper (index+1) cellList@[cell]
  helper 0 []

let fromCells (cells: Cell List) = 
  List.map _cellToChar cells

//let Cwnt n p d =
//    match (List.filter(p) d).Length = n with 
//    |true -> Some //
//    | _ -> None 

let p2Cell (ptn: Pattern) =
    match ptn with
    | BlackP -> Pos
    | WhiteP -> Neg
    | UnknownP -> Unk
    | _ -> failwith "Not implemented"


let exactlyMatch num ptn cells =
    let out = List.take num cells
    let predicate = (fun entry -> entry<> (p2Cell ptn))  //returns everything not as speficies
    let m = List.filter predicate out
    match (List.length m) > 0 with
    | true -> None
    | _ -> Some out

let patternMatch (ptn: Pattern) (d: Cell list): Option<Cell list>=
    match ptn with
     |BlackP -> // Some 'b'
     |WhiteP ->  // some 'w'
     |UnknownP ->  // some '.'
     |ZeroOrMore a -> // patternMatch a d [agreed]
     |OneOrMore a -> // patternMatch a d 
     |Exactly (n, p) -> exactlyMatch n p d  
     |FewerThan (n, p) ->

 // toCells "bbw_B!W"

// toCells "bbw_B!W"
//                                                                                                                                                                                        |> fromCells

#endif

let toCells v = failwith "Not implemented"

let fromCells v = failwith "Not implemented" 

let patternMatch pattern cells = failwith "Not implemented"

let find pattern cells = failwith "Not implemented"

let map func pattern cells = failwith "Not implemented"

let toCells v = failwith "Not implemented"

let fromCells v = failwith "Not implemented" 

let patternMatch pattern cells = failwith "Not implemented"

let find pattern cells = failwith "Not implemented"

let map func pattern cells = failwith "Not implemented"
