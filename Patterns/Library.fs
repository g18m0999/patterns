#if !INTERACTIVE
module Patterns
#endif
#if !INTERACTIVE
#if !INTERACTIVE
module Patterns
open System

type Cell =
| Black
| White 
| Yellow

type Pattern = 
| BlackP
| WhiteP 
| UnknownP
| OneOrMore of Pattern
| Exactly of int * Pattern
| FewerThan of int * Pattern
| Either of Pattern
| Anything
| EndOfCells
| Sequence of Cell List
| ZeroOrMore of Pattern

let _charToCell ch =
  match ch with 
  | 'b'| 'B' -> Black
  | 'w'| 'W'-> White
  | _ -> Yellow

let _cellToChar cell =
  match cell with 
  | Black ->  "b"
  | White ->  "w"
  | Yellow ->  "."

let toCells (str:string) =
  let rec helper index cellList = 
    match index < str.Length with
    | false -> cellList
    | true -> 
      let cell = _charToCell str.[index]
      helper (index+1) cellList@[cell]
  helper 0 []

let fromCells (cells: Cell List) = 
    let a = List.rev (List.map _cellToChar cells)
    List.fold (fun state elem-> state+elem) "" a


let p2Cell (ptn: Pattern) =
    match ptn with
    | BlackP -> Black
    | WhiteP -> White
    | UnknownP -> Yellow
    | _ -> failwith "Not implemented"   

let matchFn num p cells =
    let out = List.take num cells
    let fn = (fun a -> a<> (p2Cell p))  //returns everything not as speficies
    let m = List.filter fn out
    match (List.length m) > 0 with
    | true -> None
    | _ -> Some out

let patternMatch1 p d = 
    match p with 
         |ZeroOrMore a -> helper a d r
         |OneOrMore a -> helper a d r
         |Exactly (n, p) -> matchFn n p d  
         |FewerThan (n, p) -> failwith "Not implemented"//cwnt (n-1) p d 
         |Sequence lst -> failwith "Not implemented"
         |Either a -> failwith "Not implemented"
         |Anything -> failwith "Not implemented"
         |EndOfCells -> failwith "Not implemented"
         | _ -> failwith "Not implemented"

let patternMatch (p: Pattern) (d: Cell list): Option<Cell list>=
    let rec helper (p: Pattern) (d: Cell list) (r: Cell list) =
         match p with
         |BlackP -> Some ( r@[Black] ) // Some 'b'
         |WhiteP -> Some ( r@[White] ) // some 'w'
         |UnknownP -> Some ( r@[Yellow] ) // some '.'

    helper p d []
(*

let cwnt n p d =
    match (List.filter(p) d).Length < n with 
    |true -> patternMatch p d 
    | _ -> None  



*)
 

 // toCells "bbw_B!W"

// toCells "bbw_B!W"
//                                                                                                                                                                                        |> fromCells

#endif

let find pattern cells = failwith "Not implemented"

let map func pattern cells = failwith "Not implemented"
