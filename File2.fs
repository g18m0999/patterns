module Patterns
#endif

type Cell =
| Black
| White 
| Unknown

type Pattern = 
| BlackP | WhiteP | UnknownP
|OneOrMore of Pattern
|Exactly of int * Pattern | FewerThan of int * Pattern
|Either of Pattern * Pattern
|Anything | EndOfCells
|Sequence of Pattern List
|ZeroOrMore of Pattern


let charToCell c =
  match c with 
  | 'b'| 'B' -> Black
  | 'w'| 'W'-> White
  | _ -> Unknown

let cellToChar cell =
  match cell with 
  | Black -> "b"
  | White -> "w"
  | Unknown -> "." 

let toCells (str:string) =
  let rec perStr index cellList = 
    match index < str.Length with
    | true -> 
            let cell = charToCell str.[index]
            perStr (index+1) (cell::cellList)
    | false -> cellList
  List.rev (perStr 0 [])

let fromCells (cells: Cell List) = 
  let x = List.map(cellToChar) cells
  List.fold (fun state e -> state + e) "" x

let ptnToCell (ptn: Pattern) (cell:Cell) =
    match ptn with
    | BlackP -> Black = cell
    | WhiteP -> White = cell
    | UnknownP -> Unknown = cell
    | Anything -> true
    | _ -> failwith "Pattern not recognized"   

let patternMatch (ptn:Pattern) (lst:Cell list) : Option<Cell List> =
  let check (ptn:Pattern) (cells:Cell list) =
    match cells with 
    | [] -> None
    | x::_ -> 
      match ptnToCell ptn x with
      | true -> Some x
      | _ -> None

  let rec Adder (p:Pattern) (cellList:Cell list) (rtn:Cell list) =
    match p with
    | BlackP | WhiteP | UnknownP  -> 
        let ans = check ptn cellList
        match ans with
        | Some a -> Some (rtn @ (a::[]), cellList.Tail)
        | _ -> None 
        
    |ZeroOrMore a -> 
        match lst.Length > 0 with
        | true ->
            let ans = adder p cellList rtn
            match ans with
            | None -> Some (rtn, cellList)
            | Some (ptnCells, rest) -> helper p rest ptnCells
        | false -> Some (rtn, cellList)

    | OneOrMore a ->
      match cellList.Length > 0 with
      | true ->
        let ans = Adder (ZeroOrMore a) cellList rtn
        match ans with
        | Some (ptnCells, rest) -> 
            match (ptnCells.Length > 0 ) with
            | false -> None
            | true -> Some (ptnCells, rest)
        | _ -> None
      | false -> None

    | Exactly (count, a) ->
      match (count > 0) with
      | true ->
        match (Adder a cellList rtn) with
        | Some (ptnCells, rest) ->  Adder (Exactly (count-1, a)) rest ptnCells
        | _ -> None
      | false -> Some (rtn, cellList)

    | FewerThan (count, a) ->
      match count > 0 with
      | true ->
        let ans = Adder (ZeroOrMore a) cellList []
        match ans with
        | Some (ptnCells, rest) -> 
          match (ptnCells.Length > count) with
          | false -> Some (rtn @ ptnCells, rest)
          | true -> 
            let alist = List.take (count-1) ptnCells
            let rem = cellList.[alst.Length ..]
            Some (rtn @ alist, rem)
        | _ -> failwith "FewerThan only returns 'None' when (count < 0)"
      | false -> None

    | Sequence ptns ->
      match ptns with
        | h::t -> 
          let ans = Adder h cellList rtn
          match ans with
          | None -> ans
          | Some (ptnResult, rest) -> helper (Sequence t) rest ptnResult
        | [] -> Some (rtnt, cellList)

    | Anything ->
      match cellList with
      | [] -> None
      | h::t -> 
        Some (rtn @ (h::[]), t)
    
    |EndOfCells -> match lst with |[] -> Some [] | _ -> None  
  
    match (Adder ptn lst []) with
    | Some (x, _) -> Some x
    | _ -> None

let find ptn cells = 
  let rec finder (lst:Cell List) (i:int) =
    let result = patternMatch ptn lst
    match result with
    | None ->
      match lst.Length = 0 with
      | true -> None
      | false ->
        finder lst.Tail (i + 1)
    | Some x -> Some (x, i) 
  finder cells 0

   
let map (func:(Cell List->Cell List)) (ptn:Pattern) (cells:Cell List) = 
  let rec mapper (lst:Cell List) (alist:Cell List) =
    match lst.IsEmpty with
    | true -> alist
    | false ->
      match (find ptn lst) with
      | None -> alist @ lst
      | Some (x, i) ->
        let (y, trans) = (alist.[.. (i - 1)], func x
        let Result = alist @ y @ trans
        let startIndex = 
          match i + x.Length with
              | 0 -> i + 1
              | v -> v
        mapper lst.[startIndex..] Result
     mapper cells []


