#if !INTERACTIVE
module Patterns
#endif
#if !INTERACTIVE
#if !INTERACTIVE
module Patterns 

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

type Func =
  |MakeUnknown |MakeWhite |MakeBlack 
  |Delete 
  |Double


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
  let rec helper index cellList = 
    match index < str.Length with
    | true -> 
            let cell = charToCell str.[index]
            helper (index+1) (cell::cellList)
    | false -> cellList
  List.rev (helper 0 [])

let fromCells (cells: Cell List) = 
  let x = List.map(cellToChar) cells
  List.fold (fun state e -> state + e) "" x

let ToCell (ptn: Pattern) =
    match ptn with
    | BlackP -> Black
    | WhiteP -> White
    | UnknownP -> Unknown
    | _ -> failwith "Pattern not recognized"   



let matchCell (ptn: Pattern) (lst: Cell list) =
    //matches a black/white/unknown cells with given pattern 
    match lst.Length > 0 with 
    |true ->
        let x = lst.Head            
        let y = ToCell ptn
        match x = y with
            |true -> Some [x]
            |false -> None 
    |false -> None 

let rec zeroOrMore (ptn: Pattern) (lst: Cell list) (rtn:Cell List) = 
//matches zero or more patterns  
    match lst.Length > 0 with
    |true -> 
      let x = lst.Head 
      let y = ToCell ptn 
      match x = y with 
      | false -> Some rtn
      | true -> zeroOrMore ptn (lst.Tail) (rtn@[x])
    |false -> Some rtn
   
let rec oneOrMore (ptn: Pattern) (lst: Cell list) (rtn:Cell List) = 
//matches one or more patterns
    match lst.Length > 0 with
    |true -> 
      let x = lst.Head 
      let y = ToCell ptn
      match rtn.Length > 0 with
      |true ->
         match x = y with
         | true -> zeroOrMore ptn (lst.Tail) (rtn@[x])
         | false -> Some rtn
      |false ->
          match x = y with 
           | false -> None 
           | true -> zeroOrMore ptn (lst.Tail) (rtn@[x])
    |false -> None 

let exact (n:int) (ptn: Pattern) (lst: Cell list) (rtn:Cell List) =
 //matches exactly n instances of a pattern
    let out = zeroOrMore ptn lst rtn
    match out with
    | None -> Some []
    | Some ans ->
        match ans.Length = n with
        |true -> Some (List.take n ans)
        |false -> None

let fewerThan (n:int) (ptn: Pattern) (lst: Cell list) (rtn:Cell List) =
//matches fewer than n instances of a pattern
    let out = zeroOrMore ptn lst rtn
    match n > 0 with
     |true ->
            match out with
            | None -> Some []
            | Some ans ->
                match ans.Length > n with
                |true -> Some (List.take n ans)
                |false -> None
     |false -> None 

let seqMatch (plst: Pattern list) (lst: Cell list) (output: Cell List) =
//matches a sequence of patterns
    let cList = List.map (fun ptn -> ToCell ptn) plst
    let boolList = List.mapi (fun index entry -> entry=lst.[index]) cList 
    let result= List.fold (fun state flagEntry -> state&&flagEntry) true boolList
    match result with
    | true -> Some cList
    | false -> None

let orMatch (ptn1: Pattern) (ptn2: Pattern) (lst: Cell List) =
//matches either of two patterns
    let x = matchCell ptn1 lst
    let y = matchCell ptn2 lst
    match x , y with
    | Some p, Some q -> Some p
    | None, None -> None
    | Some ans, None | None, Some ans -> Some ans

let anyMatch (lst: Cell list) =
//matches any cell
    let x = lst.Head 
    Some (x::[]) 

let patternMatch (ptn: Pattern) (lst: Cell list): Option<Cell list>=
    match ptn with
        |BlackP -> matchCell ptn lst 
        |WhiteP -> matchCell ptn lst
        |UnknownP -> matchCell ptn lst
        |ZeroOrMore a -> zeroOrMore a lst []
        |OneOrMore a -> oneOrMore a lst []
        |Exactly (n, p) ->  exact n p lst [] 
        |FewerThan (n, p) -> fewerThan n p lst []
        |Sequence ptns -> seqMatch ptns lst []
        |Either (a,m) -> orMatch a m lst
        |Anything  -> Some (lst.Head::[]) 
        |EndOfCells -> match lst with |[] -> Some [] | _ -> None  
        | _ -> failwith "Not implemented"
(*
let Find (ptn: Pattern) (lst: Cell list): Option<Cell list> = 
    let x = patternMatch ptn lst
    let y = List.findIndex(patternMatch ptn lst ) x
    match (x, y) with 
    | :? (Cell list*int) -> Some (x, y)   
    |_ -> None 
    *)

let Map (func:Func) (ptn:Pattern) (lst:Cell list) =
     match func with 
     |MakeUnknown -> lst.Head = Unknown >> lst
     |MakeWhite -> lst.Head = White >> lst
     |MakeBlack -> lst.Head = Black >> lst
     |Delete -> List.filter(fun x -> not((ToCell ptn) = x )lst
     |Double -> let x = (ToCell ptn) >> x::lst
     | _ -> failwith "Not implemented"

let Map (func:Cell list->Cell list) (ptn:Pattern) (lst:Cell list) =
      
    let z = patternMatch ptn lst
    let fnlst = List.filter (fun x -> not((ToCell ptn) = x )) lst
    let x = 0
    let a = func z.Value 
    let num = lst.Length
    
    let rec maker lst alst = 
        match lst with
        |t::rest  -> maker rest (alst@[t+1])   
        |[] -> []

    match func with 
        |makeUnknown -> maker lst []
        |makeWhite -> 
        |Delete -> fnlst
        |Double -> z::lst
        | _ -> failwith "Not implemented"

    
(*
let Find (ptn: Pattern) (lst: Cell list): Option<Cell list> = 
    let x = patternMatch p d
    let y = List.find() 
     (*| -> some (x, y)
     | -> none*) 

let map func pattern cells = failwith "Not implemented"
*)
