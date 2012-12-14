open System
open System.IO
open System.Collections.Generic
open System.Linq
open System.Text
open Microsoft.VisualBasic.FileIO
open Microsoft.FSharp.Control.CommonExtensions

type Binary = 
    | Equal
    | Gt
    | Or
and Expr =
    | Bin    of Binary * Expr * Expr
    | NotT   of Expr
    | Ref    of String
    | Val    of String

type Lexems =
    | Open
    | Close
    | Not
    | Oper        of Binary
    | Symbol      of String
    | StringConst of String
    | Reference   of String

let is_symbol_char c = Char.IsLetterOrDigit c || Char.IsSymbol      c
let is_separator   c = Char.IsSeparator     c || Char.IsPunctuation c
let ltos           l = new String(l |> Array.ofList)

let rec eat_string chars acc =
    match chars with
        | []          -> (acc, [])
        | '"' :: tail -> (List.rev acc, tail.Tail)
        | x   :: tail ->  eat_string tail (x::acc)

and eat_symbol chars acc =
    match chars with
        | []                                 -> (acc, [])
        | x   :: tail when is_separator x    -> (List.rev acc, chars)
        | x   :: tail                        ->  eat_symbol tail (x::acc)

and get_string lmxs get_lexem =
    let (str, tl) = eat_string lmxs []
    get_lexem(ltos str) :: (lexems tl)

and get_symbol lmxs =
    let (str, tl) = eat_symbol lmxs []
    Symbol(ltos str) :: (lexems tl)

and lexems code  =
    match code with
        | '(' :: tail      -> Open        :: (lexems tail)
        | '!' :: tail      -> Not         :: (lexems tail)
        | ')' :: tail      -> Close       :: (lexems tail)
        | '|' :: tail      -> Oper(Or)    :: (lexems tail)
        | '>' :: tail      -> Oper(Gt)    :: (lexems tail)
        | '=' :: tail      -> Oper(Equal) :: (lexems tail)
        | '#' :: tail      -> if (tail.Head = '"') then 
                                  let (str, tl) = eat_string tail.Tail []
                                  Reference(ltos str) :: (lexems tl)
                              else
                                  let (str, tl) = eat_symbol tail []
                                  Reference(ltos str) :: (lexems tl)
        | '"' :: tail      -> get_string tail <| fun s -> StringConst(s)
        | x   :: tail when 
          is_symbol_char x -> get_symbol code
        | _   :: tail      -> lexems tail
        | []               -> []

let rec eat_expr e = 
    match e with
        | Oper(bin)::tail                                 -> let (op1, op2lexems) = eat_expr tail
                                                             let (op2, tl)        = eat_expr op2lexems
                                                             (Bin(bin, op1, op2), tl)
        | Not::tail                                       -> let (e, t) = eat_expr tail 
                                                             (NotT e, t)
        | Reference(str)::tail                            -> (Ref str, tail)
        | StringConst(str)::tail                          -> (Val str, tail)
        | Symbol(str)::tail                               -> (Val str, tail)
        | Open :: tail                                    -> match eat_expr tail with
                                                                | (e, Close::tail) -> (e, tail)
                                                                | (_, [])          -> raise(Exception ") expected")
                                                                | (_, _)           -> raise(Exception "Wrong code line")
        | _                                               -> raise(Exception "Unsupported code line")

let header_dict header_cells =
    let mutable i = 0
    let dict = Dictionary()
    for cell in header_cells do
        if not <| dict.ContainsKey cell  then dict.Add(cell, i)
        i <- i + 1
    dict

let rec check_refs expr (header:Dictionary<string, int>) =
    match expr with
        | Bin(m, l, r) -> check_refs l  header; check_refs r header
        | Ref(r)       -> if not(header.ContainsKey(r)) then raise <| Exception(sprintf "Column name '%s' not found in headers" r)
        | _            -> ()

let rec compile expr (headers:Dictionary<string, int>) =
    let get_ref_val s (cs:string[]) = cs.[headers.[s]]
    let rec compile_bin = function
        | Bin(Equal, Val x, Val y)  -> fun cs -> x = y
        | Bin(Gt,    Val x, Val y)  -> fun cs -> x > y
        | Bin(Equal, Ref r, Val y)  -> fun cs -> get_ref_val r cs = y
        | Bin(Gt,    Ref r, Val y)  -> fun cs -> get_ref_val r cs > y
        | Bin(Equal, Val x, Ref r)  -> fun cs -> x = get_ref_val r cs
        | Bin(Gt,    Val x, Ref r)  -> fun cs -> x > get_ref_val r cs
        | Bin(op, x, y)             -> let fx = compile_bin x
                                       let fy = compile_bin y
                                       match (op, x, y) with
                                           | (Equal, x, y) -> fun cs -> fx cs =  fy cs
                                           | (Or, x, y)    -> fun cs -> fx cs || fy cs
                                           | (Gt, x, y)    -> fun cs -> fx cs >  fy cs
        | NotT(exp)                 -> let f = compile_bin exp
                                       fun cs -> not(f cs)
        | _                         -> raise(Exception "Expression compilation error. Unsupported expression")
    compile_bin expr

let parse str = eat_expr <| lexems str

// получить вычисляемое выражение
// проверить ссылки
// получить функцию подсчета фильтра
// применить функцию фильтрации к каждой строке
let filename  = Environment.GetCommandLineArgs().[1]
let code_line = Environment.GetCommandLineArgs().[2]

let filter = 
    use tfp = new TextFieldParser(filename)
    tfp.SetDelimiters([| "\t" |])

    let headers = header_dict(tfp.ReadFields())
    let (exp, tail)     = parse [for c in code_line -> c]

    if (not tail.IsEmpty) then
        raise(Exception "Wrong code line")
    
    check_refs exp headers

    let mutable count  = 0
    let mutable filter = compile exp headers
    let mutable y      = 0

    while (tfp.EndOfData = false) do
        let cells = tfp.ReadFields()
        if (filter cells) then
            y <- y + 1
            Console.WriteLine "Y"
        else
            Console.WriteLine "N"
        count <- count + 1
