open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr


let getSubst (t:typing_judgement): subst = 
  let (a, _, _) = t in a

let gettexpr (t:typing_judgement): texpr = 
  let (_, _, c) = t in c

let getTypingJudgement (t:(int*typing_judgement) error): typing_judgement =
  match t with
  | OK (a, b) -> b
  | Error a -> failwith a

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit -> OK (n, (create (), Unit, UnitType))
  | Var e -> OK (n, (create (), Var e, VarType e))
  | Int e -> OK (n, (create (), Int e, IntType))
  | Add (e, f) -> OK (n, 
    (join [getContext e n; getContext f n],
    Add (e, f), IntType))

  | Sub (e, f) -> OK (n, 
    (join [getContext e n; getContext f n],
    Sub (e, f), IntType))

  | Mul (e, f) -> OK (n, 
    (join [getContext e n; getContext f n],
    Mul (e, f), IntType))

  | Div (e, f) -> OK (n, 
    (join [getContext e n; getContext f n],
    Div (e, f), IntType))

  | Let (e, f, g) -> 
    let typeenv = getContext g n in
      extend typeenv e (getTexpr f n) ;
      OK (n,
      (typeenv,
       Let (e, f, g), getTexpr g n))

  | IsZero e -> OK (n,
    (getContext e n,
    IsZero e, BoolType))

  | ITE -> failwith "Later"

  | Proc -> failwith "Later"

  | ProcUntyped -> failwith "Later"

  | App -> failwith failwith "Later"

  | Letrec -> failwith "Later"

  | LetrecUntyped -> failwith "Later"

  | Set -> failwith "Later"

  | BeginEnd -> failwith "Later"

  | NewRef -> failwith "Later"

  | DeRef -> failwith "Later"

  | SetRef -> failwith "Later"

  | _ -> failwith "infer': undefined"

and getContext (e:expr) (n:int): subst = 
  getSubst (getTypingJudgement (infer' e n))

and getTexpr (e:expr) (n:int): texpr = 
  gettexpr (getTypingJudgement (infer' e n))


let string_of_typing_judgement e = 
  let (a, b, c) = e in 
   string_of_subs a ^ "  " ^ string_of_expr b ^ "  " ^ string_of_texpr c
    


let infer_type (AProg e) =
  match infer' e 0 with
  | OK (_, tj) -> string_of_typing_judgement tj
  | Error s -> "Error! "^   s



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let inf (e:string) : string =
  e |> parse |> infer_type 

let test (n:int) : string =
  Examples.expr n |> parse |> infer_type 
