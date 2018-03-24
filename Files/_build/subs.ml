open Ast

type subst = (string,Ast.texpr) Hashtbl.t

let create () = Hashtbl.create 123456;;

let extend table s e = Hashtbl.add table s e

let remove table s = Hashtbl.remove table s

let lookup table s = Hashtbl.find_opt table s
                                    
let apply_to_texpr table t = t

let apply_to_expr table e = e

let apply_to_env table table = () 
    
let rec list_to_string x: string = 
	if List.length x == 0 then "" else
	let (k, v) = List.hd x in 
		"(" ^ k ^ ", " ^ string_of_texpr v ^ ") " ^ list_to_string (List.tl x)

let string_of_subs table = 
	let list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table[] in
	"[" ^ (list_to_string list) ^ "]"
    
let domain table = ["asdf"]

let join tables = List.hd tables
    
