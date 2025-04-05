open Ast

(* This takes care of if elseif and else chain *)
let rec build_if_chain cond then_block elseif_list else_opt =
  (* First we check if there is a list of Else Ifs *)
  match elseif_list with
  (* If the list is empty we go to the first option *)
  | [] ->
    (* 
      Here we return the Sif with the condition and block for the initial if.
      We also check if there is an else block, by checking the Some b.
      If there is we return it or else we just return an empty block.
    *)
      Sif(cond, then_block, else_opt)
  (* 
    If there is an Else If we come to this match.
    Here we take the first element of the list consiting of (expr, block),
    and we leave the rest of the list in the rest variable. 
    We then call the build_if_chain function recursively with expr as the cond,
    the b as the then_block and the rest as the elseif_list and the else_opt as
    the else_opt.
    This will continue until the list is empty and we return the Sif with the
    condition and block for the initial if.
  *)
  | (e, b) :: rest ->
      Selseif(cond, then_block, Some (build_if_chain e b rest else_opt))