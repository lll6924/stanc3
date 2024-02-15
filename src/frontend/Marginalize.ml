open Ast
open Core
open Middle.Operator

let process_operator op =
  match op with
  | Plus | PPlus -> "+"
  | Minus | PMinus -> "-"
  | Times -> "*"
  | Divide ->  "/"
  | IntDivide -> "%%/%%"
  | Modulo -> "%%"
  | LDivide -> "\\"
  | EltTimes -> ".*"
  | EltDivide -> "./"
  | Pow -> "^"
  | EltPow -> ".^"
  | Or -> "||"
  | And -> "&&"
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | PNot -> "!"
  | Transpose -> "'"

let process_identifier id = id.name

let rec process_list_of_variables ids = 
  match ids with
  | id :: rem ->
    (process_identifier id.identifier) ^ (process_list_of_variables rem)
  | [] ->
    ""

let rec process_index i = 
  match i with
  | All -> (":", [])
  | Single { expr = e ; _ } -> 
    let (final, exprs) = process_expression e in
    (final, exprs)
  | Upfrom { expr = e ; _ } -> 
    let (final, exprs) = process_expression e in
    (final ^ ":", exprs)
  | Downfrom { expr = e ; _ } -> 
    let (final, exprs) = process_expression e in
    (":" ^ final, exprs)
  | Between ({ expr = e1 ; _ }, { expr = e2 ; _ }) -> 
    let (final1, exprs1) = process_expression e1 in
    let (final2, exprs2) = process_expression e2 in
    (final1 ^ ":" ^ final2, List.append exprs1 exprs2) 

and process_list_of_indices l =
    match l with
    | id :: rem ->
      let (final1, exprs1) = process_index id in
      let (final2, exprs2) = process_list_of_indices rem in
      (final1 ^ "," ^ final2, List.append exprs1 exprs2)
    | [] -> ("", [])

and process_list_of_expression es =
  match es with
  | e :: l -> 
    let (final1,res1) = process_expression e in
    let (final2,res2) = process_list_of_expression l in
    let res = List.append res1 res2 in 
    let final = List.append final2 [final1] in 
    (final, res)
  | [] -> ([],[])

and process_expression e
  =
  match e with
  | TernaryIf ({expr=e1;_}, {expr=e2;_}, {expr=e3;_}) ->
    let (_,ex1) = process_expression e2 in
    let (_,ex2) = process_expression e1 in
    let (_,ex3) = process_expression e3 in
    let l = List.append (List.append ex1 ex2) ex3 in 
    ("x", l)
  | BinOp ({expr=e1;_}, op, {expr=e2;_}) ->
      let opname = process_operator op in
      let (f1,ex1) = process_expression e1 in
      let (f2,ex2) = process_expression e2 in
      let st1 = "x="^f1 in
      let st2 = "x="^f2 in
      let l = List.append (List.append ex1 ex2) [st1;st2] in
      ("x" ^ opname ^ "x", l)
  | PrefixOp (op, {expr=e;_}) ->
    let opname = process_operator op in
    let (f,ex) = process_expression e in
    let l = List.append ex ["x="^f] in
      (opname ^ "x", l)
  | PostfixOp ({expr=e;_}, op) -> 
    let opname = process_operator op in
    let (f,ex) = process_expression e in
    let l = List.append ex ["x="^f] in
    ("x"^opname, l)
  | Variable id -> (process_identifier id, [])
  | IntNumeral i -> (i, [])
  | RealNumeral r -> (r, [])
  | ImagNumeral z -> (z, [])
  | FunApp (_, id, es) ->
      let funid = process_identifier id in
      let eprs = List.map ~f:(fun (x) -> x.expr) es in
      let (_,exprs) = process_list_of_expression eprs in
      ("function "^funid, exprs)
  | CondDistApp (_, id, es) -> (
      match es with
      | [] ->
          ("",[])
      | [{expr=e;_}] ->
        let funid = process_identifier id in
        let (final, exprs) = process_expression e in
        (funid ^ " " ^final, exprs)
      | {expr=e;_} :: es' ->
        let funid = process_identifier id in
        let eprs = List.map ~f:(fun (x) -> x.expr) es' in
        let (_, conditioned_exprs) = process_list_of_expression eprs in
        let (final, exprs) = process_expression e in
        (funid ^ " " ^final, (List.append conditioned_exprs exprs))
  )
  | GetTarget -> ("",[])
  | ArrayExpr es -> 
      let eprs = List.map ~f:(fun (x) -> x.expr) es in
      let (_, exprs) = process_list_of_expression eprs in
      ("x", exprs)
  | RowVectorExpr es->
      let eprs = List.map ~f:(fun (x) -> x.expr) es in
      let (_, exprs) = process_list_of_expression eprs in
      ("x", exprs)
  | Paren {expr=e;_} -> process_expression e
  | Promotion ({expr=e;_}, _, _) -> process_expression e
  | Indexed ({expr=e;_}, l) -> 
      let (final, _) = process_expression e in
      let (indices, exprs) = process_list_of_indices l in
      (final ^ "["^ indices ^"]", exprs)
  | TupleProjection ({expr = e; _}, i) -> 
    let (final, exprs) = process_expression e in
    (final ^ "_" ^ (string_of_int i), exprs)
  | TupleExpr es ->
      let eprs = List.map ~f:(fun (x) -> x.expr) es in
      let (_, exprs) = process_list_of_expression eprs in
      ("x", exprs)

let rec process_lvalue l 
  =
  match l with
  | LValue lhs -> 
    let {expr; _} = expr_of_lvalue lhs in 
    let (final, _) = process_expression expr in
    final
  | LTuplePack {lvals; _} ->
      let vallist = List.map ~f:(fun (lval) -> process_lvalue lval) lvals in
      "(" ^ (String.concat ~sep:"," vallist)^")"

let process_transformed_type _ _ =
  ""

let process_truncation t =
  match t with
    | NoTruncate -> ("",[])
    | TruncateUpFrom e -> 
      let (final, exprs) = process_expression e.expr in
      (final^">", exprs)
    | TruncateDownFrom  e ->
      let (final, exprs) = process_expression e.expr in
      ("<" ^ final, exprs)
    | TruncateBetween (e1, e2) ->
      let (final1, exprs1) = process_expression e1.expr in
      let (final2, exprs2) = process_expression e2.expr in
      (final1^"><"^final2, List.append exprs1 exprs2)

let rec process_statement name ({stmt= s_content; _} : typed_statement)
    =
  match s_content with
  | Assignment {assign_lhs= l; assign_op= _; assign_rhs= e} ->
      let dest = process_lvalue l in
      let (final, exprs) = process_expression e.expr in
      List.append exprs [dest ^ "=" ^ final]
  | NRFunApp (_, _, es) ->
      let processed_ex = List.map ~f:(fun (x) -> x.expr) es in 
      let (exprs, _) = process_list_of_expression processed_ex in
      exprs
  | TargetPE e -> 
      let (_, exprs) = process_expression e.expr in
      exprs
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      let (dest, _) = process_expression e.expr in
      let dist = process_identifier id in
      let processed_ex = List.map ~f:(fun (x) -> x.expr) es in 
      let (finals, exprs) = process_list_of_expression processed_ex in
      let (truc, truncexprs) = process_truncation t in
      let fns = String.concat ~sep:"*" finals in 
      List.append (List.append exprs truncexprs) [dest ^ "=" ^dist ^ ":" ^ truc ^  "-" ^ fns]
  | Break -> []
  | Continue -> []
  | Return e -> 
      let (_, exprs) = process_expression e.expr in
      exprs
  | ReturnVoid -> []
  | Print _ -> []
  | Reject _ -> []
  | Skip -> []
  | IfThenElse (_, _, _) -> []
  (*process_recursive_ifthenelse ppf s_content*)    
  | While (e, stmt) -> 
      let (_, exprs) = process_expression e.expr in
      let res = process_statement name stmt in
      List.append exprs res
  (*| For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      pp_identifier id
      pp_expression e1
      pp_expression e2 
      pp_indent_unless_block s
  | ForEach (id, e, s) ->
      pp_identifier id 
      pp_expression e
      pp_indent_unless_block s*)
  | For {loop_variable= _; lower_bound= _; upper_bound= _; loop_body= _} -> []
  | ForEach (_, _, _) -> []
  | Block vdsl ->
      process_list_of_statements name vdsl
  | Profile (name, vdsl) ->
      process_list_of_statements name vdsl
  | VarDecl {decl_type= pst; transformation= trans; variables; is_global= _} ->
      let tp = process_transformed_type pst trans in
      let vlist = process_list_of_variables variables in
      [tp ^ " " ^ vlist]
  | FunDef {returntype= _; funname= id; arguments= _; body= _} -> 
      let name = process_identifier id in
      [name]

and process_list_of_statements name stmts =
  match stmts with
  | stmt :: l -> 
    let res1 = process_statement name stmt in
    (*let res1 = ["a"] in*)
    let res2 = process_list_of_statements name l in
    List.append res1 res2
  | [] -> []

let process_block name block = 
  match block with
  | Some {stmts; _} -> 
      process_list_of_statements name stmts
  | None -> []

let process_program ppf = 
 match ppf with
    { functionblock= bf
    ; datablock= bd
    ; transformeddatablock= btd
    ; parametersblock= bp
    ; transformedparametersblock= btp
    ; modelblock= bm
    ; generatedquantitiesblock= bgq
    ; _ } ->
    let fun_res = process_block "functions" bf in
    let dat_res = process_block "data" bd in
    let tra_res = process_block "transformed data" btd in
    let par_res = process_block "parameters" bp in
    let trp_res = process_block "transformed parameters" btp in
    let mol_res = process_block "model" bm in
    let gen_res = process_block "generated quantities" bgq in
    [("functions", fun_res);("data", dat_res);("transformed data", tra_res);("parameters", par_res);
    ("transformed parameters", trp_res);("model", mol_res);("generated quantities", gen_res)]

let apply_m p =
  process_program p

let marginalize p =
  apply_m p