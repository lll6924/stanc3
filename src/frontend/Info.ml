open Core_kernel
open Ast
open Middle

(* Implementation of the [--info] option which outputs on the standard
   output a JSON object containing information about the model.

   The JSON object has a field [inputs], [parameters], [transformed
   parameters], and [generated quantities] containing a dictionary
   where each entry corresponds to a variable in respectively the
   [data], [parameters], [transformed parameters], and [generated
   quantities] blocks. To each variable is associated an object with
   two fields:
   - [type]: the base type of the variable (["int"] or ["real"]).
   - [dimensions]: the number of dimensions ([0] for a scalar, [1] for
     a vector or row vector, etc.).
   The JSON object also have a field [stanlib_calls] containing the name
   of the standard library functions called.
*)

module SSet = Set.Make(String)

let rec sized_basetype_dims t =
  match t with
  | SizedType.SInt -> ("int", 0)
  | SReal -> ("real", 0)
  | SVector _ | SRowVector _ -> ("real", 1)
  | SMatrix _ -> ("real", 2)
  | SArray (t, _) ->
      let bt, n = sized_basetype_dims t in
      (bt, n + 1)

let rec unsized_basetype_dims t =
  match t with
  | UnsizedType.UInt -> ("int", 0)
  | UReal -> ("real", 0)
  | UVector | URowVector -> ("real", 1)
  | UMatrix -> ("real", 2)
  | UArray t ->
      let bt, n = unsized_basetype_dims t in
      (bt, n + 1)
  | UMathLibraryFunction | UFun _ -> assert false

let basetype_dims t =
  match t with
  | Type.Sized t -> sized_basetype_dims t
  | Type.Unsized t -> unsized_basetype_dims t

let get_var_decl stmts =
  List.fold_right ~init:[]
    ~f:(fun stmt acc ->
      match stmt.Ast.stmt with
      | Ast.VarDecl decl ->
          let t, n = basetype_dims decl.decl_type in
          (decl.identifier.name, t, n) :: acc
      | _ -> acc )
    stmts

let block_info name ppf block =
  let var_info ppf (name, t, n) =
    Fmt.pf ppf "\"%s\": { \"type\": \"%s\", \"dimensions\": %d}" name t n
  in
  let vars_info = Fmt.list ~sep:Fmt.comma var_info in
  Fmt.pf ppf "\"%s\": { @[<v 0>%a @]}" name vars_info
    (Option.value_map block ~default:[] ~f:get_var_decl)

let rec get_function_calls_expr acc expr =
  let acc =
    match expr.expr with
    | FunApp (_, f, _) -> SSet.add acc f.name
    | _ -> acc
  in
  fold_expression
    get_function_calls_expr
    (fun acc _ -> acc)
    acc expr.expr

let rec get_function_calls_stmt acc stmt =
  let acc =
    match stmt.stmt with
    | NRFunApp (_, f, _) -> SSet.add acc f.name
    | _ -> acc
  in
  fold_statement
    get_function_calls_expr
    get_function_calls_stmt
    (fun acc _ -> acc)
    (fun acc _ -> acc)
    acc stmt.stmt

let function_calls ppf p =
  let calls = fold_program get_function_calls_stmt SSet.empty p in
  Fmt.pf ppf "\"stanlib_calls\": [ @[<v 0>%a @]]"
    (Fmt.list ~sep:Fmt.comma (fun ppf s -> Fmt.pf ppf "\"%s\"" s))
    (SSet.to_list calls)

let info ast =
  Fmt.strf "{ @[<v 0>%a,@,%a,@,%a,@,%a,@,%a @]}@." (block_info "inputs")
    ast.datablock (block_info "parameters") ast.parametersblock
    (block_info "transformed parameters")
    ast.transformedparametersblock
    (block_info "generated quantities")
    ast.generatedquantitiesblock
    function_calls ast
