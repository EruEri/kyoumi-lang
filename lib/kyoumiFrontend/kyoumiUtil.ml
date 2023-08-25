(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kyoumi                                                                *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kyoumi is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kyoumi is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kyoumi.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Util.Position
open KyoumiAst

module KyoFunctionNode = struct
  type t = KExpresssion.kyo_expression KExpresssion.global_declaration

  (* Can use KExpresssion.kyo_expression which contains located *)
  let compare = Stdlib.compare
end

module KyoFunctionGraph = Util.Graph.Make(KyoFunctionNode)

module KyoTypeConstraintSet = Set.Make(struct
  type t = KyoumiAst.KyoType.kyo_type_constraint

  let compare = Stdlib.compare
end)

module KyoNodeConstraintHashedType : Hashtbl.HashedType = struct
  type t = KyoumiAst.kyo_node

  (* We can use the physic equality since a node is unique and not recreated *)
  let equal = ( == )

  let hash : 'a . 'a -> int = Hashtbl.hash
end

module KyoNodeConstraintHash = Hashtbl.Make(KyoNodeConstraintHashedType)
(* 
let kyo_node_constraint = KyoNodeConstraintHash.create 21 *)

module KyoFunctionDeclaration = struct
  let calling_name = function
  | KyoFnExternal {sig_name = name; sig_external_name = _; sig_function = _}
  | KyoFnDeclaration {gvariable_name = name; greturn_type = _; gbody = _} -> 
    name
end

module KyoEnv = struct

  type kyo_opened_module = {
    current_module : KyoumiAst.kyo_module;
    modules: KyoumiAst.kyo_module list
  }
  type kyo_tying_env = (string * KyoumiAst.KyoType.kyo_type) list
  type kyo_tying_constraints = KyoTypeConstraintSet.t

  type kyo_env = {
    program: KyoumiAst.kyo_program;
    opened_modules: kyo_opened_module;
    variable_env: kyo_tying_env;
    kyo_tying_constraints: kyo_tying_constraints;
  }

  let merge_constraints base rhs = 
    {
      base with kyo_tying_constraints = KyoTypeConstraintSet.union base.kyo_tying_constraints rhs.kyo_tying_constraints
    }
  let fresh_variable reset =
    let counter = ref 0 in
    fun () -> 
      let () = match reset with
        | false -> ()
        | true -> counter := 0 
      in
      let n = !counter in
      let () = incr counter in
      Printf.sprintf "'t%u" n
  
  let fresh_variable_type ?(reset = false) () = 
    KyoumiAst.KyoType.TyPolymorphic(KyTyPolymorphic (fresh_variable reset ()))
  
  let add_constraint ~lhs ~rhs env =
    let constrai = KyoumiAst.KyoType.{cstr_lhs = lhs; cstr_rhs = rhs} in
    { env with kyo_tying_constraints = KyoTypeConstraintSet.add constrai env.kyo_tying_constraints}
  


  (**
    [add_module kyo_module kyo_env] adds the module [kyo_module] to the list of opened modules in [kyo_env]
  *)
  let add_module kyo_module kyo_env =
    let opened_modules = kyo_env.opened_modules in
    let opened_modules = {
      opened_modules with modules = kyo_module::opened_modules.modules
    } in
    { kyo_env with opened_modules }
  

  (**
    [add_variable variable kyo_type kyo_env] extends the variable environment [kyo_env] by the binding of [variable] with the type [kyo_type]
  *)
  let add_variable variable kyo_type kyo_env = 
    {
      kyo_env with variable_env = (variable, kyo_type)::kyo_env.variable_env
    }

  (** 
    [assoc_type_opt name kyo_env] returns the type associated with the identifier [name] in the variable environment [kyo_env].
    Returns [None] if [name] doesn't exist in [kyo_env]
  *)
  let assoc_type_opt name kyo_env =
    List.assoc_opt name kyo_env.variable_env

  (** [mem name kyo_env] checks if the identifier [name] exists in the variable environment [kyo_env]*)
  let mem name kyo_env = 
    Option.is_some @@ assoc_type_opt name kyo_env

  let modules kyo_env = 
    kyo_env.opened_modules.modules @ [kyo_env.opened_modules.current_module]

end

module KyoSmartType = struct
  let kyo_type pure_type = 
    KyoumiAst.KyoType.TyPureType  pure_type

  let ty_unit = kyo_type TyUnit
  let ty_ordered = kyo_type TyOredered
  let ty_bool = kyo_type TyBool
  let ty_integer = kyo_type TyInteger
  let ty_float = kyo_type TyFloat
  let ty_string = kyo_type TyString
  let ty_tuples ttys = kyo_type @@ TyTuple ttys
end

module KyTypeEffect = struct

  let rec flatten_kyo_effect effect = match effect.value with
  | (KyoumiAst.KyoLocType.EffLocPolymorphic _ | EffLocType _) -> effect::[]
  | EffLocList effects ->
    effects |> List.map flatten_kyo_effect |> List.flatten

  let rec of_kyoloc_pure_type = function
  | KyoLocType.TyLocParametricIdentifier {
    module_resolver;
    parametrics_type;
    name
  } -> 
    let parametrics_type = List.map of_kyoloc_type' parametrics_type in
    let module_resolver = values module_resolver in
    let name = value name in 
    KyoType.TyParametricIdentifier {module_resolver; parametrics_type; name}
  | TyLocIdentifier {
    module_resolver;
    name
  } -> 
    let module_resolver = values module_resolver in
    let name = value name in 
    TyIdentifier {module_resolver; name}
  | TyLocHandler effects ->
    let effects = of_kyoloc_effect' effects in
    TyHandler effects
  | TyLocPolymorphic kyoloc_type_polymorphic ->
    TyPolymorphic (of_kyoloc_type_polymorphic kyoloc_type_polymorphic)
  | TyLocRef kyoloc_type ->
    TyRef (of_kyoloc_type' kyoloc_type)
  | TyLocTuple kts ->
    TyTuple (List.map of_kyoloc_type' kts)
  | TyLocFunction kyoloc_type_function -> 
    TyFunction (of_kyoloc_type_function kyoloc_type_function)
  | TyLocArray { ktype; size } ->
    TyArray {
      ktype = of_kyoloc_type' ktype;
      size = size.value
    }
  | TyLocInteger -> TyInteger
  | TyLocFloat -> TyFloat
  | TyLocOredered -> TyOredered
  | TyLocString -> TyString
  | TyLocUnit -> TyUnit
  | TyLocBool -> TyBool
  | TyLocChar -> TyChar
  and of_kyoloc_pure_type' ky_loc = of_kyoloc_pure_type @@ value ky_loc
  and of_kyoloc_type' ky_loc = of_kyoloc_type @@ value ky_loc 
  and of_kyoloc_type = function
  | TyLocEffectedType {kyo_effect; kyo_type} -> 
    let kyo_effect = of_kyoloc_effect' kyo_effect in
    let kyo_type = of_kyoloc_pure_type' kyo_type in
    TyEffectedType {kyo_effect; kyo_type}
  | TyLocPureType t -> 
    TyPureType (of_kyoloc_pure_type t)

  and of_kyoloc_effect' keff_loc = of_kyoloc_effect @@ value keff_loc
  and of_kyoloc_effect = function
  | EffLocPolymorphic s -> 
    EffPolymorphic s.value
  | EffLocType {module_resolver; effect_name; eff_parametric_type} ->
    let eff_parametric_type = List.map of_kyoloc_type' eff_parametric_type in
    let module_resolver = values module_resolver in
    let effect_name = value effect_name in 
    EffType {module_resolver; effect_name; eff_parametric_type}
  | EffLocList effts -> 
    EffList (List.map of_kyoloc_effect' effts)
  and of_kyoloc_type_polymorphic = function
  | KyLocTyPolymorphic s -> KyoType.KyTyPolymorphic s.value
  and of_kyoloc_type_function : KyoLocType.kyoloc_type_function -> KyoType.kyo_type_function = function
  | {parameters; return_type} -> 
    {
      parameters = List.map of_kyoloc_type' parameters;
      return_type = of_kyoloc_type' return_type
    }

  type kyo_type = KyoumiAst.KyoLocType.kyoloc_type 
end

module Module = struct
  let find_module modules kyo_program = 
    kyo_program 
    |> List.find_map (fun {filename; kyo_module} ->
      let name = Util.Convertion.filename_of_module modules in
      if name = filename then Some kyo_module else None
    )


    let function_declarations kyo_module = 
      kyo_module
      |> List.filter_map @@ function
        | KNFunction function_decl -> Some function_decl
        | KNType _ | KNEffect _ -> None

    let effect_declarations kyo_module = 
      kyo_module
      |>  List.filter_map @@ function
      | KNType type_decl -> Some type_decl
      | KNFunction _  | KNEffect _ -> None

    (** 
      [find_single_declaration name kyo_module] looks into the module [kyo_module] for the function declaration [name]
      Returns [None] if nothing, [Some declaration] if single and raise [KyoumiError] if multiple declarations have the
      same name

      @raise KyoumiError
    *)
    let find_single_declaration name = fun kyo_module -> 
      let declarations = function_declarations kyo_module in
      let matched_name_declaration = List.find_all (fun decl ->
        let decl_name = KyoFunctionDeclaration.calling_name decl in
        decl_name.value = name.value
      ) declarations 
      in
      match matched_name_declaration with
      | [] -> None
      | t::[] -> Some t
      | _::_ as declarations -> raise @@ KyoumiError.multiple_function_defintions declarations

    
    let rec calling_graph_expr' current_declaration (kyo_env: KyoEnv.kyo_env) graph expr = calling_graph_expr current_declaration kyo_env graph @@ Util.Position.value expr
    and calling_graph_expr current_declaration kyo_env graph = 
    let open KyoumiAst.KExpresssion in
    function
    | EUnit | ECmpLess | ECmpEqual | ECmpGreater | EInteger _ | EFloat _ | EString _ -> 
      graph
    | EOpen {module_resolver; next} -> 
      let kyo_module = 
        match find_module module_resolver kyo_env.program with
        | Some kyo_module -> kyo_module
        | None -> raise @@ KyoumiError.unbound_module module_resolver
      in
      let kyo_env = KyoEnv.add_module kyo_module kyo_env in
      calling_graph_expr' current_declaration kyo_env graph next
    | EIdentifier {module_resolver = []; name}  -> 
      let graph = match KyoEnv.mem name.value kyo_env with
        | true -> graph 
        | false -> 
          let declaration = 
            kyo_env
            |> KyoEnv.modules
            |> List.find_map (find_single_declaration name)
          in
          let graph = match declaration with
            | None -> raise @@ KyoumiError.undefined_identifier name
            | Some KyoFnExternal _ -> graph
            | Some KyoFnDeclaration declaration -> 
              KyoFunctionGraph.add_link current_declaration ~along:declaration graph
          in
          graph
        in
      graph
    | EIdentifier {module_resolver = _::_ as module_resolver; name} -> 
        let kyo_module = match find_module module_resolver kyo_env.program with 
          | Some m -> m
          | None -> raise @@ KyoumiError.unbound_module module_resolver
        in
        let declaration = find_single_declaration name kyo_module in
        let graph = match declaration with
          | None -> raise @@ KyoumiError.undefined_identifier name
          | Some KyoFnExternal _ -> graph
          | Some KyoFnDeclaration declaration -> 
            KyoFunctionGraph.add_link current_declaration ~along:declaration graph
        in
      graph
    | ETuple expressions -> 
      (* Doubt on the kyo_env between expression *)
      List.fold_left (calling_graph_expr' current_declaration kyo_env ) graph expressions
    | EEnum _
    | ERecord _ 
    | ERecordAccess _ 
    | EDeclaration (_, _)
    | EAnonFunction _ 
    | EFunctionCall _ 
    | EHandler _ 
    | EPerform _ 
    | EWhile _ 
    | EMatch (_, _) -> failwith ""

  (** 
    [calling_graph kyo_program] builds a graph where where each node is and a [kyo_node] and
    and each each edge an function call

    It uses to detemine which function are mutualy recursive
  *)
  let calling_graph _kyo_program = 
    failwith ""
end

module Pattern = struct
  open KyoumiAst.KExpresssion
  let rec flatten_por pattern =
    match pattern.value with
    | POr patterns ->
        patterns |> List.map flatten_por |> List.flatten
    | _ ->
        pattern :: []
end