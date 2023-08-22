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
open KyoumiAst.KExpresssion
open KyoumiUtil
open KyoumiError
module KyoEnv = KyoumiUtil.KyoEnv

  
let rec typeof_expr' kyo_env expr = typeof_expr kyo_env @@ value expr

and typeof_expr (kyo_env : KyoEnv.kyo_env) = 
let open KyoumiAst.KyoType in
function
| EUnit -> kyo_env, TyUnit
| ECmpLess | ECmpEqual | ECmpGreater ->
  kyo_env, TyOredered
| EInteger _ -> 
  kyo_env, TyInteger
| EFloat _ -> 
  kyo_env, TyFloat
| EString _ -> 
  kyo_env, TyString
| EOpen {module_resolver; next} ->
  let kyo_module = 
    match Module.find_module module_resolver kyo_env.program with
    | Some kyo_module -> kyo_module
    | None -> raise @@ unbound_module module_resolver
  in
  let kyo_env = KyoEnv.add_module kyo_module kyo_env in
  let next_type = typeof_expr' kyo_env next in
  next_type
| ETuple kyo_exprs ->
  let kyo_extented_env, kyo_types = 
  List.fold_left_map typeof_expr' kyo_env kyo_exprs
  in
  kyo_extented_env, TyTuple (kyo_types)
| EWhile {w_condition; w_body} ->
  let (env, c) = typeof_expr' kyo_env w_condition in
  let kyo_env = KyoEnv.merge_constraints kyo_env env in
  let (env, b) = typeof_expr' env w_body in
  let kyo_env = KyoEnv.merge_constraints kyo_env env in
  let kyo_env = 
    kyo_env
    |> KyoEnv.add_constraint ~lhs:c ~rhs:TyBool
    |> KyoEnv.add_constraint ~lhs:b ~rhs:TyUnit
  in
  kyo_env, TyUnit
| EFunctionCall {e_module_resolver; e_function_name; parameters; handlers} ->
  let () = ignore (e_module_resolver,e_function_name, parameters, handlers) in
  failwith ""
| _ -> failwith ""
and typeof_pattern' scrutinee_type kyo_env pattern = typeof_pattern scrutinee_type kyo_env @@ value pattern
and typeof_pattern scrutinee_type kyo_env = 
let open KyoumiAst.KyoType in
function
| PTrue | PFalse -> 
  let kyo_env = KyoEnv.add_constraint ~lhs:scrutinee_type ~rhs:KyoumiAst.KyoType.TyBool kyo_env in 
  kyo_env, KyoumiAst.KyoType.TyBool
| PEmpty -> 
  let ptype = TyUnit in
  let kyo_env = KyoEnv.add_constraint ~lhs:scrutinee_type ~rhs:ptype kyo_env in 
  kyo_env, ptype
| PCmpLess | PCmpEqual | PCmpGreater ->
  let ptype = TyOredered in
  let kyo_env = KyoEnv.add_constraint ~lhs:scrutinee_type ~rhs:ptype kyo_env in 
  kyo_env, ptype
| PWildcard ->
  kyo_env, scrutinee_type
| PFloat _ ->
  let ptype = TyFloat in
  let kyo_env = KyoEnv.add_constraint ~lhs:scrutinee_type ~rhs:ptype kyo_env in 
  kyo_env, ptype
| PInteger _ ->
  let ptype = TyInteger in
  let kyo_env = KyoEnv.add_constraint ~lhs:scrutinee_type ~rhs:ptype kyo_env in 
  kyo_env, ptype
| PIdentifier id ->
  let kyo_env = KyoEnv.add_variable id.value scrutinee_type kyo_env in
  kyo_env, scrutinee_type
| _ -> failwith ""