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



module KyTypeEffect = struct

  let rec of_kyoloc_type = function
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
  and of_kyoloc_type' ky_loc = of_kyoloc_type @@ value ky_loc
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
  | {effects; parameters; return_type} -> 
    {
      effects = of_kyoloc_effect' effects;
      parameters = List.map of_kyoloc_type' parameters;
      return_type = of_kyoloc_type' return_type
    }

  type kyo_type = KyoumiAst.KyoLocType.kyoloc_type 
end

module KyoTypeConstraintSet = Set.Make(struct
  type t = KyoumiAst.KyoType.kyo_type_constraint

  let compare = Stdlib.compare
end)

module Module = struct
  let find_module modules kyo_program = 
    kyo_program 
    |> List.find_map (fun {filename; kyo_module} ->
      let name = Util.Convertion.filename_of_module modules in
      if name = filename then Some kyo_module else None
    )
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