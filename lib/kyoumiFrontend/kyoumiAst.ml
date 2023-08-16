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

module KyoType = struct
  type kyo_type_polymorphic = 
    | KyTyPolymorphic of string location
  type kyo_type_function = 
    {
      effects: kyo_effect location;
      parameters: kyo_type location list;
      return_type: kyo_type location
    }
  and kyo_type = 
    | TyParametricIdentifier of {
      module_resolver: string location list;
      parametrics_type : kyo_type location list;
      name: string location
    }
    | TyIdentifier of {
      module_resolver: string location list;
      name: string location
    }
    | TyHandler of kyo_effect location
    | TyPolymorphic of kyo_type_polymorphic
    | TRef of kyo_type location
    | TTuple of kyo_type location list
    | TFunction of kyo_type_function
    | TArray of { ktype : kyo_type location; size : int64 location }
    | TInteger 
    | TFloat
    | TOredered
    | TString
    | TUnit
    | TBool
    | TChar
    and kyo_effect = 
    (* `a list<int> *)
    | KyEffPolymorphic of string location
    (* 
      ask list<int>
      ask<'a, 'b> list<int>
      ask{'a} list<'a>
      ask(int) list<string>
    *)
    | KyEffType of {
      module_resolver: string location list;
      effect_name: string location;
      eff_parametric_type: kyo_type location list
    }
    (*
      (ask<int> & raise) list<int>
    *)
    | KyEffList of kyo_effect location list
end

module KNodeEnum = struct
  type enum_cases = {
    case_name: string location;
    assoc_types: KyoType.kyo_type location list
  }
  type enum_declaration = {
    enum_name: string location;
    polymorp_vars: KyoType.kyo_type_polymorphic location list;
    cases: enum_cases list 
  }
end 

module KNodeRecord = struct

  type record_field = {
    field_name: string location;
    field_kyotype: KyoType.kyo_type location;
  }

  type record_declaration = {
    record_name: string location;
    polymorp_vars: KyoType.kyo_type_polymorphic location list;
    fields: record_field list
  }
end

module KnodeExternal = struct
  type external_declaration = {
    sig_name: string location;
    sig_external_name: string location;
    sig_function: KyoType.kyo_type_function;
  }
end

module KNodeEffect = struct
  (**
    val default : int
  *)
  type effect_value = {
    name: string location;
    effect_type: KyoType.kyo_type location
  }

  type effect_function = {
    name: string location;
    effect_sig: KyoType.kyo_type_function;
  }

  type effect_signature = 
  | KEffVal of effect_value
  | KEffSig of effect_function

  type effect_declaration = {
    name: string location;
    polymorp_vars: KyoType.kyo_type_polymorphic location list;
    signatures: effect_signature list
  }
end

module KExpresssion = struct
  type function_declaration = {
    function_name: string location;
    fparameters: (kyo_pattern location * (KyoType.kyo_type location option)) list;
    freturn_effect: KyoType.kyo_effect location;
    freturn_type: KyoType.kyo_type location; 
    fbody: kyo_expression location;
  }
  and global_declaration = {
    gvariable_name: string location;
    greturn_type: KyoType.kyo_type location option;
    gbody: kyo_expression location;
  }
  and kyo_eff_value_decl = 
    | KyEffValGlobal of global_declaration
    | KyEffValFunction of function_declaration
  and kyo_eff_handler = 
    (* identifier name *)
    | KyEffHandler of string location
  and kyo_pattern =
  | PTrue
  | PFalse
  | PEmpty
  | PCmpLess
  | PCmpEqual
  | PCmpGreater
  | PNullptr
  | PWildcard
  | PFloat of float location
  (* | PChar of char location *)
  | PInteger of int location
  | PIdentifier of string location
  | PTuple of kyo_pattern location list
  | PCase of {
      variant : string location;
      assoc_patterns : kyo_pattern location list;
    }
  | PRecord of {
    module_resolver: string location location;
    pfields : (string location * kyo_pattern location) list
    }
  | POr of kyo_pattern location list
  | PTyped of {
    ptype: KyoType.kyo_type location;
    pattern: kyo_pattern location
  }
  and kyo_declaration = {
    kd_pattern: kyo_pattern location;
    explicit_type: KyoType.kyo_type location option;
    expression: kyo_expression location
  }
  and kyo_pattern_branch = {
    kpb_pattern: kyo_pattern location;
    kpb_expr: kyo_expression location;
  }
  and kyo_effect_handler = {
    module_resolver: string location list;
    effect_name: string location;
    effects: kyo_eff_value_decl list
  }
  and kyo_expression = 
  | EUnit
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | EIdentifier of {
    module_resolver: string location list;
    name: string location
  }
  | EInteger of int location
  | EFloat of float location
  | EString of string location
  | EEnum of {
    module_resolver: string location list;
    name: string location;
    assoc_exprs: kyo_expression location list;
  }
  | EOpen of {
    module_resolver: string location list;
    next: kyo_expression location
  }
  | ERecord of {
    module_resolver: string location list;
    name: string location;
    fields: (string location * kyo_expression location) list
  }
  | ERecordAccess of {
    expr: kyo_expression location;
    field: string location;
  }
  | EDeclaration of kyo_declaration * (kyo_expression location)
  | EAnonFunction of {
    parameters: kyo_pattern location list;
    body: kyo_expression location
  }
  | EFunctionCall of {
    module_resolver: string location list;
    function_name: string location;
    parameters: kyo_expression location list;
    handlers: kyo_eff_handler list;
  }
  | EHandler of {
    module_resolver: string location list;
    effect_name: string location; 
    effect_impls: kyo_effect_implementation list; 
  }
  | EWhile of {
    w_condition: kyo_expression location;
    w_body: kyo_expression location
  }
  | EMatch of kyo_expression location * (kyo_pattern_branch list)
  | ETuple of kyo_expression location list
  and kyo_effect_implementation = 
  | KyEffImplLet of global_declaration
  | KyEffImplFn of function_declaration 
end





type kyo_node = 
| KNEffect of KNodeEffect.effect_declaration
| KNEnum of KNodeEnum.enum_declaration
| KNRecord of KNodeRecord.record_declaration
| KNExternal of KnodeExternal.external_declaration
| KNFunction of KExpresssion.function_declaration
| KNGlobal of KExpresssion.global_declaration

type kyo_module = kyo_node list