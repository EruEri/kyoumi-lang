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
  | KyTyPolymorphic of string
  type kyo_type_function = 
    {
      effects: kyo_effect;
      parameters: kyo_type list;
      return_type: kyo_type
    }
  and kyo_type = 
    | TyParametricIdentifier of {
      module_resolver: string list;
      parametrics_type : kyo_type list;
      name: string
    }
    | TyIdentifier of {
      module_resolver: string list;
      name: string
    }
    | TyHandler of kyo_effect
    | TyPolymorphic of kyo_type_polymorphic
    | TyRef of kyo_type
    | TyTuple of kyo_type list
    | TyFunction of kyo_type_function
    | TyArray of { ktype : kyo_type; size : int64 }
    | TyInteger 
    | TyFloat
    | TyOredered
    | TyString
    | TyUnit
    | TyBool
    | TyChar
    and kyo_effect = 
    (* `a list<int> *)
    | EffPolymorphic of string
    (* 
      ask list<int>
      ask<'a, 'b> list<int>
      ask{'a} list<'a>
      ask(int) list<string>
    *)
    | EffType of {
      module_resolver: string list;
      effect_name: string;
      eff_parametric_type: kyo_type list
    }
    (*
      (ask<int> & raise) list<int>
    *)
    | EffList of kyo_effect list


    type kyo_type_scheme = 
    | KyTYScheme of {
      type_vars: kyo_type_polymorphic list;
      scheme_signature: kyo_type_function
    }


    type kyo_type_constraint = {
      cstr_lhs: kyo_type;
      cstr_rhs: kyo_type
    }
end

module KyoLocType = struct
  type kyoloc_type_polymorphic = 
    | KyLocTyPolymorphic of string location
  type kyoloc_type_function = 
    {
      effects: kyoloc_effect location;
      parameters: kyoloc_type location list;
      return_type: kyoloc_type location
    }
  and kyoloc_type = 
    | TyLocParametricIdentifier of {
      module_resolver: string location list;
      parametrics_type : kyoloc_type location list;
      name: string location
    }
    | TyLocIdentifier of {
      module_resolver: string location list;
      name: string location
    }
    | TyLocHandler of kyoloc_effect location
    | TyLocPolymorphic of kyoloc_type_polymorphic
    | TyLocRef of kyoloc_type location
    | TyLocTuple of kyoloc_type location list
    | TyLocFunction of kyoloc_type_function
    | TyLocArray of { ktype : kyoloc_type location; size : int64 location }
    | TyLocInteger 
    | TyLocFloat
    | TyLocOredered
    | TyLocString
    | TyLocUnit
    | TyLocBool
    | TyLocChar
    and kyoloc_effect = 
    (* `a list<int> *)
    | EffLocPolymorphic of string location
    (* 
      ask list<int>
      ask<'a, 'b> list<int>
      ask{'a} list<'a>
      ask(int) list<string>
    *)
    | EffLocType of {
      module_resolver: string location list;
      effect_name: string location;
      eff_parametric_type: kyoloc_type location list
    }
    (*
      (ask<int> & raise) list<int>
    *)
    | EffLocList of kyoloc_effect location list
end

module KNodeEnum = struct
  type enum_cases = {
    case_name: string location;
    assoc_types: KyoLocType.kyoloc_type location list
  }
  type enum_declaration = {
    enum_name: string location;
    polymorp_vars: KyoLocType.kyoloc_type_polymorphic location list;
    cases: enum_cases list 
  }
end 

module KNodeRecord = struct

  type record_field = {
    field_name: string location;
    field_kyotype: KyoLocType.kyoloc_type location;
  }

  type record_declaration = {
    record_name: string location;
    polymorp_vars: KyoLocType.kyoloc_type_polymorphic location list;
    fields: record_field list
  }
end

module KnodeExternal = struct
  type external_declaration = {
    sig_name: string location;
    sig_external_name: string location;
    sig_function: KyoLocType.kyoloc_type_function;
  }
end

module KNodeEffect = struct
  (**
    val default : int
  *)
  type effect_value = {
    name: string location;
    effect_type: KyoLocType.kyoloc_type location
  }

  type effect_function = {
    name: string location;
    effect_sig: KyoLocType.kyoloc_type_function;
  }

  type effect_signature = 
  | KEffVal of effect_value
  | KEffSig of effect_function

  type effect_declaration = {
    name: string location;
    polymorp_vars: KyoLocType.kyoloc_type_polymorphic location list;
    signatures: effect_signature list
  }
end

module KExpresssion = struct
  type 'a function_declaration = {
    function_name: string location;
    fparameters: (kyoloc_pattern location * (KyoLocType.kyoloc_type location option)) list;
    freturn_effect: KyoLocType.kyoloc_effect location;
    freturn_type: KyoLocType.kyoloc_type location; 
    fbody: 'a location;
  }
  and 'a global_declaration = {
    gvariable_name: string location;
    greturn_type: KyoLocType.kyoloc_type location option;
    gbody: 'a location;
  }
  and kyo_eff_value_decl = 
    | KyEffValGlobal of kyo_resumable_expression global_declaration
    | KyEffValFunction of kyo_resumable_expression function_declaration
  and kyo_eff_handler = 
    (* identifier name *)
    | KyEffHandler of string location
  and kyoloc_pattern =
  | PTrue
  | PFalse
  | PEmpty
  | PCmpLess
  | PCmpEqual
  | PCmpGreater
  | PWildcard
  | PFloat of float location
  (* | PChar of char location *)
  | PInteger of int location
  | PIdentifier of string location
  | PTuple of kyoloc_pattern location list
  | PCase of {
      variant : string location;
      assoc_patterns : kyoloc_pattern location list;
    }
  | PRecord of {
    module_resolver: string location location;
    pfields : (string location * kyoloc_pattern location) list
    }
  | POr of kyoloc_pattern location list
  | PAs of {
    pas_pattern: kyoloc_pattern location;
    pas_bound: string location;
  }
  | PTyped of {
    ptype: KyoLocType.kyoloc_type location;
    pattern: kyoloc_pattern location
  }
  and kyo_declaration = {
    kd_pattern: kyoloc_pattern location;
    explicit_type: KyoLocType.kyoloc_type location option;
    expression: kyo_expression location
  }
  and kyo_pattern_branch = {
    kpb_pattern: kyoloc_pattern location;
    kpb_expr: kyo_expression location;
  }
  and kyo_effect_handler = {
    module_resolver: string location list;
    effect_name: string location;
    effects: kyo_eff_value_decl list
  }
  and kyo_perform_effect = 
    | KyPeffIdentifier of {
      module_resolver: string location list;
      name : string location
    }
    | KyPeffFunctionCall of kyo_expression_function_call
  and kyo_resumable_expression = 
    | KyoExpr of kyo_expression location
    | KyoResumeExpr of kyo_expression location
  and kyo_expression_function_call = {
    e_module_resolver: string location list;
    e_function_name: string location;
    parameters: kyo_expression location list;
    handlers: kyo_eff_handler list;
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
    parameters: kyoloc_pattern location list;
    body: kyo_expression location
  }
  | EFunctionCall of kyo_expression_function_call
  | EHandler of {
    module_resolver: string location list;
    effect_name: string location; 
    effect_impls: kyo_effect_implementation list; 
  }
  | EPerform of kyo_perform_effect
  | EWhile of {
    w_condition: kyo_expression location;
    w_body: kyo_expression location
  }
  | EMatch of kyo_expression location * (kyo_pattern_branch list)
  | ETuple of kyo_expression location list
  and kyo_effect_implementation = 
  | KyEffImplLet of kyo_resumable_expression global_declaration
  | KyEffImplFn of kyo_resumable_expression function_declaration 
end

type kyo_node = 
| KNEffect of KNodeEffect.effect_declaration
| KNEnum of KNodeEnum.enum_declaration
| KNRecord of KNodeRecord.record_declaration
| KNExternal of KnodeExternal.external_declaration
| KNFunction of KExpresssion.kyo_expression KExpresssion.function_declaration
| KNGlobal of KExpresssion.kyo_expression KExpresssion.global_declaration

type kyo_module = kyo_node list

type named_kyo_module = {
  filename: string;
  kyo_module: kyo_module
}

type kyo_program = named_kyo_module list