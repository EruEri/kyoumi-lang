open Util.Position

module KyoType = struct
  type kyo_type_polymorphic = 
    | KyTyPolymorphic of string location
  type kyo_type = 
    | TyParametricIdentifier of {
      module_resolver: string location list;
      parametrics_type : kyo_type location list;
      name: string location
    }
    | TyIdentifier of {
      module_resolver: string location list;
      name: string location
    }
    | TyPolymorphic of kyo_type_polymorphic
    | TRef of kyo_type location
    | TTuple of kyo_type location list
    | TFunction of {
      effects: kyo_effect location;
      parameters: kyo_type location list;
      return_type: kyo_type location
    }
    | TArray of { ktype : kyo_type location; size : int64 location }
    | TInteger 
    | TFloat
    | TOredered
    | TString
    | TUnit
    | TBool
    | TChar
    and kyo_effect = 
    | EfWilcard 
    | EfContrete of kyo_effect_concrete
    and kyo_effect_concrete =
    (* `a list<int> *)
    | EfPolymorphic of string location
    (* 
      ask list<int>
      ask<'a, 'b> list<int>
      ask<'a> list<'a>
      ask<int> list<string>
    *)
    | EfType of {
      effect_name: string location;
      eff_parametric_type: kyo_type location list
    }
    (*
      (ask<int> & raise) list<int>
    *)
    | EfList of kyo_effect_concrete location list
end

module KExpresssion = struct
  type kyo_expression = 
  |
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
    effect_sig_param: KyoType.kyo_type location list;
    effect_sig_return_type: KyoType.kyo_type list;
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

module KNodeEnum = struct
  type enum_cases = {
    case_name: string location;
    assoc_types: KyoType.kyo_type location list
  }
  type enum_declaration = {
    name: string location;
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
    sig_param: KyoType.kyo_type location list;
    sig_return_type: KyoType.kyo_type location;
    sig_external_name: string location
  }
end



type function_declaration

type global_declaration

type kyo_node = 
| KNEffect of KNodeEffect.effect_declaration
| KNEnum of KNodeEnum.enum_declaration
| KNRecord of KNodeRecord.record_declaration
| KNExternal of KnodeExternal.external_declaration
| KNFunction of function_declaration
| KNGlobal of global_declaration

type kyo_module = kyo_node list

