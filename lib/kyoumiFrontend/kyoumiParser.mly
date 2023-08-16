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

%{

    open Util
    open KyoumiAst
    open KyoumiAst.KyoType
    open KyoumiAst.KNodeEffect
    open KyoumiAst.KExpresssion
%}

%token <string> IDENT
%token <string> PolymorphicVar
%token <string> PolymorphicEff
%token <string> Module_IDENT
%token <string> String_lit
%token <float> Float_lit
%token <int> Integer_lit
%token <string> BUILTIN
%token <string> INFIX_PIPE
%token <string> INFIX_AMPERSAND
%token <string> INFIX_EQUAL
%token <string> INFIX_INF
%token <string> INFIX_SUP
%token <string> INFIX_CARET (* ^ *)
%token <string> INFIX_PLUS
%token <string> INFIX_MINUS
%token <string> INFIX_MULT
%token <string> INFIX_DIV
%token <string> INFIX_DOLLAR
%token <string> INFIX_PERCENT
%token <string> INFIX_TILDE
%token <string> PREFIX_EXCLA
%token <string> PREFIX_QUESTIONMARK
%token MINUS_SUP /*EQUAL_SUP*/
%token BACKTICK
%token EFFECT TYPE EXTERNAL FUNCTION ANON_FUNCTION LET AS HANDLER 
%token CMP_LESS CMP_EQUAL CMP_GREATER
%token TRUE FALSE
%token LSQBRACE RSQBRACE
%token LBRACE RBRACE
%token LPARENT RPARENT
%token WILDCARD
%token WHILE MATCH VAL WITH END IN OPEN
%token REF
%token COLON SEMICOLON DOUBLECOLON EQUAL
%token PIPE DOT AMPERSAND
%token COMMA
%token EOF



%left INFIX_PLUS
%left DOT
%left PIPE

%start kyo_module

%type <KyoumiAst.kyo_module> kyo_module

%%


%inline located(X): x=X {
  Position.located_value $startpos $endpos x
}

%inline parenthesis(X):
    | delimited(LPARENT, X, RPARENT) { $1 }

%inline bracketed(X):
    | delimited(LBRACE, X, RPARENT) { $1 }

%inline module_resolver:
    | mp=loption(terminated(separated_nonempty_list(DOUBLECOLON, located(Module_IDENT)), DOT)) { 
        mp
    }

%inline trailing_separated_list(sep, elt):
    | nonempty_list(terminated(elt, sep)) { $1 }
    | separated_nonempty_list(sep, elt) { $1 }


%inline generics(X):
    | loption(parenthesis(separated_nonempty_list(COMMA, located(X)))) {
        $1
    }
%inline backticked(X):
    | delimited(BACKTICK, X, BACKTICK) { $1 }

%inline signature_return:
    | COLON effects=located(kyo_effect) return_type=located(kyo_type) {effects, return_type}

%inline signature:
    | parameters=delimited(LPARENT, separated_list(COMMA, located(kyo_type)), RPARENT) sig_r=signature_return {
        let effects, return_type = sig_r in
        {parameters; return_type; effects}
    }
    

%inline operator:
    | INFIX_PIPE
    | INFIX_AMPERSAND
    | INFIX_EQUAL
    | INFIX_INF
    | INFIX_SUP
    | INFIX_CARET (* ^ *)
    | INFIX_PLUS
    | INFIX_MINUS
    | INFIX_MULT
    | INFIX_DIV
    | INFIX_DOLLAR
    | INFIX_PERCENT
    | INFIX_TILDE
    | PREFIX_EXCLA
    | PREFIX_QUESTIONMARK { $1 }

%inline loc_var_identifier:
    | located(IDENT)
    | backticked(located(operator)) { $1 }

kyo_module:
    | nodes = list(kyo_node) EOF { nodes }

kyo_node:
    | kyo_effect_decl { KNEffect $1 }
    | kyo_type_decl { $1 }
    | kyo_external_decl { KNExternal $1 }
    | kyo_function_decl { KNFunction $1 }
    | kyo_global_decl { KNGlobal $1 }

kyo_pattern:
    | TRUE { PTrue }
    | FALSE { PFalse }
    | CMP_LESS { PCmpLess }
    | CMP_EQUAL { PCmpEqual }
    | CMP_GREATER { PCmpGreater }
    | WILDCARD { PWildcard }
    | located(Float_lit) {
        PFloat $1
    }
    // | located(Char_lit) { 
    //     PChar $1
    // }
    | located(Integer_lit) {
        PInteger $1
    }
    | loc_var_identifier {
        PIdentifier $1
    }
    | parenthesis(separated_list(COMMA, located(kyo_pattern))) {
        match $1 with
        | [] -> PEmpty
        | p::[] -> p.value
        | list -> PTuple list
    }
    | DOT located(IDENT) loption(delimited(LPARENT, separated_nonempty_list(COMMA, located(kyo_pattern)) ,RPARENT))  {
        PCase {
            variant = $2;
            assoc_patterns = $3
        }
    }
    | lpattern=located(kyo_pattern) PIPE rpattern=located(kyo_pattern) {
        let lpattern = KyoumiUtil.Pattern.flatten_por lpattern in
        let rpattern = KyoumiUtil.Pattern.flatten_por rpattern in
        let patterns = lpattern @ rpattern in
        POr patterns
    }
    | parenthesis(pattern=located(kyo_pattern) COLON ptype=located(kyo_type) {pattern, ptype}) {
        let open KyoumiAst.KExpresssion in
        let pattern, ptype = $1 in
        PTyped {ptype; pattern}
    } 

kyo_pattern_branch:
    | PIPE kpb_pattern=located(kyo_pattern) MINUS_SUP kpb_expr=located(kyo_expression) {
        {kpb_pattern; kpb_expr}
    }

%inline kyo_eff_handler_param:
    | preceded(WITH, separated_nonempty_list(AMPERSAND, loc_var_identifier)) {
        $1 |> List.map @@ fun name -> KyEffHandler name
    }


%inline kyo_anon_function:
    | ANON_FUNCTION parameters=parenthesis(separated_list(COMMA, located(kyo_pattern))) MINUS_SUP body=located(kyo_expression) END {
        EAnonFunction {parameters; body}
    }

%inline kyo_function_call_spe:
    | parameters=parenthesis(separated_list(COMMA, located(kyo_expression))) trailing_clo=option(located(kyo_anon_function)) handlers=loption(kyo_eff_handler_param)  {
        let parameters = match trailing_clo with
            | None -> parameters
            | Some p -> parameters @ [p]
        in
        parameters, handlers
    }

%inline either_COLON_EQUAL:
    | COLON { () }
    | EQUAL { () }

%inline kyo_expr_record_line:
    | located(IDENT) option(preceded(either_COLON_EQUAL, located(kyo_expression))) {
        let none = $1 |> Position.map @@ fun _ -> EIdentifier {module_resolver = []; name = $1} in
        let rhs = Option.fold ~none ~some:(Fun.id) $2 in
        $1, rhs
    }

kyo_pathed_expression:
    | module_resolver=module_resolver name=loc_var_identifier fn_spe=option(kyo_function_call_spe) {
        match fn_spe with
        | None -> EIdentifier {module_resolver; name}
        | Some (parameters, handlers) -> EFunctionCall {module_resolver; function_name = name; parameters; handlers}
    }
    | module_resolver=module_resolver name=located(IDENT) DOT fields=bracketed(trailing_separated_list(COMMA, kyo_expr_record_line)) {
        ERecord {module_resolver; name; fields}
    }
    | module_resolver=module_resolver DOT name=located(IDENT) assoc_exprs=loption(parenthesis(separated_nonempty_list(COMMA, located(kyo_expression)))) {
        EEnum {module_resolver; name; assoc_exprs}
    }

%inline kyo_handler_implementation:
    | kyo_global_decl { KyEffImplLet $1 }
    | kyo_function_decl { KyEffImplFn $1 }

kyo_handler:
    | HANDLER module_resolver=module_resolver effect_name=located(IDENT) DOT effect_impls=bracketed(nonempty_list(kyo_handler_implementation)) { 
        EHandler {
            module_resolver;
            effect_name;
            effect_impls
        }
    }


kyo_expression:
    | kyo_pathed_expression { $1 }
    | CMP_LESS { ECmpLess }
    | CMP_EQUAL { ECmpEqual }
    | CMP_GREATER { ECmpGreater }
    | located(Integer_lit) { EInteger $1 }
    | located(Float_lit) { EFloat $1 }
    | located(String_lit) { EString $1 }
    // | lhs=located(kyo_expression) function_name=located(INFIX_PLUS) rhs=located(kyo_expression) {
    //     EFunctionCall {
    //         module_resolver = [];
    //         function_name;
    //         parameters = lhs::rhs::[];
    //         handlers = []
    //     }
    // }
    | LET kd_pattern=located(kyo_pattern) explicit_type=option(preceded(COLON, located(kyo_type))) EQUAL expression=located(kyo_expression) IN next=located(kyo_expression) {
        let decl = {kd_pattern; explicit_type; expression} in
        EDeclaration (decl, next)
    }
    | LET OPEN module_resolver=separated_nonempty_list(DOUBLECOLON, located(Module_IDENT)) IN next=located(kyo_expression) {
        EOpen {module_resolver; next}
    }
    // | expr=located(kyo_pathed_expression) DOT field=located(IDENT) {
    //     ERecordAccess { expr; field }
    // }
    | WHILE w_condition=located(kyo_expression) w_body=bracketed(located(kyo_expression)) {
        EWhile {w_condition; w_body}
    }
    | parenthesis(separated_list(COMMA, located(kyo_expression)) ) {
        match $1 with
        | [] -> EUnit
        | t::[] -> t.value
        | list -> ETuple list
    }
    | kyo_handler { $1 }
    | kyo_anon_function { $1 }
    | MATCH e=located(kyo_expression) ps=bracketed(nonempty_list(kyo_pattern_branch)) {
        EMatch (e, ps)
    }

%inline kyo_function_param:
    | p=located(kyo_pattern) ot=option(preceded(COLON, located(kyo_type))) {
        p, ot
    }

kyo_function_decl:
    | FUNCTION function_name=loc_var_identifier 
        fparameters=parenthesis(separated_list(COMMA,kyo_function_param )) sig_r=signature_return EQUAL fbody=located(kyo_expression) 
    { 
        let freturn_effect, freturn_type = sig_r in
        {function_name; fparameters; freturn_effect; freturn_type; fbody}
    }

kyo_external_decl:
    | EXTERNAL sig_name=loc_var_identifier sig_function=signature EQUAL sig_external_name=located(String_lit) { 
        let open KnodeExternal in
        {sig_name; sig_function; sig_external_name}
    }

kyo_global_decl:
    | LET gvariable_name=loc_var_identifier greturn_type=option(preceded(COLON, located(kyo_type))) EQUAL gbody=located(kyo_expression) { 
        {gvariable_name; greturn_type; gbody}
    }

kyo_type_decl:
    | TYPE record_name=located(IDENT) polymorp_vars=generics(kyo_ky_polymorphic) EQUAL fields=kyo_record_decl {
        let open KNodeRecord in
        KNRecord { record_name; polymorp_vars; fields }
    }
    | TYPE enum_name=located(IDENT) polymorp_vars=generics(kyo_ky_polymorphic) EQUAL cases=kyo_enum_decl {
        let open KNodeEnum in
        KNEnum { enum_name; polymorp_vars; cases }
    }

kyo_enum_decl:
    | nonempty_list(kyo_enum_case) { $1 }

%inline kyo_enum_case:
    | PIPE case_name=located(IDENT) assoc_types=generics(kyo_type) {
        let open KNodeEnum in
        { case_name; assoc_types }
    }

kyo_record_decl:
    | bracketed(trailing_separated_list(COMMA, kyo_record_field))  { $1 }

%inline kyo_record_field:
    | field_name=located(IDENT) COLON field_kyotype=located(kyo_type) {
        let open KNodeRecord in
        {field_name; field_kyotype}
    }

kyo_effect_decl: 
    | EFFECT name=located(IDENT) polymorp_vars=generics(kyo_ky_polymorphic) signatures=delimited(LBRACE, list(kyo_effect_sig) ,RBRACE) {
       {
        name;
        polymorp_vars;
        signatures
       } 
    }

kyo_effect_sig:
    | VAL name=located(IDENT) COLON effect_type=located(kyo_type) {
        KEffVal {
            name;
            effect_type 
        } 
    }
    | FUNCTION name=located(IDENT) effect_sig=signature {
        KEffSig {
            name;
            effect_sig;
        }
    }

%inline kyo_ky_polymorphic:
    | located(PolymorphicVar) { 
        KyTyPolymorphic $1
    }

kyo_effect:
    | located(PolymorphicEff) {
        KyoType.KyEffPolymorphic $1
    }
    | module_resolver=module_resolver effect_name=located(IDENT) {
        KyoType.KyEffType {
            module_resolver;
            effect_name;
            eff_parametric_type = [];
        }
    }
    | LPARENT module_resolver=module_resolver effect_name=located(IDENT) eff_parametric_type=parenthesis(separated_nonempty_list(COMMA, located(kyo_type))) RPARENT {
        KyoType.KyEffType {
            module_resolver;
            effect_name;
            eff_parametric_type;
        }
    }
    | parenthesis(separated_nonempty_list(AMPERSAND, located(kyo_effect))) {
        KyoType.KyEffList $1
    }

kyo_type:
    | kyo_ky_polymorphic {
        TyPolymorphic $1
    }
    | REF parenthesis(located(kyo_type)) {
        TRef $2
    }
    | HANDLER parenthesis(located(kyo_effect)) {
        TyHandler $2
    }
    | FUNCTION signature {
        TFunction $2
    }
    | parenthesis(separated_list(COMMA, located(kyo_type))) {
        match $1 with
        | [] -> TUnit
        | t::[] -> t.value
        | list -> TTuple list
    }
     
    | module_resolver=module_resolver name=located(IDENT) parametrics_type=generics(kyo_type) {
        match parametrics_type with
        | [] ->
            let ktype = match module_resolver with
                | _::_ -> TyIdentifier { module_resolver; name }
                | [] -> begin 
                    match name.value with
                    | "char" -> TChar
                    | "bool" -> TBool
                    | "unit" -> TUnit
                    | "string" -> TString
                    | "order" -> TOredered
                    | "int" -> TInteger
                    | _ -> TyIdentifier { module_resolver; name }
                end
            in
            ktype
        | _::_ ->
            TyParametricIdentifier {
                module_resolver;
                parametrics_type;
                name
            }

    }