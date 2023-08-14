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
%token MINUS_SUP EQUAL_SUP
%token EFFECT TYPE EXTERNAL FUNCTION ANON_FUNCTION LET AS
%token CMP_LESS CMP_EQUAL CMP_GREATER
%token TRUE FALSE
%token LSQBRACE RSQBRACE
%token LBRACE RBRACE
%token LPARENT RPARENT
%token WILDCARD
%token WHILE MATCH VAL
%token REF
%token COLON SEMICOLON DOUBLECOLON EQUAL
%token PIPE DOT AMPERSAND
%token COMMA
%token EOF

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

%inline signature:
    | delimited(LPARENT, separated_list(COMMA, located(kyo_type)) , RPARENT) COLON located(kyo_type) {
        ($1, $3)
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
    | parenthesis(located(operator)) { $1 }

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

kyo_expr:
    | loc_var_identifier { EIdentifier $1 }
    | located(Integer_lit) { EInteger $1 }
    | LET kd_pattern=located(kyo_pattern) explicit_type=option(preceded(COLON, located(kyo_type))) EQUAL expression=located(kyo_expr) SEMICOLON next=located(kyo_expr) {
        let decl = {kd_pattern; explicit_type; expression} in
        EDeclaration (decl, next)
    }
    | ANON_FUNCTION parameters=parenthesis(separated_list(COMMA, located(kyo_pattern))) MINUS_SUP body=located(kyo_expr) {
        EAnonFunction {parameters; body}
    }
    | parenthesis(separated_list(COMMA, located(kyo_expr)) ) {
        match $1 with
        | [] -> EUnit
        | t::[] -> t.value
        | list -> ETuple list
    }

kyo_function_decl:
    | FUNCTION { failwith "TODO: kyo_function_decl" }

kyo_external_decl:
    | EXTERNAL { failwith "TODO: kyo_external_decl" }

kyo_global_decl:
    | LET loc_var_identifier EQUAL located(kyo_expr) { failwith "TODO: kyo_global_decl" }

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
    | FUNCTION name=located(IDENT) sign=signature {
        let effect_sig_param, effect_sig_return_type = sign in
        KEffSig {
            name;
            effect_sig_param;
            effect_sig_return_type
        }
    }

%inline kyo_ky_polymorphic:
    | located(PolymorphicVar) { 
        KyTyPolymorphic $1
    }

kyo_type:
    | kyo_ky_polymorphic {
        TyPolymorphic $1
    }
    | REF parenthesis(located(kyo_type)) {
        TRef $2
    }
    | delimited(LPARENT, separated_list(COMMA, located(kyo_type)) , RPARENT) {
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