%{

    open Util
    open KyoumiAst
    open KyoumiAst.KyoType
    open KyoumiAst.KNodeEffect
%}

%token <string> IDENT
%token <string> PolymorphicVar
%token <string> PolymorphicEff
%token <string> Module_IDENT
%token <float> Float_lit
%token <int> Integer_lit
%token EFFECT TYPE EXTERNAL FUNCTION ANON_FUNCTION LET AS
%token LBRACE RBRACE
%token WILDCARD
%token WHILE MATCH VAL
%token COLON SEMICOLON DOUBLECOLON
%token  COMMA

%start kyo_module

%type <KyoumiAstkyo_module.kyo_module> kyo_module

%%


%inline located(X): x=X {
  Position.located_value $startpos $endpos x
}

%inline module_resolver:
    | mp=located( loption(terminated(separated_nonempty_list(DOUBLECOLON, Module_IDENT), DOT)) ) { 
        mp
    }

%inline trailing_separated_list(sep, elt):
    | nonempty_list(terminated(elt, sep)) { $1 }
    | separated_nonempty_list(sep, elt) { $1 }


%inline generics(X):
    | loption(delimited(INF, separated_nonempty_list(COMMA, located(X)), SUP)) {
        $1
    }

%inline signature:
    | delimited(LPARENT, located(kyo_type) ,RPARENT) COLON located(kyo_type) {

    }

kyo_module:
    | nodes = list(kyo_node) { nodes }

kyo_node:
    | kyo_effect_decl { KNEffect $1 }
    | kyo_enum_decl { KNEnum $1 }
    | kyo_record_decl { KNRecord $1 }
    | kyo_external_decl { KNExternal $1 }
    | kyo_function_decl { KNFunction $1 }
    | kyo_global_decl { KNGlobal $1 }



kyo_effect_decl: 
    | EFFECT IDENT generics delimited(LBRACE, , RBRACE)

kyo_effect:
    | 

kyo_type:
    | located(PolymorphicVar) {
        TyPolymorphic (KyTyPolymorphic $1)
    }
    | REF delimited(INF, located(kyo_type) , SUP) {
        TRef $2
    }
    | delimited(LPARENT, separated_list(located(kyo_type)) , RPARENT) {
        match $1 with
        | [] -> TUnit
        | t::[] -> t.value
        | list -> TTuple list
    }
    | FUNCTION 
    | module_resolver=module_resolver name=IDENT parametrics_type=generics(kyo_type) {
        match parametrics_type with
        | [] ->
            let ktype = match module_resolver with
                | _::_ -> TyIdentifier { module_resolver; name }
                | [] -> begin 
                    match name with
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