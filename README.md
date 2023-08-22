# Kyoumi

Kyoumi is to be an somewhat functional programming language with a kind of like of OCaml with algebraic effect like in Koka

This language will be an opportunity to work on other concept than I haven't been able to work in [Kosu](https://github.com/EruEri/kosu-lang):
- Garbage collector
- Algebraic effect
- Pattern matching and branches completeness
- Hindley-Milner type system
- Compiler optimization 


An example of the syntax (which will probably be subject of changes):


```rust

type option('a) = 
    | none
    | some('a)

type coordinate = {
    x: int,
    y: int
}

effect raise('a) {

    val default : int
    // symbole a backtick (`) are polymorphic variable effect
    val raise: fn(string) -> $`a 'a
}

external `+`(int, int) : $()  int = "c_implementation"
//                         ^     ^
//                         |     | 
//                       effect type

let const_value = 10

let throw = fun() ->
    let res = perform raise.raise("A message") in
    // or 
    let perform2 = &. raise.raise("A another syntax") in
    res + perform2
    // which is a syntaxic sugar for
    `+`(res, perform2)
end

let main: int = 
    let handler_raise = handler raise {
        let default = 10
        fn raise(message) : `a int = 
            resume 30
    } in
    let res = throws() with handler_raise in
    res

```