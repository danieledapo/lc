# All you need is a lambda

Little experiments with [lambda
calculus](https://en.wikipedia.org/wiki/Lambda_calculus). As of now there are
two different implementations of lambda calculus. There is a vanilla one that is
just a straight implementation of lambda calculus as descibed by [Alonzo
Church](https://en.wikipedia.org/wiki/Alonzo_Church). On top of that there is an
"Enhanced" lambda calculus that also supports `let` statements.

Also, there are two interpreters(a.k.a reducers): `deBruijnInterpreter` that
uses [De Bruijn Index](https://en.wikipedia.org/wiki/De_Bruijn_index) and
therefore is strict in the sense that all the referenced variables must be in
scope before referencing them. The second one is `dynamicInterpreter` that
allows to reference not yet defined variables.

For example `(λfoo. (foo x) b)(λx y. y (x x))` will reduce to `x (b b)` if using
`deBruijnInterpreter`, but it will reduce to `b (b b)` when using
`dynamicInterpreter`.

## Quickstart

Be sure to have [`stack`](https://github.com/commercialhaskell/stack) installed.

```shell
$ stack build
$ stack exec lc
λ >> λx.x
λx.x
λ >> (λx.x) a
a
λ >> mytrue = \x y. x
λx.λy.x
λ >> myfalse = \x y. y
λx.λy.x
λ >> true
λx.λy.x
λ >> false
λx.λy.y
λ >> true t f
t
λ >> false t f
f
λ >> (not true) t f
f
λ >> 0
λf.λx.x
λ >> 0 f x
x
λ >> (succ 0) f x
f x
```

## Stdlib

The repl also provides a limited form of standard library. Here are some bindings:

- `id`, `const` and `s` also called [SKI combinators](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
- `omega` and [`Y`](https://en.wikipedia.org/wiki/Fixed-point_combinator) combinators
- [`church booleans`](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans)
- [`church integers`](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals)

Feel free to grep for `stdLib` in `app/Main.hs` though :wink:.
