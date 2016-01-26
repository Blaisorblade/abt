# Prototyping Abstract Binding Trees

While this might grow into more (say, a usable library), currently this is a
playground for me to learn about Abstract Binding Trees.

I started off from the code in
http://semantic-domain.blogspot.de/2015/03/abstract-binding-trees.html.

Scala-specific goodies:
- simplify manipulating ABTs through extractors, so you can write `Lam("x", Var("x"))` instead of the underlying
  `TermInt(Set(),_Tm(_Lam(TermInt(Set(),__Abs(x,TermInt(Set(x),_Var(x)))))))`.
  Unfortunately, the latter is still visible through `.toString`, and calls for an
  implemetation of pretty-printing.

There are many possible TODOs:

1. Use better algorithms -- the blog post declares it's using simple ones
   - [x] For instance, use parallel substitution to avoid quadratic complexity (done in 1db5ee73a1cec39c6674a04b417b30bd212a0626).
   - [x] Avoid concatenating string to build names, that's slow (see https://github.com/Blaisorblade/abt/compare/topic/faster-names).
   - [x] Replace the freshness generator with something faster.
2. Review those fancier algorithms to ensure they're correct --- ABT
   mean you need to get binding right only once, but you still have to.
3. Try out whether the implementation can use other techniques: Must one use
   names to implement ABTs, or would it be possible to use, say, deBrujin
   indexes? ABTs relate to HOAS, and are in fact probably a more awkward
   approach to achieve the same benefits, but is it possible to implement the
   ABT interface *using* HOAS? What's the design space?

# Motivation

Usually, when implementing languages with binding, you have lots of
language-specific boilerplate, of size proportional to the size of the
language's grammar and to the number of languages. This becomes worse when a
project contains multiple languages with binding.

Abstract Binding Trees promise to minimize the language-specific overhead; in
particular, one needs only to take the language's syntax, remove binding
information, turn the algebraic data types into a *pattern functor* and
implement an instance of Foldable for this functor; the latter step is
mechanical enough that it can even be automated (and it is in many languages).
