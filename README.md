# Prototyping Abstract Binding Trees

While this might grow into more, currently this is a playground for me to learn
about Abstract Binding Trees.

I started off from the code in
http://semantic-domain.blogspot.de/2015/03/abstract-binding-trees.html

There are many possible TODOs
1. Use better algorithms -- the blog post declares it's using simple ones
   - For instance, use parallel substitution to avoid quadratic complexity.
   - Avoid concatenating string to build names, that's slow.
   - Replace the freshness generator with something faster.
     2. To allow easy manipulation of the resulting ASTs, implement smart constructors and extractors.
   Clarify how to use the resulting language. Neelk's example code accesses
   directly the implementation of ABTs. In particular, implement smart
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
