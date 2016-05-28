Algebraic Operations and Dissections of Types
=============================================

This talk discusses how regular algebraic data types


How to use
----------

The presentation can be created by typing `make`, assuming latex and lhs2TeX
is installed.

The presentation is also a literate Haskell file, and can be loaded into the
ghci REPL using `:l type_algebra.lhs`.

Made using
[beamer](https://en.wikibooks.org/wiki/LaTeX/Presentations),
[Metropolis theme](http://mirror.jmu.edu/pub/CTAN/macros/latex/contrib/beamer-contrib/themes/metropolis/doc/metropolistheme.pdf),
and [lhs2TeX](https://www.andres-loeh.de/lhs2tex/Guide2-1.17.pdf)


Note on regular data types
--------------------------

Regular data types are when the type constructor on the left side of
an equation appears in the same way on the right hand side (see [Bird,
Meertens](http://www.cs.ox.ac.uk/richard.bird/online/BirdMeertens98Nested.pd f)). For example, the following is a regular data type.

```haskell
data List a = Nil | Cons a (List a)
```

Whereas the following is a nested data type since the type to the type constructor `Nest` is different on the left and right hand side.

```haskell
data Nest a = NilN | ConsN a (Nest (a, a))
```


References
----------

- [Data types a la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) by Wouter Swierstra
- [Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures](http://www.cis.upenn.edu/~cis39903/static/clowns.pdf) by Conor McBride
- [Polynomial Functors Constrained by Regular
Expressions](http://ozark.hendrix.edu/~yorgey/pub/type-matrices.pdf) by Brent Yorgey and Dan Piponi
- [The algebra (and calculus!) of algebraic data types](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types) by Joel Burget
- [Zippers, Part 2: Zippers as Derivatives](https://pavpanchekha.com/blog/zippers/derivative.html#sec-2) by Pavel Panchekha.
- [The Algebra of Algebraic Data Types, Part 2](http://chris-taylor.github.io/blog/2013/02/11/the-algebra-of-algebraic-data-types-part-ii/) by Chris Taylor
- Dan Piponi's series on type divided differences: [part 1](http://blog.sigfpe.com/2009/09/finite-differences-of-types.html), [part 2](http://blog.sigfpe.com/2010/08/divided-differences-and-tomography-of.html), [part 3](http://blog.sigfpe.com/2010/08/constraining-types-with-regular.html)
