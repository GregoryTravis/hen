hen
====

### Overview

hen is a dynamically typed pure functional language with pattern-matching,
monad-ish IO and basic FFI support.

I was inspired by Haskell, which I had been reading about for years.  I didn't
want the type system, I just wanted capitalized constructors and pattern
matching.  I was so obsessed with having a Scheme-like language with pattern
matching that before starting the implementation of the language, I wrote a
crude pattern-matching macro for Scheme, so I could match patterns while I
matched patterns.

hen considers functions to be tree rewriting, which seemed at the time to be a
really elegant and simple core for a semantics.  I really wanted the rewrite
engine to be as simple as possible, and did a lot of rewrites, using a variety
of language cores -- SKI combinators, CPS, metacircular interpreter.  All the
implementations shared a lambda calculus interpreter written in C, with the hen
code compiled to C data structures.

I didn't write a garbage collector, I've never written one, I just used the
Boehm garbage collector.

### History and motivation

This was my third language project.  My first language project was about
imperative lenses and coroutines, and my second was about couroutines and wound
up just being Javascript with first-class continuations.

By the time I started this project, I had been reading about Haskell for years.
I was a big fan, but for some reason did not try to actually use it.  I had
always been a Scheme fanboy and just wanted an untyped pure functional language
and I didn't want to understand any more type theory.  (I have since changed my
mind and am completely switched over to Haskell and my next and final language
project, TMI.)

So Haskell without types is really just capitalized constructors.  hen started
under a different name, 'Swat', which stands for 'Scheme With Algebraic Types'.
Yeah, I know, you can't have algebraic datatypes without a type system, but I
really didn't want to implement Hindley-Milner.  (I have since changed my mind
and implemented it in Scheme, and almost started implementing it in Haskell.
That's when I fell in love with Haskell and was like, why would I try to
recreate this?)

So Swat was really a simple preprocessor that provided Haskell-style pattern
matching.  I threw in a few other things, like partial application and a hacky
'[]' syntax.  I found it really fun to program in, and actually used it to
generate two "albums" of procedural music.  I got really obsessed with thinking
about pattern matching as tree rewriting, thought of a way to inline tree
rewrites, and decided to start from scratch, resulting in hen.

I created an idiosyncratic FFI autogen module using a GCC command line option
that no longer exists.  The option dumped a flat file describing the entire
program's call graph, including types, and wrote a Perl (!) script to extract
function signatures from this.  I call it idiosyncratic because it only really
works with APIs that are largely simple scalar types and arrays, which happens
to be true of a lot of OpenGL.

Developed under MzScheme (precursor of Racket) and ported partially or
completely to MIT Scheme, Racket, and Chez Scheme.
