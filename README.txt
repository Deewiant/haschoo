Haschoo: the Scheme interpreter to be sneezed at
=======

Haschoo is a little R5RS [1] interpreter written in Haskell for a university
course. The name, if it's not obvious, is a portmanteau of "Haskell", "Scheme",
and "achoo", the last of which is meant to signify something along the lines of
it being a sneeze's worth of code and not a particularly serious endeavour.

For licensing information, see LICENSE.txt.

Usage
-----

Haschoo is quite spartan. It doesn't understand any command line options: if
given no parameters, it becomes a REPL which terminates on end-of-file,
otherwise it runs the file named by each argument as a standalone Scheme
program.

Building
--------

Haschoo is not written in standard Haskell 98, but depends on GHC-only (at the
time of writing) compiler extensions, so you'll need GHC to build it. Get it at
http://www.haskell.org/ghc/; version 6.10.2 was used for testing, but later
6.10 versions should work as well.

Nowadays you can also try the Haskell Platform, intended as a simple installer
to get you started quickly: http://hackage.haskell.org/platform/

If you have the cabal-install tool installed ([2] or the Haskell Platform),
installing Haschoo should be as simple as:

cabal configure
cabal build
cabal install

Or, for some reason, if you don't have Haschoo downloaded already, you should
be able to get the same results by doing:

cabal install Haschoo

If you don't have cabal-install, the following command, run from Haschoo's root
directory, should also work:

ghc --make Haschoo/Main.hs -o haschoo -O2

Goal
----

If Haschoo can be said to have had a goal, it is minimalism: it tries to be a
bit of a DeathStation 9000 (see e.g. [3]) for R5RS. Currently it achieves this
in the following ways:

- No non-R5RS procedures are implemented or recognized.
- No extensions are made to existing R5RS procedures.
- Only two optional procedures are implemented: - and / for more than two
  arguments.
- Literal lists, strings, and vectors are immutable, as allowed by R5RS 3.4.

The basic idea was that if something works in Haschoo, it should work in any
R5RS system. That's not quite the current situation, but it seems to be mostly
true, at least based on my limited testing.

Known bugs
----------

Due to time and energy constraints I didn't manage to iron out the following:

- Continuations, regretfully, are not implemented at all.

- Nested ellipses in macros don't always work correctly.
  (e.g. tests/macros/nested-ellipses.scm)

- The "read" procedure doesn't work very well: due to annoying technical
  issues, it works by reading one character at a time and then trying to parse
  the string gathered thus far, stopping on a successful parse. Hence reading
  "-1" gives "-" followed by "1".

- Quasiquotation has some silly bugs. For example, `(1 2 . ,(list 3 4)) results
  in '(1 2 unquote (list 3 4)) instead of '(1 2 (list 3 4)).

No doubt there are many other lurking bugs. Time and energy constraints tend to
limit testing as well.

References
----------

[1]: http://schemers.org/Documents/Standards/R5RS/
[2]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall
[3]: http://wikibin.org/articles/deathstation-9000.html
