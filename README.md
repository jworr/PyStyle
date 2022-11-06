# PyStyle

PyStyle is a design quality/style checker for Python 3 aimed at beginner programmers.
PyStyle looks for mistakes not detected in other linters such as `pep8` or `pylint`.
These kinds of mistakes not found by other linters because they assume a moderate amount of proficiency.
PyStyle is a rules-based model for Python code quality, it is written in Haskell and is easily extensible.


## Dependencies & Installation

Currently PyStyle is not organized as a proper cabal package, build it directly with `ghc`.
You will need the following tools/libraries:

* ghc
* cabal
* `language-python`

PyStyle depends on (language-python)[https://hackage.haskell.org/package/language-python].
Install the `language-python` as a library with this command: `cabal install language-python --lib`.
Finally build PyStyle with the following command `ghc --make PyStyle.hs -outputdir=lib`.


## Paper & Publication

There is a (paper about PyStyle)[https://arxiv.org/abs/2208.12654], but please note that a lot more rules have been added since publication.
