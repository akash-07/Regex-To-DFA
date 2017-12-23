## Realizing Deterministic Finite State Automata from Regular Expressions

The code is the direct implementation of algorithm for converting regular expressions directly into DFA taken from the book **Compilers: Principles, Techniques and Tools by Aho, Ullman, Sethi and Lam**.

**Using the tool**

Make sure Halex package is installed on your computer or else use the
following command.

```
cabal install halex
```
Graphviz software needs to be installed to generate png image of the automaton directly. If not installed, a '.dot' file will be generated which can later be converted into a '.png' file using an online converted or can be opened using a dot file opener.

Installation instructions for Graphviz can be found here - https://graphviz.gitlab.io/download/

After having succesfully installed, type in the Regular expression in the source code as follows.

**Steps to Input the regular expression**

First define literals as follows so that it becomes easy to
write larger regex.

```
a = Literal 'a'
b = Literal 'b'
k = Literal 'k'
e = Epsilon
```
`e` stands for Epsilon

The input should be given in prefix from as shown.
1. `(a|b)* -> Star (Or a b)`

2. `(ab| e ) c -> Then (Or (Then a b) e) c`

An augmented regular expression is needed as the
algorithm demands so. The `augMentedReg` takes in your
regex and augments a `#` at the end.

Rewrite `reg` for the regular expression you want in the source code.

**Sample illustration**

Regular Expression - `(Then (Then (Star a) (Star b)) c)` which is `(a*b*c)`
(Written in source code)
```
Regex-To-DFA>ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :l regToDfa.hs
[1 of 1] Compiling Main             ( regToDfa.hs, interpreted )

regToDfa.hs:376:12: warning: [-Wtabs]
    Tab character found here, and in 19 further locations.
    Please use spaces instead.
Ok, modules loaded: Main.
*Main> main
Initial State:
[1,3,6]

States :
[1,3,6]
[8]
[3,6]
[]


Moves:
[] on c = []
[] on b = []
[] on a = []
[] on # = []
[3,6] on c = [8]
[3,6] on b = [3,6]
[3,6] on a = []
[3,6] on # = []
[8] on c = []
[8] on b = []
[8] on a = []
[8] on # = []
[1,3,6] on c = [8]
[1,3,6] on b = [3,6]
[1,3,6] on a = [1,3,6]
[1,3,6] on # = []


Final States:
[8]

```
**Image generated**

![astar-bstar-c](https://user-images.githubusercontent.com/24961068/34318880-0654f478-e7f8-11e7-911a-710b2e22cdef.png)
