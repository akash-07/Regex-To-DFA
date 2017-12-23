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
