# hsProust

A Haskell implementation of Proust, an interactive proof assistant for [intuitionistic propositional logic ](https://plato.stanford.edu/entries/logic-intuitionistic/) described in the online textbook 
[*Logic and Computation Interwined*](https://cs.uwaterloo.ca/~plragde/flaneries/LACI/index.html) by Prabhakar Ragde. The deductive system we
adopt is given in Chapter 2.

# Syntax
We do not require expressions to be fully parenthesized as in original Proust. Except for λ and → which are right associative,
all other operators associate to the left. Operator precedences are shown in the grammar. We allow haskell style variable names.
## Grammar for roof terms
```
expr = λ x ⇒ expr
     | expr : t
     | expr . expr
     | ∧-intro expr expr
     | ∧-elim0 expr expr
     | ∧-elim1 expr expr 
     | ∨-intro0 expr expr
     | ∨-intro1 expr expr
     | ∨-elim expr expr expr 
     | ⊥-elim expr
     | ?
     | x
```

## Grammar for types
```
t = t → t
  | t ∨ t
  | t ∧ t
  | ¬ t
  | ⊥
  | x
 ```

# Demo
*Check validity of proof terms:*
![Type check](demo/demo2.png)

*Interactively proving:*
![prototype](demo/demo1.gif)

# Build & Run
``` bash
$ cabal build
$ cabal run
```
