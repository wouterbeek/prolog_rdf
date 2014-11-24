:- module(
  rdfs_subproperty_test,
  [
    subproperty_closure/1 % -SubpropertyClosure:ordset(iri)
  ]
).

/** <module> RDFS Subproperty Test

# Graph

```ntriples
ex:subPropertyOf2   ex:subPropertyOf1   ex:subPropertyOf1  .
ex:subPropertyOf1 rdfs:subPropertyOf  rdfs:subPropertyOf   .
ex:property2      rdfs:subPropertyOf    ex:property1       .
ex:property1        ex:subPropertyOf2   ex:property        .
```

# Results in SWI-Prolog 7.1.26

```ntriples
ex:property2      rdfs:subPropertyOf    ex:property1       .
ex:subPropertyOf2 rdfs:subPropertyOf    ex:subPropertyOf1  .
ex:subPropertyOf2 rdfs:subPropertyOf  rdfs:subPropertyOf   .
ex:subPropertyOf1 rdfs:subPropertyOf  rdfs:subPropertyOf   .
```

# Correct results

The correct results according to RDF 1.1 Semantics should contain
 the following:

```ntriples
ex:relation2  rdfs:subPropertyOf  ex:relation .
```

# Solution

The subproperty hierarchy can only be calculated as a fixpoint.

## Initialization

```latex
\leq_0 := \{ \leq \}
\forall s \in S . cl_0(s) := \{ \langle s, s \rangle \}
```

## Recursion

```latex
cl_{i+1}(s)_{\leq_i} :=
    \{
      s'
    \vert
      \exists s_1, \ldots, s_n (
        \bigwedge_{1 \leq j < n}
          \langle s_j, s_{j+1} \rangle \in \leq_i
      )
    \}
\leq_{i+1} :=
    \{
      \leq'
    \vert
      \langle \leq', \leq \rangle \in cl_{i+1}(\{ \leq \}_{\leq_i})
    \}
```

## Goal condition

```latex
\leq_{i+1} = \leq_i
```

---

@author Wouter Beek
@compat RDF 1.1 Semantics
@version 2014/11
*/

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(closure)).

:- rdf_register_prefix(ex, 'http://www.example.com/').

:- rdf_meta(rdf_property(r)).

:- initialization(init_subproperty_test).





%! subproperty_closure(-SubpropertyClosure:ordset(iri)) is det.
% Returns the transitive reflexive closure of `rdfs:subPropertyOf`
%  under itself (iterative calculation of fixpoint).

subproperty_closure(ClQ0):-
  rdf_global_id(rdfs:subPropertyOf, Q),
  subproperty_closure([Q], ClQ0).

subproperty_closure(ClQ, Sol):-
  aggregate_all(
    set(Q0),
    (
      member(Q, ClQ),
      closure0(rdfs_backward(ClQ), Q, Q0)
    ),
    ClQ0
  ),
  (   ClQ == ClQ0
  ->  Sol = ClQ
  ;   subproperty_closure(ClQ0, Sol)
  ).

rdfs_backward(ClQ, X, Y):-
  member(Q, ClQ),
  rdf(Y, Q, X).





% INITIALIZATION

init_subproperty_test:-
  rdf_assert(ex:subPropertyOf2, ex:subPropertyOf1,  ex:subPropertyOf1,  ex),
  rdf_assert(ex:subPropertyOf1, rdfs:subPropertyOf, rdfs:subPropertyOf, ex),
  rdf_assert(ex:property2,      rdfs:subPropertyOf, ex:property1,       ex),
  rdf_assert(ex:property1,      ex:subProperty2,    ex:property,        ex),

  forall(
    (
      rdf_property(P1),
      rdfs_subproperty_of(P1, P2),
      P1 \== P2
    ),
    (
      rdf_global_id(Prefix1:LocalName1, P1),
      rdf_global_id(Prefix2:LocalName2, P2),
      format(
        '~a:~a\trdfs:subPropertyOf\t~a:~a\n',
        [Prefix1,LocalName1,Prefix2,LocalName2]
      )
    )
  ).

rdf_property(ex:property1).
rdf_property(ex:property2).
rdf_property(ex:subPropertyOf2).
rdf_property(ex:subPropertyOf1).
rdf_property(rdfs:subPropertyOf).
