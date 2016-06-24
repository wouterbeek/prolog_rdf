plRdf
=====

Additional support for RDF 1.1 support for SWI-Prolog.



Installation
------------

  1. Install [SWI-Prolog](http://www.swi-prolog.org/Download.html).
  
  2. Run the following from the SWI-Prolog top-level:
  
     ```prolog
     ?- pack_install(plRdf).
     ```



Abbreviations
-------------

We use the following RDF-specific abbreviations for often occurring
variable names:

| **Variable name** | **Expansion**  |
|:-----------------:|:--------------:|
| `B`               | Blank node     |
| `C`               | Class          |
| `D`               | Datatype       |
| `G`               | Graph          |
| `I`               | Instance       |
| `Lit`             | Literal        |
| `Lex`             | Lexical form   |
| `Name`            | Name           |
| `Node`            | Node           |
| `NS`              | Namespace      |
| `O`               | Object term    |
| `P`               | Predicate term |
| `Prop`            | Property       |
| `Quad`            | Quadruple      |
| `Rel`             | Relation       |
| `S`               | Subject term   |
| `Term`            | Term           |
| `Triple`          | Triple         |
| `Tuple`           | Tuple          |
| `V`               | Value          |



Create resources and assert relations between them
--------------------------------------------------

Make sure your IRI prefix has been registered with
`rdf_register_prefix/2`:

```prolog
?- [library(rdf/rdf_build)].
?- rdf_register_prefix(ex, 'http://www.example.org/').
```

Create fresh IRIs that name two resources:

```prolog
?- fresh_iri(ex, [animal,hog], Hog1).
?- fresh_iri(ex, [animal,hog], Hog2).
```

Assert that the new resources are hogs:

```prolog
?- rdf_assert_instance($Hog1, ex:'Hog').
?- rdf_assert_instance($Hog2, ex:'Hog').
```

Assert that the two hogs know each other:

```prolog
?- rdf_assert($Hog1, foaf:knows, $Hog2).
```

Notice that we did not declare namespace `foaf` with
`rdf_register_prefix/2` since it is auto-loaded.

Let's look at the contents of our RDF graph:

```prolog
?- [library(rdf/rdf_print)].
?- rdf_print_graph(user).
〈ex:animal/hog/5b6151..., ∊, ex:Hog〉
〈ex:animal/hog/5b6155..., ∊, ex:Hog〉
〈ex:animal/hog/5b6151..., foaf:knows, ex:animal/hog/5b6155...〉
```

The triple dots indicate that IRI local names were elipsed to ensure
that every triple fits within an 80 character wide terminal.  The
appearance can be tweaked through options:

```prolog
?- rdf_print_graph(user, [ellip_ln(inf),logic_sym(false),style(turtle)]).
ex:animal/hog/5b61515c486b11e58bcb002268684c92 rdf:type ex:Hog .
ex:animal/hog/5b6155da486b11e5a357002268684c92 rdf:type ex:Hog .
ex:animal/hog/5b61515c486b11e58bcb002268684c92 foaf:knows ex:animal/hog/5b6155da486b11e5a357002268684c92 .
```

Option `ellip_ln(inf)` (elipsis localname) disables the use of
ellipses for IRI local names.  Option `logic_sym(false)` (logical
symbols) disables the replacement of some often occurring properties
with related logical symbolism.  Option `style(turtle)` displays
triples using a Turtle-like syntax i.o. the default tuple syntax.



Data-typed assertions
---------------------

Continuing our example of the two hogs, we can now assert the first
hog's age:

```prolog
?- rdf_assert_literal($Hog1, ex:age, xsd:nonNegativeInteger, 2).
?- rdf_assert_now($Hog1, ex:registrationDate).
```

Let's look at the contents of our graph:

```prolog
?- rdf_print_graph(user).
〈ex:animal/hog/5b6151..., ∊, ex:Hog〉
〈ex:animal/hog/5b6155..., ∊, ex:Hog〉
〈ex:animal/hog/5b6151..., foaf:knows, ex:animal/hog/5b6155...〉
〈ex:animal/hog/5b6151..., ex:age, "2"^^xsd:nonNegativeInteger〉
〈ex:animal/hog/5b6151..., ex:registrationDate, "2015-08-22T01:16:03Z"^^xsd:dateTime〉
```

If you do not want to choose an RDF datatype (like
`xsd:nonNegativeInteger` above) then you can do the following to let
the library choose an appropriate type for you:

```prolog
?- rdf_assert_literal_pl($Hog2, ex:age, 2.3).
?- rdf_assert_literal_pl($Hog2, ex:age, 23 rdiv 10).
?- rdf_assert_literal_pl($Hog2, rdfs:comment, "This is a fine hog.").
```

Our graph now has the following contents:

```prolog
?- rdf_print_graph(user).
〈ex:animal/hog/4d5018..., ∊, ex:Hog〉
〈ex:animal/hog/4d5020..., ∊, ex:Hog〉
〈ex:animal/hog/4d5018..., foaf:knows, ex:animal/hog/4d5020...〉
〈ex:animal/hog/4d5018..., ex:age, "2"^^xsd:nonNegativeInteger〉
〈ex:animal/hog/4d5018..., ex:registrationDate, "2015-08-22T02:27:15Z"^^xsd:dateTime〉
〈ex:animal/hog/4d5020..., ex:age, "2.3"^^xsd:float〉
〈ex:animal/hog/4d5020..., ex:age, "2.3"^^xsd:decimal〉
〈ex:animal/hog/4d5020..., rdfs:comment, "This is a fine hog."^^xsd:string〉
```

Notice that the RDF datatypes actually matter: `"2.3"^^xsd:float` and
`"2.3"^^xsd:decimal` denote different RDF resources even though their
lexical expressions are the same.  This library comes with support for
reading back literals as Prolog values:

```prolog
?- [library(rdf/rdf_read)].
?- rdf_literal($Hog2, ex:age, D, V).
D = http://www.w3.org/2001/XMLSchema#float,
V = 2.3 ;
D = http://www.w3.org/2001/XMLSchema#decimal,
V = 23 rdiv 10
```



RDF lists with members of mixed type
------------------------------------

RDF lists come in handy when we want to store a number of resources in
a given order.  However, the built-in predicates
`rdfs_assert_list/[2,3]` and `rdfs_list_to_prolog_list/2` in
`library(semweb/rdfs)` do not support recursive lists nor do they
allow easy assertion of typed list elements.

In the following we assert an RDF list consisting of the following
elements (in the order indicated):

  1. The integer `1`.
  
  2. The list consisting of the list containing atom `a` and the
     floating point number `1.0`.
  
  3. The atom `b` accompanied by the language tag denoting the English
     language as spoken in the United States.

The last argument denotes the named graph (`list_test`) in which the
RDF list is asserted.  All RDF assertion predicates in
`library(rdf/rdf_list)` come with variants with and without a graph
argument.

```prolog
?- [library(rdf/rdf_list)].
?- rdf_assert_list([1,[[a],1.0],[en,'US']-b], _X, list_test).
```

The list has been asserted using the RDF linked lists notation.  RDF
and XSD datatypes are used for the non-list elements and nesting is
used for the list elements:

```prolog
?- rdf_print_graph(list_test, [abbr_list(false)]).
〈_:2, ∊, rdf:List〉
〈_:2, rdf:first, "1"^^xsd:integer〉
〈_:3, ∊, rdf:List〉
〈_:4, ∊, rdf:List〉
〈_:5, ∊, rdf:List〉
〈_:5, rdf:first, "a"^^xsd:string〉
〈_:5, rdf:rest, rdf:nil〉
〈_:4, rdf:first, _:5〉
〈_:6, ∊, rdf:List〉
〈_:6, rdf:first, "1.0"^^xsd:float〉
〈_:6, rdf:rest, rdf:nil〉
〈_:4, rdf:rest, _:6〉
〈_:3, rdf:first, _:4〉
〈_:7, ∊, rdf:List〉
〈_:7, rdf:first, "b"@en-US〉
〈_:7, rdf:rest, rdf:nil〉
〈_:3, rdf:rest, _:7〉
〈_:2, rdf:rest, _:3〉
```

Since the RDF linked list notation is rather verbose library **plRdf**
allows RDF lists to be read back as Prolog lists, preserving both
nesting and taking the RDF datatypes into account:

```prolog
?- rdf_list($_X, Y).
Y = [1, [[a], 1.0], 'en-US'-b].
```


Triple store profiles
---------------------

**plRdf** comes with different profiles for storing triples.


### Plain RDF assertion

Load the 'plain' RDF profile in the following way:

```prolog
?- [library(profile/profile_rdf)].
```

This profile is similar to `library(semweb/rdf_db)`, allowing plain
triples to be asserted/retrieved/retracted.  The only difference is
that `rdf_assert/4` does not given an error in mode `(+,+,+,-)` but
calls `rdf_assert/3`.


### Generalized RDF assertion

Load the generalized RDF profile in the following way:

```profile
?- [library(profile/profile_gen)].
```

This profile extends the plain profile by allowing literals to appear
in the subject position.  This allows all RDF(S) and OWL entailment to
be expressed in terms of RDF (something which is not possible in the
plain profile).


### OWL assertion

Load the OWL profile in the following way:

```profile
?- [library(profile/profile_owl)].
```

This profile extends the generalized RDF profile by storing relations
between identity sets i.o. individual resources.  This significantly
speeds up calculating the identity closure (`owl:sameAs`) and avoids
OWL entailment to store the identity closure in an exponential way.


# Variable names

We use the following variable names to denote often occurring
concepts.

| **Variable name** | **Intended expansion** |
|:-----------------:|:----------------------:|
| `BN`              | Blank node             |
| `C`               | Class                  |
| `D`               | Datatype               |
| `G`               | Graph                  |
| `I`               | Instance               |
| `Lit`             | Literal                |
| `Lex`             | Lexical expression     |
| `O`               | Object term            |
| `P`               | Predicate term         |
| `P`               | Property               |
| `S`               | Subject term           |
| `T`               | RDF term               |
| `V`               | Value                  |



# Disk or memory?

  * Disk-based SGP access: `hdt/4`, `hdt_<NAME>/<ARITY>`.
  
  * Memory-based SGP access: `rdf/[3,4]`, `rdf_<NAME>/<ARITY>`.
  
  * Shared private implementation: `rdf_<NAME>0/<ARITY+1>`, where:
  
    * `hdt_<NAME>(<ARTIY-ARGS>) :- rdf_<NAME>0(disk, <ARITY-ARGS>).`
    
    * `rdf_<NAME>(<ARTIY-ARGS>) :- rdf_<NAME>0(mem, <ARITY-ARGS>).`
