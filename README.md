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


Create a new resource
---------------------

Make sure your IRI prefix has been registered with `rdf_register_prefix/2`:

```prolog
?- use_module(library(semweb/rdf_db)).
?- rdf_register_prefix(mh, 'http://moonhog.net/resource/').
```

Create a fresh IRI that names the new resource:

```prolog
?- [library(rdf/rdf_build)].
?- fresh_iri(mh, NewHog).
```

Assert that the new resource is a hog:

```prolog
?- rdf_assert_instance($NewHog, mh:'Hog', hog_db).
```

Let's look at the content of graph `hog_db`:

```prolog
?- [library(rdf/rdf_print)].
?- rdf_print_graph(hog_db).
mh:69d4a8902b9911e5bbbc18a905c4d41b rdf:type mh:Hog .
```



Make data-typed assertions
-------------------------

Assume that `NewHog` is bound to the IRI denoting a hog (see above),
we can now state its age:

```prolog
?- rdf_assert_literal($NewHog, mh:age, xsd:nonNegativeInteger, 2, hog_db).
?- rdf_assert_now($NewHog, mh:registered, hog_db).
```

Let's look at the contents of graph `hog_db`:

```prolog
?- [library(rdf/rdf_print)].
?- rdf_print_graph(hog_db).
mh:69d4a8902b9911e5bbbc18a905c4d41b rdf:type mh:Hog .
mh:69d4a8902b9911e5bbbc18a905c4d41b mh:age "2"^^xsd:nonNegativeInteger .
mh:69d4a8902b9911e5bbbc18a905c4d41b mh:registrationDate "2015-07-16T11:02:42+0200"^^xsd:dateTime .
```


RDF lists with members of mixed type
------------------------------------

RDF lists come in handy when we want to store a number of resources
in a given order.
However, the built-in predicates `rdfs_assert_list/[2,3]`
and `rdfs_list_to_prolog_list/2` does not allow recursive lists
to be asserted/read, nor do they allow easy assertion of
typed list elements.

In the following we assert an RDF list consisting of
(1) the integer `1`,
(2) the list consisting of the list containing atom `a`
and the floating point number `1.0`,
(3) the atom `b` accompanied by the language tag denoting
the English language as spoken in the Uniterd States.

```prolog
?- [library(rdf/rdf_list)].
?- rdf_assert_list([1,[[a],1.0],[en,'US']-b], _X).
```

As you can see this has used RDF's linked lists notation,
RDF and XSD datatypes for the non-list elements,
and nesting for the list elements:

```prolog
?- rdf_print_graph(user).
_:1 rdf:type rdf:List .
_:1 rdf:first "1"^^xsd:integer .
_:2 rdf:type rdf:List .
_:3 rdf:type rdf:List .
_:4 rdf:type rdf:List .
_:4 rdf:first "a"^^xsd:string .
_:4 rdf:rest rdf:nil .
_:3 rdf:first _:4 .
_:5 rdf:type rdf:List .
_:5 rdf:first "1.0"^^xsd:float .
_:5 rdf:rest rdf:nil .
_:3 rdf:rest _:5 .
_:2 rdf:first _:3 .
_:6 rdf:type rdf:List .
_:6 rdf:first "b"@en-US .
_:6 rdf:rest rdf:nil .
_:2 rdf:rest _:6 .
_:1 rdf:rest _:2 .
true.
```

The RDF list can be easily read back as Prolog list,
preserving both nesting and types:

```prolog
?- rdf_list($_X, Y).
Y = [1, [[a], 1.0], 'en-US'-b].
```

---

This library was programmed by [Wouter Beek](http://www.wouterbeek.com)
in 2015 and is distributed under the MIT License.
