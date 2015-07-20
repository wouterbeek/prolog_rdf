plRdf
=====

RDF support library for SWI-Prolog.


# Installation

  1. [Install SWI-Prolog](http://www.swi-prolog.org/Download.html)
  2. `$ swipl`
  3. `?- pack_install(plRdfLite).`


# Create a new resource

Make sure your IRI prefix has been registered with `rdf_register_prefix/2`:

```prolog
?- use_module(library(semweb/rdf_db)).
?- rdf_register_prefix(mh, 'http://moonhog.net/resource/').
```

Create a fresh IRI that names the new resource:

```prolog
?- use_module(library(rdf/rdf_build)).
?- fresh_iri(mh, NewHog).
```

Assert that the new resource is a hog:

```prolog
?- rdf_assert_instance($NewHog, mh:'Hog', hog_db).
```

Let's look at the content of graph `hog_db`:

```prolog
?- rdf_print_graph(hog_db).
mh:69d4a8902b9911e5bbbc18a905c4d41b rdf:type mh:Hog .
```

# Make datatyped assertions

```prolog
?- rdf_assert_literal($NewHog, mh:age, xsd:nonNegativeInteger, 2, hog_db).
?- rdf_assert_now($NewHog, mh:registered, hog_db).
```

Let's look at the contents of graph `hog_db`:

```prolog
?- rdf_print_graph(hog_db).
mh:69d4a8902b9911e5bbbc18a905c4d41b rdf:type mh:Hog .
mh:69d4a8902b9911e5bbbc18a905c4d41b mh:age "2"^^xsd:nonNegativeInteger .
mh:69d4a8902b9911e5bbbc18a905c4d41b mh:registrationDate "2015-07-16T11:02:42+0200"^^xsd:dateTime .
```

---

This library is programmed by [Wouter Beek](http://www.wouterbeek.com)
in 2015 and distributed under the MIT License.

