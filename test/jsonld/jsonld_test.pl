:- module(
  jsonld_test,
  [
    run_test/1, % +Number:positive_integer
    run_tests/0
  ]
).

/** <module> JSON-LD tests

Largely derived from the JSON-LD 1.0 specification (W3C).

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_download)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(terms)).

:- rdf_meta
   isomorphic_graphs(r, r).


run_test(I) :-
  json_read_any('toRdf-manifest.jsonld', Manf),
  length(Manf.sequence, Len),
  must_be(between(1, Len), I),
  nth1(I, Manf.sequence, D),
  run_test0(D).

run_tests :-
  catch(
    forall(between(1, inf, I), run_test(I)),
    E,
    writeln(E)
  ).


%! run_test0(+D) is det.

run_test0(D):-
  rdf_equal(ex:input, G1),
  rdf_equal(ex:expect, G2),
  maplist(rdf_unload_graph, [G1,G2]),
  formatln("Id: ~s", [D.'@id']),
  write("Type: "), forall(member(Type, D.'@type'), write(Type)), nl,
  formatln("Name: ~s", [D.name]),
  formatln("Purpose: ~s", [D.purpose]),
  maplist(rdf_create_graph, [G1,G2]),
  json_read_any(D.input, DIn),
  print_dict(DIn), nl,
  rdf_load_file(D.input, [graph(G1)]),
  formatln("Parsed statements:"),
  rdf_print_graph(G1),
  rdf_load_file(D.expect, [graph(G2)]),
  (   isomorphic_graphs(G1, G2)
  ->  true
  ;   formatln("Expected statements:"),
      rdf_print_graph(G2)
  ).

%! isomorphic_graphs(+G1, +G2) is semidet.
% Is true if there is a consistent mapping between the blank nodes in G1
% and the blank nodes in G2 that makes both graphs equal.  This maps to
% the Prolog notion of *variant* if there was a canonical ordering of triples.

isomorphic_graphs(G1, G2) :-
  once(graph_permutation(G1, Ordered1)),
  graph_permutation(G2, Ordered2),
  variant(Ordered1, Ordered2), !.

graph_permutation(G, GPerm) :-
  rdf_graph_triples(G, Trips1),
  replace_bnodes_with_vars(Trips1, Trips2),
  partition(ground, Trips2, Ground, NonGround),
  sort(Ground, Sorted),
  append(Sorted, NonGroundPermutation, GPerm),
  permutation(NonGround, NonGroundPermutation).

replace_bnodes_with_vars(L1, L2) :-
  replace_bnodes_with_vars(L1, [], L2).

replace_bnodes_with_vars([], _, []).
replace_bnodes_with_vars([H1|T1], Map1, [H2|T2]) :-
  replace_bnodes_with_vars0(H1, Map1, H2, Map2),
  replace_bnodes_with_vars(T1, Map2, T2).

replace_bnodes_with_vars0(rdf(S1,P,O1), Map1, rdf(S2,P,O2), Map3) :-
  replace_bnode_with_var0(S1, Map1, S2, Map2),
  replace_bnode_with_var0(O1, Map2, O2, Map3).

replace_bnode_with_var0(B, Map, Var, Map) :-
  rdf_is_bnode(B),
  memberchk(B-Var, Map), !.
replace_bnode_with_var0(B, Map, Var, [B-Var|Map]) :-
  rdf_is_bnode(B), !.
replace_bnode_with_var0(T, Map, T, Map).

/*
% Test 0001: Plain literal with URIs
% Tests generation of a triple using full URIs and a plain literal.
%
% ```ntriples
% <http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/name> "Gregg Kellogg" .
% ```

test(1, _{
  '@id': 'http://greggkellogg.net/foaf#me',
  'http://xmlns.com/foaf/0.1/name': 'Gregg Kellogg'
}).

% Test 0002: Plain literal with CURIE from default context
% Tests generation of a triple using a CURIE defined in the default context.
%
% ```nquads
% <http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/name> "Gregg Kellogg" .
% ```

test(2, _{
  '@context': _{'foaf': 'http://xmlns.com/foaf/0.1/'},
  '@id': 'http://greggkellogg.net/foaf#me',
  'foaf:name': "Gregg Kellogg"
}).

% Test 0003: Default subject is BNode
% Tests that a BNode is created if no explicit subject is set.
%
% ```nquads
% _:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
% ```

test(3, _{
  '@context': _{'foaf': 'http://xmlns.com/foaf/0.1/'},
  '@type': 'foaf:Person'
}).

% Test 0004: Literal with language tag
% Tests that a plain literal is created with a language tag.
%
% ```nquads
% _:b0 <http://www.w3.org/2000/01/rdf-schema#label> "A plain literal with a lang tag."@en-us .
% ```

test(4, _{
  'http://www.w3.org/2000/01/rdf-schema#label': _{
    '@value': "A plain literal with a lang tag.",
    '@language': 'en-us'
  }
}).

% Test 0005: Extended character set literal
% Tests that a literal may be created using extended characters.
%
% ```nquads
% <http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/knows> _:b0 .
% _:b0 <http://xmlns.com/foaf/0.1/name> "Herman Iván"@hu .
% ```

test(5, _{
  '@id': 'http://greggkellogg.net/foaf#me',
  'http://xmlns.com/foaf/0.1/knows': _{
    'http://xmlns.com/foaf/0.1/name': _{
      '@value': "Herman Iván",
      '@language': hu
    }
  }
        }).

% Test 0006: Typed literal
% Tests creation of a literal with a datatype.
%
% ```nquads
% <http://greggkellogg.net/foaf#me> <http://purl.org/dc/terms/created> "1957-02-27"^^<http://www.w3.org/2001/XMLSchema#date> .
% ```

test(6, _{
  '@id':  'http://greggkellogg.net/foaf#me',
  'http://purl.org/dc/terms/created': _{
    '@value': "1957-02-27",
    '@type': 'http://www.w3.org/2001/XMLSchema#date'
  }
}).

% Test 0007: Tests 'a' generates rdf:type and object is implicit IRI
% Verify that 'a' is an alias for rdf:type, and the object is created as an IRI.
%
% ```nquads
% <http://greggkellogg.net/foaf#me> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
% ```

test(7, _{
  '@id': 'http://greggkellogg.net/foaf#me',
  '@type': 'http://xmlns.com/foaf/0.1/Person'
}).

% Test 0008: Test prefix defined in @context
% Generate an IRI using a prefix defined within an @context.
%
% ```nquads
% _:b0 <http://example.com/default#foo> "bar" .
% ```

test(8, _{
  '@context': _{
    d: 'http://example.com/default#'
  },
  'd:foo': "bar"
}).

% Test 0009: Test using an empty suffix
% An empty suffix may be used.
%
% ```
% _:b0 <http://example.com/default#> "bar" .
% ```

test(9, _{
  '@context': _{
    foo: 'http://example.com/default#'
  },
  'foo:': "bar"
}).

% Test 0010: Test object processing defines object
% A property referencing an associative array gets object from subject of array.
%
% ```nquads
% <http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/knows> <http://manu.sporny.org/#me> .
% <http://manu.sporny.org/#me> <http://xmlns.com/foaf/0.1/name> "Manu Sporny" .
% ```

test(10, _{
  '@context': _{
    foaf: 'http://xmlns.com/foaf/0.1/'
  },
  '@id': 'http://greggkellogg.net/foaf#me',
  'foaf:knows': _{
    '@id': 'http://manu.sporny.org/#me',
    'foaf:name': "Manu Sporny"
  }
}).

% Test 0011: Test object processing defines object with implicit BNode
% If no @ is specified, a BNode is created, and will be used as the object of an enclosing property.
%
% ```nquads
% <http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/knows> _:b0 .
% _:b0 <http://xmlns.com/foaf/0.1/name> "Dave Longley" .
% ```

test(11, _{
  '@context': _{
    foaf: 'http://xmlns.com/foaf/0.1/'
  },
  '@id': 'http://greggkellogg.net/foaf#me',
  'foaf:knows': _{
    'foaf:name': "Dave Longley"
  }
}).
*/
