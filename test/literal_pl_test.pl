:- module(literal_pl_test, [run_tests/0]).

/** <module> Test rdf_assert_literal_pl/[3,4]

You can run this script in the following way:

```prolog
?- pack_install(plRdf).
?- [library('../test/test_literal_pl')].
?- run_tests.
```

After running the tests in this module you should be able to
read the triples back in as follows:

```prolog
?- rdf_literal(S, P, D, O, test_literal_pl).
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',
O = one-[en, 'US'] ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#string',
O = '1000' ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#date',
O = date(1000, 10, 1, _G302, _G303, _G304, 0, 0, -) ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#dateTime',
O = date(1000, 10, 1, 10, 11, 11, 0, 0, -) ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#time',
O = date(_G897, _G898, _G899, 11, 11, 11, 0, 0, -) ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#float',
O = 1.11111111 ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML',
O = [element(html, [], [element(p, [], [monkey])])] ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#integer',
O = 1 ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#decimal',
O = 111111111 rdiv 100000000 ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/2001/XMLSchema#string',
O = '1' ;
S = 'http://www.example.org/s',
P = 'http://www.example.org/p',
D = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',
O = [element(something, [], ['\n  ', element(inner, [], []), '\n'])] ;
```

Notice that all Prolog types covered by rdf_assert_literal_pl/[3,4] are
preserved when read back in through rdf_literal/[3,4] *except for SWI7
strings*.

---

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(rdf/rdf_build)).
:- reexport(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_prefix(ex, 'http://www.example.org/').





run_tests:-
  G = test_literal_pl,
  % Atom
  rdf_assert_literal_pl(ex:s, ex:p, '1000', G),
  % date/3
  rdf_assert_literal_pl(ex:s, ex:p, date(1000,10,1), G),
  % date/9
  rdf_assert_literal_pl(ex:s, ex:p, date(1000,10,1,10,11,11.111,111,-,true), G),
  % time/3
  rdf_assert_literal_pl(ex:s, ex:p, time(11,11,11.111), G),
  % float
  rdf_assert_literal_pl(ex:s, ex:p, 1.11111111, G),
  % HTML DOM
  rdf_assert_literal_pl(ex:s, ex:p, [element(html,[],[element(p,[],[monkey])])], G),
  % integer
  rdf_assert_literal_pl(ex:s, ex:p, 1, G),
  % Pair denoting a language-tagged string.
  rdf_assert_literal_pl(ex:s, ex:p, one-'en-US', G),
  % rational
  rdf_assert_literal_pl(ex:s, ex:p, 111111111 rdiv 100000000, G),
  % string
  rdf_assert_literal_pl(ex:s, ex:p, "1", G),
  % XML DOM
  rdf_assert_literal_pl(ex:s, ex:p, [element(something,[],[element(inner,[],[])])], G),

  rdf_print_graph(G, [ellip_lit(inf),ellip_iri(inf)]).
