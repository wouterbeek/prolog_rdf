:- module(
  address_store,
  [
    create_address_store/1, % +NumberOfThreads:positive_integer
    address/3 % ?RdfTerm:iri
              % ?LexicalForm:atom
              % ?Md5:atom
  ]
).

/** <module> Address store

An example of how to use concurrent LDF processing (module rdf_concurrent.pl).

@author Wouter Beek
@author Filip Ilievski
@version 2016/01
*/

:- use_module(library(rdf/rdf_term)).

:- dynamic(address/3).



create_address_store(N):-
  tmp_set_prolog_flag(cpu_count, N, create_address_store).

create_address_store:-
  rdf_triple_concurrent(_, vcard:'street-address', _, store_addresses).

store_addresses(S, P, O, Md5):-
  ldf_triple(S, P, O, Md5),
  rdf_literal_lexical_form(O, Lex),
  Lex \== '',
  assert(address(S, Lex, Md5)),
  fail.
store_addresses(_, _, _, _).
