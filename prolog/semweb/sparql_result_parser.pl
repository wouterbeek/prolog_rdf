:- module(
  sparql_result_parser,
  [
    sparql_result/2 % +In, -Result
  ]
).

/** <module> SPARQL result parser

@author Wouter Beek
@compat SPARQL Query Results XML Format (Second Edition)
@see https://www.w3.org/TR/rdf-sparql-XMLres/
@version 2017/08-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(xml/xml_ext)).

:- rdf_meta
   sparql_term(+, o).

:- thread_local
   result/1.





%! sparql_result(+In:stream, -Result:list(compound)) is nondet.

sparql_result(In, Result) :-
  retractall(result(_)),
  call_on_xml(In, [result], sparql_result),
  retract(result(Result)).

sparql_result([element(result,_,Bindings)]) :-
  maplist(sparql_binding, Bindings, Result),
  assert(result(Result)).

sparql_binding(element(binding,Attrs,Dom), Binding) :-
  memberchk(name=Name, Attrs),
  sparql_term(Dom, Term),
  Binding =.. [Name,Term].

sparql_term([element(bnode,_,[BNode])], BNode) :- !.
sparql_term([element(uri,_,[Uri])], Uri) :- !.
sparql_term([element(literal,['xml:lang'=LTag],[Lex])], Str@LTag) :- !,
  atom_string(Lex, Str).
sparql_term([element(literal,[datatype=D],[Lex])], Val^^D) :- !,
  rdf_lexical_value(D, Lex, Val).
sparql_term([element(literal,[],[Lex])], Str^^xsd:string) :- !,
  atom_string(Lex, Str).
sparql_term(Dom, _) :-
  domain_error(sparql_term, Dom).
