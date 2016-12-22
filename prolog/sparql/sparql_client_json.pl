:- module(
  sparql_client_json,
  [
    sparql_read_json_result/2 % +Source, -Result
  ]
).

/** <module> SPARQL JSON result set (client parser)

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(json_ext)).
:- use_module(library(semweb/rdf11)).





%! sparql_read_json_result(+Source, -Result) is det.
%
% The returned Result term is of the form:
%
%   * ask(Bool)
%
%     Where Bool is either =true= or =false=
%
%   * select(VarNames, Rows)
%
%     Where VarNames is a term v(Name, ...) and Rows is a list of
%     row(....) containing the column values in the same order as the
%     variable names.

sparql_read_json_result(Source, Result) :-
  json_read_any(Source, Dict),
  json_result(Dict, Result).

json_result(Dict, ask(Result)) :-
  get_dict(boolean, Dict, Result), !.
json_result(Dict, select(VarNames,Rows)) :-
  get_dict_path([head,vars], Dict, VarNames0),
  maplist(atom_string, VarNames, VarNames0),
  get_dict_path([results,bindings], Dict, Bindings),
  maplist(json_row(VarNames), Bindings, Rows).

json_row(VarNames, Binding, Row) :-
  maplist(json_cell(Binding), VarNames, Row).

json_cell(Binding, VarName, Term) :-
  get_dict(VarName, Binding, Dict), !,
  get_dict(type, Dict, Type0),
  get_dict(value, Dict, Val0),
  maplist(atom_string, [Type,Val], [Type0,Val0]),
  json_term(Type, Val, Dict, Term).
json_cell(_, _, '$null$').

json_term(uri, Iri, _, Iri) :- !.
json_term(bnode, BNode, _, BNode) :- !.
json_term(literal, Lex, Dict, Lit) :-
  (   get_dict(datatype, Dict, D0),
      atom_string(D, D0)
  ->  Lit = Lex^^D
  ;   get_dict('xml:lang', Dict, LTag0),
      atom_string(LTag, LTag0)
  ->  Lit = Lex@LTag
  ;   rdf_equal(xsd:string, D),
      Lit = Lex^^D
  ).
