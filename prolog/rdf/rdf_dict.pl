:- module(rdf_dict,
  [
    rdf_describe_dict/2 % ?Resource:rdf_term
                        % -Description:dict
  ]
).

/** <module> RDF dictionaries

Allows RDF resources to be described in and be found with
dictionary descriptions.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db)).





%! rdf_describe_dict(?Resource:rdf_term, -Description:dict) is det.
% Describes a resource in a key-value dictionary.
%
% ### Use
%
% ```prolog
% ?- [library(rdf/rdf_dict)].
% ?- [library(dict_ext)].
% ?- rdf_describe_dict(R, D), print_dict(D).
% ```

rdf_describe_dict(S, D):-
  rdf_subject2(S),
  aggregate_all(set(P), rdf2(S, P, _), Ps),
  maplist(property_pair(S), Ps, Pairs),
  dict_pairs(D, json, Pairs).
  
property_pair(S, P, P-Os):-
  aggregate_all(set(O), object_term(S, P, O), Os0),
  (Os0 = [O] -> Os = O ; Os = Os0).

object_term(S, P, O):-
  rdf2(S, P, O0),
  (   rdf_is_bnode(O0)
  ->  rdf_describe_dict(O0, O)
  ;   O = O0
  ).
