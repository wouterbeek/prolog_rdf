:- module(
  hdt_stat,
  [
    hdt_meta/4,                 % ?S, ?P, ?O, +G
    hdt_number_of_objects/2,    % ?G, -N
    hdt_number_of_properties/2, % ?G, -N
    hdt_number_of_subjects/2,   % ?G, -N
    hdt_number_of_triples/2     % ?G, -N
  ]
).

/** <module> HDT statistics

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt), []).
:- use_module(library(hdt/hdt__io)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   hdt_meta(r, r, o, r),
   hdt_number_of_triples(r, -).





%! hdt_meta(?S, ?P, ?O, ?G) is nondet.
%
% The following predicates are supported:
%
%   * `'<http://rdfs.org/ns/void#triples>'` with object `N^^xsd:integer`

hdt_meta(S, P, O, G) :-
  hdt__graph(G, Hdt),
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_number_of_objects(?G, -N) is nondet.

hdt_number_of_objects(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#distinctObjects>', N^^xsd:integer, G).



%! hdt_number_of_properties(?G, -N) is nondet.

hdt_number_of_properties(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#properties>', N^^xsd:integer, G).



%! hdt_number_of_subjects(?G, -N) is nondet.

hdt_number_of_subjects(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#distinctSubjects>', N^^xsd:integer, G).



%! hdt_number_of_triples(?G, -N) is nondet.

hdt_number_of_triples(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#triples>', N^^xsd:integer, G).
