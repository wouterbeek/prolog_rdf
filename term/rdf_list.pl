:- module(
  rdf_list,
  [
    rdf_assert_list/4, % +PrologList:list
                       % ?RdfList:or([bnode,iri])
                       % ?Graph:atom
                       % +Options:list(nvpair)
    rdf_list/1, % ?List:or([bnode,iri])
    rdf_list/2, % +RdfList:or([bnode,iri])
                % -PrologList:list
    rdf_list/4, % +RdfList:or([bnode,iri])
                % -PrologList:list
                % ?Graph:atom
                % +Options:list(nvpair)
    rdf_list_after/2, % ?After:rdf_term
                      % ?Before:rdf_term
    rdf_list_after/4, % ?After:rdf_term
                      % ?Before:rdf_term
                      % ?List:or([bnode,iri])
                      % ?Graph:atom
    rdf_list_before/2, % ?Before:rdf_term
                       % ?After:rdf_term
    rdf_list_before/4, % ?Before:rdf_term
                       % ?After:rdf_term
                       % ?List:or([bnode,iri])
                       % ?Graph:atom
    rdf_list_directly_after/4, % ?After:rdf_term
                               % ?Before:rdf_term
                               % ?List:or([bnode,iri])
                               % ?Graph:atom
    rdf_list_directly_before/4, % ?Before:rdf_term
                                % ?After:rdf_term
                                % ?List:or([bnode,iri])
                                % ?Graph:atom
    rdf_list_first/2, % ?List:or([bnode,iri])
                      % ?First:rdf_term
    rdf_list_first/3, % ?List:or([bnode,iri])
                      % ?First:rdf_term
                      % ?Graph:atom
    rdf_list_last/2, % ?List:or([bnode,iri])
                     % ?Last:rdf_term
    rdf_list_last/3, % ?List:or([bnode,iri])
                     % ?Last:rdf_term
                     % ?Graph:atom
    rdf_list_length/3, % ?List:or([bnode,iri])
                       % ?Length:number
                       % ?Graph:atom
    rdf_list_member/2, % ?Element:rdf_term
                       % ?List:or([bnode,iri])
    rdf_list_member/3, % ?Element:rdf_term
                       % ?List:or([bnode,iri])
                       % ?Graph:atom
    rdf_list_nth0/4, % ?Index:nonneg
                     % ?List:or([bnode,iri])
                     % ?Element:rdf_term
                     % ?Graph:atom
    rdf_list_triples/3 % +PrologList:list
                       % -RdfList:bnode
                       % -Triples:list(compound)
  ]
).

/** <module> RDF list

Support for RDF lists.

@author Wouter Beek
@compat [RDF Schema 1.1](http://www.w3.org/TR/2014/REC-rdf-schema-20140225/)
@tbd Add RDF list retraction.
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/05, 2013/07-2013/09,
         2014/01-2014/02, 2014/06, 2014/10-2015/01
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plc(generics/closure)).
:- use_module(plc(generics/lambda_meta)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).

:- predicate_options(rdf_assert_list/4, 4, [
  datatype(+atom)
]).
:- predicate_options(rdf_list/4, 4, [
  datatype(+atom),
  recursive(+boolean)
]).

:- rdf_meta(rdf_assert_list(o,r,?,+)).
:- rdf_meta(rdf_list(r)).
:- rdf_meta(rdf_list(r,-)).
:- rdf_meta(rdf_list(r,-,?,+)).
:- rdf_meta(rdf_list_after(o,o)).
:- rdf_meta(rdf_list_after(o,o,r,?)).
:- rdf_meta(rdf_list_before(o,o)).
:- rdf_meta(rdf_list_before(o,o,r,?)).
:- rdf_meta(rdf_list_directly_after(o,o,r,?)).
:- rdf_meta(rdf_list_directly_before(o,o,r,?)).
:- rdf_meta(rdf_list_first(r,o)).
:- rdf_meta(rdf_list_first(r,o,?)).
:- rdf_meta(rdf_list_last(r,o)).
:- rdf_meta(rdf_list_last(r,o,?)).
:- rdf_meta(rdf_list_length(r,?,?)).
:- rdf_meta(rdf_list_member(r,o)).
:- rdf_meta(rdf_list_member(r,o,?)).
:- rdf_meta(rdf_list_nth0(?,r,o,?)).
:- rdf_meta(rdf_list_triples(+,-,t)).





%! rdf_assert_list(
%!   +PrologList:list,
%!   ?RdfList:or([bnode,iri]),
%!   ?Graph:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Asserts the given, possibly nested list into RDF.
%
% The following options are supported:
%   - `datatype(+Datatype:iri)`
%
% @author Wouter Beek, elaborating on Sanders original, adding optional
%         graph, lists of datatyped literals, and return argument for root.
% @author Sander Latour, who wrote the original version, dealing with
%         nested lists.
% @tbd Add Prolog term ---> RDF datatype conversion,
%      e.g., `rdf_assert_list([a,1,0.1,com(pound)], -RdfList, _, [])`.

rdf_assert_list([], rdf:nil, _, _):- !.
rdf_assert_list(PlList, RdfList, Graph, Options):-
  add_list_individual(RdfList, Graph),
  rdf_assert_list_items(PlList, RdfList, Graph, Options).

rdf_assert_list_items([], rdf:nil, _, _).
rdf_assert_list_items([H|T], RdfList, Graph, Options):-
  % rdf:first
  (   % Nested list.
      is_list(H)
  ->  rdf_assert_list(H, First, Graph, Options)
  ;   % Dedicated support for RDF datatypes.
      option(datatype(Datatype), Options)
  ->  rdf_bnode(First),
      rdf_assert_typed_literal(First, rdf:value, H, Datatype, Graph)
  ;   First = H
  ),
  rdf_assert2(RdfList, rdf:first, First, Graph),

  % rdf:rest
  (   T == []
  ->  rdf_global_id(rdf:nil, Rest)
  ;   add_list_individual(Rest, Graph),
      rdf_assert_list_items(T, Rest, Graph, Options)
  ),
  rdf_assert2(RdfList, rdf:rest, Rest, Graph).



%! rdf_list(+List:or([bnode,iri])) is semidet.
%! rdf_list(-List:or([bnode,iri])) is nondet.
% Succeeds if the given RDF term is an RDF list
% or enumerates RDF lists.
% Notice that every sublist of an RDF list is an RDF list.
%
% ### Tricky stuff
%
% For triple [1], simple entailment deduces [2].
% Do we want to represent both `_:x` and  `_:y` as RDF lists?
%
% ```ntriples
% [1]   _:x rdf:type rdf:List .
% [2]   _:y rdf:type rdf:List .
% ```
%
% Basically [1] and [2] both states that there is a list (nothing more).
% Yet [1] is an RDF list, syntactically speaking,
%  i.e., it occurs in the subject position of a triple with predicate
%  `rdf:first`.
%
% @tbd Solve syntax/semantics distinction as to what is an RDF list.

rdf_list(List):-
  rdfs_individual_of(List, rdf:'List').

%! rdf_list(+RdfList:or([bnode,iri]), -PrologList:list) is det.

rdf_list(RdfL, PlL):-
  rdf_list(RdfL, PlL, _, []).

%! rdf_list(
%!   +RdfList:or([bnode,iri]),
%!   -PrologList:list,
%!   ?Graph:atom,
%!   +Options:list(nvpair)
%! ) is det
% Returns the given RDF list in Prolog list notation.
%
% The following options are supported:
%   * `datatype(+Datatype:iri)`
%   * `recursive(+RecursivelyApplied:boolean)`
%     Whether RDF list items are also converted to Prolog list notation.
%     Default: `true`.
%
% @author Wouter Beek
% @author Sander Latour
% @tbd Add RDF datatype --> Prolog term conversion,
%      e.g., `"0.1"^^xsd:float` ---> `0.1`.

rdf_list(rdf:nil, [], _, _):- !.
rdf_list(RdfList, [H|T], Graph, Options):-
  % `rdf:first`
  rdf(RdfList, rdf:first, First, Graph:_),
  (   % Nested list.
      option(recursive(true), Options, true),
      rdf_list(First)
  ->  rdf_list(First, H, Graph, Options)
  ;   % Dedicated support for RDF datatypes.
      option(datatype(Datatype), Options)
  ->  rdf_typed_literal(First, rdf:value, H, Datatype, Graph)
  ;   H = First
  ),

  % `rdf:rest`
  rdf(RdfList, rdf:rest, Rest, Graph:_),
  rdf_list(Rest, T, Graph, Options).



%! rdf_list_after(?After:rdf_term, ?Before:rdf_term) is nondet.

rdf_list_after(After, Before):-
  rdf_list_after(After, Before, _, _).

%! rdf_list_after(
%!   ?After:rdf_term,
%!   ?Before:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case After occurs after Before in an RDF list.
%
% @see The inverse of rdf_list_before/4.

rdf_list_after(After, Before, List, Graph):-
  rdf_list_before(Before, After, List, Graph).



%! rdf_list_before(?Before:rdf_term, ?After:rdf_term) is nondet.

rdf_list_before(Before, After):-
  rdf_list_before(Before, After, _, _).

%! rdf_list_before(
%!   ?Before:rdf_term,
%!   ?After:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case Before occurs before After in an RDF list.
%
% @see Transitive closure of rdf_list_directly_before/3.

rdf_list_before(Before, After, List, Graph):-
  closure(
    \Before^After^rdf_list_directly_before(Before, After, List, Graph),
    Before,
    After
  ).



%! rdf_list_directly_after(
%!   ?After:rdf_term,
%!   ?Before:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case After occurs directly after Before in an RDF list.
%
% @see Inversion of rdf_list_directly_before/4.

rdf_list_directly_after(After, Before, List, Graph):-
  rdf_list_directly_before(Before, After, List, Graph).



%! rdf_list_directly_before(
%!   ?Before:rdf_term,
%!   ?After:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case Before occurs directly before After in an RDF list.

rdf_list_directly_before(Before, After, List, Graph):-
  rdf(List, rdf:first, Before, Graph),
  rdf(List, rdf:rest, Rest, Graph),
  rdf(Rest, rdf:first, After, Graph).



%! rdf_list_first(?List:or([bnode,iri]), ?First:rdf_term) is nondet.

rdf_list_first(List, First):-
  rdf_list_first(List, First, _).

%! rdf_list_first(
%!   ?List:or([bnode,iri]),
%!   ?First:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Relates RDF lists to their first element.

rdf_list_first(List, First, Graph):-
  rdf(List, rdf:first, First, Graph).



%! rdf_list_last(?List:or([bnode,iri]), ?Last:rdf_term) is det.

rdf_list_last(List, Last):-
  rdf_list_last(List, Last, _).

%! rdf_list_last(
%!   ?List:or([bnode,iri]),
%!   ?Last:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Pairs of lists and their last element.

rdf_list_last(List, Last, Graph):-
  rdf(List, rdf:rest, rdf:nil, Graph), !,
  rdf(List, rdf:first, Last, Graph).
rdf_list_last(List, Last, Graph):-
  rdf(List, rdf:rest, Rest, Graph),
  rdf_list_last(Rest, Last, Graph).



%! rdf_list_length(
%!   ?List:or([bnode,iri]),
%!   ?Length:nonneg,
%!   ?Graph:atom
%! ) is nondet.
% Succeeds if Length is the number of elements in List.

rdf_list_length(List, N, Graph):-
  rdf_list_length(List, 0, N, Graph).

rdf_list_length(List, N, N, Graph):-
  rdf(List, rdf:rest, rdf:nil, Graph), !.
rdf_list_length(List, N1, N3, Graph):-
  rdf(List, rdf:rest, Rest, Graph),
  rdf_list_length(Rest, N1, N2, Graph),
  succ(N2, N3).



%! rdf_list_member(?Member:rdf_term, ?List:or([bnode,iri])) is nondet.

rdf_list_member(Member, List):-
  rdf_list_member(Member, List, _).

%! rdf_list_member(
%!   ?Member:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds if Member occurs in List.

rdf_list_member(Member, List, Graph):-
  rdf_list_first(List, Member, Graph).
rdf_list_member(Member, List, Graph):-
  rdf(List, rdf:rest, Rest, Graph),
  rdf_list_member(Member, Rest, Graph).



%! rdf_list_nth0(
%!   ?Index:nonneg,
%!   ?List:or([bnode,iri]),
%!   ?Elem:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_list_nth0(I, L, E, G):-
  rdf_list_nth0(0, I, L, E, G).

rdf_list_nth0(I, I, L, E, G):-
  rdf_list_first(L, E, G).
rdf_list_nth0(I1, I3, L, E, G):-
  rdf(L, rdf:rest, R, G),
  rdf_list_nth0(I1, I2, R, E, G),
  succ(I2, I3).



%! rdf_list_triples(
%!   +PrologList:list,
%!   -RdfList:bnode,
%!   -Triples:list(compound)
%! ) is det.

rdf_list_triples(L, B, Ts):-
  rdf_bnode(B),
  rdf_list_triples0(L, B, Ts).

rdf_list_triples0([], _, []).
rdf_list_triples0([H], B, [rdf(B,rdf:first,H),rdf(B,rdf:rest,rdf:nil)]).
rdf_list_triples0([H|T], B1, [rdf(B1,rdf:first,H),rdf(B1,rdf:rest,B2)|Ts]):-
  rdf_bnode(B2),
  rdf_list_triples0(T, B2, Ts).





% HELPERS %

%! add_list_individual(?List:or([bnode,iri]), ?Graph:atom) is det.
% @arg List Defaults to a new;y created blank node.
% @arg Graph Defaults to graph `user`.

add_list_individual(List, Graph):-
  (   var(List)
  ->  rdf_bnode(List)
  ;   true
  ),
  rdf_assert_instance(List, rdf:'List', Graph).
