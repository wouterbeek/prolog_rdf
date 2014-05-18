:- module(
  rdf_list,
  [
    rdf_is_list/1, % +RdfList:iri
    rdf_assert_list/3, % +List:list
                       % -RdfList:iri
                       % +Graph:atom
    rdf_assert_list/4, % +Options:list(nvpair)
                       % +List:list
                       % -RdfList:iri
                       % +Graph:atom
    rdf_list/2, % +RdfList:iri
                % -List:list
    rdf_list/3, % +Options:list(nvpair)
                % +RdfList:iri
                % -List:list
    rdf_list_first/2, % +List:iri
                      % -FirstElement:iri
    rdf_list_last/2, % +List:iri
                     % -LastElement:iri
    rdf_list_length/2, % +List:iri
                       % -Length:number
    rdf_list_next/2, % ?Element:iri
                     % ?NextElement:iri
    rdf_list_occurs_after/2, % +After:iri
                             % +Before:iri
    rdf_list_occurs_before/2, % +Before:iri
                              % +After:iri
    rdf_list_previous/2, % ?Element:iri
                         % ?PreviousElement:iri
    rdf_list_member/2, % ?Element
                       % ?RdfList:iri
    rdf_list_nth0/3, % +Index:nonneg
                     % +RdfList:iri
                     % -Element:or([bnode,iri])
% DEBUG
    rdf_list_name//2 % +Options:list(nvpair)
                     % +RdfList:or([bnode,iri])
  ]
).

/** <module> RDF_LIST

Support for RDF lists.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/05, 2013/07-2013/09,
         2014/01-2014/02
*/

:- use_module(dcg(dcg_collection)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_reasoning(rdf_bnode_map)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_is_list(r)).
:- rdf_meta(rdf_assert_list(t,r,+)).
:- rdf_meta(rdf_assert_list(t,t,r,+)).
:- rdf_meta(rdf_list(r,-)).
:- rdf_meta(rdf_list(+,r,-)).
:- rdf_meta(rdf_list_first(r,r)).
:- rdf_meta(rdf_list_last(r,r)).
:- rdf_meta(rdf_list_length(r,-)).
:- rdf_meta(rdf_list_next(r,r)).
:- rdf_meta(rdf_list_occurs_after(r,r)).
:- rdf_meta(rdf_list_occurs_before(r,r)).
:- rdf_meta(rdf_list_previous(r,r)).
:- rdf_meta(rdf_list_member(r,r)).
:- rdf_meta(rdf_list_name(+,r,?,?)).



%! rdf_is_list(?RdfList:rdf_list) is semidet.
% Succeeds if the given term is an RDF list.
%
% ## Tricky stuff
%
% For a triple [1] simple entailment can deduce [2].
% We do *not* want to represent `bnode2` as an RDF list,
% since `bnode2` maps to `bnode1` in the blank node map.
%
% ~~~
% [1] <bnode1,rdf:type,rdf:List>
% [2] <bnode2,rdf:type,rdf:List>
% ~~~

rdf_is_list(RDF_List1):-
  nonvar(RDF_List1),
  \+ rdf_is_literal(RDF_List1),
  rdf_global_id(rdf:'List', C),
  % WATCH OUT! THIS IS VERY TRICKY!
  (b2r(_, RDF_List1, RDF_List2), ! ; RDF_List2 = RDF_List1),
  rdfs_individual(m(t,f,f), RDF_List2, C, _).

%! rdf_assert_list(+PrologList:list, -RdfList:iri, +Graph:atom) is det.
%! rdf_assert_list(
%!   +Options:list(nvpair),
%!   +PrologList:list,
%!   -RdfList:iri,
%!   +Graph:atom
%! ) is det.
% Asserts the given, possibly nested list into RDF.
%
% The following options are supported:
%   * =|datatype(+Datatype:iri)|=
%
% @arg Options A list of name-value pairs.
% @arg PrologList The, possibly nested, Prolog list.
% @arg RdfList The URI of the node at which the RDF list starts.
% @arg Graph The atomic name of a graph or unbound.
%
% @author Wouter Beek, elaborating on Sanders original, allowing the graph
%         to be optional and returning the root of the asserted list.
% @author Sander Latour, who wrote the original version, dealing with
%         nested lists.

rdf_assert_list(List, RdfList, G):-
  rdf_assert_list([], List, RdfList, G).

rdf_assert_list(O1, List, RdfList, G):-
  add_blank_list_individual(RdfList, G),
  rdf_assert_list0(O1, List, RdfList, G).

rdf_assert_list0(_, [], rdf:nil, _Graph).
rdf_assert_list0(O1, [H|T], RdfList, G):-
  (
    is_list(H)
  ->
    rdf_assert_list0(O1, H, H1, G)
  ;
    option(datatype(D), O1)
  ->
    rdf_bnode(H1),
    rdf_global_id(rdf:value, P),
    rdf_assert_datatype(H1, P, H, D, G)
  ;
    H1 = H
  ),
  rdf_assert(RdfList, rdf:first, H1, G),
  (
    T == []
  ->
    rdf_global_id(rdf:nil, TList)
  ;
    add_blank_list_individual(TList, G),
    rdf_assert_list0(O1, T, TList, G)
  ),
  rdf_assert(RdfList, rdf:rest, TList, G).

add_blank_list_individual(Blank, G):-
  rdf_bnode(Blank),
  rdf_assert_individual(Blank, rdf:'List', G).

%! rdf_list(+RdfList:rdf_list, -List:list) is det.
% @see Wrapper around rdf_list/3.

rdf_list(RdfList, List):-
  rdf_list([], RdfList, List).

%! rdf_list(+Options:list(nvpair), +RDFList:iri, -List:list) is det
% Returns the list that starts at the given node.
%
% The following options are supported:
%   * =|datatype(+Datatype:iri)|=
%   * =|recrsive(+Recursive:boolean)|=
%
% @arg Options A list of name-value pairs.
% @arg StartNode The URI of a node that starts the RDF list.
% @arg List A prolog list.
%
% @author Wouter Beek
% @author Sander Latour
%
% The following options are supported:
%   * =|recursive(+RecursivelyApplied:boolean)|=
%     The default value is `true`.

rdf_list(_, RDFList, []):-
  rdf_global_id(rdf:nil, RDFList), !.
rdf_list(O1, RDFList, [H1 | T]):-
  rdf_has(RDFList, rdf:first, H),
  (
    option(recursive(true), O1, true),
    rdf_is_list(H)
  ->
    rdf_list(O1, H, H1)
  ;
    option(datatype(D), O1)
  ->
    rdf_global_id(rdf:value, P),
    rdf_datatype(H, P, H1, D, _)
  ;
    H1 = H
  ),
  rdf_has(RDFList, rdf:rest, RDFTail), !,
  rdf_list(O1, RDFTail, T).

%! rdf_list_first(?List:iri, ?First:iri) is nondet.
% Pairs of lists and their first element.
%
% @arg List an RDF list.
% @arg First The first element of an RDF list.

rdf_list_first(List, First):-
  rdf_has(List, rdf:first, First).

%! rdf_list_first(?List:iri, ?Last:iri) is nondet.
% Pairs of lists and their last element.
%
% @arg List an RDF list.
% @arg Last The last element of an RDF list.

rdf_list_last(List, Last):-
  rdf_has(List, rdf:rest, rdf:nil), !,
  rdf_has(List, rdf:first, Last).
rdf_list_last(List, Last):-
  rdf_has(List, rdf:rest, NextList),
  rdf_list_last(NextList, Last).

%! rdf_list_length(+List:iri, -Length:integer) is det.
% Returns the number of elements in the given list.
%
% @arg List An RDF list.
% @arg Length An integer.

rdf_list_length(List, Length):-
  rdf_list_length(List, 0, Length).

rdf_list_length(List, Length, Length):-
  rdf_has(List, rdf:rest, rdf:nil), !.
rdf_list_length(List, Length, Length):-
  rdf_has(List, rdf:rest, PartialList),
  rdf_list_length(PartialList, PartialLength, Length),
  succ(PartialLength, Length).

%! rdf_list_next(Element, NextElement) is nondet.
% Returns pairs of consecutive elements in a list.
%
% @arg Element A resource that is an element in an RDF list.
% @arg NextElement A resource that is an element in an RDF list.

rdf_list_next(Element, NextElement):-
  rdf_has(List, rdf:first, Element),
  rdf_has(List, rdf:rest, NextList),
  \+ rdf_global_id(rdf:nil, NextList),
  rdf_has(NextList, rdf:first, NextElement).

rdf_list_occurs_after(After, Before):-
  After \== Before,
  rdf_list_occurs_after0(After, Before).
rdf_list_occurs_after0(X, X).
rdf_list_occurs_after0(After1, Before):-
  rdf_list_previous(After1, After2),
  rdf_list_occurs_after0(After2, Before).

rdf_list_occurs_before(Before, After):-
  Before \== After,
  rdf_list_occurs_before0(Before, After).
rdf_list_occurs_before0(X, X).
rdf_list_occurs_before0(Before1, After):-
  rdf_list_next(Before1, Before2),
  rdf_list_occurs_before0(Before2, After).

%! rdf_list_previous(Element, PreviousElement) is nondet.
% Returns pairs of inverted consecutive elements in a list.
%
% @arg Element A resource that is an element in an RDF list.
% @arg PreviousElement A resource that is an element in an RDF list.

rdf_list_previous(Element, PreviousElement):-
  rdf_list_next(PreviousElement, Element).

%! rdf_list_member(?Element, ?RdfList:rdf_list) is nondet.
% @see Variant of member/2 for RDF lists.

rdf_list_member(Element, RdfList):-
  rdf_list_first(RdfList, FirstElement),
  rdf_list_member_(Element, FirstElement).
rdf_list_member_(Element, Element).
rdf_list_member_(Element, TempElement1):-
  rdf_list_next(TempElement1, TempElement2),
  rdf_list_member_(Element, TempElement2).


%! rdf_list_nth0(+Index:nonneg, +RdfList:iri, -Element:or([bnode,iri])) is det.

rdf_list_nth0(I, L, E):-
  rdf_list_first(L, E1),
  rdf_list_nth0_(I, E1, E).

rdf_list_nth0_(0, E, E):- !.
rdf_list_nth0_(I1, E1, E):-
  rdf_list_next(E1, E2),
  I2 is I1 - 1,
  rdf_list_nth0_(I2, E2, E).



% DEBUG %

rdf_list_name(O1, RdfList) -->
  % Recursively retrieve the contents of the RDF list.
  % This has to be done non-recursively, since the nested
  % Prolog list `[a,[b,c]]` would bring rdf_term_name//1 into
  % trouble when it comes accross `[b,c]`
  % (which fails the check for RDF list).
  {rdf_list([recursive(false)], RdfList, RDF_Terms)},

  list(rdf_term_name(O1), RDF_Terms).

