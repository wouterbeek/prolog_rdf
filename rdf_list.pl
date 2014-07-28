:- module(
  rdf_list,
  [
    rdf_is_list/1, % +RdfList:iri
    rdf_assert_list/3, % +PrologList:list
                       % -RdfList:iri
                       % +Options:list(nvpair)
    rdf_list/2, % +RdfList:iri
                % -List:list
    rdf_list/3, % +RdfList:iri
                % -List:list
                % +Options:list(nvpair)
    rdf_list_first/2, % +List:iri
                      % -FirstElement:iri
    rdf_list_last/2, % +List:iri
                     % -LastElement:iri
    rdf_list_length/2, % +List:iri
                       % -Length:number
    rdf_list_member/2, % ?Element
                       % ?RdfList:iri
    rdf_list_name//2, % +Options:list(nvpair)
                      % +RdfList:or([bnode,iri])
    rdf_list_next/2, % ?Element:iri
                     % ?NextElement:iri
    rdf_list_nth0/3, % +Index:nonneg
                     % +RdfList:iri
                     % -Element:or([bnode,iri])
    rdf_list_occurs_after/2, % +After:iri
                             % +Before:iri
    rdf_list_occurs_before/2, % +Before:iri
                              % +After:iri
    rdf_list_previous/2 % ?Element:iri
                        % ?PreviousElement:iri
  ]
).

/** <module> RDF list

Support for RDF lists.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/05, 2013/07-2013/09,
         2014/01-2014/02, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(predicate_options)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_collection)).
:- use_module(xml(xml_namespace)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_name)).
:- use_module(plRdf(rdfs_read)).
:- use_module(plRdf_ent(rdf_bnode_map)).
:- use_module(plRdf_term(rdf_datatype)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- predicate_options(rdf_assert_list/3, 3, [
     datatype(+atom)
   ]).
:- predicate_options(rdf_list/3, 3, [
     datatype(+atom),
     recursive(+boolean)
   ]).

:- rdf_meta(rdf_is_list(r)).
:- rdf_meta(rdf_assert_list(t,r,+)).
:- rdf_meta(rdf_assert_list(t,r,+,+)).
:- rdf_meta(rdf_list(r,-)).
:- rdf_meta(rdf_list(r,-,+)).
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
% ### Tricky stuff
%
% For triple [1], simple entailment deduces [2].
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
  (bnode_to_term(_, RDF_List1, RDF_List2), ! ; RDF_List2 = RDF_List1),
  rdfs_individual(m(t,f,f), RDF_List2, C, _).


%! rdf_assert_list(
%!   +PrologList:list,
%!   -RdfList:iri,
%!   +Options:list(nvpair)
%! ) is det.
% Asserts the given, possibly nested list into RDF.
%
% The following options are supported:
%   * =|datatype(+Datatype:iri)|=
%
% @author Wouter Beek, elaborating on Sanders original, adding optional
%         graph, lists of datatyped literals, and return argument for root.
% @author Sander Latour, who wrote the original version, dealing with
%         nested lists.

rdf_assert_list(List, RdfList, Options):-
  option(graph(Graph), Options, _VAR),
  add_blank_list_individual(RdfList, Graph),
  rdf_assert_list(List, RdfList, Graph, Options).

rdf_assert_list([], rdf:nil, _, _).
rdf_assert_list([H|T], RdfList, Graph, Options):-
  (
    is_list(H)
  ->
    rdf_assert_list(H, H1, Graph, Options)
  ;
    option(datatype(Datatype), Options)
  ->
    rdf_bnode(H1),
    rdf_global_id(rdf:value, P),
    rdf_assert_datatype(H1, P, H, Datatype, Graph)
  ;
    H1 = H
  ),
  rdf_assert(RdfList, rdf:first, H1, Graph),
  (
    T == []
  ->
    rdf_global_id(rdf:nil, TList)
  ;
    add_blank_list_individual(TList, Graph),
    rdf_assert_list(T, TList, Graph, Options)
  ),
  rdf_assert(RdfList, rdf:rest, TList, Graph).

add_blank_list_individual(Blank, Graph):-
  rdf_bnode(Blank),
  rdf_assert_individual(Blank, rdf:'List', Graph).


%! rdf_list(+RdfList:rdf_list, -List:list) is det.
% @see Wrapper around rdf_list/3.

rdf_list(RdfList, List):-
  rdf_list(RdfList, List, []).

%! rdf_list(+RDFList:iri, -List:list, +Options:list(nvpair)) is det
% Returns the list that starts at the given node.
%
% The following options are supported:
%   * =|datatype(+Datatype:iri)|=
%   * =|recursive(+RecursivelyApplied:boolean)|=
%     The default value is `true`.
%
% @arg StartNode The URI of a node that starts the RDF list.
% @arg List A prolog list.
% @arg Options A list of name-value pairs.
%
% @author Wouter Beek
% @author Sander Latour

rdf_list(RDFList, [], _):-
  rdf_global_id(rdf:nil, RDFList), !.
rdf_list(RDFList, [H1|T], Options):-
  rdf_has(RDFList, rdf:first, H),
  (
    option(recursive(true), Options, true),
    rdf_is_list(H)
  ->
    rdf_list(H, H1, Options)
  ;
    option(datatype(D), Options)
  ->
    rdf_global_id(rdf:value, P),
    rdf_datatype(H, P, H1, D, _)
  ;
    H1 = H
  ),
  rdf_has(RDFList, rdf:rest, RDFTail), !,
  rdf_list(RDFTail, T, Options).


%! rdf_list_first(+List:iri, +FirstElement:iri) is semidet.
%! rdf_list_first(+List:iri, -FirstElement:iri) is nondet.
%! rdf_list_first(-List:iri, +FirstElement:iri) is nondet.
%! rdf_list_first(-List:iri, -FirstElement:iri) is nondet.
% Relates RDF lists to their first element.

rdf_list_first(List, First):-
  rdf_has(List, rdf:first, First).


%! rdf_list_last(+List:iri, +LastElement:iri) is nondet.
%! rdf_list_last(+List:iri, -LastElement:iri) is nondet.
%! rdf_list_last(-List:iri, +LastElement:iri) is nondet.
%! rdf_list_last(-List:iri, -LastElement:iri) is nondet.
% Pairs of lists and their last element.

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


%! rdf_list_member(+Element, +RdfList:iri) is semidet.
%! rdf_list_member(+Element, -RdfList:iri) is multi.
%! rdf_list_member(-Element, +RdfList:iri) is multi.
%! rdf_list_member(-Element, -RdfList:iri) is multi.
% Variant of member/2 for RDF lists.

rdf_list_member(Element, RdfList):-
  rdf_list_first(RdfList, FirstElement),
  rdf_list_member0(Element, FirstElement).
rdf_list_member0(Element, Element).
rdf_list_member0(Element, TempElement1):-
  rdf_list_next(TempElement1, TempElement2),
  rdf_list_member0(Element, TempElement2).


%! rdf_list_next(+Element:iri, +NextElement:iri) is semidet.
%! rdf_list_next(+Element:iri, -NextElement:iri) is semidet.
%! rdf_list_next(-Element:iri, +NextElement:iri) is semidet.
%! rdf_list_next(-Element:iri, -NextElement:iri) is multi.
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


%! rdf_list_name(+Options:list(nvpair), +RdfList:iri)// is det.

rdf_list_name(Options, RdfList) -->
  % Recursively retrieve the contents of the RDF list.
  % This has to be done non-recursively, since the nested
  % Prolog list `[a,[b,c]]` would bring rdf_term_name//1 into
  % trouble when it comes accross `[b,c]`
  % (which fails the check for RDF list).
  {rdf_list([recursive(false)], RdfList, Terms)},
  list(rdf_term_name(Options), Terms).


%! rdf_list_nth0(+Index:nonneg, +RdfList:iri, -Element:or([bnode,iri])) is det.

rdf_list_nth0(I, L, E):-
  rdf_list_first(L, E1),
  rdf_list_nth0_(I, E1, E).

rdf_list_nth0_(0, E, E):- !.
rdf_list_nth0_(I1, E1, E):-
  rdf_list_next(E1, E2),
  I2 is I1 - 1,
  rdf_list_nth0_(I2, E2, E).

