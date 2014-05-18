:- module(
  rdf_mt_i,
  [
    mt_i/3 % +In:or([atom,compound,list(compound)])
           % +Model:atom
           % -AssignmentsOut:list(pair(bnode,resource))
  ]
).

/** <module> RDF_MT

Model theory for RDF(S).

## Simple interpretation

A simple interpretation `I` of a vocabulary `V` is defined by:
  1. A non-empty set of resources `IR`, called the domain or universe of `I`.
     We prefer _universe_ since domain can also refer to a specific term
     in the RDFS vocabulary.
  2. A set `IP` of properties of `I`.
  3. A mapping `IEXT` from `IP` into the powerset of $IR \times IR$.
  4. A mapping `IS` from URI references in `V` into $IR \cup IP$.
  5. A mapping `IL` from typed literals in `V` into `IR`.
  6. A distinguished subset `LV` of `IR` called the set of literal values,
     containing all plain literals in `V`.

Note that `IR` is called the domain, but `IP` are semantic objects as well.
Some objects are in both `IR` and `IP`;
some are exclusively in `IR`;
some are exclusively in `IP`.

Interpretation function `I`:
  1. If `E` is a plain literal =|"aaa"|=, then =|I(E) = aaa|=.
  2. If `E` is a plain literal, then =|I(E) = aaa|=.
  3. If `E` is a typed literal in `V`, then =|I(E) = IL(E)|=.
  4. If `E` is a URI reference in `V`, then =|I(E) = IS(E)|=.
  5. If `E` is a ground triple $\langle s, p, o \rangle$,
     the =|I(E) = true|= iff `s`, `p`, and `o` are in `V`,
     $I(p) \in IP$ and $\langle I(s), I(o) \rangle \in IEXT(I(p))$.
  6. A graph is true if each triple is true.

@author Wouter Beek
@see Hayes2004
@tbd Add the additional semantic constraints imposed by
     the RDF vocabulary and the RDFS vocabulary.
@version 2013/05, 2013/08
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_mt(rdf_mt)).

% The overarching interpretation plus assignment function.
:- rdf_meta(mt_i(r,-,+,-)).



%! mt_i(
%!   +In:or([atom,compound,list(compound)]),
%!   +Model:atom,
%!   +AssingmentsOut:list(pair(bnode,resource))
%! ) is nondet.
% @see mt_i/3.

mt_i(G, M, A):-
  mt_i(G, M, [], A).

%! mt_i(
%!   +Graph:atom,
%!   +Model:atom,
%!   +AssingmentsIn:list(pair(bnode,resource)),
%!   -AssignmentsOut:list(pair(bnode,resource))
%! ) is nondet.
% Satisfaction for RDF graphs, lists of triples, and individual triples.
%
% Since the meaning of (a collection of) triples is a truth value,
% the meaning no longer needs to be a parameter (as in mt_i/4).
%
% @arg In Either the atomic name of an RDF graph,
%           a list of triple compound terms,
%           or a single triple compound term (i.e. rdf/3).
% @arg Model The atomic name of a semantic model.
% @arg AssignmentsIn A list of pairs, representing a functional map
%                      from blank nodes to resources.
% @arg AssignmentsOut A list of pairs, representing a functional map
%                       from blank nodes to resources.

% Satisfaction for an RDF graph.
mt_i(G, M, A1, A2):-
  rdf_graph(G), model(M), !,
  rdf_graph:rdf_graph_to_triples(G, Ts),
  mt_i(G, M, Ts, A1, A2).

%! mt_i(
%!   +Graph:atom,
%!   +Model:atom,
%!   +Triples:list(compound),
%!   +AssingmentsIn:list(pair(bnode,resource)),
%!   -AssignmentsOut:list(pair(bnode,resource))
%! ) is nondet.

% Satisfaction for a list of RDF triples.
mt_i(G, M, Ts, A1, An):-
  is_list(Ts), !,
  % Notice that the assignments should be consistent accross
  % the meaning that is assigned to the list of triples.
  foldl(mt_i(G, M), Ts, A1, An).
% Satisfaction for an RDF triple.
mt_i(G, M, rdf(SYN_S1,SYN_P1,SYN_O1), A1, A4):-
  maplist(
    % This works for subject and predicate terms as well...
    rdf_global_object,
    [SYN_S1,SYN_P1,SYN_O1],
    [SYN_S2,SYN_P2,SYN_O2]
  ),
  mt_i(G, M, SYN_S2, Resource1, A1, A2),
  mt_i(G, M, SYN_P2, Property,  A2, A3),
  mt_i(G, M, SYN_O2, Resource2, A3, A4),
  once(i_ext(M, Property, Resource1, Resource2)).

%! mt_i(
%!   +Graph:atom,
%!   +Model:atom,
%!   +RDF_Term:or([bnode,literal,iri]),
%!   -Meaning,
%!   +AssignmentsIn:list(pair(bnode,resource)),
%!   -AssignmentsOut:list(pair(bnode,resource))
%! ) is nondet.
% The interpretation function for subsentential components.

% Blank nodes.
mt_i(_G, M, BNode, Resource, A1, A2):-
  rdf_is_bnode(BNode), !,
  (
    memberchk([BNode,Resource], A1)
  ->
    A2 = A1
  ;
    % Since the blank node does not occur in the assignment function yet,
    % we add it. Notice that the assignment is nondet.
    resource(M, Resource),
    A2 = [BNode,Resource|A1]
  ).
% Plain literals. There map onto themselves.
mt_i(G, M, PlainLit, PlainLit, A, A):-
  once(lv(G, M, PlainLit)), !.
% Typed literals. These map onto resources.
mt_i(G, M, TypedLiteral, Resource, A, A):-
  once(i_l(G, TypedLiteral, M, Resource)), !.
% IRIs. These map onto properties and resources.
mt_i(G, M, IRI, ResourceOrProperty, A, A):-
  % Checking for resources is not that good.
  rdf_iri(IRI), !,
  once(i_s(G, IRI, M, ResourceOrProperty)).

