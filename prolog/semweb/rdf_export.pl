:- module(
  rdf_export,
  [
    rdf_reserialize/3,      % +Uri, +In, +File
    rdf_save2/1,            % +Out
    rdf_save2/2,            % +Out, +Type
    rdf_save2/3,            % +Out, +Type, +G
    rdf_save2/6,            % +Out, +Type, ?S, ?P, ?O, ?G
    rdf_write_iri/2,        % +Out, +Iri
    rdf_write_literal/2,    % +Out, +Literal
    rdf_write_nonliteral/2, % +Out, +S
    rdf_write_quad/2,       % +Out, +Quad
    rdf_write_quad/3,       % +Out, +Triple, +G
    rdf_write_quad/5,       % +Out, +S, +P, +O, +G
    rdf_write_term/2,       % +Out, +Term
    rdf_write_triple/2,     % +Out, +Triple
    rdf_write_triple/4,     % +Out, +S, +P, +O
    rdf_write_tuple/2       % +Out, +Tuple
  ]
).

/** <module> RDF export

@author Wouter Beek
@version 2017/09-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/turtle), []).

:- rdf_meta
   rdf_save2(+, +, r),
   rdf_save2(+, +, r, r, o, r),
   rdf_write_iri(+, r),
   rdf_write_literal(+, o),
   rdf_write_nonliteral(+, r),
   rdf_write_quad(+, t),
   rdf_write_quad(+, t, r),
   rdf_write_quad(+, r, r, o, r),
   rdf_write_term(+, o),
   rdf_write_triple(+, t),
   rdf_write_triple(+, r, r, o),
   rdf_write_tuple(+, t).





%! rdf_reserialize(+Uri:atom, +In:stream, +File:atom) is det.

rdf_reserialize(Uri, In, File) :-
  setup_call_cleanup(
    open(File, write, Out),
    rdf_deref_stream(Uri, In, rdf_clean_assert(Out)),
    close(Out)
  ).

rdf_clean_assert(Out, Triples, _) :-
  maplist(rdf_clean_assert(Out), Triples).

rdf_clean_assert(Out, rdf(S,P,O)):-
  rdf_write_triple(Out, S, P, O).



%! rdf_save2(+Out:stream) is det.
%! rdf_save2(+Out:stream, +Type:oneof([quads,triples])) is det.
%! rdf_save2(+Out:stream, +Type:oneof([quads,triples]), +G) is det.
%! rdf_save2(+Out:stream, +Type:oneof([quads,triples]), ?S, ?P, ?O, ?G) is det.

rdf_save2(Out) :-
  rdf_save2(Out, quads).


rdf_save2(Out, Type) :-
  rdf_save2(Out, Type, _).


rdf_save2(Out, Type, G) :-
  rdf_save2(Out, Type, _, _, _, G).


rdf_save2(Out, quads, S, P, O, G) :- !,
  forall(
    rdf(S, P, O, G),
    rdf_write_quad(Out, rdf(S,P,O,G))
  ).
rdf_save2(Out, triples, S, P, O, G) :-
  forall(
    rdf(S, P, O, G),
    rdf_write_triple(Out, rdf(S,P,O,G))
  ).



%! rdf_write_iri(+Out:stream, +Iri:rdf_iri) is det.

rdf_write_iri(Out, Iri) :-
  turtle:turtle_write_uri(Out, Iri).



%! rdf_write_literal(+Out:stream, +Literal:rdf_literal) is det.

rdf_write_literal(Out, literal(type(D,Lex))) :- !,
  turtle:turtle_write_quoted_string(Out, Lex),
  format(Out, "^^", []),
  rdf_write_iri(Out, D).
rdf_write_literal(Out, literal(lang(LTag,Lex))) :- !,
  turtle:turtle_write_quoted_string(Out, Lex),
  format(Out, "@~a", [LTag]).
rdf_write_literal(Out, literal(Lex)) :- !,
  rdf_write_literal(Out, literal(type(xsd:string,Lex))).
rdf_write_literal(Out, Value^^D1) :- !,
  rdf11:in_type(D1, Value, D2, Lex),
  rdf_write_literal(Out, literal(type(D2,Lex))).
rdf_write_literal(Out, String@LTag) :- !,
  atom_string(Lex, String),
  rdf_write_literal(Out, literal(lang(LTag,Lex))).



%! rdf_write_nonliteral(+Out:stream, +Iri:rdf_iri) is det.

rdf_write_nonliteral(Out, BNode) :-
  rdf_is_bnode(BNode), !,
  format(Out, '~a', [BNode]).
rdf_write_nonliteral(Out, Iri) :-
  rdf_is_iri(Iri), !,
  rdf_write_iri(Out, Iri).



%! rdf_write_quad(+Out, +Quad) is det.
%! rdf_write_quad(+Out, +Triple, +G) is det.
%! rdf_write_quad(+Out, +S, +P, +O, G) is det.
%
% Quad must be a quadruple (denoted by compound term rdf/4).  Triples
% (denoted by compound term rdf/3) are not supported.

rdf_write_quad(Out, rdf(S,P,O,G)) :-
  rdf_write_quad(Out, S, P, O, G).


rdf_write_quad(Out, rdf(S,P,O), G) :- !,
  rdf_write_quad(Out, rdf(S,P,O,G)).
rdf_write_quad(Out, rdf(S,P,O,_), G) :-
  rdf_write_quad(Out, rdf(S,P,O,G)).


rdf_write_quad(Out, S, P, O, G) :-
  rdf_write_triple_open(Out, S, P, O),
  rdf_write_iri(Out, G),
  format(Out, " .\n", []).

rdf_write_triple_open(Out, S, P, O) :-
  rdf_write_nonliteral(Out, S),
  put_char(Out, ' '),
  rdf_write_iri(Out, P),
  put_char(Out, ' '),
  rdf_write_term(Out, O),
  put_char(Out, ' ').



%! rdf_write_term(+Out:stream, +Term:rdf_term) is det.

rdf_write_term(Out, S) :-
  rdf_write_nonliteral(Out, S), !.
rdf_write_term(Out, Literal) :-
  rdf_write_literal(Out, Literal).



%! rdf_write_triple(+Out, +Triple:compound) is det.
%! rdf_write_triple(+Out, +S:rdf_nonliteral, +P:rdf_iri, +O:rdf_term) is det.
%
% rdf_write_triple/2 also accepts quadrupleds (denoted by compound
% term rdf/4), but writes them as triples.

rdf_write_triple(Out, rdf(S,P,O)) :- !,
  rdf_write_triple(Out, S, P, O).
rdf_write_triple(Out, rdf(S,P,O,_)) :-
  rdf_write_triple(Out, S, P, O).


rdf_write_triple(Out, S, P, O) :-
  rdf_write_triple_open(Out, S, P, O),
  format(Out, ".\n", []).



%! rdf_write_tuple(+Out:stream, +Tuple:compound) is det.
%
% If Tuple is a triple (denoted by compound term rdf/3), it is written
% as a triple.  If Tuple is a quadrupled (denoted by compound term
% rdf/4), it is written as a quadruple.

rdf_write_tuple(Out, rdf(S,P,O)) :- !,
  rdf_write_triple(Out, S, P, O).
rdf_write_tuple(Out, rdf(S,P,O,G)) :-
  rdf_default_graph(G), !,
  rdf_write_triple(Out, S, P, O).
rdf_write_tuple(Out, rdf(S,P,O,G)) :-
  rdf_write_quad(Out, S, P, O, G).
