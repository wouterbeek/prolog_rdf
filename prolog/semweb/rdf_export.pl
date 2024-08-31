:- encoding(utf8).
:- module(
  rdf_export,
  [
    rdf_write_quad/2,    % +Out, +Quad
    rdf_write_quad/3,    % +Out, +Tuple, +GraphName
    rdf_write_triple/2,  % +Out, +Tuple
    rdf_write_tuple/2    % +Out, +Tuple
  ]
).

/** <module> RDF export

RDF export predicates that are backend-independent.

*/

:- use_module(library(lists)).
:- use_module(library(semweb/turtle), []).

:- use_module(library(hash_ext)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   rdf_write_iri(+, r),
   rdf_write_literal(+, o),
   rdf_write_name(+, o),
   rdf_write_quad(+, t),
   rdf_write_quad(+, t, r),
   rdf_write_quad(+, r, r, o, r),
   rdf_write_triple(+, t),
   rdf_write_tuple(+, t).





%! rdf_write_graph(+Out:ostream, +GraphName:rdf_graph_name) is det.

rdf_write_graph(Out, G) :-
  rdf_write_iri(Out, G).



%! rdf_write_iri(+Out:ostream, +Iri:rdf_iri) is det.

rdf_write_iri(Out, Iri) :-
  turtle:turtle_write_uri(Out, Iri).



%! rdf_write_literal(+Out:ostream, +Literal:rdf_literal) is det.

rdf_write_literal(Out, Input) :-
  rdf_literal_dwim(Input, Literal),
  rdf_write_literal_(Out, Literal).

rdf_write_literal_(Out, literal(lang(LTag,Lex))) :- !,
  turtle:turtle_write_quoted_string(Out, Lex),
  format(Out, "@~a", [LTag]).
rdf_write_literal_(Out, literal(type(D,Lex))) :-
  turtle:turtle_write_quoted_string(Out, Lex),
  format(Out, "^^", []),
  rdf_write_iri(Out, D).



%! rdf_write_name(+Out:ostream, +Name:rdf_name) is det.

% IRI.
rdf_write_name(Out, Iri) :-
  rdf_is_iri(Iri), !,
  rdf_write_iri(Out, Iri).
% Literal.
rdf_write_name(Out, Literal) :-
  rdf_is_literal(Literal),
  rdf_write_literal(Out, Literal).



%! rdf_write_quad(+Out:ostream, +Quad:rdf_quad) is det.
%! rdf_write_quad(+Out:ostream, +Tuple:rdf_tuple, +GraphName:rdf_graph_name) is det.
%
% Quad must be a quadruple (denoted by compound term rdf/4).  Triples
% (denoted by compound term rdf/3) are not supported.

rdf_write_quad(Out, tp(S,P,O,G)) :-
  rdf_write_triple_open_(Out, tp(S,P,O)),
  put_char(Out, ' '),
  rdf_write_graph(Out, G),
  format(Out, ".\n", []).


rdf_write_quad(Out, tp(S,P,O), G) :- !,
  rdf_write_quad(Out, tp(S,P,O,G)).
rdf_write_quad(Out, tp(S,P,O,_), G) :-
  rdf_write_quad(Out, tp(S,P,O,G)).



%! rdf_write_term(+Out:ostream, +Name:rdf_name) is det.

rdf_write_term(Out, Iri) :-
  rdf_is_iri(Iri), !,
  rdf_write_iri(Out, Iri).
rdf_write_term(Out, Literal) :-
  rdf_write_literal(Out, Literal).



%! rdf_write_triple(+Out:ostream, +Tuple:rdf_tuple) is det.
%
% Accepts triples (tp/3) and quadrupleds (tp/4), but always writes a
% triple.

rdf_write_triple(Out, tp(S,P,O)) :- !,
  rdf_write_triple_open_(Out, tp(S,P,O)),
  format(Out, ".\n", []).
rdf_write_triple(Out, tp(S,P,O,_)) :-
  rdf_write_triple(Out, tp(S,P,O)).

rdf_write_triple_open_(Out, tp(S,P,O)) :-
  rdf_write_term(Out, S),
  put_char(Out, ' '),
  rdf_write_iri(Out, P),
  put_char(Out, ' '),
  rdf_write_term(Out, O).



%! rdf_write_tuple(+Out:ostream, +Tuple:rdf_tuple) is det.
%
% Writes a triple if Tuple is a triple pattern term (tp/3).  Writes a
% quadruple if Tuple is a quadruple pattern term (tp/4).

rdf_write_tuple(Out, tp(S,P,O)) :- !,
  rdf_write_triple(Out, tp(S,P,O)).
rdf_write_tuple(Out, tp(S,P,O,G)) :-
  rdf_write_quad(Out, tp(S,P,O,G)).
