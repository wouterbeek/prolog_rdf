:- module(
  oaei,
  [
    oaei/4,                    % ?M, ?From, ?To, ?G
    oaei/6,                    % ?M, ?From, ?To, ?Rel, ?V, ?G
    qb_oaei/3,                 % +M, +Pair, ?G
    qb_oaei/5,                 % +M, +Pair, +Rel, +V, ?G
    oaei_convert_rdf_to_tsv/2, % +Source, +Sink
    oaei_convert_tsv_to_rdf/2, % +Source, +Sink
    oaei_load_rdf/2,           % +Source, -Alignments
    oaei_load_rdf/3,           % +Source, -Alignments, +Opts
    oaei_load_tsv/2,           % +Source, -Alignments
    oaei_save_rdf/2,           % +Sink,   +Alignments
    oaei_save_tsv/2            % +Sink,   +Alignments
  ]
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI)

During loading and saving alignments are represented as pairs.

@author Wouter Beek
@version 2015/10, 2015/12-2016/01, 2016/05-2016/07
*/

:- use_module(library(csv_ext)).
:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(pair_ext)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(yall)).

:- rdf_create_alias(align, 'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').

:- rdf_meta
   oaei(?, o, o, r),
   oaei(?, o, o, ?, ?, r),
   oaei_assert(+, t, r),
   oaei_assert(+, t, +, +, r).





%! oaei(?M, ?From, ?To, ?G) is nondet.
%! oaei(?M, ?From, ?To, ?Rel, ?V, ?G) is nondet.

oaei(M, From, To, G) :-
  oaei(M, From, To, =, 1.0, G).


oaei(M, From, To, Rel, V, G) :-
  q(M, X, align:entity1, From, G),
  q(M, X, align:entity2, To, G),
  q(M, X, align:relation, Rel^^xsd:string, G),
  q(M, X, align:measure, V^^xsd:float, G).



%! qb_oaei(+M, +Pair, ?G) is det.
%! qb_oaei(+M, +Pair, +Rel, +V:between(0.0,1.0), ?G) is det.

qb_oaei(M, Pair, G) :-
  qb_oaei(M, Pair, =, 1.0, G).


qb_oaei(M, From-To, Rel, V, G) :-
  qb_bnode(B),
  qb(M, B, align:entity1, From, G),
  qb(M, B, align:entity2, To, G),
  qb(M, B, align:relation, Rel, G),
  qb(M, B, align:measure, V, G).



%! oaei_convert_rdf_to_tsv(+Source, +Sink) is det.

oaei_convert_rdf_to_tsv(Source, Sink) :-
  oaei_load_rdf(Source, L),
  oaei_save_tsv(L, Sink).



%! oaei_convert_tsv_to_rdf(+Source, +Sink) is det.

oaei_convert_tsv_to_rdf(Source, Sink) :-
  oaei_load_tsv(Source, L),
  oaei_save_rdf(L, Sink).



%! oaei_load_rdf(+Source, -Alignments) is det.
%! oaei_load_rdf(+Source, -Alignments, +Opts) is det.

oaei_load_rdf(Source, L) :-
  oaei_load_rdf(Source, L, []).


oaei_load_rdf(Source, L, Opts) :-
  rdf_call_on_graph(Source, oaei_load_rdf0(L), Opts).


oaei_load_rdf0(L, G) :-
  findall(From-To, oaei(trp, From, To, G), L).



%! oaei_load_tsv(+Source, -Alignments) is det.

oaei_load_tsv(Source, Pairs) :-
  tsv_read_file(Source, Rows, [arity(2)]),
  maplist(pair_row, Pairs, Rows).



%! oaei_save_rdf(+Sink, +Alignments) is det.

oaei_save_rdf(Sink, Pairs) :-
  rdf_call_to_graph(Sink, qb_oaei0(Pairs)).

qb_oaei0(Pairs, G) :-
  maplist({G}/[Pair]>>qb_oaei(trp, Pair, G), Pairs).



%! oaei_save_tsv(+Sink, +Alignments) is det.

oaei_save_tsv(Sink, Pairs) :-
  call_to_stream(
    Sink,
    {Pairs}/[Out]>>maplist(oaei_save_tsv_pair0(Out), Pairs)
  ).


oaei_save_tsv_pair0(Out, Pair) :-
  pair_row(Pair, Row),
  tsv_write_stream(Out, [Row]).
