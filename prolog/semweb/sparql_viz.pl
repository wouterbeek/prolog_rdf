:- module(
  sparql_viz,
  [
    sparql_viz/4,     % +Query, +Method, +Format, -Out
    sparql_viz_file/3 % +File, +Method, +Format
  ]
).

/** <module> SPARQL visualization

@author Wouter Beek
@version 2017/05, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(graph/dot)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/sparql_parser)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).





%! sparql_viz(+Query:string, +Method:atom, +Format:atom, -Out:stream) is det.

sparql_viz(Query, Method, Format, ProcOut) :-
  sparql_parse('https://example.org/', dataset([],[]), Query, State, Algebra),
  setup_call_cleanup(
    graphviz(Method, ProcIn, Format, ProcOut),
    with_output_to(ProcIn, sparql_viz(State, Algebra)),
    close(ProcIn)
  ).

sparql_viz(State, Algebra) :-
  flag(node_id, _, 1),
  Algebra =.. [Pred|Args],
  format("graph g {\n"),
  format("  node [penwidth=0.5,shape=box]\n"),
  format("  edge [arrowhead=open,penwidth=0.5]\n"),
  format("  ~a [label=<~a>]\n", [0,Pred]),
  append(
    [fn-'http://www.w3.org/2005/xpath-functions#'],
    State.prefixes,
    Prefixes
  ),
  rdf_dcg_options(
    _{prefixes: Prefixes, variable_map: State.variable_map},
    Options
  ),
  maplist(sparql_viz_arg(Options, 0), Args),
  format("}\n").

sparql_viz_arg(Options, N, Var) :-
  var(Var), !,
  write_label(Options, Var, Label),
  write_node(Options, N, _, Label).
sparql_viz_arg(Options, N, Args) :-
  is_list(Args), !,
  maplist(sparql_viz_arg(Options, N), Args).
sparql_viz_arg(Options, N, rdf(S,P,O,_)) :- !,
  dcg_with_output_to(atom(Label), (
    "〈",
    rdf_dcg_subject(S, Options),
    ", ",
    rdf_dcg_predicate(P, Options),
    ", ",
    rdf_dcg_term(O, Options),
    "〉"
  )),
  write_node(Options, N, _, Label).
sparql_viz_arg(Options, N1, QAlg) :-
  \+ var(QAlg),
  QAlg =.. [Pred1|Args], !,
  (   binary_predicate(Pred1, Pred2)
  ->  maplist(write_label(Options), Args, Labels),
      Term =.. [Pred2|Labels],
      write_node(Options, N1, _, Term)
  ;   write_node(Options, N1, N2, Pred1),
      maplist(sparql_viz_arg(Options, N2), Args)
  ).
sparql_viz_arg(Options, N, Term) :-
  write_node(Options, N, _, Term).

binary_predicate(<, '&lt;').
binary_predicate(=<, '=&lt;').
binary_predicate(=, =).
binary_predicate(>=, '&gt;=').
binary_predicate(>, '&gt;').

write_label(Options, Var, Label) :-
  var(Var), !,
  dcg_with_output_to(atom(Label), rdf_dcg_var(Var, Options)).
write_label(Options, Uri, Label) :-
  is_uri(Uri), !,
  dcg_with_output_to(atom(Label), rdf_dcg_iri(Uri, Options)).
write_label(_, Term, Label) :-
  format(atom(Label), "~w", [Term]).

write_node(Options, N1, N2, Term) :-
  flag(node_id, N2, N2+1),
  write_label(Options, Term, Label),
  format("  ~d [label=<~a>]\n", [N2,Label]),
  format("  ~d -- ~d\n", [N1,N2]).



%! sparql_viz_file(+File:atom, +Method:atom, +Format:atom) is det.

sparql_viz_file(File1, Method, Format) :-
  file_name_extension(Base, _, File1),
  file_to_string(File1, Query),
  file_name_extension(Base, Format, File2),
  call_to_file(
    File2,
    {Query,Method,Format}/[Out,Meta,Meta]>>sparql_viz(Query, Method, Format, Out)
  ).
