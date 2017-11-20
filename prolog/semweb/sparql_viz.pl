:- module(
  sparql_viz,
  [
    sparql_viz/2 % +Query, +Out
  ]
).

/** <module> SPARQL visualization

Example of use:

```prolog
?- graphviz_show(dot, gtk, [Out]>>sparql_viz(Out, graph:default)).
```

@author Wouter Beek
@version 2017/05, 2017/08, 2017/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(graph/dot)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).





%! sparql_viz(+Query:string, +Out:stream) is det.

sparql_viz(Query, Out) :-
  sparql_parse('https://example.org/', dataset([],[]), Query, State, Algebra),
  debug_format(sparql, Out, "graph g {"),
  debug_format(sparql, Out, "  node [penwidth=0.5,shape=box]"),
  debug_format(sparql, Out, "  edge [arrowhead=open,penwidth=0.5]"),
  append(
    [fn-'http://www.w3.org/2005/xpath-functions#'],
    State.prefixes,
    Prefixes
  ),
  rdf_dcg_options(
    _{prefixes: Prefixes, variable_map: State.variable_map},
    Options
  ),
  sparql_viz_term(Out, Algebra, Options).
  debug_format(sparql, Out, "}").



%! sparql_viz_edge(+Out:stream, +Term1:term, +Term2:term) is det.

sparql_viz_edge(Out, Term1, Term2) :-
  maplist(dot_id, [Term1,Term2], [Id1,Id2]),
  debug_format(sparql, Out, "  ~a -- ~a", [Id1,Id2]).



%! write_label(+Term:term, -Label:string, +Options:list(compound)) is det.

write_label(Var, Label, Options) :-
  var(Var), !,
  dcg_with_output_to(string(Label), rdf_dcg_var(Var, Options)).
write_label(Uri, Label, Options) :-
  is_uri(Uri), !,
  dcg_with_output_to(string(Label), rdf_dcg_iri(Uri, Options)).
write_label(Term, Label, _) :-
  format(string(Label), "~w", [Term]).



%! write_node(+Out:stream, +Term:term, +Options:list(compound)) is det.

write_node(Out, Term, Options) :-
  dot_id(Term, Id),
  write_label(Term, Label, Options),
  debug_format(sparql, Out, "  ~d [label=<~a>]", [Id,Label]).



%! sparql_viz_term(+Out:stream, +Term:term, +Options:list(compound)) is det.

sparql_viz_term(Out, Var, Options) :-
  var(Var), !,
  write_label(Var, Label, Options),
  write_node(Out, Label, Options).
sparql_viz_term(Out, Args, Options) :-
  is_list(Args), !,
  maplist(
    {Out,Options}/[Arg]>>sparql_viz_term(Out, Arg, Options),
    Args
  ).
sparql_viz_term(Out, rdf(S,P,O,_), Options) :- !,
  dcg_with_output_to(string(Label), (
    "〈",
    rdf_dcg_subject(S, Options),
    ", ",
    rdf_dcg_predicate(P, Options),
    ", ",
    rdf_dcg_term(O, Options),
    "〉"
  )),
  write_node(Out, Label, Options).
sparql_viz_term(Out, Term, Options) :-
  compound(Term),
  Term =.. [Pred1|Args], !,
  (   binary_predicate(Pred1, Pred2)
  ->  maplist(
        {Options}/[Arg,Label]>>write_label(Arg, Label, Options),
        Args,
        Labels
      ),
      Term =.. [Pred2|Labels],
      write_node(Out, Term, Options)
  ;   write_node(Out, Pred1, Options),
      maplist(
        {Out,Options}/[Arg]>>sparql_viz_term(Out, Arg, Options),
        Args
      ),
      maplist(sparql_viz_edge(Out, Pred1), Args)
  ).
sparql_viz_term(Out, Term, Options) :-
  write_node(Out, Term, Options).

binary_predicate(<, '&lt;').
binary_predicate(=<, '=&lt;').
binary_predicate(=, =).
binary_predicate(>=, '&gt;=').
binary_predicate(>, '&gt;').
