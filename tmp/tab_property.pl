:- module(
  tab_property,
  [
    tab_property//1,   % +Prop
    tab_properties//1  % ?G
  ]
).

/** <module> RDF tabular predicate term

Generates HTML tables that descrive RDF predicate terms.

@author Wouter Beek
@version 2015/08, 2015/12-2016/01, 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/zh)).
:- use_module(library(http/html_write)).
:- use_module(library(list_ext)).
:- use_module(library(option_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdfs), [rdfs_individual_of/2]).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(tab/tab_generics)).





%! tab_property(+Prop)// is det.

tab_property(Prop) -->
  % The extension of the interpretation of the property consists of pairs.
  % We enumerate the classes of individuals that occur in these pairs.
  % We distinguish between individuals that occur in
  % the first argument position
  % (classes denoting the domain of the property)
  % and the second argument position
  % (classes denoting the range of the property).
  tab_property_domain(Prop),
  tab_property_range(Prop),

  % For literal ranges we also display the values that occur.
  tab_predicate_literals(Prop),

  % Subject-object pairs.
  tab_subject_object_pairs(Prop, _),

  % Triples that describe the property, if any.
  tab_predicate_object_pairs(Prop, _).



%! tab_property_domain(+Prop)// is det.

tab_property_domain(Prop) -->
  {
    aggregate_all(
      set([Dom]),
      (rdf(S, Prop, _), rdfs_individual_of(S, Dom)),
      Rows
    )
  },
  bs_table(
    html(["Overview of the domain of property ",\zh_property(Prop),"."]),
    html_table_header_row(["Class"]),
    html_maplist(zh_class_row, Rows)
  ).



%! tab_property_range(+Prop)// is det.

tab_property_range(Prop) -->
  {
    aggregate_all(
      set([Ran]),
      (rdf(_, Prop, O), rdfs_individual_of(O, Ran)),
      Rows
    )
  },
  bs_table(
    html(["Overview of the range of property ",\zh_property(Prop),"."]),
    html_table_header_row(["Class"]),
    html_maplist(zh_class_row, Rows)
  ).
  


%! tab_predicate_literals(+P)// is det.

tab_predicate_literals(P) -->
  {
    aggregate_all(
      set([Lex]),
      (rdf(_, P, O), rdf_is_literal(O), rdf_lexical_form(O, Lex)),
      Rows
    ),
    length(Rows, Len)
  },
  bs_table(
    html([
      \qh_predicate(P),
      " has ",
      \html_thousands(Len),
      " unique values."
    ]),
    \html_table_header_row(["Literal value"]),
    \html_maplist(zh_literal_row, Rows)
  ).



%! tab_properties(?G)// is det.

tab_properties(G) -->
  {
    (var(G) -> Goal_0 = rdf_predicate(P) ; Goal_0 = rdf_predicate(P, G)),
    aggregate_all(set(P), Goal_0, Ps),
    maplist(rdf_number_of_triples0, Ps, Ns),
    pairs_keys_values(Pairs, Ns, Ps),
    desc_pairs(Pairs, DescPairs),
    list_truncate(DescPairs, 100, TopPairs),
    maplist(tab_properties_row0, TopPairs, TopRows)
  },
  bs_table(
    html(["Overview of properties",\zh_in_graph(G),"."]),
    html_table_header_row(['Property','Occurrences']),
    maplist(html_table_data_row, TopRows)
  ).
rdf_number_of_triples0(P, N):-
  rdf_number_of_triples(_, P, _, N).
tab_properties_row0(N-P, [P,N]).





% HELPERS %

%! zh_class_row(+C)// is det.

zh_class_row(C) -->
  html(tr(td(\zh_class(C)))).



%! zh_in_graph(?G)// is det.

zh_in_graph(G) --> {var(G)}, !, [].
zh_in_graph(G) --> html([" in RDF graph ",\zh_graph(G)]).



%! zh_literal_row(+Lit)// is det.

zh_literal_row(Lit) -->
  html(tr(td(\zh_literal(Lit)))).
