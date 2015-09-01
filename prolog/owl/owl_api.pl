:- module(
  owl_api,
  [
    rdf_assert3/1, % +Triple:compound
    rdf_assert3/3, % +Subject:rdf_term
                   % +Predicate:iri
                   % +Object:rdf_term
    rdf_assert_instance3/2, % +Instance:rdf_term
                            % +Class:rdf_term
    rdf_assert_list3/2, % +PrologList:list(rdf_term)
                        % ?RdfList:rdf_term
    rdf_assert_literal3/4, % +Subject:rdf_term
                           % +Predicate:rdf_term
                           % ?Datatype:rdf_term
                           % +Value
    rdf3/3, % ?Subject:rdf_term
            % ?Predicate:iri
            % ?Object:rdf_term
    rdf_description_size3/2, % +Resource:rdf_term
                             % ?Size:nonneg
    rdf_html_term3//2, % +Term:rdf_term
                       % +Options:list(compound)
    rdf_html_triple3//4, % +Subject:rdf_term
                         % +Predicate:rdf_term
                         % +Object:rdf_term
                         % +Options:list(compound)
    rdf_image3/2, % ?Subject:rdf_term
                  % ?Image:iri
    rdf_instance3/2, % ?Instance:rdf_term
                     % ?Class:rdf_term
    rdf_langstring3/4, % ?Subject:rdf_term
                       % ?Predicate:rdf_term
                       % +LanguagePreference:atom
                       % ?Value:pair(atom)
    rdf_list3/2, % ?RdfList:rdf_term
                 % -PrologList:list(rdf_term)
    rdf_list_member3/2, % ?Member:rdf_term
                        % ?List:rdf_term
    rdf_literal3/4, % ?Subject:rdf_term
                    % ?Predicate:rdf_term
                    % ?Datatype:rdf_term
                    % ?Value
    rdf_number_of_triples3/4, % ?Subject:rdf_term
                              % ?Predicate:rdf_term
                              % ?Object:rdf_term
                              % ?Size:nonneg
    rdf_print3/0,
    rdf_print3/1, % +Options:list(compound)
    rdf_print_triple3/3, % ?Subject:rdf_term
                         % ?Predicate:rdf_term
                         % ?Object:rdf_term
    rdf_print_triple3/4, % ?Subject:rdf_term
                         % ?Predicate:rdf_term
                         % ?Object:rdf_term
                         % +Options:list(compound)
    rdfs_label3/4 % +Subject:rdf_term
                  % ?LanguagePreference:atom
                  % -Language:atom
                  % -LexicalForm:atom
  ]
).

/** <module> OWL API

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(html/content/html_collection)).
:- use_module(library(html/rdf_html_term)).
:- use_module(library(lambda)).
:- use_module(library(langtag/langtag_match)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(owl/id_store)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- assert_cc_prefixes.
:- assert_dbpedia_localizations.

:- rdf_meta(rdf_assert3(t)).
:- rdf_meta(rdf_assert3(o,r,o)).
:- rdf_meta(rdf_assert_instance3(o,o)).
:- rdf_meta(rdf_assert_list3(t,o)).
:- rdf_meta(rdf_assert_literal3(o,o,o,+)).
:- rdf_meta(rdf3(o,r,o)).
:- rdf_meta(rdf_description_size3(o,?)).
:- rdf_meta(rdf_html_term3(o,+,?,?)).
:- rdf_meta(rdf_html_triple3(o,o,o,+,?,?)).
:- rdf_meta(rdf_image3(o,?)).
:- rdf_meta(rdf_instance3(o,o)).
:- rdf_meta(rdf_langstring3(o,o,+,?)).
:- rdf_meta(rdf_list3(o,t)).
:- rdf_meta(rdf_list_member3(o,o)).
:- rdf_meta(rdf_literal3(o,o,o,?)).
:- rdf_meta(rdf_number_of_triples3(o,o,o,?)).
:- rdf_meta(rdf_print_triple3(o,o,o,+)).
:- rdf_meta(rdfs_label3(o,?,-,-)).

:- predicate_options(rdf_print3/1, 1, [
     pass_to(rdf_print_triple3/4, 4)
   ]).
:- predicate_options(rdf_print_triple3/4, 4, [
     pass_to(rdf_print_triple/5, 5)
   ]).





%! rdf_assertt3(+Triple:compound) is det.

rdf_assert3(rdf(S,P,O)):-
  rdf_assert3(S, P, O).



%! rdf_assert3(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.

rdf_assert3(S, P, O):-
  rdf_global_id(owl:sameAs, P), !,
  store_id(S, O).
rdf_assert3(S, P, O):-
  maplist(term_id, [S,P,O], [S0,P0,O0]),
  rdf_assert(S0, P0, O0).



%! rdf_assert_instance3(+Instance:rdf_term, +Class:rdf_term) is det.

rdf_assert_instance3(I, C):-
  rdf_assert3(I, rdf:type, C).



%! rdf_assert_list3(+PrologList:list, ?RdfList:or([bnode,iri])) is det.
% Asserts the given, possibly nested, list into RDF.

rdf_assert_list3(L1, L2):-
  rdf_transaction(rdf_assert_list0(L1, L2)).

rdf_assert_list0([], rdf:nil):- !.
rdf_assert_list0(L1, L2):-
  add_list_instance0(L2),
  rdf_assert_list_items0(L1, L2).

rdf_assert_list_items0([], rdf:nil):- !.
rdf_assert_list_items0([H1|T1], L2):-
  % rdf:first
  (   % Nested list.
      is_list(H1)
  ->  rdf_assert_list0(H1, H2)
  ;   % Non-nested list.
      H2 = H1
  ),
  rdf_assert3(L2, rdf:first, H2),

  % rdf:rest
  (   T1 == []
  ->  rdf_global_id(rdf:nil, T2)
  ;   add_list_instance0(T2),
      rdf_assert_list_items0(T1, T2)
  ),
  rdf_assert3(L2, rdf:rest, T2).

add_list_instance0(L):-
  (var(L) -> rdf_bnode(L) ; true),
  rdf_assert_instance3(L, rdf:'List').



%! rdf_assert_literal3(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value
%! ) is det.

% Language-tagged strings.
rdf_assert_literal3(S, P, rdf:langString, Lang-Lex):- !,
  rdf_assert3(S, P, literal(lang(Lang,Lex))).
% Simple literals (as per RDF 1.0 specification)
% assumed to be of type `xsd:string` (as per RDF 1.1 specification).
rdf_assert_literal3(S, P, D, V):-
  var(D), !,
  rdf_assert_literal3(S, P, xsd:string, V).
% Typed literals (as per RDF 1.0 specification).
rdf_assert_literal3(S, P, D, V):-
  rdf_canonical_map(D, V, Lex),
  rdf_assert3(S, P, literal(type(D,Lex))).



%! rdf3(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is nondet.

rdf3(S, P, O):-
  rdf_global_id(owl:sameAs, P),
  (   nonvar(S)
  ->  term_term(S, O)
  ;   nonvar(O)
  ->  term_term(O, S)
  ;   id_terms(_, Ts),
      member(S, O, Ts)
  ).
rdf3(S, P, O):-
  (nonvar(S) -> (S = id(SId) -> true ; term_id(S, SId)) ; true),
  (nonvar(P) -> term_id(P, PId) ; true),
  (nonvar(O) -> term_id(O, OId) ; true),
  rdf(SId, PId, OId),
  (ground(S) -> true ; id_term(SId, S)),
  id_term(PId, P),
  id_term(OId, O).



%! rdf_description_size3(+Resource:rdf_term, -Size:nonneg) is det.

rdf_description_size3(S, N):-
  rdf_number_of_triples3(S, _, _, N).



%! rdf_html_term3(+Term:rdf_term, +Options:list(compound))// is det.

rdf_html_term3(T, Opts) -->
  {term_terms(T, Ts)},
  html_set(\T^rdf_html_term(T, Opts), Ts).



%! rdf_html_triple3(
%!   +Subject:rdf_term,
%!   +Predicate:rdf_term,
%!   +Object:rdf_term,
%!   +Options:list(compound)
%! )// is det.

rdf_html_triple3(S, P, O, Opts) -->
  html_triple(\T^rdf_html_term3(T, Opts), S, P, O).



%! rdf_image3(?Subject:rdf_term, ?Image:iri) is nondet.

rdf_image3(S, Img):-
  rdf3(S, dbo:thumbnail, Img).
rdf_image3(S, Img):-
  rdf3(S, foaf:depiction, Img).
rdf_image3(S, Img):-
  rdf3(S, _, Img),
  rdf_instance3(Img, dcmit:'Image').



% rdf_instance3(?Instance:rdf_term, ?Class:rdf_term) is nondet.

rdf_instance3(I, C):-
  rdf3(I, rdf:type, C).



%! rdf_langstring3(
%!   ?Subject:rdf_term,
%!   ?Predicate:rdf_term,
%!   +LanguagePreference:atom,
%!   ?Value:pair(atom)
%! ) is nondet.

rdf_langstring3(S, P, Pref, V):-
  rdf_literal3(S, P, rdf:langString, V),
  V = Lang-_,
  atom(Lang),
  basic_filtering(Pref, Lang).



%! rdf_list3(?RdfList:rdf_term, -PrologList:list(rdf_term)) is det.

rdf_list3(rdf:nil, []):- !.
rdf_list3(L1, [H2|T2]):-
  % rdf:first
  rdf3(L1, rdf:first, H1),
  (   % Nested list
      rdf_instance3(H1, rdf:'List')
  ->  rdf_list3(H1, H2)
  ;   % Non-nested list.
      H2 = H1
  ),
  % rdf:rest
  rdf3(L1, rdf:rest, T1),
  rdf_list3(T1, T2).



%! rdf_list_member3(?Member:rdf_term, ?List:rdf_term) is nondet.

rdf_list_member3(X, L):-
  rdf3(L, rdf:first, X).
rdf_list_member3(X, L):-
  rdf3(L, rdf:rest, L0),
  rdf_list_member3(X, L0).



%! rdf_literal3(
%!   ?Subject:rdf_term,
%!   ?Predicate:rdf_term,
%!   ?Datatype:rdf_term,
%!   ?Value
%! ) is nondet.

% Language-tagged strings.
rdf_literal3(S, P, rdf:langString, V):-
  V = Lang-Lex,
  O = literal(lang(Lang,Lex)),
  rdf3(S, P, O),
  atom(Lang).
% Ground datatype and value.
rdf_literal3(S, P, D, V):-
  ground(D),
  ground(V), !,
  % Map to lexical form.
  rdf_canonical_map(D, V, Lex),
  (   rdf_equal(D, xsd:string),
      O = literal(Lex)
  ;   O = literal(type(D,Lex))
  ),
  rdf3(S, P, O).
% Typed literal (as per RDF 1.0 specification).
rdf_literal3(S, P, D, V):-
  O = literal(type(D,Lex)),
  rdf3(S, P, O),
  rdf_lexical_map(D, Lex, V).
% Simple literal (as per RDF 1.0 specification).
rdf_literal3(S, P, xsd:string, V):-
  O = literal(Lex),
  rdf3(S, P, O),
  atom(Lex),
  rdf_global_id(xsd:string, D),
  rdf_lexical_map(D, Lex, V).



%! rdf_number_of_triples3(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_triples3(S, P, O, N):-
  aggregate_all(count, rdf3(S, P, O), N).



%! rdf_print3 is det.

rdf_print3:-
  rdf_print3([]).

%! rdf_print3(+Options:list(compound)).

rdf_print3(Opts):-
  rdf_print_triple3(_, _, _, Opts),
  fail.
rdf_print3(_).



%! rdf_print_triple3(
%!   ?Subject:rdf_term,
%!   ?Predicate:rdf_term,
%!   ?Object:rdf_term
%! ) is nondet
% Wrapper around rdf_print_triple3/4 with default options.

rdf_print_triple3(S, P, O):-
  rdf_print_triple(S, P, O, []).

%! rdf_print_triple3(
%!   ?Subject:rdf_term,
%!   ?Predicate:rdf_term,
%!   ?Object:rdf_term,
%!   +Options:list(compound)
%! ) is nondet.

rdf_print_triple3(S, P, O, Opts):-
  % NONDET
  rdf3(S, P, O),
  rdf_print_triple(S, P, O, _, Opts).



%! rdfs_label3(
%!   +Subject:rdf_term,
%!   ?LanguagePreference:atom,
%!   -Language:atom,
%!   -LexicalForm:atom
%! ) is nondet.

rdfs_label3(S, Pref, Lang, Lex):-
  rdf_global_id(rdfs:label, P),
  (   % First look for language-tagged strings with matching language tag.
      rdf_langstring3(S, P, Pref, Lang-Lex)
  ;   % Secondly look for XSD strings with no language tag.
      rdf_global_id(xsd:string, D),
      rdf_literal3(S, P, D, Lex)
  ).
