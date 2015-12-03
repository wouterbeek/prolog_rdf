:- module(
  owl_api,
  [
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
                       % +LanguagePriorityList:list(atom)
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
                  % ?LanguagePriorityList:list(atom)
                  % -LanguageTag:atom
                  % -LexicalForm:atom
  ]
).

:- assert_cc_prefixes.
:- assert_dbpedia_localizations.

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





%! rdf_html_term3(+Term:rdf_term, +Options:list(compound))// is det.

rdf_html_term3(T, Opts) -->
  {term_to_terms(T, Ts)},
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
  user:rdf(S, dbo:thumbnail, Img).
rdf_image3(S, Img):-
  user:rdf(S, foaf:depiction, Img).
rdf_image3(S, Img):-
  user:rdf(S, _, Img),
  rdf_instance3(Img, dcmit:'Image').



% rdf_instance3(?Instance:rdf_term, ?Class:rdf_term) is nondet.

rdf_instance3(I, C):-
  user:rdf(I, rdf:type, C).



%! rdf_langstring3(
%!   ?Subject:rdf_term,
%!   ?Predicate:rdf_term,
%!   +LanguagePriorityList:list(atom),
%!   ?Value:pair(atom)
%! ) is nondet.

rdf_langstring3(S, P, LRanges, V):-
  rdf_literal3(S, P, rdf:langString, V),
  V = _-LTag,
  atom(LTag),
  basic_filtering(LRanges, LTag).



%! rdf_list3(?RdfList:rdf_term, -PrologList:list(rdf_term)) is det.

rdf_list3(rdf:nil, []):- !.
rdf_list3(L1, [H2|T2]):-
  % rdf:first
  user:rdf(L1, rdf:first, H1),
  (   % Nested list
      rdf_instance3(H1, rdf:'List')
  ->  rdf_list3(H1, H2)
  ;   % Non-nested list.
      H2 = H1
  ),
  % rdf:rest
  user:rdf(L1, rdf:rest, T1),
  rdf_list3(T1, T2).



%! rdf_list_member3(?Member:rdf_term, ?List:rdf_term) is nondet.

rdf_list_member3(X, L):-
  user:rdf(L, rdf:first, X).
rdf_list_member3(X, L):-
  user:rdf(L, rdf:rest, L0),
  rdf_list_member3(X, L0).



%! rdf_literal3(
%!   ?Subject:rdf_term,
%!   ?Predicate:rdf_term,
%!   ?Datatype:rdf_term,
%!   ?Value
%! ) is nondet.

% Language-tagged strings.
rdf_literal3(S, P, rdf:langString, Val):-
  Val = Lex-LTag,
  O = literal(lang(LTag,Lex)),
  user:rdf(S, P, O),
  atom(LTag).
% Ground datatype and value.
rdf_literal3(S, P, D, Val):-
  ground(D),
  ground(Val), !,
  % Map to lexical form.
  rdf_canonical_map(D, Val, literal(type(D,Lex))),
  (   rdf_equal(D, xsd:string),
      Lit = literal(Lex)
  ;   Lit = literal(type(D,Lex))
  ),
  user:rdf(S, P, Lit).
% Typed literal (as per RDF 1.0 specification).
rdf_literal3(S, P, D, Val):-
  Lit = literal(type(D,_)),
  user:rdf(S, P, Lit),
  rdf_lexical_map(Lit, Val).
% Simple literal (as per RDF 1.0 specification).
rdf_literal3(S, P, xsd:string, Val):-
  O = literal(Lex),
  user:rdf(S, P, O),
  atom(Lex),
  rdf_global_id(xsd:string, D),
  rdf_lexical_map(D, Lex, Val).



%! rdf_number_of_triples3(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_triples3(S, P, O, N):-
  aggregate_all(count, user:rdf(S, P, O), N).



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
  user:rdf(S, P, O),
  rdf_print_triple(S, P, O, _, Opts).



%! rdfs_label3(
%!   +Subject:rdf_term,
%!   ?LanguagePriorityList:list(atom),
%!   -LanguageTag:atom,
%!   -LexicalForm:atom
%! ) is nondet.

rdfs_label3(S, LRanges, LTag, Lex):-
  rdf_global_id(rdfs:label, P),
  (   % First look for language-tagged strings with matching language tag.
      rdf_langstring3(S, P, LRanges, Lex-LTag)
  ;   % Secondly look for XSD strings with no language tag.
      rdf_global_id(xsd:string, D),
      rdf_literal3(S, P, D, Lex)
  ).
