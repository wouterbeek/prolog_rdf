:- module(
  rdfs_ext,
  [
    rdfs_assert_class/5,       % +C, ?D, ?Lbl, ?Comment, ?G
    rdfs_assert_comment/2,     % +S, +Comment
    rdfs_assert_comment/3,     % +S, +Comment, ?G
    rdfs_assert_domain/3,      % +P, +C, ?G
    rdfs_assert_isDefinedBy/2, % +S, ?G
    rdfs_assert_isDefinedBy/3, % +S, ?Iri, ?G
    rdfs_assert_label/2,       % +S, +Lbl
    rdfs_assert_label/3,       % +S, +Lbl, ?G
    rdfs_assert_property/4,    % +C, +P, +D, ?G
    rdfs_assert_range/3,       % +P, +C, ?G
    rdfs_assert_seeAlso/3,     % +S, +Uri, +G
    rdfs_assert_subclass/2,    % +C, ?G
    rdfs_assert_subclass/3,    % +C, ?D, ?G
    rdfs_assert_subproperty/3, % +P, +Q, ?G
    rdfs_class/1,              % ?C
    rdfs_class/2,              % ?C, ?G
    rdfs_domain/2,             % ?P, ?Dom
    rdfs_domain/3,             % ?P, ?Dom, ?G
    rdfs_has/3,                % ?S, ?P, ?O
    rdfs_has/4,                % ?S, ?P, ?O, ?Q
    rdfs_has/5,                % ?S, ?P, ?O, ?Q, ?G
    rdfs_image/2,              % +S, -Img
    rdfs_instance/2,           % ?I, ?C
    rdfs_instance/3,           % ?I, ?C, ?G
    rdfs_pref_label/2,         % ?S, -Lit
    rdfs_pref_lex/3,         % ?S, ?P, ?Lex
    rdfs_pref_string/3,      % ?S, ?P, ?Lit
    rdfs_property/1,           % ?Prop
    rdfs_property/2,           % ?Prop, ?G
    rdfs_range/2,              % ?P, ?Ran
    rdfs_range/3,              % ?P, ?Ran, ?G
    rdfs_retractall_class/1    % +C
  ]
).

/** <module> RDFS extensions

@author Wouter Beek
@version 2016/04-2016/06
*/

:- use_module(library(nlp/nlp_lang)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(solution_sequences)).
:- use_module(library(vocab/vocab_ext)).
:- use_module(library(z/z_datatype)).
:- use_module(library(z/z_term)).

:- rdf_meta
   rdfs_assert_class(r, t, ?, ?, r),
   rdfs_assert_comment(r, +),
   rdfs_assert_comment(r, +, r),
   rdfs_assert_domain(r, r, r),
   rdfs_assert_instance(r, r),
   rdfs_assert_instance(r, r, r),
   rdfs_assert_instances(r, t),
   rdfs_assert_instances(r, t, r),
   rdfs_assert_isDefinedBy(r, r),
   rdfs_assert_isDefinedBy(r, r, r),
   rdfs_assert_label(r, +),
   rdfs_assert_label(r, +, r),
   rdfs_assert_list(r, r, t, r),
   rdfs_assert_property(r, r, r, r),
   rdfs_assert_range(r, r, r),
   rdfs_assert_seeAlso(r, +, r),
   rdfs_assert_subclass(r, r),
   rdfs_assert_subclass(r, t, r),
   rdfs_assert_subproperty(r, t, r),
   rdfs_class(r),
   rdfs_class(r, r),
   rdfs_domain(r, r),
   rdfs_domain(r, r, r),
   rdfs_has(r, r, o),
   rdfs_has(r, r, o, r, r),
   rdfs_image(r, -),
   rdfs_instance(o, r),
   rdfs_instance(o, r, r),
   rdfs_lts(r, r, o),
   rdfs_pref_label(r, o),
   rdfs_pref_lex(r, r, ?),
   rdfs_pref_string(r, r, o),
   rdfs_pref_string(r, r, -, o),
   rdfs_property(r),
   rdfs_property(r, r),
   rdfs_range(r, r),
   rdfs_range(r, r, r),
   rdfs_retractall_class(r).





%! rdfs_assert_class(+C, ?D, ?Lbl, ?Comment, ?G) is det.

rdfs_assert_class(C, Parent, Lbl, Comm, G) :-
  rdf_assert_instance(C, rdfs:'Class', G),
  rdfs_assert_class0(C, Parent, Lbl, Comm, G).



%! rdfs_assert_comment(+S, +Comment) is det.
%! rdfs_assert_comment(+S, +Comment, ?G) is det.

rdfs_assert_comment(S, Comment) :-
  rdfs_assert_comment(S, Comment, _).


rdfs_assert_comment(S, Comment, G) :-
  rdf_assert(S, rdfs:comment, Comment, G).



%! rdfs_assert_domain(+P, +C, ?G) is det.

rdfs_assert_domain(P, D, G) :-
  rdf_assert(P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+S, ?G) is det.
%! rdfs_assert_isDefinedBy(+S, ?Iri, ?G) is det.
%
% If Iri is uninstantiated, the IRI denoted by the registered RDF
% prefix of Term, if any, is used.

rdfs_assert_isDefinedBy(S, G) :-
  rdfs_assert_isDefinedBy(S, _, G).

rdfs_assert_isDefinedBy(S, Prefix, G) :-
  var(Prefix), !,
  rdf_iri_alias_prefix_local(S, _, Prefix, _),
  rdf_assert(S, rdfs:isDefinedBy, Prefix^^xsd:anyURI, G).
rdfs_assert_isDefinedBy(S, Iri, G) :-
  rdf_assert(S, rdfs:isDefinedBy, Iri^^xsd:anyURI, G).



%! rdfs_assert_label(+S, +Lbl) is det.
%! rdfs_assert_label(+S, +Lbl, ?G) is det.
%
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

rdfs_assert_label(S, Lbl) :-
  rdfs_assert_label(S, Lbl, _).


rdfs_assert_label(S, Lbl, G) :-
  rdf_assert(S, rdfs:label, Lbl, G).



%! rdfs_assert_property(+C, +P, +D, ?G) is det.

rdfs_assert_property(C, P, D, G) :-
  rdfs_assert_domain(P, C, G),
  rdfs_assert_range(P, D, G).



%! rdfs_assert_range(+P, +C, ?G) is det.

rdfs_assert_range(P, C, G) :-
  rdf_assert(P, rdfs:range, C, G).



%! rdfs_assert_seeAlso(+S, +Iri, ?G) is det.

rdfs_assert_seeAlso(S, Iri, G) :-
  rdf_assert(S, rdfs:seeAlso, Iri, G).



%! rdfs_assert_subclass(+C, ?G) is det.
%! rdfs_assert_subclass(+C, ?D, ?G) is det.
% If D is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(C, G) :-
  rdfs_assert_subclass(C, _, G).
rdfs_assert_subclass(C, D, G) :-
  % Allow the parent class to be uninstantiated.
  (   var(D)
  ->  rdf_assert(C, rdfs:subClassOf, rdfs:'Resource', G)
  ;   is_list(D)
  ->  forall(member(D0, D), rdf_assert(C, rdfs:subClassOf, D0, G))
  ;   rdf_assert(C, rdfs:subClassOf, D, G)
  ).



%! rdfs_assert_subproperty(+P, ?Q, ?G) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% If Q is uninstantiated it defaults to `rdf:Property`.

rdfs_assert_subproperty(P, Qs, G) :-
  is_list(Qs), !,
  forall(member(Q, Qs), rdfs_assert_subproperty(P, Q, G)).
rdfs_assert_subproperty(P, Q, G) :-
  rdf_defval(rdf:'Property', Q),
  rdf_assert(P, rdfs:subPropertyOf, Q, G).



%! rdfs_class(?C) is semidet.
%! rdfs_class(?C, ?G) is nondet.

rdfs_class(C) :-
  distinct(C, rdfs_class(C, _)).


rdfs_class(C, G) :-
  distinct(C, rdfs_class0(C, G)).


rdfs_class0(C, G) :-
  rdfs_instance(C, rdfs:'Class', G).
rdfs_class0(C, G) :-
  rdfs_has(C, rdfs:subClassOf, _, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdfs:subClassOf, C, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdfs:domain, C, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdfs:range, C, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdf:type, C, _, G).



%! rdfs_domain(?P, ?Dom) is nondet.
%! rdfs_domain(?P, ?Dom, ?G) is nondet.

rdfs_domain(P, Dom) :-
  distinct(P-Dom, rdfs_domain(P, Dom, _)).


rdfs_domain(P, Dom, G) :-
  rdfs_has(P, rdfs:domain, Dom, _, G).



%! rdfs_has(?S, ?P, ?O) is nondet.
%! rdfs_has(?S, ?P, ?O, -Q) is nondet.
%! rdfs_has(?S, ?P, ?O, -Q, ?G) is nondet.

rdfs_has(S, P, O) :-
  rdfs_has(S, P, O, _).


rdfs_has(S, P, O, Q) :-
  rdfs_has(S, P, O, Q, _).


rdfs_has(S, P, O, Q, G) :-
  rdfs_has(S, P, O, Q),
  rdf(S, Q, O, G).



%! rdfs_image(+S, -Img) is nondet.

rdfs_image(S, Img) :-
  rdfs_has(S, dbo:thumbnail, Img^^xsd:anyURI).
rdfs_image(S, Img) :-
  rdfs_has(S, foaf:depiction, Img^^xsd:anyURI).
rdfs_image(S, Img) :-
  rdf(S, _, Img),
  rdfs_instance(Img, dcmit:'Image').



%! rdfs_instance(?I, ?C) is nondet.
%! rdfs_instance(?I, ?C, ?G) is nondet.

rdfs_instance(I, D) :-
  distinct(I-D, rdfs_instance(I, D, _)).


rdfs_instance(I, D, G) :-
  distinct(I-D-G, rdfs_instance0(I, D, G)).


rdfs_instance0(I, D, G) :-
  nonvar(D), !,
  rdf_reachable(C, rdfs:subClassOf, D), % @tbd
  rdfs_has(I, rdf:type, C, _, G).
rdfs_instance0(I, D, G) :-
  rdfs_has(I, rdf:type, C, _, G),
  rdf_reachable(C, rdfs:subClassOf, D). % @tbd
rdfs_instance0(Lex^^C, D, G) :-
  rdf(_, _, Lex^^C, G),
  z_subdatatype_of(C, D).
rdfs_instance0(Lex@LTag, rdf:langString, G) :-
  rdf(_, _, Lex@LTag, G).



%! rdfs_lts(?S, ?P, -Lit) is nondet.
%
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.  Notice that results for
% each of the prioritized languages are given in arbitrary order.

rdfs_lts(S, P, Lit) :-
  current_lrange(LRange),
  rdfs_lts(S, P, LRange, Lit).


rdfs_lts(S, P, LRange, Lit) :-
  rdfs_has(S, P, V@LTag),
  basic_filtering(LRange, LTag),
  Lit = V@LTag.



%! rdfs_pref_lex(?S, ?P, ?Lex) is nondet.
%
% Like rdfs_pref_string, but returns only the lexical form.

rdfs_pref_lex(S, P, Lex) :-
  rdfs_pref_string(S, P, Lit),
  z_literal_lex(Lit, Lex).



%! rdfs_pref_label(?S, -Lit) is nondet.

rdfs_pref_label(S, Lit) :-
  rdfs_pref_string(S, rdfs:label, Lit).



%! rdfs_pref_string(?S, ?P, ?Lit) is nondet.
%
% Returns, in this exact order:
%
%   1. The language-tagged strings that match the given language
%   priority list; returning results for higher priority language
%   earlier.
%
%   2. The language-tagged strings that do not match the given
%   language priority list.
%
%   3. XSD strings.

rdfs_pref_string(S, P, Lit) :-
  current_lrange(LRange),
  rdfs_pref_string(S, P, LRange, Lit).


% Matching language-tagged strings.
rdfs_pref_string(S, P, LRange, Lit) :-
  rdfs_lts(S, P, LRange, Lit).
% Non-matching language-tagged strings.
rdfs_pref_string(S, P, LRange, Lit) :-
  rdfs_has(S, P, V@LTag),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = V@LTag.
% Plain XSD strings.
rdfs_pref_string(S, P, _, V^^xsd:string) :-
  rdfs_has(S, P, V^^xsd:string).



%! rdfs_property(?Prop) is nondet.
%! rdfs_property(?Prop, ?G) is nondet.

rdfs_property(Prop) :-
  distinct(Prop, rdfs_property(Prop, _)).


rdfs_property(Prop, G) :-
  distinct(Prop-G, rdfs_property0(Prop, G)).


rdfs_property0(Prop, G) :-
  rdf_predicate(Prop, G).
rdfs_property0(Prop, G) :-
  rdfs_instance(Prop, rdf:'Property', G).



%! rdfs_range(?P, ?Ran) is nondet.
%! rdfs_range(?P, ?Ran, ?G) is nondet.

rdfs_range(P, Ran) :-
  rdfs_range(P, Ran, _).


rdfs_range(P, Ran, G) :-
  rdfs_has(P, rdfs:range, Ran, _, G).



%! rdfs_retractall_class(+C) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under
% identity.
%
% This connects all subclasses of Class to all superclasses of Class.

rdfs_retractall_class(C) :-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      rdfs_has(SubC, rdfs:subClassOf, C),
      rdfs_has(C, rdfs:subClassOf, SuperC)
    ),
    (
      % The transitive link is now a direct one.
      rdfs_assert_subclass(SubC, SuperC, _),
      % Remove the link to a subclass.
      rdf_retractall(SubC, rdfs:subClassOf, C)
    )
  ),

  % [2] Remove the links to superclasses.
  rdf_retractall(C, rdfs:subClassOf, _),

  % [3] Remove other triples in which the class occurs.
  rdf_retractall(C, _, _),
  rdf_retractall(_, C, _),
  rdf_retractall(_, _, C).





% HELPERS %

rdfs_assert_class0(C, Parent, Lbl, Comm, G) :-
  rdfs_assert_subclass(C, Parent, G),
  (var(Lbl) -> true ; rdfs_assert_label(C, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(C, Comm, G)),
  rdfs_assert_isDefinedBy(C, G).



%! basic_filtering(
%!   +LanguagePriorityList:list(atom),
%!   +LanguageTag:atom
%! ) is semidet.
% Succeeds if the LanguagePriorityList matches the LanguageTag according to
% the basic filtering algorithm described in RFC 4647,
% i.e., if the former is a case-insensitive prefix of the latter,
% while also treating the `*` sign as a wildcard.
%
% @compat RFC 4647

basic_filtering(Ranges, Tag):-
  % NONDET
  member(Range, Ranges),
  atomic_list_concat(Subtags1, -, Range),
  atomic_list_concat(Subtags2, -, Tag),
  basic_filtering0(Subtags1, Subtags2), !.


basic_filtering0(_, []).
basic_filtering0([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  basic_filtering0(T1, T2).



%! subtag_match(+RangeSubtag:atom, +Subtag:atom) is semidet.
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).
