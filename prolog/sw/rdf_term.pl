:- module(
  rdf_term,
  [
    rdf_atom_to_term/2,           % +Atom, -Term
    rdf_bnode_iri/1,              % -Iri
    rdf_bnode_iri/2,              % +Local, -Iri
   %rdf_create_bnode/1,           % --BNode
   %rdf_equal/2,                  % ?Term1, ?Term2
   %rdf_default_graph/1,          % ?G
   %rdf_graph/1,                  % ?G
    rdf_iri//1,                   % -Iri
   %rdf_is_bnode/1,               % @Term
   %rdf_is_iri/1,                 % @Term
   %rdf_is_literal/1,             % @Term
   %rdf_is_object/1,              % @Term
   %rdf_is_predicate/1,           % @Term
    rdf_is_skip_node/1,           % @Term
   %rdf_is_subject/1,             % @Term
    rdf_is_well_known_iri/1,      % @Term
    rdf_language_tagged_string/3, % ?LTag, ?Lex, ?Literal
    rdf_lexical_value/3,          % ?D, ?Lex, ?Val
    rdf_literal//1,               % -Literal
    rdf_literal/4,                % ?D, ?LTag, ?Lex, ?Literal
    rdf_literal_datatype_iri/2,   % +Literal, ?D
    rdf_literal_lexical_form/2,   % +Literal, ?Lex
    rdf_literal_value/2,          % +Literal, -Value
    rdf_term//1,                  % -Term
    rdf_term_to_atom/2,           % +Term, -Atom
    rdf_typed_literal/3           % ?D, ?Lex, ?Literal
   %(rdf_meta)/1,
   %op(1150, fx, (rdf_meta))
  ]
).

/** <module> RDF term support

@author Wouter Beek
@version 2018
*/

:- reexport(library(semweb/rdf_db), [
     rdf_is_literal/1
   ]).
:- reexport(library(semweb/rdf11), [
     rdf_create_bnode/1,
     rdf_equal/2,
     rdf_default_graph/1,
     rdf_graph/1,
     rdf_is_bnode/1,
     rdf_is_iri/1,
     rdf_is_object/1,
     rdf_is_predicate/1,
     rdf_is_subject/1,
     (rdf_meta)/1,
     op(1150, fx, (rdf_meta))
   ]).

:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(settings)).
:- use_module(library(uuid)).

:- rdf_meta
   rdf_is_skip_node(r),
   rdf_is_well_known_iri(r),
   rdf_language_tagged_string(?, ?, o),
   rdf_lexical_value(r, ?, ?),
   rdf_literal(r, ?, ?, o),
   rdf_literal_datatype_iri(o, r),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_value(o, -),
   rdf_term_to_atom(t, -),
   rdf_typed_literal(r, ?, o).

:- setting(base_uri, atom, 'https://example.org/base-uri/',
           "The default base URI for RDF IRIs.").
:- setting(bnode_prefix, atom, 'https://example.org/.well-known/genid/',
           "The IRI prefix of Skolemized blank nodes.").





%! rdf_atom_to_term(+Atom:atom, -Term:rdf_term) is semidet.
%
% Parses the given atom (`Atom') in order to extract the encoded RDF
% term.  The following syntactic forms are supported:
%
%  1. RDF terms defined by the N-Triples 1.1 grammar (blank nodes,
%     IRIs, and literals).
%
%  2. The Turtle 1.1 abbreviation ‘a’ for ‘rdf:type’.
%
%  3. Turtle 1.1 prefix notation for IRIs.
%
% @throws rdf(cannot_parse,rdf_term,Atom) if no RDF term can be parsed
% from `Atom'.

rdf_atom_to_term(Atom, Term) :-
  atom_codes(Atom, Codes),
  phrase(rdf_term(Term), Codes), !.
% Expansion of commonly used abbreviation `a'.
rdf_atom_to_term(a, Iri) :- !,
  rdf_equal(rdf:type, Iri).
% Expansion of commonly used prefixes.
rdf_atom_to_term(Atom, Iri) :-
  atomic_list_concat([Alias,Local], :, Atom),
  rdf_prefix(Alias), !,
  rdf_prefix_iri(Alias:Local, Iri).
rdf_atom_to_term(Atom, _) :-
  throw(rdf(cannot_parse,rdf_term,Atom)).



%! rdf_bnode_iri(-Iri:atom) is det.

rdf_bnode_iri(Iri) :-
  uuid(Uuid),
  rdf_bnode_iri(Uuid, Iri).



%! rdf_bnode_iri(+Local:atom, -Iri:atom) is det.

rdf_bnode_iri(Local, Iri2) :-
  setting(bnode_prefix, Iri1),
  uri_resolve(Iri1, Local, Iri2).



%! rdf_iri(-Iri:iri)// .

rdf_iri(Iri) -->
  "<",
  ...(Codes),
  ">", !,
  {atom_codes(Iri, Codes)}.



%! rdf_is_skip_node(@Term) is semidet.

rdf_is_skip_node(Term) :-
  rdf_is_bnode(Term), !.
rdf_is_skip_node(Term) :-
  rdf_is_well_known_iri(Term).



%! rdf_is_well_known_iri(@Term) is semidet.

rdf_is_well_known_iri(Iri) :-
  rdf_is_iri(Iri),
  uri_components(Iri, uri_components(Scheme,Authority,Path,_,_)),
  ground(Scheme-Authority),
  atom_prefix(Path, '/.well-known/genid/').



%! rdf_language_tagged_string(+LTag:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_language_tagged_string(-LTag:atom, -Lex:atom, +Literal:rdf_literal) is det.

rdf_language_tagged_string(LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_lexical_value(+D:atom, +Lex:atom, -Value:term) is det.
%! rdf_lexical_value(+D:atom, -Lex:atom, +Value:term) is det.
%
% Translate between a value (`Value') and its serialization, according
% to a given datatype IRI (`D'), into a lexical form (`Lex').

rdf_lexical_value(rdf:'HTML', Lex, Dom) :-
  (   atom(Lex)
  ->  load_structure(atom(Lex), Dom, [dialect(html5),max_errors(0)])
  ;   rdf11:write_xml_literal(html, Dom, Lex)
  ).
rdf_lexical_value(rdf:'XMLLiteral', Lex, Dom) :-
  (   atom(Lex)
  ->  load_structure(atom(Lex), Dom, [dialect(xml),max_errors(0)])
  ;   rdf11:write_xml_literal(xml, Dom, Lex)
  ).
rdf_lexical_value(D, Lex, Val) :-
  xsd_lexical_value(D, Lex, Val).



%! rdf_literal(-Literal:rdf_literal)// .
%
% Parses N-Triples 1.1 literals according to its syntactic delimiters,
% but does not check for validity of the content characters.

rdf_literal(Literal) -->
  "\"",
  ...(Codes),
  "\"", !,
  ("^^" -> rdf_iri(D) ; "@" -> rest_as_atom(LTag) ; ""),
  {
    atom_codes(Lex, Codes),
    rdf_literal(D, LTag, Lex, Literal)
  }.



%! rdf_literal(+D:iri, +LTag:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_literal(-D:iri, -LTag:atom, -Lex:atom, +Literal:rdf_literal) is det.
%
% Compose/decompose literals.

rdf_literal(D, _, Lex, literal(type(D,Lex))).
rdf_literal(rdf:langString, LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_literal_datatype_iri(+Literal:rdf_literal, +D:iri) is semidet.
%! rdf_literal_datatype_iri(+Literal:rdf_literal, -D:iri) is det.

rdf_literal_datatype_iri(literal(type(D,_)), D).
rdf_literal_datatype_iri(literal(lang(_,_)), rdf:langString).



%! rdf_literal_lexical_form(+Literal:rdf_literal, +Lex:atom) is semidet.
%! rdf_literal_lexical_form(+Literal:rdf_literal, -Lex:atom) is det.

rdf_literal_lexical_form(literal(type(_,Lex)), Lex).
rdf_literal_lexical_form(literal(lang(_,Lex)), Lex).



%! rdf_literal_value(+Literal:rdf_literal, -Value) is det.
%
% Notice that languages-tagged strings do not have a value.

rdf_literal_value(literal(type(D,Lex)), Value) :- !,
  rdf_lexical_value(D, Lex, Value).
rdf_literal_value(literal(lang(LTag,Lex)), _) :-
  existence_error(rdf_value,LTag-Lex).



%! rdf_term(-Term:rdf_term)// .

rdf_term(Iri) -->
  rdf_iri(Iri), !.
rdf_term(Literal) -->
  rdf_literal(Literal).
rdf_term(BNode) -->
  "_:",
  remainder(T),
  {atom_codes(BNode, [0'_,0':|T])}.



%! rdf_term_to_atom(+Term:rdf_term, -Atom:atom) is det.

rdf_term_to_atom(literal(lang(LTag,Lex)), Atom) :-
  nonvar(LTag), !,
  format(atom(Atom), '"~a"@~a', [Lex,LTag]).
rdf_term_to_atom(literal(type(D,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"^^<~a>', [Lex,D]).
rdf_term_to_atom(Iri, Atom) :-
  rdf_is_iri(Iri), !,
  format(atom(Atom), '<~a>', [Iri]).
rdf_term_to_atom(BNode, BNode) :-
  rdf_is_bnode(BNode), !.
rdf_term_to_atom(Term, _) :-
  type_error(rdf_term, Term).



%! rdf_typed_literal(+D:iri, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_typed_literal(-D:iri, -Lex:atom, +Literal:rdf_literal) is det.

rdf_typed_literal(D, Lex, literal(type(D,Lex))).