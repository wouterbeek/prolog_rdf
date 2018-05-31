:- module(
  rdf_term,
  [
    rdf_atom_to_term/2,           % +Atom, -Term
    rdf_bnode_iri/1,              % -Iri
    rdf_bnode_iri/2,              % ?Local, -Iri
    rdf_bnode_iri/3,              % +Document, ?Local, -Iri
    rdf_bnode_prefix/1,           % -Iri
    rdf_bnode_prefix/2,           % +Document, -Iri
   %rdf_create_bnode/1,           % --BNode
    rdf_create_iri/3,             % +Alias, +Segments, -Iri
   %rdf_equal/2,                  % ?Term1, ?Term2
   %rdf_default_graph/1,          % ?G
   %rdf_graph/1,                  % ?G
    rdf_iri//1,                   % -Iri
   %rdf_is_bnode/1,               % @Term
    rdf_is_bnode_iri/1,           % @Term
    rdf_is_container_membership_property/1, % @Term
   %rdf_is_iri/1,                 % @Term
   %rdf_is_literal/1,             % @Term
    rdf_is_name/1,                % @Term
    rdf_is_object/1,              % @Term
   %rdf_is_predicate/1,           % @Term
    rdf_is_skip_node/1,           % @Term
   %rdf_is_subject/1,             % @Term
    rdf_is_term/1,                % @Term
    rdf_language_tagged_string/3, % ?LTag, ?Lex, ?Literal
    rdf_lexical_value/3,          % +D, ?Lex, ?Val
    rdf_literal//1,               % -Literal
    rdf_literal/4,                % ?D, ?LTag, ?Lex, ?Literal
    rdf_literal_datatype_iri/2,   % +Literal, ?D
    rdf_literal_lexical_form/2,   % +Literal, ?Lex
    rdf_literal_value/2,          % +Literal, -Value
    rdf_literal_value/3,          % -Literal, +D, +Value
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
     rdf_is_predicate/1,
     rdf_is_subject/1,
     (rdf_meta)/1,
     op(1150, fx, (rdf_meta))
   ]).

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(uuid)).

:- use_module(library(atom_ext)).
:- use_module(library(dcg)).
:- use_module(library(hash_ext)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(uri_ext)).
:- use_module(library(xsd/xsd)).

:- discontiguous
    rdf_lexical_to_value/3,
    rdf_value_to_lexical/3.

:- dynamic
    rdf_lexical_to_value_hook/3,
    rdf_value_to_lexical_hook/3.

:- multifile
    rdf_lexical_to_value_hook/3,
    rdf_value_to_lexical_hook/3.

:- rdf_meta
   rdf_is_bnode_iri(r),
   rdf_is_name(o),
   rdf_is_object(o),
   rdf_is_skip_node(r),
   rdf_is_term(o),
   rdf_language_tagged_string(?, ?, o),
   rdf_lexical_value(r, ?, ?),
   rdf_lexical_to_value(r, +, -),
   rdf_lexical_to_value_error(r, +),
   rdf_value_to_lexical(r, +, -),
   rdf_literal(r, ?, ?, o),
   rdf_literal_datatype_iri(o, r),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_value(o, -),
   rdf_literal_value(o, r, +),
   rdf_term_to_atom(t, -),
   rdf_typed_literal(r, ?, o),
   rdf_value_to_lexical(r, +, -),
   rdf_value_to_lexical_error(r, +).

:- setting(base_uri, atom, 'https://example.org/base-uri/',
           "The default base URI for RDF IRIs.").
:- setting(bnode_prefix_authority, atom, 'example.org', "").
:- setting(bnode_prefix_scheme, atom, https, "").





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
  rdf_global_id(Alias:Local, Iri).
rdf_atom_to_term(Atom, _) :-
  throw(rdf(cannot_parse(rdf_term,Atom))).



%! rdf_bnode_iri(-Iri:atom) is det.
%! rdf_bnode_iri(+Local:atom, -Iri:atom) is det.
%! rdf_bnode_iri(+Document:atom, +Local:atom, -Iri:atom) is det.

rdf_bnode_iri(Iri) :-
  uuid(Local),
  rdf_bnode_iri(Local, Iri).


rdf_bnode_iri(Local, Iri) :-
  rdf_bnode_iri_([Local], Iri).


rdf_bnode_iri(Doc, Local, Iri) :-
  md5(Doc, DocId),
  rdf_bnode_iri_([DocId,Local], Iri).

rdf_bnode_iri_(T, Iri) :-
  setting(bnode_prefix_scheme, Scheme),
  setting(bnode_prefix_authority, Auth),
  uri_comps(Iri, uri(Scheme,Auth,['.well-known',genid|T],_,_)).



%! rdf_bnode_prefix(-Iri:atom) is det.
%! rdf_bnode_prefix(+Document:atom, -Iri:atom) is det.

rdf_bnode_prefix(Iri) :-
  rdf_bnode_prefix_([], Iri).


rdf_bnode_prefix(Doc, Iri) :-
  md5(Doc, DocId),
  rdf_bnode_prefix_([DocId], Iri).

rdf_bnode_prefix_(T, Iri) :-
  setting(bnode_prefix_scheme, Scheme),
  setting(bnode_prefix_authority, Auth),
  uri_comps(Iri0, uri(Scheme,Auth,['.well-known',genid|T],_,_)),
  atom_terminator(Iri0, 0'/, Iri).



%! rdf_create_iri(+Alias, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Alias, Segments2, Iri) :-
  rdf_prefix(Alias, Prefix),
  uri_comps(Prefix, uri(Scheme,Auth,Segments1,_,_)),
  append_segments(Segments1, Segments2, Segments3),
  uri_comps(Iri, uri(Scheme,Auth,Segments3,_,_)).



%! rdf_iri(-Iri:iri)// .

rdf_iri(Iri) -->
  "<",
  ...(Codes),
  ">", !,
  {atom_codes(Iri, Codes)}.



%! rdf_is_container_membership_property(@Term) is semidet.

rdf_is_container_membership_property(P) :-
  rdf_is_iri(P),
  rdf_equal(rdf:'_', Prefix),
  atom_concat(Prefix, Atom, P),
  atom_number(Atom, N),
  integer(N),
  N >= 0.



%! rdf_is_bnode_iri(@Term) is semidet.

rdf_is_bnode_iri(Iri) :-
  rdf_is_iri(Iri),
  uri_comps(Iri, uri(Scheme,Auth,Segments,_,_)),
  maplist(ground, [Scheme,Auth]),
  prefix(['.well-known',genid], Segments).



%! rdf_is_name(@Term) is semidet.

rdf_is_name(Iri) :-
  rdf_is_iri(Iri), !.
rdf_is_name(Literal) :-
  rdf_is_literal(Literal).



%! rdf_is_object(@Term) is semidet.

rdf_is_object(S) :-
  rdf_is_subject(S), !.
rdf_is_object(Literal) :-
  rdf_is_literal(Literal).



%! rdf_is_skip_node(@Term) is semidet.

rdf_is_skip_node(Term) :-
  rdf_is_bnode(Term), !.
rdf_is_skip_node(Term) :-
  rdf_is_bnode_iri(Term).



%! rdf_is_term(@Term) is semidet.

rdf_is_term(O) :-
  rdf_is_object(O).



%! rdf_language_tagged_string(+LTag:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_language_tagged_string(-LTag:atom, -Lex:atom, +Literal:rdf_literal) is det.

rdf_language_tagged_string(LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_lexical_value(+D:atom, +Lex:atom, -Value:term) is det.
%! rdf_lexical_value(+D:atom, -Lex:atom, +Value:term) is det.
%
% Translate between a value (`Value') and its serialization, according
% to a given datatype IRI (`D'), into a lexical form (`Lex').
%
% @error syntax_error(+Literal:compound)
% @error type_error(+D:atom,+Value:term)
% @error unimplemented_datatype_iri(+D:atom)

rdf_lexical_value(D, Lex, Value) :-
  (   nonvar(Lex)
  ->  rdf_lexical_to_value(D, Lex, Value)
  ;   rdf_value_to_lexical(D, Value, Lex)
  ), !.
rdf_lexical_value(D, _, _) :-
  throw(error(unimplemented_datatype_iri(D))).


% hooks
rdf_lexical_to_value(D, Lex, Value) :-
  rdf_lexical_to_value_hook(D, Lex, Value), !.
rdf_value_to_lexical(D, Value, Lex) :-
  rdf_value_to_lexical_hook(D, Value, Lex), !.
% rdf:HTML
rdf_lexical_to_value(rdf:'HTML', Lex, Value) :- !,
  (   rdf11:parse_partial_xml(load_html, Lex, Value)
  ->  true
  ;   rdf_lexical_to_value_error(rdf:'HTML', Lex)
  ).
rdf_value_to_lexical(rdf:'HTML', Value, Lex) :-
  (   rdf11:write_xml_literal(html, Value, Lex)
  ->  true
  ;   rdf_value_to_lexical_error(rdf:'HTML', Value)
  ).

% rdf:XMLLiteral
rdf_lexical_to_value(rdf:'XMLLiteral', Lex, Value) :-
  (   rdf11:parse_partial_xml(load_xml, Lex, Value)
  ->  true
  ;   rdf_lexical_to_value_error(rdf:'XMLLiteral', Lex)
  ).
rdf_value_to_lexical(rdf:'XMLLiteral', Value, Lex) :-
  (   rdf11:write_xml_literal(xml, Value, Lex)
  ->  true
  ;   rdf_value_to_lexical_error(rdf:'XMLLiteral', Value)
  ).

% XSD datatype IRIs
rdf_lexical_to_value(D, Lex, Value) :-
  xsd_lexical_to_value(D, Lex, Value).
rdf_value_to_lexical(D, Value, Lex) :-
  xsd_value_to_lexical(D, Value, Lex).

rdf_lexical_to_value_error(D, Lex) :-
  syntax_error(literal(type(D,Lex))).

rdf_value_to_lexical_error(D, Value) :-
  type_error(D, Value).



%! rdf_literal(-Literal:rdf_literal)// .
%
% Parses N-Triples 1.1 literals according to its syntactic delimiters,
% but does not check for validity of the content characters.

rdf_literal(Literal) -->
  "\"",
  ...(Codes),
  "\"", !,
  ("^^" -> rdf_iri(D) ; "@" -> remainder_as_atom(LTag) ; ""),
  {
    atom_codes(Lex, Codes),
    rdf_literal(D, LTag, Lex, Literal)
  }.



%! rdf_literal(+D:iri, +LTag:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_literal(-D:iri, -LTag:atom, -Lex:atom, +Literal:rdf_literal) is det.
%
% Compose/decompose literals.

rdf_literal(D, LTag, Lex, literal(type(D,Lex))) :-
  var(LTag).
rdf_literal(rdf:langString, LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_literal_datatype_iri(+Literal:rdf_literal, +D:iri) is semidet.
%! rdf_literal_datatype_iri(+Literal:rdf_literal, -D:iri) is det.

rdf_literal_datatype_iri(literal(type(D,_)), D).
rdf_literal_datatype_iri(literal(lang(_,_)), rdf:langString).



%! rdf_literal_lexical_form(+Literal:rdf_literal, +Lex:atom) is semidet.
%! rdf_literal_lexical_form(+Literal:rdf_literal, -Lex:atom) is det.

rdf_literal_lexical_form(literal(type(_,Lex)), Lex).
rdf_literal_lexical_form(literal(lang(_,Lex)), Lex).



%! rdf_literal_value(+Literal:rdf_literal, -Value:term) is det.
%! rdf_literal_value(+Literal:rdf_literal, -D:iri, -Value:term) is det.
%! rdf_literal_value(-Literal:rdf_literal, +D:iri, +Value:term) is det.
%
% Notice that languages-tagged strings do not have a value.

rdf_literal_value(Literal, Value) :-
  rdf_literal_value(Literal, _, Value).


% `rdf:langString' does not have a value space.
rdf_literal_value(literal(lang(LTag,Lex)), rdf:langString, Lex-LTag) :- !.
rdf_literal_value(literal(type(D,Lex)), D, Value) :-
  rdf_lexical_value(D, Lex, Value).



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
