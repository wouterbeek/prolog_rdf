:- encoding(utf8).
:- module(
  rdf_term,
  [
    rdf_atom_term/2,                     % ?Atom, ?Term
    rdf_bnode_iri/1,                     % -Iri
    rdf_bnode_iri/2,                     % ?Local, -Iri
    rdf_bnode_iri/3,                     % +Scope, ?Local, -Iri
    rdf_bnode_prefix/1,                  % -Iri
    rdf_bnode_prefix/2,                  % +Scope, -Iri
    rdf_canonical_lexical_form/3,        % +D, +Lex, -CanonicaldLex
    rdf_canonical_literal/2,             % +Literal, -CanonicaldLiteral
    rdf_container_membership_property/1, % ?P
    rdf_container_membership_property/2, % ?P, ?N
   %rdf_create_bnode/1,                  % --BNode
    rdf_create_hash_iri/3,               % +Alias, +Term, -Iri
    rdf_create_hash_iri/4,               % +Alias, +Segments, +Term, -Iri
    rdf_create_iri/3,                    % +Alias, +Segments, -Iri
   %rdf_default_graph/1,                 % ?G
   %rdf_graph/1,                         % ?G
    rdf_iri//1,                          % ?Iri
   %rdf_is_bnode/1,                      % @Term
    rdf_is_bnode_iri/1,                  % @Term
   %rdf_is_iri/1,                        % @Term
   %rdf_is_literal/1,                    % @Term
    rdf_is_name/1,                       % @Term
    rdf_is_numeric_literal/1,            % @Term
    rdf_is_object/1,                     % @Term
   %rdf_is_predicate/1,                  % @Term
    rdf_is_skip_node/1,                  % @Term
   %rdf_is_subject/1,                    % @Term
    rdf_is_term/1,                       % @Term
    rdf_language_tagged_string/3,        % ?LTag, ?Lex, ?Literal
    rdf_lexical_value/3,                 % +D, ?Lex, ?Val
    rdf_literal//1,                      % ?Literal
    rdf_literal/4,                       % ?D, ?LTag, ?Lex, ?Literal
    rdf_literal_datatype_iri/2,          % +Literal, ?D
    rdf_literal_dwim/2,                  % +DWIM, ?Literal
    rdf_literal_lexical_form/2,          % +Literal, ?Lex
    rdf_literal_value/2,                 % +Literal, -Value
    rdf_literal_value/3,                 % ?Literal, ?D, ?Value
    rdf_name_string/2,                   % +Name, -String
    rdf_object_dwim/2,                   % +DWIM, ?Term
    rdf_predicate_dwim/2,                % +DWIM, ?P
    rdf_term//1,                         % ?Term
    rdf_term_to_string/2,                % +Term, -String
    rdf_typed_literal/3,                 % ?D, ?Lex, ?Literal
    tp_object_dwim/2,                    % ?DWIM, -O
    tp_predicate_dwim/2,                 % ?DWIM, -P
    well_known_iri/2                     % +Segments, -Iri
  ]
).

/** <module> RDF term support

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).
:- use_module(library(lists)).
:- reexport(library(semweb/rdf_db), [
     rdf_is_bnode/1,
     rdf_is_literal/1
   ]).
:- reexport(library(semweb/rdf11), [
     rdf_create_bnode/1,
     rdf_default_graph/1,
     rdf_graph/1,
     rdf_is_iri/1,
     rdf_is_predicate/1,
     rdf_is_subject/1
   ]).
:- use_module(library(settings)).
:- use_module(library(uuid)).

:- use_module(library(atom_ext)).
:- use_module(library(dcg)).
:- use_module(library(gis/wkt)).
:- use_module(library(hash_ext)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(string_ext)).
:- use_module(library(uri_ext)).
:- use_module(library(xsd/xsd)).
:- use_module(library(xsd/xsd_grammar)).

:- discontiguous
    rdf_lexical_to_value/3,
    rdf_value_to_lexical/3.

:- dynamic
    rdf_lexical_to_value_hook/3,
    rdf_value_to_lexical_hook/3.

:- maplist(rdf_register_prefix, [geo,rdf]).

:- multifile
    rdf_lexical_to_value_hook/3,
    rdf_value_to_lexical_hook/3.

:- rdf_meta
   rdf_atom_term(?, o),
   rdf_canonical_lexical_form(r, +, -),
   rdf_canonical_literal(o, o),
   rdf_container_membership_property(r),
   rdf_container_membership_property(r, ?),
   rdf_is_bnode_iri(r),
   rdf_is_name(o),
   rdf_is_numeric_literal(o),
   rdf_is_object(o),
   rdf_is_skip_node(r),
   rdf_is_term(o),
   rdf_language_tagged_string(?, ?, o),
   rdf_lexical_to_value(r, +, -),
   rdf_lexical_to_value_error(r, +),
   rdf_lexical_value(r, ?, ?),
   rdf_literal(r, ?, ?, o),
   rdf_literal_datatype_iri(o, r),
   rdf_literal_dwim(o, -),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_value(o, -),
   rdf_literal_value(o, r, ?),
   rdf_name_string(o, -),
   rdf_object_dwim(t, -),
   rdf_predicate_dwim(r, r),
   rdf_term_to_string(o, -),
   rdf_typed_literal(r, ?, o),
   rdf_value_to_lexical(r, +, -),
   rdf_value_to_lexical_error(r, +),
   tp_object_dwim(o, -),
   tp_predicate_dwim(r, -).

:- setting(base_uri, atom, 'https://example.org/base-uri/',
           "The default base URI for RDF IRIs.").
:- setting(bnode_prefix_authority, atom, 'example.org', "").
:- setting(bnode_prefix_scheme, atom, https, "").





%! rdf_atom_term(+Atom:atom, +Term:rdf_term) is semidet.
%! rdf_atom_term(+Atom:atom, -Term:rdf_term) is semidet.
%! rdf_atom_term(-Atom:atom, +Term:rdf_term) is semidet.
%
% Parses the given Atom in order to extract the encoded RDF Term.  The
% following syntactic forms are supported:
%
%  1. RDF terms defined by the N-Triples 1.1 grammar (blank nodes,
%     IRIs, and literals).
%
%  2. Turtle 1.1 prefix notation for IRIs.
%
% @throws syntax_error if Atom cannot be parsed as a term in
%         Turtle-family notation.
%
% @throws type_error if the Turtle-family notation for Term cannot be
%         generated.

rdf_atom_term(Atom, Term) :-
  atom_phrase(rdf_term(Term), Atom), !.
rdf_atom_term(Atom, _) :-
  atom(Atom), !,
  syntax_error(grammar(rdf,term,Atom)).
rdf_atom_term(_, Term) :-
  type_error(rdf_term, Term).

:- begin_tests(rdf_atom_term).

test('rdf_atom_term(+,+)', [forall(test_rdf_atom_term(Atom,Term))]) :-
  rdf_atom_term(Atom, Term).
test('rdf_atom_term(+,-)', [forall(test_rdf_atom_term(Atom,Term))]) :-
  rdf_atom_term(Atom, Term0),
  assertion(Term == Term0).
test('rdf_atom_term(-,+)', [forall(test_rdf_atom_term(Atom,Term))]) :-
  rdf_atom_term(Atom0, Term),
  assertion(Atom == Atom0).

test_rdf_atom_term('<mailto:x>', 'mailto:x').
test_rdf_atom_term('""^^<mailto:x>', literal(type('mailto:x',''))).

:- end_tests(rdf_atom_term).



%! rdf_bnode(+BNode:rdf_bnode)// .
%! rdf_bnode(-BNode:rdf_bnode)// .
%
% Generates or parses a blank node in Turtle-family notation.

rdf_bnode(BNode) -->
  {ground(BNode)}, !,
  rdf_bnode_generate_(BNode).
rdf_bnode(BNode) -->
  "_:",
  rdf_bnode_parse_(BNode).

rdf_bnode_generate_(BNode) -->
  atom(BNode).

rdf_bnode_parse_(BNode) -->
  remainder(T),
  {atom_codes(BNode, [0'_,0':|T])}.



%! rdf_bnode_iri(-Iri:atom) is det.
%! rdf_bnode_iri(+Local:atom, -Iri:atom) is det.
%! rdf_bnode_iri(+Scope:atom, +Local:atom, -Iri:atom) is det.

rdf_bnode_iri(Iri) :-
  uuid(Local),
  rdf_bnode_iri(Local, Iri).


rdf_bnode_iri(Local, Iri) :-
  well_known_iri([Local], Iri).


rdf_bnode_iri(Scope, Local, Iri) :-
  md5_text(Scope, ScopeId),
  well_known_iri([ScopeId,Local], Iri).



%! rdf_bnode_prefix(-Iri:atom) is det.
%! rdf_bnode_prefix(+Scope:atom, -Iri:atom) is det.

rdf_bnode_prefix(Iri) :-
  rdf_bnode_prefix_([], Iri).


rdf_bnode_prefix(Scope, Iri) :-
  md5_text(Scope, ScopeId),
  rdf_bnode_prefix_([ScopeId], Iri).

rdf_bnode_prefix_(Segments, Iri) :-
  well_known_iri(Segments, Iri0),
  atom_terminator(Iri0, '/', Iri).



%! rdf_canonical_lexical_form(+D:iri, +Lex:atom, -CanonicaldLex:atom) is det.

rdf_canonical_lexical_form(D, Lex, CanonicalLex) :-
  rdf_lexical_value(D, Lex, Value),
  rdf_lexical_value(D, CanonicalLex, Value).



%! rdf_canonical_literal(+Literal:rdf_literal, -CanonicaldLiteral:rdf_literal) is det.

rdf_canonical_literal(Literal, CanonicalLiteral) :-
  rdf_literal_value(Literal, D, Value),
  rdf_literal_value(CanonicalLiteral, D, Value).



%! rdf_container_membership_property(+P:rdf_predicate) is semidet.
%! rdf_container_membership_property(-P:rdf_predicate) is multi.

rdf_container_membership_property(P) :-
  rdf_container_membership_property(P, _).


%! rdf_container_membership_property(+P:rdf_predicate, +N:positive_integer) is semidet.
%! rdf_container_membership_property(+P:rdf_predicate, -N:positive_integer) is det.
%! rdf_container_membership_property(-P:rdf_predicate, +N:positive_integer) is det.
%! rdf_container_membership_property(-P:rdf_predicate, -N:positive_integer) is multi.
%
% True when Property is the Nth container membership property.
%
% Success of this goal does not imply that Property is present in the
% database.

rdf_container_membership_property(P, N) :-
  var(P), !,
  between(1, inf, N),
  rdf_equal(rdf:'_', Prefix),
  atom_concat(Prefix, N, P).
rdf_container_membership_property(P, N) :-
  rdf_equal(rdf:'_', Prefix),
  atom_concat(Prefix, Atom, P),
  atom_number(Atom, N),
  integer(N),
  N >= 0.



%! rdf_create_hash_iri(+Alias:atom, +Term:term, -Iri:atom) is det.
%! rdf_create_hash_iri(+Alias:atom, +Segments:list(atom), +Term:term, -Iri:atom) is det.

rdf_create_hash_iri(Alias, Term, Iri) :-
  rdf_create_hash_iri(Alias, [], Term, Iri).


rdf_create_hash_iri(Alias, Segments1, Term, Iri) :-
  md5(Term, Hash),
  append(Segments1, [Hash], Segments2),
  rdf_create_iri(Alias, Segments2, Iri).



%! rdf_create_iri(+Alias, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Alias, Segments2, Iri) :-
  rdf_prefix(Alias, Prefix),
  uri_comps(Prefix, uri(Scheme,Auth,Segments1,_,_)),
  append_segments(Segments1, Segments2, Segments3),
  uri_comps(Iri, uri(Scheme,Auth,Segments3,_,_)).



%! rdf_iri(+Iri:atom)// .
%! rdf_iri(-Iri:atom)// .
%
% Generates or parses an IRI in Turtle-family notation.

% Generate a full or abbreviated IRI.
rdf_iri(Iri) -->
  {ground(Iri)}, !,
  rdf_iri_generate_(Iri).
rdf_iri(Iri) -->
  rdf_iri_parse_(Iri).

% Generate a full IRI.
rdf_iri_generate_(Iri) -->
  "<",
  atom(Iri),
  ">".

% Parse a full IRI.
rdf_iri_parse_(Iri) -->
  "<",
  ...(Codes),
  ">", !,
  {atom_codes(Iri, Codes)}.
% Parse an abbreviated IRI.
rdf_iri_parse_(Iri) -->
  ...(Codes),
  ":",
  {
    atom_codes(Alias, Codes),
    (rdf_prefix(Alias) -> true ; existence_error(rdf_alias,Alias))
  },
  remainder_as_atom(Local),
  {rdf_prefix_iri(Alias:Local, Iri)}.



%! rdf_is_bnode_iri(@Term) is semidet.

rdf_is_bnode_iri(Iri) :-
  rdf_is_iri(Iri),
  uri_comps(Iri, uri(Scheme,Auth,Segments,_,_)),
  ground([Scheme,Auth]),
  prefix(['.well-known',genid], Segments).



%! rdf_is_name(@Term) is semidet.

rdf_is_name(Iri) :-
  rdf_is_iri(Iri), !.
rdf_is_name(Literal) :-
  rdf_is_literal(Literal).



%! rdf_is_numeric_literal(@Term) is semidet.

rdf_is_numeric_literal(literal(type(D,_))) :-
  xsd_numeric_type(D).



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

rdf_lexical_value(D, Lex, Value) :-
  nonvar(Lex), !,
  rdf_lexical_to_value(D, Lex, Value).
rdf_lexical_value(D, Lex, Value) :-
  rdf_value_to_lexical(D, Value, Lex).

% hooks
rdf_lexical_to_value(D, Lex, Value) :-
  rdf_lexical_to_value_hook(D, Lex, Value), !.
rdf_value_to_lexical(D, Value, Lex) :-
  rdf_value_to_lexical_hook(D, Value, Lex), !.
% geo:hasGeometry
rdf_lexical_to_value(geo:wktLiteral, Lex, Value) :- !,
  (   wkt_shape_atom(Value0, Lex)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(geo:wktLiteral,Lex)
  ).
rdf_value_to_lexical(geo:wktLiteral, Value, Lex) :- !,
  (   wkt_shape_atom(Value, Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(geo:wktLiteral, Value)
  ).
% rdf:HTML
rdf_lexical_to_value(rdf:'HTML', Lex, Value) :- !,
  (   rdf11:parse_partial_xml(load_html, Lex, Value0)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(rdf:'HTML', Lex)
  ).
rdf_value_to_lexical(rdf:'HTML', Value, Lex) :- !,
  (   rdf11:write_xml_literal(html, Value, Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(rdf:'HTML', Value)
  ).

% rdf:XMLLiteral
rdf_lexical_to_value(rdf:'XMLLiteral', Lex, Value) :- !,
  (   rdf11:parse_partial_xml(load_xml, Lex, Value0)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(rdf:'XMLLiteral', Lex)
  ).
rdf_value_to_lexical(rdf:'XMLLiteral', Value, Lex) :- !,
  (   rdf11:write_xml_literal(xml, Value, Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(rdf:'XMLLiteral', Value)
  ).

% xsd:anyURI
rdf_lexical_to_value(xsd:anyURI, Lex, Value) :- !,
  (   is_uri(Lex)
  ->  Value = Lex
  ;   rdf_lexical_to_value_error(xsd:anyURI, Lex)
  ).
rdf_value_to_lexical(xsd:anyURI, Value, Lex) :- !,
  (   is_uri(Value)
  ->  Lex = Value
  ;   rdf_value_to_lexical_error(xsd:anyURI, Value)
  ).

% xsd:boolean
rdf_lexical_to_value(xsd:boolean, Lex, Value) :- !,
  (   xsd_lexical_to_value_boolean(Lex, Value0)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(xsd:boolean, Lex)
  ).

xsd_lexical_to_value_boolean('0', false).
xsd_lexical_to_value_boolean(false, false).
xsd_lexical_to_value_boolean('1', true).
xsd_lexical_to_value_boolean(true, true).

rdf_value_to_lexical(xsd:boolean, Value, Lex) :- !,
  (   xsd_value_to_lexical_boolean(Value, Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(xsd:boolean, Value)
  ).

xsd_value_to_lexical_boolean(false, false).
xsd_value_to_lexical_boolean(true, true).

% xsd:dayTimeDuration
rdf_lexical_to_value(xsd:dayTimeDuration, Lex, Value) :- !,
  (   atom_phrase(dayTimeDurationMap(Value0), Lex)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(xsd:dayTimeDuration, Lex)
  ).
rdf_value_to_lexical(xsd:dayTimeDuration, Value, Lex) :- !,
  (   atom_phrase(dayTimeDurationCanonicalMap(Value), Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(xsd:dayTimeDuration, Value)
  ).

% xsd:decimal
rdf_lexical_to_value(xsd:decimal, Lex, Value) :- !,
  (   atom_phrase(decimalLexicalMap(Value0), Lex)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(xsd:decimal, Lex)
  ).
rdf_value_to_lexical(xsd:decimal, Value, Lex) :- !,
  (   atom_phrase(decimalCanonicalMap(Value), Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(xsd:decimal, Value)
  ).

% xsd:byte
% xsd:decimal
% xsd:double
% xsd;float
% xsd:int
% xsd:integer
% xsd:long
% xsd:negativeInteger
% xsd:nonNegativeInteger
% xsd:nonPositiveInteger
% xsd:positiveInteger
% xsd:short
% xsd:unsignedByte
% xsd:unsignedInt
% xsd:unsignedLong
% xsd:unsignedShort
rdf_lexical_to_value(D, Lex, Value) :-
  rdf11:xsd_numerical(D, Domain, Type), !,
  (   (   Type == double
      ->  catch(xsd_number_string(Value0, Lex), _, fail)
      ;   Type == integer
      ->  catch(xsd_number_string(Value0, Lex), _, fail),
          rdf11:check_integer_domain(Domain, D, Value0)
      )
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(D, Lex)
  ).
rdf_value_to_lexical(D, Value, Lex) :-
  rdf11:xsd_numerical(D, Domain, Type), !,
  (   rdf11:in_number(Type, Domain, D, Value, Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(D, Value)
  ).

% xsd:string
rdf_lexical_to_value(xsd:string, Lex, Value) :- !,
  (   atom_string(Lex, Value0)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(xsd:string, Lex)
  ).
rdf_value_to_lexical(xsd:string, Value, Lex) :- !,
  (   atom_string(Atom, Value)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(xsd:string, Value)
  ).

% xsd:date
% xsd:dateTime
% xsd:gDay
% xsd:gMonth
% xsd:gMonthDay
% xsd:gYear
% xsd:gYearMonth
% xsd:time
rdf_lexical_to_value(D, Lex, Dt) :-
  xsd_date_time_type(D), !,
  (   catch(xsd_time_string(XsdDt, D, Lex), _, fail)
  ->  xsd_date_time(Dt, D, XsdDt)
  ;   rdf_lexical_to_value_error(D, Lex)
  ).
rdf_value_to_lexical(D, Dt, Lex) :-
  xsd_date_time_type(D), !,
  (   xsd_date_time(Dt, D, XsdDt),
      catch(xsd_time_string(XsdDt, D, String), _, true),
      atom_string(Atom, String)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(D, Dt)
  ).

rdf_lexical_to_value(D, Lex, _) :-
  (   ground(D)
  ->  throw(error(unimplemented_lex2val(D,Lex),rdf_lexical_to_value/3))
  ;   instantiation_error(D)
  ).
rdf_value_to_lexical(D, Value, _) :-
  (   ground(D)
  ->  throw(error(unimplemented_val2lex(D,Value),rdf_value_to_lexical/3))
  ;   instantiation_error(D)
  ).

rdf_lexical_to_value_error(D, Lex) :-
  syntax_error(grammar(D,Lex)).
rdf_value_to_lexical_error(D, Value) :-
  type_error(D, Value).

:- begin_tests(rdf_lexical_value).

:- rdf_meta
   test_rdf_lexical_value(r, ?, ?).

test('rdf_lexical_value(+,+)', [forall(test_rdf_lexical_value(D, Lex, Value))]) :-
  rdf_lexical_value(D, Lex, Value).
test('rdf_lexical_value(+,-)', [forall(test_rdf_lexical_value(D, Lex, Value))]) :-
  rdf_lexical_value(D, Lex, Value0),
  assertion(Value == Value0).
test('rdf_lexical_value(-,+)', [forall(test_rdf_lexical_value(D, Lex, Value))]) :-
  rdf_lexical_value(D, Lex0, Value),
  assertion(Lex == Lex0).

test_rdf_lexical_value(xsd:string, abc, "abc").

:- end_tests(rdf_lexical_value).



%! rdf_literal(+Literal:rdf_literal)// .
%! rdf_literal(-Literal:rdf_literal)// .
%
% Generates or parses a literal in Turtle-family notation.

rdf_literal(Literal) -->
  {ground(Literal)}, !,
  rdf_literal_generate_(Literal).
rdf_literal(Literal) -->
  "\"",
  rdf_literal_parse_(Literal).

% Generate a language-tagged string.
rdf_literal_generate_(literal(lang(LTag,Lex))) --> !,
  "\"",
  atom(Lex),
  "\"@",
  atom(LTag).
% Generate a typed literal.
rdf_literal_generate_(literal(type(D,Lex))) -->
  {atom(D)}, !,
  "\"",
  atom(Lex),
  "\"^^",
  rdf_iri_generate_(D).

rdf_literal_parse_(Literal) -->
  ...(Codes),
  "\"", !,
  ("^^" -> rdf_iri(D) ; "@" -> remainder_as_atom(LTag) ; ""),
  {
    atom_codes(Lex, Codes),
    rdf_literal(D, LTag, Lex, Literal)
  }.



%! rdf_literal(+D:atom, +LTag:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_literal(-D:atom, -LTag:atom, -Lex:atom, +Literal:rdf_literal) is det.
%
% Compose/decompose literals.

rdf_literal(D, LTag, Lex, literal(type(D,Lex))) :-
  var(LTag).
rdf_literal(rdf:langString, LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_literal_datatype_iri(+Literal:rdf_literal, +D:atom) is semidet.
%! rdf_literal_datatype_iri(+Literal:rdf_literal, -D:atom) is det.

rdf_literal_datatype_iri(literal(type(D,_)), D).
rdf_literal_datatype_iri(literal(lang(_,_)), rdf:langString).



%! rdf_literal_dwim(+DWIM, +Literal:rdf_literal) is semidet.
%! rdf_literal_dwim(+DWIM, -Literal:rdf_literal) is det.
%
% Allows literal terms to be created based on various simplified
% inputs:
%
%
%   | *Input format*              | *Datatype IRI*         |
%   |-----------------------------+------------------------|
%   | date(Y,Mo,D)                | xsd:date               |
%   | date_time(Y,Mo,D,H,Mi,S)    | xsd:dateTime           |
%   | date_time(Y,Mo,D,H,Mi,S,TZ) | xsd:dateTime           |
%   | double(N)                   | xsd:double             |
%   | float                       | xsd:double             |
%   | float(N)                    | xsd:float              |
%   | integer                     | xsd:integer            |
%   | integer(N)                  | xsd:integer            |
%   | literal(lang(D,Lex))        | rdf:langString         |
%   | literal(type(D,Lex))        | D                      |
%   | literal(Lex)                | xsd:string             |
%   | month_day(Mo,D)             | xsd:gMonthDay          |
%   | nonneg(N)                   | xsd:nonNegativeInteger |
%   | oneof([false,true])         | xsd:boolean            |
%   | pair(string,atom)           | rdf:langString         |
%   | positive_integer(N)         | xsd:positiveInteger    |
%   | string                      | xsd:string             |
%   | string(atom)                | xsd:string             |
%   | time(H,Mi,S)                | xsd:time               |
%   | uri(Uri)                    | xsd:anyURI             |
%   | year(Y)                     | xsd:gYear              |
%   | year_month(Y,Mo)            | xsd:gYearMonth         |

rdf_literal_dwim(Term, _) :-
  var(Term), !,
  instantiation_error(Term).
% geospatial shapes
rdf_literal_dwim(shape(Z,LRS,CRS,Shape), Literal) :- !,
  wkt_shape_atom(shape(Z,LRS,CRS,Shape), Lex),
  rdf_typed_literal(geo:wktLiteral, Lex, Literal).
% language-tagged string
rdf_literal_dwim(String-LTag, literal(lang(LTag,Lex))) :- !,
  atom_string(Lex, String).
% date/3, date_time/[6.7], month_day/2, time/3, year_month/2
rdf_literal_dwim(Compound, literal(type(D,Lex))) :-
  xsd_date_time_term_(Compound), !,
  xsd_time_string(Compound, D, String),
  atom_string(Lex, String).
% double/1 → xsd:double
rdf_literal_dwim(double(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:double),
  atom_number(Lex, N).
% float/1 → xsd:float
rdf_literal_dwim(float(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:float),
  atom_number(Lex, N).
% integer/1 → xsd:integer
rdf_literal_dwim(integer(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:integer),
  atom_number(Lex, N).
% nonneg/1 → xsd:nonNegativeInteger
rdf_literal_dwim(nonneg(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:nonNegativeInteger),
  must_be(nonneg, N),
  xsd_number_string(N, Lex).
% positive_integer/1 → xsd:positiveInteger
rdf_literal_dwim(positive_integer(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:positiveInteger),
  must_be(positive_integer, N),
  xsd_number_string(N, Lex).
% str/1 → xsd:string
rdf_literal_dwim(string(Atomic), literal(type(D,Lex))) :- !,
  atom_string(Atomic, String),
  rdf_equal(D, xsd:string),
  atom_string(Lex, String).
% uri/1 → xsd:anyURI
rdf_literal_dwim(uri(Uri), literal(type(D,Uri))) :- !,
  rdf_equal(xsd:anyURI, D).
% year/1 → xsd:gYear
rdf_literal_dwim(year(Year), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:gYear),
  atom_number(Lex, Year).
% double → xsd:double
% float → xsd:double
rdf_literal_dwim(Value, literal(type(D,Lex))) :-
  float(Value), !,
  rdf_equal(D, xsd:double),
  xsd_number_string(Value, String),
  atom_string(Lex, String).
% integer → xsd:integer
rdf_literal_dwim(Value, literal(type(D,Lex))) :-
  integer(Value), !,
  rdf_equal(D, xsd:integer),
  atom_number(Lex, Value).
% string → xsd:string
rdf_literal_dwim(Value, literal(type(D,Lex))) :-
  string(Value), !,
  rdf_equal(D, xsd:string),
  atom_string(Lex, Value).
% regular typed literal
rdf_literal_dwim(literal(type(D,Lex)), literal(type(D,Lex))) :- !.
% regular language-tagged string
rdf_literal_dwim(literal(lang(LTag,Lex)), literal(lang(LTag,Lex))) :- !.
% legacy untyped literals
rdf_literal_dwim(literal(Lex), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:string).
% atom `false' and `true' → xsd:boolean
rdf_literal_dwim(Lex, literal(type(D,Lex))) :-
  memberchk(Lex, [false,true]), !,
  rdf_equal(D, xsd:boolean).

xsd_date_time_term_(date(_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_,_)).
xsd_date_time_term_(month_day(_,_)).
xsd_date_time_term_(time(_,_,_)).
xsd_date_time_term_(year_month(_,_)).



%! rdf_literal_lexical_form(+Literal:rdf_literal, +Lex:atom) is semidet.
%! rdf_literal_lexical_form(+Literal:rdf_literal, -Lex:atom) is det.

rdf_literal_lexical_form(literal(type(_,Lex)), Lex) :- !.
rdf_literal_lexical_form(literal(lang(_,Lex)), Lex) :- !.
rdf_literal_lexical_form(literal(Lex), Lex).



%! rdf_literal_value(+Literal:rdf_literal, -Value:term) is det.
%! rdf_literal_value(+Literal:rdf_literal, -D:atom, -Value:term) is det.
%! rdf_literal_value(-Literal:rdf_literal, +D:atom, +Value:term) is det.
%
% Notice that languages-tagged strings do not have a value.

rdf_literal_value(Literal, Value) :-
  rdf_literal_value(Literal, _, Value).


% language-tagged strings do not have a value space.
rdf_literal_value(literal(lang(LTag,Lex)), rdf:langString, Lex-LTag) :- !.
% typed literal
rdf_literal_value(literal(type(D,Lex)), D, Value) :- !,
  rdf_lexical_value(D, Lex, Value).
% simple literal
rdf_literal_value(literal(Lex), xsd:string, Value) :-
  atom_string(Lex, Value).



%! rdf_name_string(+Name:rdf_name, -String:string) is semidet.
%
% Tries to return a readable String for the given RDF Term.

rdf_name_string(Literal, String) :-
  rdf_is_literal(Literal), !,
  rdf_literal_lexical_form(Literal, Lex),
  atom_string(Lex, String).
rdf_name_string(Iri, String) :-
  rdf_is_iri(Iri), !,
  (   rdf_prefix_iri(Alias, Local, Iri)
  ->  string_list_concat([Alias,Local], ":", String)
  ;   atom_string(Iri, String)
  ).



%! rdf_object_dwim(+DWIM, +Term:rdf_term) is semidet.
%! rdf_object_dwim(+DWIM, -Term:rdf_term) is det.
%
% Supports the Input formats of rdf_literal_dwim/2.

rdf_object_dwim(Term, Literal) :-
  rdf_literal_dwim(Term, Literal), !.
% blank node, IRI
rdf_object_dwim(Term, Term).



%! rdf_predicate_dwim(+DWIM, -Predicate:rdf_predicate) is det.

rdf_predicate_dwim(a, rdf:type) :- !.
rdf_predicate_dwim(P, P).



%! rdf_term(+Term:rdf_term)// .
%! rdf_term(-Term:rdf_term)// .
%
% Generates or parses a term in Turtle-family notation.

rdf_term(Term) -->
  {ground(Term)}, !,
  rdf_term_generate_(Term).
rdf_term(Term) -->
  rdf_term_parse_(Term).

rdf_term_generate_(Literal) -->
  {rdf_is_literal(Literal)}, !,
  rdf_literal_generate_(Literal).
rdf_term_generate_(Iri) -->
  {rdf_is_iri(Iri)}, !,
  rdf_iri_generate_(Iri).
rdf_term_generate_(BNode) -->
  {rdf_is_bnode(BNode)}, !,
  rdf_bnode_generate_(BNode).

% We consume the next character in order to determine the syntactic
% kind of RDF term.  Notice that IRIs cannot be told aparat in this
% way, since we support two IRI notations.
rdf_term_parse_(Literal) -->
  "\"", !,
  rdf_literal_parse_(Literal).
rdf_term_parse_(BNode) -->
  "_:", !,
  rdf_bnode_parse_(BNode).
rdf_term_parse_(Iri) -->
  rdf_iri_parse_(Iri).



%! rdf_term_to_string(+Term:rdf_term, -String:string) is det.
%
% Use rdf_atom_term/2 when the serialization must be read back later.

% Abbreviated IRI notation.
rdf_term_to_string(Iri, String) :-
  rdf_is_iri(Iri),
  rdf_prefix(Alias, Prefix),
  atom_concat(Prefix, Local, Iri), !,
  format(string(String), "~a:~a", [Alias,Local]).
rdf_term_to_string(Term, String) :-
  rdf_atom_term(Atom, Term),
  atom_string(Atom, String).



%! rdf_typed_literal(+D:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_typed_literal(-D:atom, -Lex:atom, +Literal:rdf_literal) is det.

rdf_typed_literal(D, Lex, literal(type(D,Lex))).



%! tp_object_dwim(+DWIM:term, -O:rdf_object) is det.

tp_object_dwim(O, O) :-
  var(O), !.
tp_object_dwim(O1, O2) :-
  rdf_object_dwim(O1, O2).



%! tp_predicate_dwim(+DWIM:term, -P:rdf_predicate) is det.

tp_predicate_dwim(P, P) :-
  var(P), !.
tp_predicate_dwim(P1, P2) :-
  rdf_predicate_dwim(P1, P2).



%! well_known_iri(+Segments:list(atom), +Iri:atom) is semidet.
%! well_known_iri(+Segments:list(atom), -Iri:atom) is det.
%! well_known_iri(-Segments:list(atom), +Iri:atom) is det.

well_known_iri(Segments, Iri) :-
  setting(bnode_prefix_scheme, Scheme),
  setting(bnode_prefix_authority, Auth),
  (   ground(Iri)
  ->  uri_comps(Prefix, uri(Scheme,Auth,['.well-known',genid],_,_)),
      atom_concat(Prefix, Local, Iri),
      atomic_list_concat(Segments, /, Local)
  ;   uri_comps(Iri, uri(Scheme,Auth,['.well-known',genid|Segments],_,_))
  ).
