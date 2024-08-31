:- encoding(utf8).
:- module(
  rdf_term,
  [
    rdf_atom_node/2,                     % ?Atom, ?Node
    rdf_atom_predicate/2,                % ?Atom, ?Predicate
    rdf_base_iri/1,                      % ?BaseIri
    rdf_canonical_lexical_form/3,        % +Datatype, +LexicalForm, -CanonicaldLexicalForm
    rdf_canonical_literal/2,             % +Literal, -CanonicalLiteral
    rdf_container_membership_property/1, % ?Property
    rdf_container_membership_property/2, % ?Property, ?Index
   %rdf_create_bnode/1,                  % --BNode
    rdf_create_iri/3,                    % +Alias, +TermOrTerms, -Iri
    rdf_hash_iri/3,                      % +Alias, +Term, -Iri
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
    rdf_language_tagged_string/3,        % ?LTag, ?LexicalForm, ?Literal
    rdf_lexical_value/3,                 % +Datatype, ?LexicalForm, ?Value
    rdf_literal/4,                       % ?Datatype, ?LTag, ?LexicalForm, ?Literal
    rdf_literal_datatype_iri/2,          % +Literal, ?Datatype
    rdf_literal_dwim/2,                  % +Dwim, ?Literal
    rdf_literal_lexical_form/2,          % +Literal, ?LexicalForm
    rdf_literal_value/2,                 % +Literal, -Value
    rdf_literal_value/3,                 % ?Literal, ?Datatype, ?Value
    rdf_node_dwim/2,                     % +Dwim, ?Term
    rdf_predicate_dwim/2,                % +Dwim, ?P
    rdf_triple_term/2,                   % +Triple, ?Term
    rdf_typed_literal/3,                 % ?Datatype, ?LexicalForm, ?Literal
    well_known_iri/1,                    % -Iri
    well_known_iri/2                     % +Segments, -Iri
  ]
).
:- reexport(library(semweb/rdf_prefix)).

/** <module> Advanced support for RDF terms

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(date_time)).
:- use_module(library(lists)).
:- reexport(library(semweb/rdf_db), [
     rdf_is_bnode/1,
     rdf_is_literal/1
   ]).
:- reexport(library(semweb/rdf11), [
     rdf_create_bnode/1,
     rdf_is_iri/1,
     rdf_is_predicate/1,
     rdf_is_subject/1,
     op(110,xfx,@),
     op(650,xfx,^^)
   ]).
:- use_module(library(semweb/turtle), []).
:- use_module(library(settings)).
:- use_module(library(uuid)).

:- use_module(library(dcg)).
:- use_module(library(hash_ext)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(string_ext)).
:- use_module(library(uri_ext)).
:- use_module(library(xsd)).
:- use_module(library(xsd_grammar)).

:- discontiguous
    rdf_lexical_to_value/3,
    rdf_value_to_lexical/3.

:- dynamic
    rdf:rdf_lexical_to_value_hook/3,
    rdf:rdf_value_to_lexical_hook/3.

:- multifile
    error:has_type/2,
    prolog:message//1,
    rdf:rdf_lexical_to_value_hook/3,
    rdf:rdf_value_to_lexical_hook/3.

% IRI
error:has_type(iri, Term) :-
  rdf_is_iri(Term).
% blank node
error:has_type(rdf_bnode, Term) :-
  rdf_is_bnode(Term).
% graph name
error:has_type(rdf_graph_name, Term) :-
  error:has_type(iri, Term).
% literal
error:has_type(rdf_literal, Term) :-
  rdf_is_literal(Term).
% name
error:has_type(rdf_name,Term) :-
  error:has_type(iri, Term).
error:has_type(rdf_name,Term) :-
  error:has_type(rdf_literal, Term).
% node
error:has_type(rdf_node, Term) :-
  error:has_type(rdf_bnode, Term).
error:has_type(rdf_node, Term) :-
  error:has_type(rdf_name, Term).
% quadruple
error:has_type(rdf_quad, Term) :-
  Term = tp(S,P,O,G),
  error:has_type(maybe(rdf_node), S),
  error:has_type(maybe(iri), P),
  error:has_type(maybe(rdf_node), O),
  error:has_type(maybe(rdf_graph_name), G).
% triple
error:has_type(rdf_triple, Term) :-
  Term = tp(S,P,O),
  error:has_type(maybe(rdf_node), S),
  error:has_type(maybe(iri), P),
  error:has_type(maybe(rdf_node), O).
% tuple
error:has_type(rdf_tuple, Term) :-
  error:has_type(rdf_quad, Term).
error:has_type(rdf_tuple, Term) :-
  error:has_type(rdf_triple, Term).

:- rdf_meta
   rdf_atom_node(?, o),
   rdf_atom_predicate(?, r),
   rdf_base_iri(r),
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
   rdf_literal_dwim(o, o),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_value(o, -),
   rdf_literal_value(o, r, ?),
   rdf_node_dwim(t, -),
   rdf_predicate(r, ?, ?),
   rdf_predicate_dwim(r, r),
   rdf_triple_term(t, r),
   rdf_typed_literal(r, ?, o),
   rdf_value_to_lexical(r, +, -),
   rdf_value_to_lexical_error(r, +).

:- maplist(rdf_register_prefix, [rdf,xsd]).

:- setting(base_iri, atom, 'https://example.com/',
           "The default base IRI for RDF IRIs.").
:- setting(bnode_prefix_authority, atom, 'example.org', "").
:- setting(bnode_prefix_scheme, atom, https, "").
:- setting(rdf_container_membership_property_max, nonneg, 3,
           "The number of supported container membership properties.").





%! rdf_atom_node(+Atom:atom, +Node:rdf_node) is semidet.
%! rdf_atom_node(+Atom:atom, -Node:rdf_node) is det.
%! rdf_atom_node(-Atom:atom, +Node:rdf_node) is det.
%
% Parses the given Atom in order to extract the encoded RDF node.  The
% following syntactic forms are supported:
%
%  1. RDF nodes defined by the N-Triples 1.1 grammar (blank nodes,
%     IRIs, and literals).
%
%  2. Turtle 1.1 prefix notation for IRIs.

% Generate.
rdf_atom_node(Atom, Node) :-
  var(Atom), !,
  atom_phrase(rdf_dcg_node(Node), Atom).
% Parse.
rdf_atom_node(Atom, Node) :-
  rdf_parse_snippet(node, Atom, Node), !.
% Error.
rdf_atom_node(Atom, _) :-
  syntax_error(rdf_node(Atom)).

prolog:message(error(syntax_error(rdf_node(Atom)),_)) -->
  ["Could not parse as RDF node: ~a"-[Atom]].

:- begin_tests(rdf_atom_node).

:- rdf_meta
   test_rdf_atom_from_node(?, o),
   test_rdf_atom_to_node(?, o).

test('rdf_atom_node(+,+)', [forall(test_rdf_atom_to_node(Atom,Node))]) :-
  rdf_atom_node(Atom, Node).
test('rdf_atom_node(+,-)', [forall(test_rdf_atom_to_node(Atom,Node))]) :-
  rdf_atom_node(Atom, Node0),
  assertion(Node == Node0).
test('rdf_atom_node(-,+)', [forall(test_rdf_atom_from_node(Atom,Node))]) :-
  rdf_atom_node(Atom0, Node),
  assertion(Atom == Atom0).

test_rdf_atom_to_node(Atom, Node) :-
  test_rdf_atom_from_node(Atom, Node).

test_rdf_atom_from_node('<mailto:x>', 'mailto:x').
test_rdf_atom_from_node('""^^<mailto:x>', literal(type('mailto:x',''))).

:- end_tests(rdf_atom_node).



%! rdf_atom_predicate(+Atom:atom, +Predicate:iri) is semidet.
%! rdf_atom_predicate(+Atom:atom, -Predicate:iri) is det.
%! rdf_atom_predicate(-Atom:atom, +Predicate:iri) is det.

% Generate.
rdf_atom_predicate(Atom, P) :-
  var(Atom), !,
  atom_phrase(rdf_dcg_predicate(P), Atom).
% Parse.
rdf_atom_predicate(Atom, P) :-
  rdf_parse_snippet(predicate, Atom, P), !.
% Error.
rdf_atom_predicate(Atom, _) :-
  atom(Atom), !,
  syntax_error(rdf_predicate(Atom)).

prolog:message(error(syntax_error(rdf_predicate(Atom)),_)) -->
  ["Could not parse as RDF predicate: ~a"-[Atom]].



%! rdf_base_iri(+BaseIri:iri) is semidet.
%! rdf_base_iri(-BaseIri:iri) is det.

rdf_base_iri(BaseIri) :-
  setting(base_iri, BaseIri).



%! rdf_canonical_lexical_form(+Datatype:iri,
%!                            +LexicalForm:atom,
%!                            -CanonicaldLexicalForm:atom) is det.

rdf_canonical_lexical_form(D, Lex, CanonicalLex) :-
  rdf_lexical_value(D, Lex, Value),
  rdf_lexical_value(D, CanonicalLex, Value).



%! rdf_canonical_literal(+Literal:rdf_literal,
%!                       -CanonicaldLiteral:rdf_literal) is det.

rdf_canonical_literal(Literal, CanonicalLiteral) :-
  rdf_literal_value(Literal, D, Value),
  rdf_literal_value(CanonicalLiteral, D, Value).



%! rdf_container_membership_property(+Property:iri) is semidet.
%! rdf_container_membership_property(-Property:iri) is multi.

rdf_container_membership_property(P) :-
  rdf_container_membership_property(P, _).


%! rdf_container_membership_property(+Property:iri, +Index:positive_integer) is semidet.
%! rdf_container_membership_property(+Property:iri, -Index:positive_integer) is det.
%! rdf_container_membership_property(-Property:iri, +Index:positive_integer) is det.
%! rdf_container_membership_property(-Property:iri, -Index:positive_integer) is multi.
%
% Succeeds if P is the Nth container membership property.

rdf_container_membership_property(P, N) :-
  rdf_equal(rdf:'_', Prefix),
  (   var(P)
  ->  setting(rdf_container_membership_property_max, Max),
      between(1, Max, N),
      atom_concat(Prefix, N, P)
  ;   atom_concat(Prefix, Atom, P),
      atom_number(Atom, N),
      must_be(nonneg, N)
  ).



%! rdf_create_iri(+Alias:atom, +TermOrTerms:or([term,list(term)]), -Iri:iri) is det.

rdf_create_iri(Alias, Terms, Iri) :-
  is_list(Terms), !,
  convlist(term_to_segment_, Terms, Segments),
  atomic_list_concat(Segments, /, Local),
  %compound_name_arguments(Fingerprint, Alias, Segments),
  %md5(Fingerprint, Local),
  rdf_prefix_iri(Alias, Local, Iri).
rdf_create_iri(Alias, Term, Iri) :-
  rdf_create_iri(Alias, [Term], Iri).

term_to_segment_(dt(Y,Mo,Da,H,Mi,S,TZ), Segment) :- !,
  dt_label(dt(Y,Mo,Da,H,Mi,S,TZ), Segment).
term_to_segment_(Literal, Segment) :-
  rdf_is_literal(Literal), !,
  rdf_literal_lexical_form(Literal, Segment).
term_to_segment_(Segment, Segment) :-
  ground(Segment).



%! rdf_hash_iri(+Alias:atom, +Term:term, -Iri:iri) is det.

rdf_hash_iri(Alias, Term, Iri) :-
  md5(Term, Local),
  rdf_prefix_iri(Alias, Local, Iri).



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



%! rdf_language_tagged_string(+LTag:atom, +LexicalForm:atom, -Literal:rdf_literal) is det.
%! rdf_language_tagged_string(-LTag:atom, -LexicalForm:atom, +Literal:rdf_literal) is det.

rdf_language_tagged_string(LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_lexical_value(+Datatype:iri, +LexicalForm:atom, -Value:term) is det.
%! rdf_lexical_value(+Datatype:iri, -LexicalForm:atom, +Value:term) is det.
%
% Translate between a value (`Value') and its serialization, according
% to a given datatype IRI (`Datatype'), into a lexical form (`LexicalForm').

rdf_lexical_value(D, Lex, Value) :-
  nonvar(Lex), !,
  rdf_lexical_to_value(D, Lex, Value).
rdf_lexical_value(D, Lex, Value) :-
  rdf_value_to_lexical(D, Value, Lex).

% hooks
rdf_lexical_to_value(D, Lex, Value) :-
  rdf:rdf_lexical_to_value_hook(D, Lex, Value), !.
rdf_value_to_lexical(D, Value, Lex) :-
  rdf:rdf_value_to_lexical_hook(D, Value, Lex), !.

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
rdf_lexical_to_value(xsd:anyURI, Lex, Lex) :- !.

rdf_value_to_lexical(xsd:anyURI, Lex, Lex) :- !.

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

% xsd:duration
rdf_lexical_to_value(xsd:duration, Lex, Value) :- !,
  (   atom_phrase(durationMap(Value0), Lex)
  ->  Value = Value0
  ;   rdf_lexical_to_value_error(xsd:duration, Lex)
  ).
rdf_value_to_lexical(xsd:duration, Value, Lex) :- !,
  (   atom_phrase(durationCanonicalMap(Value), Atom)
  ->  Lex = Atom
  ;   rdf_value_to_lexical_error(xsd:duration, Value)
  ).

% xsd:byte
% xsd:decimal
% xsd:double
% xsd:float
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

rdf_lexical_to_value_error(D, Lex) :-
  syntax_error(rdf_lexical_form(D,Lex)).
rdf_value_to_lexical_error(D, Value) :-
  type_error(D, Value).

prolog:message(error(syntax_error(rdf_lexical_form(D,Lex)),_)) -->
  ["Could not parse as <~a>: ~a"-[D,Lex]].

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



%! rdf_literal(+Datatype:iri, +LTag:atom, +LexicalForm:atom, +Literal:rdf_literal) is semidet.
%! rdf_literal(+Datatype:iri, +LTag:atom, +LexicalForm:atom, -Literal:rdf_literal) is det.
%! rdf_literal(-Datatype:iri, -LTag:atom, -LexicalForm:atom, +Literal:rdf_literal) is det.
%
% Compose/decompose literals.

rdf_literal(D, LTag, Lex, literal(type(D,Lex))) :-
  var(LTag), !,
  must_be(ground, D),
  must_be(ground, Lex).
rdf_literal(rdf:langString, LTag, Lex, literal(lang(LTag,Lex))) :-
  must_be(ground, Lex),
  must_be(ground, LTag).

:- begin_tests(rdf_literal).

:- rdf_meta
   test_rdf_literal(r, ?, ?, o).

test('rdf_literal(+,+,+,+)', [forall(test_rdf_literal(D,LTag,Lex,Literal))]) :-
  rdf_literal(D, LTag, Lex, Literal).
test('rdf_literal(+,+,+,-)', [forall(test_rdf_literal(D,LTag,Lex,Literal))]) :-
  rdf_literal(D, LTag, Lex, Literal0),
  assertion(Literal0 == Literal).
test('rdf_literal(-,-,-,+)', [forall(test_rdf_literal(D,LTag,Lex,Literal))]) :-
  rdf_literal(D0, LTag0, Lex0, Literal),
  assertion(D0 == D),
  (   rdf_equal(D0, rdf:langString)
  ->  assertion(LTag0 == LTag)
  ;   assertion(var(LTag0))
  ),
  assertion(Lex0 == Lex).

test_rdf_literal(rdf:'HTML', _, '<p>test</p>', literal(type(rdf:'HTML','<p>test</p>'))).
test_rdf_literal(rdf:langString, nl, test, literal(lang(nl,test))).
test_rdf_literal(rdf:langString, nl, test, literal(lang(nl,test))).
test_rdf_literal(xsd:boolean, _, false, literal(type(xsd:boolean,false))).

:- end_tests(rdf_literal).



%! rdf_literal_datatype_iri(+Literal:rdf_literal, +Datatype:iri) is semidet.
%! rdf_literal_datatype_iri(+Literal:rdf_literal, -Datatype:iri) is det.

rdf_literal_datatype_iri(literal(type(D,_)), D).
rdf_literal_datatype_iri(literal(lang(_,_)), rdf:langString).



%! rdf_literal_dwim(+Dwim:term, +Literal:rdf_literal) is semidet.
%! rdf_literal_dwim(+Dwim:term, -Literal:rdf_literal) is semidet.
%
% Allows literal terms to be created based on various simplified
% inputs:
%
%
%   | *Input format*              | *Datatype IRI*         |
%   |-----------------------------+------------------------|
%   | boolean(LexicalForm)        | xsd:boolean            |
%   | date(Y,Mo,Da)               | xsd:date               |
%   | date_time(Y,Mo,D,H,Mi,S)    | xsd:dateTime           |
%   | date_time(Y,Mo,D,H,Mi,S,TZ) | xsd:dateTime           |
%   | day(Da)                     | xsd:gDay               |
%   | decimal(N)                  | xsd:decimal            |
%   | double(N)                   | xsd:double             |
%   | duration(S)                 | xsd:dayTimeDuration    |
%   | duration(Mo,S)              | xsd:duration           |
%   | float                       | xsd:double             |
%   | float(N)                    | xsd:float              |
%   | integer                     | xsd:integer            |
%   | integer(N)                  | xsd:integer            |
%   | literal(lang(LTag,Lex))     | rdf:langString         |
%   | literal(type(D,Lex))        | Datatype               |
%   | month(Mo)                   | xsd:gMonth             |
%   | month_day(Mo,Da)            | xsd:gMonthDay          |
%   | nonneg(N)                   | xsd:nonNegativeInteger |
%   | oneof([false,true])         | xsd:boolean            |
%   | pair(string,list(atom))     | rdf:langString         |
%   | positive_integer(N)         | xsd:positiveInteger    |
%   | string                      | xsd:string             |
%   | string(atom)                | xsd:string             |
%   | time(H,Mi,S)                | xsd:time               |
%   | uri(Uri)                    | xsd:anyURI             |
%   | year(Y)                     | xsd:gYear              |
%   | year_month(Y,Mo)            | xsd:gYearMonth         |
%   | LexicalForm^^DatatypeIri    | DatatypeIri            |
%   | LexicalForm@LanguageTag     | rdf:langstring         |

rdf_literal_dwim(Var, _) :-
  var(Var), !,
  fail.
% SWI typed literal
rdf_literal_dwim(Lex^^D, literal(type(D,Lex))) :- !.
% SWI language-tagged string
rdf_literal_dwim(Lex@LTag, literal(lang(LTag,Lex))) :- !.
% special value literal
rdf_literal_dwim(literal(value(D,Value)), Literal) :- !,
  rdf_literal_value(Literal, D, Value).
% regular typed literal
rdf_literal_dwim(literal(type(D,Lex)), literal(type(D,Lex))) :- !.
% regular language-tagged string
rdf_literal_dwim(literal(lang(LTag,Lex)), literal(lang(LTag,Lex))) :- !.
% language-tagged string
rdf_literal_dwim(String-Tags, literal(lang(LTag,Lex))) :- !,
  atom_string(Lex, String),
  atomic_list_concat(Tags, -, LTag).
% boolean/1
rdf_literal_dwim(boolean(Lex), literal(type(xsd:boolean,Lex))) :- !,
  must_be(oneof([false,true]), Lex).
% date/3, date_time/[6.7], month_day/2, time/3, year_month/2
rdf_literal_dwim(Compound, literal(type(D,Lex))) :-
  xsd_date_time_term_(Compound), !,
  xsd_time_string(Compound, D, String),
  atom_string(Lex, String).
% day/1
rdf_literal_dwim(day(Da), literal(type(xsd:gDay,Lex))) :- !,
  xsd_time_string(Da, xsd:gDay, String),
  atom_string(Lex, String).
% decimal/1 → xsd:decimal
rdf_literal_dwim(decimal(N), literal(type(xsd:decimal,Lex))) :- !,
  rdf_lexical_value(xsd:decimal, Lex, N).
% double/1 → xsd:double
rdf_literal_dwim(double(N), literal(type(xsd:double,Lex))) :- !,
  atom_number(Lex, N).
% dt/7 → xsd:dateTime
rdf_literal_dwim(dt(Y,Mo,Da,H,Mi,S,TZ), Literal) :-
  (   ground(dt(Y,Mo,Da,H,Mi,S,TZ))
  ->  rdf_literal_dwim(date_time(Y,Mo,Da,H,Mi,S,TZ), Literal)
  ;   ground(dt(Y,Mo,Da,H,Mi,S))
  ->  rdf_literal_dwim(date_time(Y,Mo,Da,H,Mi,S), Literal)
  ;   ground(dt(Y,Mo,Da))
  ->  rdf_literal_dwim(date(Y,Mo,Da), Literal)
  ;   ground(dt(Mo,Da))
  ->  rdf_literal_dwim(month_day(Mo,Da), Literal)
  ;   ground(dt(H,Mi,S))
  ->  rdf_literal_dwim(time(H,Mi,S), Literal)
  ;   ground(dt(Y,Mo))
  ->  rdf_literal_dwim(year_month(Y,Mo), Literal)
  ;   instantiation_error(dt(Y,Mo,Da,H,Mi,S,TZ))
  ).
% duration/1 → xsd:dayTimeDuration
rdf_literal_dwim(duration(S), literal(type(xsd:dayTimeDuration,Lex))) :- !,
  rdf_lexical_value(xsd:dayTimeDuration, Lex, duration(0,S)).
% duration/2 → xsd:duration
rdf_literal_dwim(duration(Mo,S), literal(type(xsd:duration,Lex))) :- !,
  rdf_lexical_value(xsd:duration, Lex, duration(Mo,S)).
% float/1 → xsd:float
rdf_literal_dwim(float(N), literal(type(xsd:float,Lex))) :- !,
  atom_number(Lex, N).
% integer/1 → xsd:integer
rdf_literal_dwim(integer(N), literal(type(xsd:integer,Lex))) :- !,
  atom_number(Lex, N).
% month/1
rdf_literal_dwim(month(Mo), literal(type(xsd:gMonth,Lex))) :- !,
  xsd_time_string(Mo, xsd:gMonth, String),
  atom_string(Lex, String).
% nonneg/1 → xsd:nonNegativeInteger
rdf_literal_dwim(nonneg(N), literal(type(xsd:nonNegativeInteger,Lex))) :- !,
  must_be(nonneg, N),
  xsd_number_string(N, String),
  atom_string(Lex, String).
% positive_integer/1 → xsd:positiveInteger
rdf_literal_dwim(positive_integer(N), literal(type(xsd:positiveInteger,Lex))) :- !,
  must_be(positive_integer, N),
  xsd_number_string(N, String),
  atom_string(Lex, String).
% str/1 → xsd:string
rdf_literal_dwim(string(Atomic), literal(type(xsd:string,Lex))) :- !,
  atom_string(Atomic, String),
  atom_string(Lex, String).
% uri/1 → xsd:anyURI
rdf_literal_dwim(uri(Lex), literal(type(xsd:anyURI,Lex))) :-!.
% year/1 → xsd:gYear
rdf_literal_dwim(year(Y), literal(type(xsd:gYear,Lex))) :- !,
  xsd_time_string(Y, xsd:gYear, String),
  atom_string(Lex, String).
% double → xsd:double
% float → xsd:double
rdf_literal_dwim(Value, literal(type(xsd:double,Lex))) :-
  float(Value), !,
  xsd_number_string(Value, String),
  atom_string(Lex, String).
% integer → xsd:integer
rdf_literal_dwim(Value, literal(type(xsd:integer,Lex))) :-
  integer(Value), !,
  atom_number(Lex, Value).
% string → xsd:string
rdf_literal_dwim(String, literal(type(xsd:string,Lex))) :-
  string(String), !,
  atom_string(Lex, String).
% atom `false' and `true' → xsd:boolean
rdf_literal_dwim(Lex, literal(type(xsd:boolean,Lex))) :-
  memberchk(Lex, [false,true]).

xsd_date_time_term_(date(_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_,_)).
xsd_date_time_term_(month_day(_,_)).
xsd_date_time_term_(time(_,_,_)).
xsd_date_time_term_(year_month(_,_)).



%! rdf_literal_lexical_form(+Literal:rdf_literal, +LexicalForm:atom) is semidet.
%! rdf_literal_lexical_form(+Literal:rdf_literal, -LexicalForm:atom) is det.

rdf_literal_lexical_form(literal(type(_,Lex)), Lex) :-
  must_be(atom, Lex).
rdf_literal_lexical_form(literal(lang(_,Lex)), Lex) :-
  must_be(atom, Lex).



%! rdf_literal_value(+Literal:rdf_literal, -Value:term) is det.
%
% Notice that languages-tagged strings do not have a value.

rdf_literal_value(Literal, Value) :-
  rdf_literal_value(Literal, _, Value).


%! rdf_literal_value(+Literal:rdf_literal, -Datatype:iri, -Value:term) is det.
%! rdf_literal_value(-Literal:rdf_literal, +Datatype:iri, +Value:term) is det.

% language-tagged strings do not have a value space.
rdf_literal_value(literal(lang(LTag,Lex)), rdf:langString, String-Tags) :- !,
  atomic_list_concat(Tags, -, LTag),
  atom_string(Lex, String).
% typed literal
rdf_literal_value(literal(type(D,Lex)), D, Value) :-
  rdf_lexical_value(D, Lex, Value).



%! rdf_node_dwim(+Dwim:term, +Node:rdf_node) is semidet.
%! rdf_node_dwim(+Dwim:term, -Node:rdf_node) is semidet.
%
% Supports the Input formats of rdf_literal_dwim/2.

% blank node or IRI
rdf_node_dwim(Term, Term) :-
  rdf_is_subject(Term), !.
% literal
rdf_node_dwim(Term, Literal) :-
  rdf_literal_dwim(Term, Literal).



%! rdf_predicate_dwim(+Dwim:term, +Predicate:iri) is semidet.
%! rdf_predicate_dwim(+Dwim:term, -Predicate:iri) is det.

rdf_predicate_dwim(a, rdf:type) :- !.
rdf_predicate_dwim(P, P).



%! rdf_triple_term(+Triple:rdf_triple, +Term:rdf_term) is semidet.
%! rdf_triple_term(+Triple:rdf_triple, -Term:rdf_term) is multi.

rdf_triple_term(t(S,_,_), S).
rdf_triple_term(t(_,P,_), P).
rdf_triple_term(t(_,_,O), O).



%! rdf_typed_literal(+Datatype:iri, +LexicalForm:atom, +Literal:rdf_literal) is semidet.
%! rdf_typed_literal(+Datatype:iri, +LexicalForm:atom, -Literal:rdf_literal) is det.
%! rdf_typed_literal(-Datatype:iri, -LexicalForm:atom, +Literal:rdf_literal) is det.

rdf_typed_literal(D, Lex, literal(type(D,Lex))).



%! well_known_iri(-Iri:iri) is det.

well_known_iri(Iri) :-
  uuid(Seed),
  md5(Seed, Hash),
  well_known_iri([Hash], Iri).


%! well_known_iri(+Segments:list(atom), +Iri:iri) is semidet.
%! well_known_iri(+Segments:list(atom), -Iri:iri) is det.
%! well_known_iri(-Segments:list(atom), +Iri:iri) is det.

well_known_iri(Segments, Iri) :-
  setting(bnode_prefix_scheme, Scheme),
  setting(bnode_prefix_authority, Auth),
  (   ground(Iri)
  ->  uri_comps(Prefix, uri(Scheme,Auth,['.well-known',genid],_,_)),
      atom_concat(Prefix, Local, Iri),
      atomic_list_concat(Segments, /, Local)
  ;   uri_comps(Iri, uri(Scheme,Auth,['.well-known',genid|Segments],_,_))
  ).



% GENERICS %

%! rdf_parse_snippet(+Kind:oneof([node,predicate]),
%!                   +Content:atom,
%!                   -Term:term) is det.

rdf_parse_snippet(Kind, Content, Term) :-
  prefix_declaration_snippet(PrefixSnippet),
  format_string(Kind, Format),
  format(string(PatternSnippet), Format, [Content]),
  string_concat(PrefixSnippet, PatternSnippet, Snippet),
  open_string(Snippet, In),
  result_pattern(Kind, Pattern, Term),
  turtle:rdf_read_turtle(In, [Pattern], []).

format_string(node, "[<>~a].").
format_string(predicate, "[~a<>].").

prefix_declaration_snippet(String2) :-
  aggregate_all(
    set(String0),
    (
      rdf_prefix(Alias, Prefix),
      format(string(String0), "prefix ~a: <~a>", [Alias,Prefix])
    ),
    Strings
  ),
  string_list_concat(Strings, '\n', String1),
  string_concat(String1, '\n', String2).

result_pattern(node, rdf(_,_,Term), Term).
result_pattern(predicate, rdf(_,Term,_), Term).
