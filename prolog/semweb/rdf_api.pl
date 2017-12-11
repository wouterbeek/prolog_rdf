:- module(
  rdf_api,
  [
    isomorphic_graphset/2,        % +GraphSet1, +GraphSet2
    prefix_local_iri/3,           % ?Prefix, ?Local, ?Iri
    rdf/4,                        % ?S, ?P, ?O, ?G
    rdf_assert/1,                 % +Triple
    rdf_assert/3,                 % +S, +P, +O
    rdf_assert_list/2,            % +PrologList, -RdfList
    rdf_assert_list/3,            % +PrologList, -RdfList, +G
    rdf_assert_list/4,            % +S, +P, +PrologList, +G
    rdf_assert_reification/4,     % +S, +P, +O, +Stmt
    rdf_assert_reification/5,     % +S, +P, +O, +G, +Stmt
    rdf_atom_to_term/2,           % +Atom, -Term
    rdf_chk/4,                    % ?S, ?P, ?O, ?G
    rdf_clean_quad/2,             % +Quad1, -Quad2
    rdf_clean_triple/2,           % +Triple1, -Triple2
    rdf_create_graph/1,           % -G
    rdf_create_iri/3,             % +Prefix, +Path, -Iri
    rdf_create_prefix/1,          % +Pair
    rdf_create_well_known_iri/1,  % -Iri
    rdf_deref_stream/3,           % +Uri, +In, :Goal_2
    rdf_deref_stream/4,           % +Uri, +In, :Goal_2, +Options
    rdf_deref_triple/2,           % +Uri, -Quads
    rdf_deref_triple/3,           % +Uri, -Quads, +Options
    rdf_deref_uri/2,              % +Uri, :Goal_2
    rdf_deref_uri/3,              % +Uri, :Goal_2, +Options
    rdf_is_skip_node/1,           % @Term
    rdf_is_well_known_iri/1,      % @Term
    rdf_language_tagged_string/3, % ?LTag, ?Lex, ?Literal
    rdf_lexical_value/3,          % ?D, ?Lex, ?Val
    rdf_iri//1,                   % -Iri
    rdf_list_member/2,            % ?X, ?L
    rdf_list_member/3,            % ?X, ?L, ?G
    rdf_literal//1,               % -Literal
    rdf_literal/4,                % ?D, ?LTag, ?Lex, ?Literal
    rdf_literal_datatype_iri/2,   % +Literal, ?D
    rdf_literal_lexical_form/2,   % +Literal, ?Lex
    rdf_literal_value/2,          % +Literal, -Value
    rdf_load2/1,                  % +File
    rdf_load2/2,                  % +File, +Options
    rdf_media_type/1,             % +MediaType
    rdf_node/2,                   % ?Node, ?G
    rdf_prefix_iri/2,             % ?PrefixedName, ?Iri
    rdf_prefix_maplist/2,         % :Goal_1, +Args1
    rdf_prefix_member/2,          % ?Elem, +L
    rdf_prefix_memberchk/2,       % ?Elem, +L
    rdf_prefix_selectchk/3,       % +Elem, +List, -Rest
    rdf_prefix_term/2,            % ?PrefixedTerm, ?Term
    rdf_query_term/2,             % +Term, -QueryTerm
    rdf_reification/4,            % ?S, ?P, ?O, ?Stmt
    rdf_reification/5,            % ?S, ?P, ?O, ?G, ?Stmt
    rdf_term//1,                  % -Term
    rdf_term_to_atom/2,           % +Term, -Atom
    rdf_triple_list_member/3,     % ?S, ?P, ?X
    rdf_triple_list_member/4,     % ?S, ?P, ?X, ?G
    rdf_triples_graphset/2,       % +Triples, -GraphSet
    rdf_typed_literal/3,          % ?D, ?Lex, ?Literal
    rdfs_instance/2,              % ?I, ?C
    rdfs_instance/3,              % ?I, ?C, ?G
    rdfs_range/2,                 % ?P, ?C
    rdfs_range/3,                 % ?P, ?C, ?G
    rdfs_subclass/2,              % ?C, ?D
    rdfs_subclass/3,              % ?C, ?D, ?G
    rdfs_subproperty/2,           % ?P, ?Q
    rdfs_subproperty/3,           % ?P, ?Q, ?G
    t/4                           % +Backend, ?S, ?P, ?O
  ]).
:- reexport(library(semweb/rdf_db), [
    rdf/3,
    rdf_assert/4,
    rdf_is_literal/1,
    rdf_load_db/1 as rdf_load_dump,
    rdf_save_db/1 as rdf_save_dump
   ]).
:- reexport(library(semweb/rdf11), [
    rdf_create_bnode/1,
    rdf_default_graph/1,
    rdf_equal/2,
    rdf_global_id/2 as rdf_prefix_iri,
    rdf_global_object/2 as rdf_prefix_term,
    rdf_global_term/2 as rdf_prefix_any,
    rdf_graph/1,
    rdf_is_bnode/1,
    rdf_is_iri/1,
    rdf_is_subject/1,
    rdf_retractall/4,
    rdf_transaction/1,
    rdf_unload_graph/1,
    (rdf_meta)/1,
    op(1150, fx, (rdf_meta))
   ]).

/** <module> RDF API

@author Wouter Beek
@version 2017/09-2017/12
*/

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(http/http_header)).
:- use_module(library(lists)).
:- use_module(library(media_type)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin), []).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_prefix), []).
:- use_module(library(semweb/rdf_prefixes)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf11_containers)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).
:- use_module(library(sgml)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(uuid)).
:- use_module(library(xml/xsd)).
:- use_module(library(xml/xsd_number)).

:- meta_predicate
    rdf_deref_stream(+, +, 2),
    rdf_deref_stream(+, +, 2, +),
    rdf_deref_uri(+, 2),
    rdf_deref_uri(+, 2, +),
    rdf_prefix_maplist(1, +).

:- multifile
    t/4.

:- rdf_meta
   prefix_local_iri(?, ?, r),
   rdf(r, r, o, r),
   rdf_assert(t),
   rdf_assert(r, r, o),
   rdf_assert_list(+, -, r),
   rdf_assert_list(r, r, t, r),
   rdf_assert_reification(r, r, o, r),
   rdf_assert_reification(r, r, o, r, r),
   rdf_chk(r, r, o, r),
   rdf_clean_lexical_form(r, +, -),
   rdf_clean_literal(o, o),
   rdf_clean_quad(t, -),
   rdf_clean_triple(t, -),
   rdf_deref_triple(r, -),
   rdf_deref_triple(r, -, +),
   rdf_deref_uri(r, :),
   rdf_deref_uri(r, :, +),
   rdf_is_skip_node(r),
   rdf_is_well_known_iri(r),
   rdf_language_tagged_string(?, ?, o),
   rdf_lexical_value(r, ?, ?),
   rdf_list_member(r, t),
   rdf_list_member(r, t, r),
   rdf_literal(r, ?, ?, o),
   rdf_literal_datatype_iri(o, r),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_value(o, -),
   rdf_node(o, r),
   rdf_prefix_maplist(:, t),
   rdf_prefix_member(t, t),
   rdf_prefix_memberchk(t, t),
   rdf_prefix_selectchk(t, t, t),
   rdf_reification(r, r, o, r),
   rdf_reification(r, r, o, r, r),
   rdf_term_to_atom(t, -),
   rdf_triple_list_member(r, r, t),
   rdf_triple_list_member(r, r, t, r),
   rdf_typed_literal(r, ?, o),
   rdfs_instance(r, r),
   rdfs_instance(r, r, r),
   rdfs_range(r, r),
   rdfs_range(r, r, r),
   rdfs_subclass(r, r),
   rdfs_subclass(r, r, r),
   rdfs_subproperty(r, r),
   rdfs_subproperty(r, r, r),
   t(+, r, r, o).

:- rdf_register_prefix(bnode, 'https://example.org/.well-known/genid/').





%! isomorphic_graphsets(+GraphSet1:ordset(compound),
%!                      +GraphSet2:ordset(compound)) is semidet.
%
% Is true if there is a consistent mapping between of blank nodes in
% Graph1 to blank nodes in Graph2 that makes both graphs equal.  This
% maps to the Prolog notion of _variant_ if there was a canonical
% ordering of triples.
%
% Blank nodes are assumed to be replaced by Prolog variables.

isomorphic_graphset(GraphSet1, GraphSet2) :-
  once(graphset_permutation(GraphSet1, Perm1)),
  graphset_permutation(GraphSet2, Perm2),
  Perm1 =@= Perm2, !.

graphset_permutation(GraphSet, Graph) :-
  partition(ground, GraphSet, Ground, NonGround),
  permutation(NonGround, NonGroundPermutation),
  append(Ground, NonGroundPermutation, Graph).



%! prefix_local_iri(-Prefix:atom, -Local:atom,   +Iri:atom) is det.
%! prefix_local_iri(+Prefix:atom, +Local:atom, -Iri:atom) is det.
%
% Variant of rdf_prefix_iri/2 that works with maplist/3 and siblings.

prefix_local_iri(Prefix, Local, Iri) :-
  rdf_prefix_iri(Prefix:Local, Iri).



%! rdf(?S, ?P, ?O, ?G) is nondet.

rdf(S, P, O, G) :-
  rdf11:pre_graph(G, G0),
  rdf_db:rdf(S, P, O, G0),
  rdf11:post_graph(G, G0).



%! rdf_assert(+Triple:rdf_triple) is det.

rdf_assert(rdf(S,P,O)) :-
  rdf_assert(S, P, O).



%! rdf_assert(+S, +P, +O) is det.

rdf_assert(S, P, O) :-
  rdf_default_graph(G),
  rdf_assert(S, P, O, G).



%! rdf_assert_list(+PrologList:list, -RdfList:bnode) is det.
%! rdf_assert_list(+PrologList:list, -RdfList:bnode, +G:iri) is det.
%! rdf_assert_list(+S:rdf_subject, +P:iri, +PrologList:list, +G:iri) is det.

rdf_assert_list(PrologList, RdfList) :-
  rdf_default_graph(G),
  rdf_assert_list(PrologList, RdfList, G).


rdf_assert_list(PrologList, RdfList, G) :-
  must_be(list, PrologList),
  rdf_transaction(rdf_assert_list_(PrologList, RdfList, G)).

rdf_assert_list_([], Nil, _) :-
  rdf_equal(rdf:nil, Nil).
rdf_assert_list_([H|T], L2, G) :-
  (var(L2) -> rdf_create_bnode(L2) ; true),
  rdf_assert(L2, rdf:type, rdf:'List', G),
  rdf_assert(L2, rdf:first, H, G),
  (   T == []
  ->  rdf_assert(L2, rdf:rest, rdf:nil, G)
  ;   rdf_create_bnode(T2),
      rdf_assert(L2, rdf:rest, T2, G),
      rdf_assert_list_(T, T2, G)
  ).


rdf_assert_list(S, P, PrologList, G) :-
  rdf_assert_list(PrologList, RdfList, G),
  rdf_assert(S, P, RdfList, G).



%! rdf_assert_reification(+S, +P, +O, +Stmt) is nondet.
%! rdf_assert_reification(+S, +P, +O, +G, +Stmt) is nondet.

rdf_assert_reification(S, P, O, Stmt) :-
  rdf_assert(Stmt, rdf:subject, S),
  rdf_assert(Stmt, rdf:predicate, P),
  rdf_assert(Stmt, rdf:object, O).


rdf_assert_reification(S, P, O, G, Stmt) :-
  rdf_assert(Stmt, rdf:subject, S, G),
  rdf_assert(Stmt, rdf:predicate, P, G),
  rdf_assert(Stmt, rdf:object, O, G).



%! rdf_atom_to_term(+Atom:atom, -Term:rdf_term) is det.

rdf_atom_to_term(Atom, Term) :-
  atom_codes(Atom, Codes),
  phrase(rdf_term_(Term), Codes).



%! rdf_chk(?S, ?P, ?O, ?G) is nondet.

rdf_chk(S, P, O, G) :-
  once(rdf(S, P, O, G)).



%! rdf_clean_bnode(+BNode:atom, -Iri:atom) is det.

rdf_clean_bnode(BNode, Iri) :-
  atomic_list_concat(L1, '_:', BNode),
  append(L2, [Local], L1),
  md5(L2, Hash),
  atomic_list_concat([Hash,Local], ':', BNodeLabel),
  rdf_prefix_iri(bnode:BNodeLabel, Iri).



%! rdf_clean_graph(+G1, -G2) is det.

rdf_clean_graph(G1, G3) :-
  rdf11:post_graph(G2, G1),
  (G2 == user -> G3 = default ; rdf_clean_iri(G2, G3)).



%! rdf_clean_iri(+Uri1, -Uri2) is det.

rdf_clean_iri(Uri1, Uri2) :-
  uri_resolve(Uri1, 'https://example.org/', Uri2).



%! rdf_clean_lexical_form(+D:atom, +Lex1:atom, -Lex2:atom) is semidet.

rdf_clean_lexical_form(rdf:'HTML', Lex1, Lex2) :-
  rdf11:write_xml_literal(html, Lex1, Lex2).
rdf_clean_lexical_form(rdf:'XMLLiteral', Lex1, Lex2) :-
  rdf11:write_xml_literal(xml, Lex1, Lex2).
rdf_clean_lexical_form(xsd:decimal, Lex1, Lex2) :-
  (   string_phrase(decimalLexicalMap(Val), Lex1)
  ->  atom_phrase(decimalCanonicalMap(Val), Lex2)
  ;   print_message(warning, invalid_decimal(Lex1)),
      fail
  ).
rdf_clean_lexical_form(rdf:langString, Lex, Lex).
rdf_clean_lexical_form(D, Lex1, Lex2) :-
  catch(rdf11:out_type(D, Value, Lex1), E, true),
  rdf11:in_type(D, Value, D, Lex2),
  (   var(E)
  ->  (   % Warning for a non-canonical lexical form.
          Lex1 \== Lex2
      ->  print_message(warning, non_canonical_lexical_form(D,Lex1,Lex2))
      ;   true
      )
  ;   % Warning+failure for an incorrect lexical form.
      print_message(warning, E),
      fail
  ).



%! rdf_clean_literal(+Literal1:compound, -Literal2:compound) is semidet.

% rdf:langString
rdf_clean_literal(literal(lang(LTag1,Lex)), literal(lang(LTag2,Lex))) :- !,
  downcase_atom(LTag1, LTag2),
  % Warning for a non-canonical language tag.
  (   LTag1 \== LTag2
  ->  print_message(warning, non_canonical_language_tag(LTag1))
  ;   true
  ).
rdf_clean_literal(literal(type(D,Lex1)), literal(type(D,Lex2))) :- !,
  rdf_clean_lexical_form(D, Lex1, Lex2).
rdf_clean_literal(literal(Lex), Literal) :-
  rdf_clean_literal(literal(type(xsd:string,Lex)), Literal).



%! rdf_clean_nonliteral(+Term1:atom, -Term2:atom) is det.

rdf_clean_nonliteral(BNode, Iri) :-
  rdf_is_bnode(BNode), !,
  rdf_clean_bnode(BNode, Iri).
rdf_clean_nonliteral(Iri1, Iri2) :-
  rdf_is_iri(Iri1), !,
  rdf_clean_iri(Iri1, Iri2).



%! rdf_clean_quad(+Quad1:compound, -Quad2:compound) is semidet.

rdf_clean_quad(rdf(S1,P1,O1,G1), rdf(S2,P2,O2,G2)) :-
  rdf_clean_triple(rdf(S1,P1,O1), rdf(S2,P2,O2)),
  rdf_clean_graph(G1, G2).



%! rdf_clean_term(+Term1, -Term2) is det.

rdf_clean_term(Term1, Term2) :-
  rdf_clean_nonliteral(Term1, Term2), !.
rdf_clean_term(Literal1, Literal2) :-
  rdf_clean_literal(Literal1, Literal2).



%! rdf_clean_triple(+Triple1:compound, -Triple2:compound) is semidet.

rdf_clean_triple(rdf(S1,P1,O1), rdf(S2,P2,O2)) :-
  rdf_clean_nonliteral(S1, S2),
  rdf_clean_iri(P1, P2),
  rdf_clean_term(O1, O2).



%! rdf_create_graph(-G:iri) is det.

rdf_create_graph(G) :-
  uuid(Id),
  rdf_prefix_iri(ex:Id, G).



%! rdf_create_iri(+Prefix:atom, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Prefix, Segments, Iri2) :-
  rdf_current_prefix(Prefix, Iri1),
  uri_comp_add(path, Iri1, Segments, Iri2).



%! rdf_create_prefix(+Pair:pair(atom)) is det.
%
% Syntactic sugar for registering multiple RDF prefixes using
% maplist/2.

rdf_create_prefix(Prefix-Iri) :-
  rdf_register_prefix(Prefix, Iri).



%! rdf_create_well_known_iri(-Iri) is det.

rdf_create_well_known_iri(Iri) :-
  uuid(Id),
  rdf_prefix_iri(bnode:Id, Iri).



%! rdf_deref_stream(+Uri:atom, +In:stream, :Goal_2) is det.
%! rdf_deref_stream(+Uri:atom, +In:stream, :Goal_2,
%!                  +Options:list(compound)) is det.

rdf_deref_stream(Uri, In, Goal_2) :-
  rdf_deref_stream(Uri, In, Goal_2, []).


rdf_deref_stream(Uri, In, Goal_2, Options1) :-
  % Serialization format
  ignore(option(media_type(MediaType), Options1)),
  (   var(MediaType)
  ->  rdf_guess_stream(In, GuessMediaType),
      (   % `Content-Type' header
          option(content_type(ContentType), Options1)
      ->  http_parse_header_value(content_type, ContentType, MediaType),
          (   'rdf_media_type_>'(MediaType, GuessMediaType)
          ->  !, true
          ;   print_message(warning,
                            inconsistent_media_types(MediaType,GuessMediaType))
          )
      ;   MediaType = GuessMediaType
      ),
      (   % URI path's file name extension
          uri_media_type(Uri, UriMediaType)
      ->  (   'rdf_media_type_>'(MediaType, UriMediaType)
          ->  !, true
          ;   print_message(warning,
                            inconsistent_media_types(MediaType,UriMediaType))
          )
      ;   true
      )
  ;   true
  ),
  % Determine the base URI.
  option(base_uri(BaseUri), Options1, Uri),

  % Determine the blank node prefix.  Use a well-known IRI with a UUID
  % component by default.
  call_default_option(
    bnode_prefix(BNodePrefix),
    Options1,
    rdf_create_well_known_iri
  ),

  % Parse according to the guessed Media Type.
  (   % N-Quads
      media_type_comps(MediaType, application, 'n-quads', _)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(nquads)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % N-Triples
      media_type_comps(MediaType, application, 'n-triples', _)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(ntriples)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % RDF/XML
      media_type_comps(MediaType, application, 'rdf+xml', _)
  ->  merge_options(
        [base_uri(BaseUri),blank_nodes(noshare)],
        Options1,
        Options2
      ),
      process_rdf(In, Goal_2, Options2)
  ;   % TriG
      media_type_comps(MediaType, application, trig, _)
  ->  merge_options(
        [
          anon_prefix(BNodePrefix),
          base_uri(BaseUri),
          format(trig),
          resources(iri)
        ],
        Options1,
        Options2
      ),
      rdf_process_turtle(In, Goal_2, Options2)
  ;   % Turtle
      media_type_comps(MediaType, text, turtle, _)
  ->  merge_options(
        [
          anon_prefix(BNodePrefix),
          base_uri(BaseUri),
          format(turtle),
          resources(iri)
        ],
        Options1,
        Options2
      ),
      rdf_process_turtle(In, Goal_2, Options2)
  ;   % RDFa
      memberchk(MediaType,
                [media(application/'xhtml+xml',_),media(text/html,_)])
  ->  merge_options(
        [anon_prefix(BNodePrefix),base(BaseUri)],
        Options1,
        Options2
      ),
      read_rdfa(In, Triples, Options2),
      maplist(Goal_2, Triples, _)
  ;   % An unsupported Media Type (e.g., JSON-LD).
      print_message(warning, unsupported_media_type(MediaType))
  ).

'rdf_media_type_>'(X, Y) :-
  'rdf_media_type_='(X, Y), !.
'rdf_media_type_>'(X, Z) :-
  'rdf_media_type_strict>'(X, Y),
  'rdf_media_type_>'(Y, Z).

'rdf_media_type_='(media(Supertype/Subtype,_),  media(Supertype/Subtype,_)).

'rdf_media_type_strict>'(media(application/trig,_), media(text/turtle,_)).
'rdf_media_type_strict>'(
  media(text/turtle,_),
  media(application/'n-triples',_)
).
'rdf_media_type_strict>'(
  media(application/'n-quads',_),
  media(application/'n-triples',_)
).



%! rdf_deref_triple(+Uri:uri, -Triple:rdf_triple) is det.
%! rdf_deref_triple(+Uri:uri, -Triple:rdf_triple,
%!                  +Options:list(compound)) is det.

rdf_deref_triple(Uri, Triple) :-
  rdf_deref_triple(Uri, Triple, []).


rdf_deref_triple(Uri, rdf(S,P,O), Options) :-
  setup_call_cleanup(
    rdf_create_graph(G),
    (
      rdf_deref_uri(Uri, rdf_deref_triples_(G), Options),
      rdf(S, P, O, G)
    ),
    rdf_unload_graph(G)
  ).
  
rdf_deref_triples_(G, Triples, _) :-
  maplist(rdf_deref_triple_(G), Triples).

rdf_deref_triple_(G, rdf(S,P,O)) :-
  rdf_assert(S, P, O, G).



%! rdf_deref_uri(+Uri:atom, :Goal_2) is det.
%! rdf_deref_uri(+Uri:atom, :Goal_2, +Options:list(compound)) is det.
%
% The following options are supported:
%
%   * accept(+MediaTypes:list(compound))
%
%     The value of the HTTP Accept header, from high to low
%     precedence.  The default value is a list of all and only
%     standardized Media Types.
%
%   * base_uri(+atom)
%
%     The default is the URI of the last metadata element.
%
%   * bnode_prefix(+atom)
%
%     The default is a well-known IRI as per RDF 1.1.
%
%   * content_type(+MediaType:compound)
%
%     Overrule the Media Type communicated in the `Content-Type' reply
%     header.
%
%   * media_type(+MediaType:compound)
%
%     Overrule the RDF serialization format.

rdf_deref_uri(Uri, Goal_2) :-
  rdf_deref_uri(Uri, Goal_2, []).


rdf_deref_uri(Uri, Goal_2, Options1) :-
  % URI
  uri_components(Uri, uri_components(Scheme,Authority,_,_,_)),
  maplist(ground, [Scheme,Authority]), !,
  % `Accept' header
  (   select_option(accept(MediaTypes), Options1, Options2)
  ->  true
  ;   findall(MediaType, rdf_media_type(MediaType), MediaTypes),
      Options2 = Options1
  ),
  setup_call_cleanup(
    http_open2(Uri, In, [accept(MediaTypes),failure(404),metadata([Meta|_])]),
    (
      _{'content-type': [ContentType]} :< Meta.headers,
      include(ground, [content_type(ContentType)], Options3),
      merge_options(Options3, Options2, Options4),
      rdf_deref_stream(Uri, In, Goal_2, Options4)
    ),
    close(In)
  ).



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
  uri_comps(Iri, uri(Scheme,Authority,['.well-known',genid|_],_,_)),
  ground(Scheme-Authority).



%! rdf_language_tagged_string(+LTag:atom, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_language_tagged_string(-LTag:atom, -Lex:atom, +Literal:rdf_literal) is det.

rdf_language_tagged_string(LTag, Lex, literal(lang(LTag,Lex))).



%! rdf_lexical_value(+D:atom, +Lex:atom, -Val:term) is det.
%! rdf_lexical_value(+D:atom, -Lex:atom, +Val:term) is det.

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



%! rdf_list_member(?X, ?L) is nondet.

rdf_list_member(X, L) :-
  rdf(L, rdf:first, X).
rdf_list_member(X, L) :-
  rdf(L, rdf:rest, T),
  rdf_list_member(T, X).



%! rdf_list_member(?X, ?L, ?G) is nondet.

rdf_list_member(X, L, G) :-
  rdf(L, rdf:first, X, G).
rdf_list_member(X, L, G) :-
  rdf(L, rdf:rest, T, G),
  rdf_list_member(X, T, G).



%! rdf_literal(-Literal:rdf_literal)// .

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



%! rdf_load2(+File:atom) is det.
%! rdf_load2(+File:atom, +Options:list(compound)) is det.
%
% Loads RDF based in the file extension of File.  The RDF
% serialization format can be overruled with the option format/1,
% which gives an RDF file name extension.

rdf_load2(File) :-
  rdf_load2(File, []).


rdf_load2(File, Options1) :-
  ignore(option(format(Ext), Options1)),
  (   var(Ext)
  ->  % Guess the RDF serialization format based on the file name
      % extension.
      file_base_name(File, Base),
      atomic_list_concat([_|Comps], ., Base),
      member(Ext, Comps),
      rdf_format_extension_(_, Ext), !
  ;   true
  ),
  rdf_format_extension_(Format0, Ext),
  merge_options([anon_prefix('_:'),format(Format0)], Options1, Options2),
  rdf_db:rdf_load(File, Options2).

rdf_format_extension_(nquads, nq).
rdf_format_extension_(ntriples, nt).
rdf_format_extension_(rdf, html).
rdf_format_extension_(trig, trig).
rdf_format_extension_(turtle, ttl).
rdf_format_extension_(xml, rdf).



%! rdf_media_type(+MediaType:compound) is semidet.
%! rdf_media_type(-MediaType:compound) is multi.

% Ordering represents precedence, as used in HTTP Accept headers, from
% lower to higher.
rdf_media_type(media(application/'xhtml+xml',[])).
rdf_media_type(media(application/'json-ld',[])).
rdf_media_type(media(application/'n-quads',[])).
rdf_media_type(media(application/'n-triples',[])).
rdf_media_type(media(application/'rdf+xml',[])).
rdf_media_type(media(application/trig,[])).
rdf_media_type(media(text/turtle,[])).



%! rdf_node(?Node, ?G) is nondet.

rdf_node(S, G) :-
  rdf(S, _, _, G).
rdf_node(O, G) :-
  rdf(_, _, O, G).



%! rdf_prefix_maplist(:Goal_1, +Args1:list) is det.

rdf_prefix_maplist(Goal_1, L) :-
  maplist(Goal_1, L).



%! rdf_prefix_member(?Elem, +L) is nondet.
%
% Calls member/2 under RDF prefix expansion.

rdf_prefix_member(Elem, L) :-
  member(Elem, L).



%! rdf_prefix_memberchk(?Elem, +L) is nondet.
%
% Calls memberchk/2 under RDF prefix expansion.

rdf_prefix_memberchk(Elem, L) :-
  memberchk(Elem, L).



%! rdf_prefix_selectchk(+Elem:rdf_term, +List:list, -Rest:list) is det.

rdf_prefix_selectchk(Elem, List, Rest) :-
  selectchk(Elem, List, Rest).



%! rdf_query_term(+Term, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Value],
  ground(Value),
  rdf_term_to_atom(Value, Atom),
  QueryTerm =.. [Key,Atom].



%! rdf_reification(?S, ?P, ?O, ?Stmt) is nondet.
%! rdf_reification(?S, ?P, ?O, ?G, ?Stmt) is nondet.

rdf_reification(S, P, O, Stmt) :-
  rdf(Stmt, rdf:subject, S),
  rdf(Stmt, rdf:predicate, P),
  rdf(Stmt, rdf:object, O).


rdf_reification(S, P, O, G, Stmt) :-
  rdf(Stmt, rdf:subject, S, G),
  rdf(Stmt, rdf:predicate, P, G),
  rdf(Stmt, rdf:object, O, G).



%! rdf_term(-Term:rdf_term)// .

rdf_term(Iri) -->
  rdf_iri(Iri), !.
rdf_term(Literal) -->
  rdf_literal(Literal).
rdf_term(BNode) -->
  "_:",
  rest(T),
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
rdf_term_to_atom(BNode, BNode).



%! rdf_triple_list_member(?S, ?P, ?X) is nondet.

rdf_triple_list_member(S, P, X) :-
  ground(X), !,
  rdf_list_member(X, L),
  rdf(S, P, L).
rdf_triple_list_member(S, P, X) :-
  rdf(S, P, L),
  rdf_list_member(X, L).



%! rdf_triple_list_member(?S, ?P, ?X, G) is nondet.

rdf_triple_list_member(S, P, X, G) :-
  ground(X), !,
  rdf_list_member(X, L, G),
  rdf(S, P, L, G).
rdf_triple_list_member(S, P, X, G) :-
  rdf(S, P, L, G),
  rdf_list_member(X, L, G).



%! rdf_triples_graphset(+Triples:list(compound),
%!                      -GraphSet:list(compound)) is det.

rdf_triples_graphset(Triples, GraphSet) :-
  rdf_triples_vars(Triples, Terms),
  sort(Terms, GraphSet).

rdf_triples_vars(Triples, Terms) :-
  empty_assoc(Map),
  rdf_triples_vars(Triples, Terms, Map, _).

rdf_triples_vars([], [], Map, Map).
rdf_triples_vars([rdf(S1,P,O1)|T1], [rdf(S2,P,O2)|T2], Map1, Map4) :-
  rdf_nonliteral_var(S1, S2, Map1, Map2),
  rdf_term_var(O1, O2, Map2, Map3),
  rdf_triples_vars(T1, T2, Map3, Map4).

rdf_nonliteral_var(BNode, Var, Map1, Map2) :-
  rdf_is_bnode(BNode), !,
  (   get_assoc(BNode, Map1, Var)
  ->  Map2 = Map1
  ;   put_assoc(BNode, Map1, Var, Map2)
  ).
rdf_nonliteral_var(Iri, Iri, Map, Map).

rdf_term_var(NonLiteral, Term, Map1, Map2) :-
  rdf_nonliteral_var(NonLiteral, Term, Map1, Map2), !.
rdf_term_var(Literal, Literal, Map, Map).



%! rdf_typed_literal(+D:iri, +Lex:atom, -Literal:rdf_literal) is det.
%! rdf_typed_literal(-D:iri, -Lex:atom, +Literal:rdf_literal) is det.

rdf_typed_literal(D, Lex, literal(type(D,Lex))).



%! rdfs_instance(?I, ?C) is nondet.
%! rdfs_instance(?I, ?C, ?G) is nondet.

rdfs_instance(I, D) :-
  rdf(I, rdf:type, C),
  rdfs_subclass(C, D).


rdfs_instance(I, D, G) :-
  rdf(I, rdf:type, C, G),
  rdfs_subclass(C, D, G).



%! rdfs_range(?P, ?C) is nondet.
%! rdfs_range(?P, ?C, ?G) is nondet.

rdfs_range(P, C) :-
  rdfs_subproperty(P, Q),
  rdf(Q, rdfs:range, C).


rdfs_range(P, C, G) :-
  rdfs_subproperty(P, Q, G),
  rdf(Q, rdfs:range, C, G).



%! rdfs_subclass(?C, ?D) is nondet.
%! rdfs_subclass(?C, ?D, ?G) is nondet.

rdfs_subclass(C, D) :-
  closure0(rdfs_subclass_, C, D).

rdfs_subclass_(C, D) :-
  rdf(C, rdfs:subClassOf, D).


rdfs_subclass(C, D, G) :-
  closure0(rdfs_subclass_(G), C, D).

rdfs_subclass_(G, C, D) :-
  rdf(C, rdfs:subClassOf, D, G).



%! rdfs_subproperty(?P, ?Q) is nondet.
%! rdfs_subproperty(?P, ?Q, ?G) is nondet.

rdfs_subproperty(P, Q) :-
  closure0(rdfs_subproperty_, P, Q).

rdfs_subproperty_(P, Q) :-
  rdf(P, rdfs:subPropertyOf, Q).


rdfs_subproperty(P, Q, G) :-
  closure0(rdfs_subproperty_(G), P, Q).

rdfs_subproperty_(G, P, Q) :-
  rdf(P, rdfs:subPropertyOf, Q, G).



%! t(+Backend, ?S, ?P, ?O) is nondet.

t(rdf, S, P, O) :-
  rdf(S, P, O).
t(rdf(G), S, P, O) :-
  rdf(S, P, O, G).
%t(hdt(HdtOrG), S, P, O) :-
%  (hdt_graph(Hdt, HdtOrG) -> true ; Hdt = HdtOrG),
%  hdt_triple(Hdt, S, P, O).
