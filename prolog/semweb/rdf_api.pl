:- module(
  rdf_api,
  [
    prefix_local_iri/3,          % ?Prefix, ?Local, ?Iri
    rdf_assert_reification/4,    % +S, +P, +O, +Stmt
    rdf_assert_reification/5,    % +S, +P, +O, +G, +Stmt
    rdf_atom_to_term/2,          % +Atom, -Term
    rdf_chk/4,                   % ?S, ?P, ?O, ?G
    rdf_clean_quad/2,            % +Quad1, -Quad2
    rdf_clean_triple/2,          % +Triple1, -Triple2
    rdf_create_iri/3,            % +Prefix, +Path, -Iri
    rdf_create_well_known_iri/1, % -Iri
    rdf_deref/2,                 % +Uri, :Goal_2
    rdf_deref/3,                 % +Uri, :Goal_2, +Options
    rdf_is_skip_node/1,          % @Term
    rdf_is_well_known_iri/1,     % @Term
    rdf_literal/4,               % ?Literal, ?D, ?LTag, ?Lex
    rdf_list_member/2,           % ?X, ?L
    rdf_list_member/3,           % ?X, ?L, ?G
    rdf_triple_list_member/3,    % ?S, ?P, ?X
    rdf_triple_list_member/4,    % ?S, ?P, ?X, ?G
    rdf_load2/1,                 % +File
    rdf_load2/2,                 % +File, +Options
    rdf_media_type_format/2,     % +MediaType, +Format
    rdf_node/2,                  % ?Node, ?G
    rdf_prefix_member/2,         % ?Elem, +L
    rdf_prefix_memberchk/2,      % ?Elem, +L
    rdf_query_term/2,            % +Term, -QueryTerm
    rdf_reification/4,           % ?S, ?P, ?O, ?Stmt
    rdf_reification/5,           % ?S, ?P, ?O, ?G, ?Stmt
    rdf_term_to_atom/2,          % +Term, -Atom
    rdfs_instance/2,             % ?I, ?C
    rdfs_instance/3,             % ?I, ?C, ?G
    rdfs_range/2,                % ?P, ?C
    rdfs_range/3,                % ?P, ?C, ?G
    rdfs_subclass/2,             % ?C, ?D
    rdfs_subclass/3,             % ?C, ?D, ?G
    rdfs_subproperty/2,          % ?P, ?Q
    rdfs_subproperty/3,          % ?P, ?Q, ?G
    op(110, xfx, @),             % must be above .
    op(650, xfx, ^^),            % must be above :
    op(1150, fx, rdf_meta)
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_load_db/1 as rdf_load_dump,
     rdf_save_db/1 as rdf_save_dump
   ]).
:- reexport(library(semweb/rdf11)).
:- reexport(library(semweb/rdf11_containers)).

/** <module> RDF API

@author Wouter Beek
@version 2017/09-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(http/rfc7231)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin), []).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_prefix), []).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(uuid)).
:- use_module(library(xsd/xsd_number)).

:- meta_predicate
    rdf_deref(+, 2),
    rdf_deref(+, 2, +),
    rdf_deref_stream(+, +, 2, +).

:- rdf_register_prefix(bnode, 'https://example.org/.well-known/genid/').

:- rdf_meta
   prefix_local_iri(?, ?, r),
   rdf_assert_reification(r, r, o, r),
   rdf_assert_reification(r, r, o, r, r),
   rdf_chk(r, r, o, r),
   rdf_clean_lexical_form(r, +, -),
   rdf_clean_literal(o, o),
   rdf_clean_quad(t, -),
   rdf_clean_triple(t, -),
   rdf_deref(+, :),
   rdf_deref(+, :, +),
   rdf_is_skip_node(r),
   rdf_is_well_known_iri(r),
   rdf_list_member(r, t),
   rdf_list_member(r, t, r),
   rdf_literal(o, r, ?, ?),
   rdf_node(o, r),
   rdf_prefix_member(t, t),
   rdf_prefix_memberchk(t, t),
   rdf_reification(r, r, o, r),
   rdf_reification(r, r, o, r, r),
   rdf_term_to_atom(o, -),
   rdf_triple_list_member(r, r, o),
   rdf_triple_list_member(r, r, o, r),
   rdfs_instance(r, r),
   rdfs_instance(r, r, r),
   rdfs_range(r, r),
   rdfs_range(r, r, r),
   rdfs_subclass(r, r),
   rdfs_subclass(r, r, r),
   rdfs_subproperty(r, r),
   rdfs_subproperty(r, r, r).





%! prefix_local_iri(-Prefix:atom, -Local:atom,   +Iri:atom) is det.
%! prefix_local_iri(+Prefix:atom, +Local:atom, -Iri:atom) is det.
%
% Variant of rdf_global_id/2 that works with maplist/3 and siblings.

prefix_local_iri(Prefix, Local, Iri) :-
  rdf_global_id(Prefix:Local, Iri).



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



%! rdf_atom_to_term(+Atom:atom, -Term:compound) is det.

rdf_atom_to_term(Atom, Literal) :-
  atom_codes(Atom, Codes),
  phrase(rdf_literal_(Literal), Codes), !.
rdf_atom_to_term(NonLiteral, NonLiteral).

rdf_literal_(Literal) -->
  "\"",
  ...(Codes),
  "\"", !,
  ("^^" -> rdf_iri_(D) ; "@" -> rest_as_atom(LTag) ; ""),
  {
    atom_codes(Lex, Codes),
    rdf_literal(Literal, D, LTag, Lex)
  }.

rdf_iri_(Iri) -->
  "<",
  ...(Codes),
  ">", !,
  {atom_codes(Iri, Codes)}.



%! rdf_chk(?S, ?P, ?O, ?G) is nondet.

rdf_chk(S, P, O, G) :-
  once(rdf(S, P, O, G)).



%! rdf_clean_bnode(+BNode:atom, -Iri:atom) is det.

rdf_clean_bnode(BNode, Iri) :-
  atomic_list_concat(L1, '_:', BNode),
  append(L2, [Local], L1),
  md5(L2, Hash),
  atomic_list_concat([Hash,Local], ':', BNodeLabel),
  rdf_global_id(bnode:BNodeLabel, Iri).



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
rdf_clean_lexical_form(D, Lex1, Lex2) :-
  catch(rdf11:out_type(D, Value, Lex1), E, true),
  rdf11:pre_ground_object(Value, literal(type(D,Lex2))),
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



%! rdf_create_iri(+Prefix:atom, +Segments:list(atom), -Iri:atom) is det.

rdf_create_iri(Prefix, Segments, Iri2) :-
  rdf_current_prefix(Prefix, Iri1),
  uri_comp_add(path, Iri1, Segments, Iri2).



%! rdf_create_well_known_iri(-Iri) is det.

rdf_create_well_known_iri(Iri) :-
  uuid(Id),
  rdf_global_id(bnode:Id, Iri).



%! rdf_deref(+Uri:atom, :Goal_2) is det.
%! rdf_deref(+Uri:atom, :Goal_2, +Options:list(compound)) is det.
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
%   * format(+MediaType:compound)
%
%     Overrule the RDF serialization format.

rdf_deref(Uri, Goal_2) :-
  rdf_deref(Uri, Goal_2, []).


rdf_deref(Uri, Goal_2, Options1) :-
  % URI
  uri_components(Uri, uri_components(Scheme,Authority,_,_,_)),
  maplist(ground, [Scheme,Authority]), !,
  % `Accept' header
  findall(MT, rdf_media_type(MT), DefaultMTs),
  select_option(accept(MTs), Options1, Options2, DefaultMTs),
  atom_phrase(accept(MTs), Accept),
  setup_call_cleanup(
    http_open(
      Uri,
      In,
      [
        header(content_type,ContentType),
        request_header('Accept'=Accept),
        status_code(Status)
      ]
    ),
    (
      assertion(Status =:= 200),
      include(ground, [content_type(ContentType)], Options3),
      merge_options(Options3, Options2, Options4),
      rdf_deref_stream(Uri, In, Goal_2, Options4)
    ),
    close(In)
  ).
rdf_deref(File, Goal_2, Options) :-
  uri_file_name(Uri, File),
  setup_call_cleanup(
    gzopen(File, read, In),
    rdf_deref_stream(Uri, In, Goal_2, Options),
    close(In)
  ).

rdf_deref_stream(Uri, In, Goal_2, Options1) :-
  % Serialization format
  ignore(option(format(MT), Options1)),
  (   var(MT)
  ->  rdf_guess(In, MTs),
      (   % `Content-Type' header
          option(content_type(ContentType), Options1)
      ->  http_parse_header_value(content_type, ContentType, MT),
          member(GuessMT, MTs),
          (   'rdf_media_type_>'(MT, GuessMT)
          ->  !, true
          ;   print_message(warning, inconsistent_media_types(MT,GuessMT))
          )
      ;   member(MT, MTs)
      ),
      (   % URI path's file name extension
          uri_media_type(Uri, UriMT)
      ->  (   'rdf_media_type_>'(MT, UriMT)
          ->  !, true
          ;   print_message(warning, inconsistent_media_types(MT,UriMT))
          )
      ;   true
      )
  ;   true
  ),
  % Determine the base URI.
  option(base_uri(BaseUri), Options1, Uri),
  
  % Determine the blank node prefix.
  call_default_option(
    bnode_prefix(BNodePrefix),
    Options1,
    rdf_create_bnode_iri
  ),
  
  % Parse according to the guessed Media Type.
  (   % N-Quads
      MT = media(application/'n-quads',_)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(nquads)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % N-Triples
      MT = media(application/'n-triples',_)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(ntriples)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % RDF/XML
      MT = media(application/'rdf+xml',_)
  ->  merge_options(
        [base_uri(BaseUri),blank_nodes(noshare)],
        Options1,
        Options2
      ),
      process_rdf(In, Goal_2, Options2)
  ;   % TriG
      MT = media(application/trig,_)
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
      MT = media(text/turtle,_)
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
      memberchk(MT, [media(application/'xhtml+xml',_),media(text/html,_)])
  ->  merge_options(
        [anon_prefix(BNodePrefix),base(BaseUri)],
        Options1,
        Options2
      ),
      read_rdfa(In, Triples, Options2),
      maplist(Goal_2, Triples, _)
  ;   % An unsupported Media Type (e.g., JSON-LD).
      print_message(warning, unsupported_media_type(MT))
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



%! rdf_is_skip_node(@Term) is semidet.

rdf_is_skip_node(Term) :-
  rdf_is_bnode(Term), !.
rdf_is_skip_node(Term) :-
  rdf_is_well_known_iri(Term).



%! rdf_is_well_known_iri(@Term) is semidet.

rdf_is_well_known_iri(Iri) :-
  uri_comps(Iri, uri(Scheme,Authority,['.well-known',genid|_],_,_)),
  ground(Scheme-Authority).



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
  rdf_list_member(T, X, G).



%! rdf_literal(+Literal:compound, -D:atom, -LTag:atom, -Lex:atom) is det.
%! rdf_literal(-Literal:compound, +D:atom, +LTag:atom, +Lex:atom) is det.
%
% @tbd Special case for `rdf:HTML' and `rdf:XMLLiteral'.
%
% @tbd Special case for `xsd:decimal'.

%rdf_literal(literal(type(rdf:'HTML',Dom)), rdf:'HTML', _, Dom) :- !.
%rdf_literal(literal(type(rdf:'XMLLiteral',Dom)), rdf:'XMLLiteral', _, Dom) :- !.
rdf_literal(literal(type(D,Lex)), D, _, Lex) :-
  atom(D), !.
rdf_literal(literal(lang(LTag,Lex)), rdf:langString, LTag, Lex) :-
  atom(LTag), !.
rdf_literal(literal(Lex), _, _, Lex) :- !,
  atom(Lex).
rdf_literal(Value^^D, D, _, Lex) :- !,
  rdf11:rdf_lexical_form(Value^^D, Lex^^D).
rdf_literal(Lex@LTag, rdf:langString, LTag, Lex) :- !.
rdf_literal(Lex, xsd:string, _, Lex).



%! rdf_load2(+File:atom) is det.
%! rdf_load2(+File:atom, +Options:list(compound)) is det.
%
% Loads RDF based in the file extension of File.

rdf_load2(File) :-
  rdf_load2(File, []).


rdf_load2(File, Options1) :-
  ignore(option(format(Format), Options1)),
  % Guess the serialization format based on the file extension.
  (   var(Format)
  ->  file_base_name(File, Base),
      atomic_list_concat([_|Comps], ., Base),
      member(Ext, Comps),
      rdf_load_format_(Ext, Format), !
  ;   true
  ),
  merge_options([format(Format)], Options1, Options2),
  rdf_load(File, Options2).

rdf_load_format_(nq, nquads).
rdf_load_format_(nt, ntriples).
rdf_load_format_(rdf, xml).
rdf_load_format_(ttl, turtle).
rdf_load_format_(trig, trig).



%! rdf_media_type(+MediaType:compound) is semidet.
%! rdf_media_type(-MediaType:compound) is multi.

rdf_media_type(MT) :-
  rdf_media_type_format(MT, _).



%! rdf_media_type_format(+MediaType:compound, +Format:atom) is semidet.
%! rdf_media_type_format(+MediaType:compound, -Format:atom) is det.
%! rdf_media_type_format(-MediaType:compound, +Format:atom) is det.
%! rdf_media_type_format(-MediaType:compound, -Format:atom) is multi.

% Ordering represents precedence, from lower to hgiher.
rdf_media_type_format(media(application/'json-ld',[]),   jsonld).
rdf_media_type_format(media(application/'rdf+xml',[]),   rdfxml).
rdf_media_type_format(media(text/turtle,[]),             turtle).
rdf_media_type_format(media(application/'n-triples',[]), ntriples).
rdf_media_type_format(media(application/trig,[]),        trig).
rdf_media_type_format(media(application/'n-quads',[]),   nquads).



%! rdf_node(?Node, ?G) is nondet.

rdf_node(S, G) :-
  rdf(S, _, _, G).
rdf_node(O, G) :-
  rdf(_, _, O, G).



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



%! rdf_term_to_atom(+Term, -Atom) is det.

rdf_term_to_atom(literal(type(D,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"^^<~a>', [Lex,D]).
rdf_term_to_atom(literal(lang(LTag,Lex)), Atom) :- !,
  format(atom(Atom), '"~a"@~a', [Lex,LTag]).
rdf_term_to_atom(literal(Lex), Atom) :- !,
  rdf_term_to_atom(literal(type(xsd:string,Lex)), Atom).
rdf_term_to_atom(Atom, Atom) :-
  rdf_is_iri(Atom).



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
