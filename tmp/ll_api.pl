:- module(
  ll_api,
  [
    ll_doc/1,             % -Doc
    ll_doc/4,             % ?S, ?P, ?O, -Doc
    ll_doc_by_size/3,     % ?Low, ?High, -Doc
    ll_doc_by_size/4,     % ?Low, ?High, -Doc, -Size
    ll_doc_source/2,      % +Doc, -Source
    ll_doc_stats/2,       % +Doc, -Stats
    ll_docs/1,            % -Docs
    ll_docs/4,            % ?S, ?P, ?O, -Docs
    ll_docs_by_size/3,    % ?Low, ?High, -Docs
    ll_iri_doc/2,         % +Iri, -Doc
    ll_iri_docs/2,        % +Iri, -Docs
    ll_ldf/3,             % ?S, ?P, ?O
    ll_ldf/4,             % ?S, ?P, ?O, ?Doc
    ll_ldf_guess_size/5,  % ?S, ?P, ?O, -Pairs, -NumTriples
    ll_ldf2gml/3,         % ?S, ?P, ?O
    ll_ldf2gml/4,         % ?S, ?P, ?O, +Doc
    ll_ldf2gml/5,         % ?S, ?P, ?O, +Doc, +Opts
    ll_metadata/3,        % ?Doc, ?P, ?O
    ll_metadata_triples/2,% ?Doc, -NumTriples
    ll_predicate_stats/3, % +Doc, +P, -Stats
    ll_prefix_doc/2,      % +Prefix, -Doc
    ll_prefix_docs/2,     % +Prefix, -Docs
    ll_sparql_select/2,   % +Query, -Rows
    ll_stream/1,          %                   :Goal_5
    ll_stream/4,          % ?S, ?P, ?O,       :Goal_5
    ll_stream/5,          % ?S, ?P, ?O, ?Doc, :Goal_5
    ll_stream/6           % ?S, ?P, ?O, ?Doc, :Goal_5, +Opts
  ]
).

/** <module> LOD Laundromat API

@author Wouter Beek
@version 2016/07, 2016/10
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(conv/rdf2gml)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_io)).
:- use_module(library(list_ext)).
:- use_module(library(memoization)).
:- use_module(library(ordsets)).
:- use_module(library(os/io)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(service/ldf)).
:- use_module(library(sparql/sparql_query_client)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).

:- rdf_create_alias(lld, 'http://lodlaundromat.org/download/').
:- rdf_create_alias(llo, 'http://lodlaundromat.org/ontology/').
:- rdf_create_alias(llr, 'http://lodlaundromat.org/resource/').

:- meta_predicate
    ll_stream(5),
    ll_stream(?, ?, ?, 5),
    ll_stream(?, ?, ?, ?, 5),
    ll_stream(?, ?, ?, ?, 5, +).

:- rdf_meta
   ll_doc(r, r, o, -),
   ll_docs(r, r, o, -),
   ll_iri_doc(r, -),
   ll_iri_docs(r, -),
   ll_ldf(r, r, o),
   ll_ldf(r, r, o, r),
   ll_ldf_guess_size(r, r, o, -, -),
   ll_ldf2gml(r, r, o),
   ll_ldf2gml(r, r, o, r),
   ll_ldf2gml(r, r, o, r, +),
   ll_metadata(r, r, o),
   ll_predicate_stats(r, r, -),
   ll_prefix_doc(r, -),
   ll_prefix_docs(r, -),
   ll_stream(r, r, o, :),
   ll_stream(r, r, o, r, :),
   ll_stream(r, r, o, r, :, +).





%! ll_doc(-Doc) is nondet.

ll_doc(Doc) :-
  ll_doc_query0(Rows),
  member([Doc], Rows).



%! ll_doc(?S, ?P, ?O, -Doc) is nondet.

ll_doc(_,_, _, Doc) :-
  nonvar(Doc), !.
ll_doc(S, P, O, Doc) :-
  ll_docs(S, P, O, Docs),
  member(Doc, Docs).



%! ll_doc_by_size(?Low, ?High, -Doc) is nondet.
%! ll_doc_by_size(?Low, ?High, -Doc, -NumTriples) is nondet.

ll_doc_by_size(Low, High, Doc) :-
  ll_doc_by_size(Low, High, Doc, _).


ll_doc_by_size(Low, High, Doc, NumTriples) :-
  ll_docs_by_size0(Low, High, Pairs),
  member(NumTriples-Doc, Pairs).



%! ll_doc_source(+Doc, -Source) is det.

ll_doc_source(Doc, Source) :-
  atom_phrase(
    sparql_build_select([llo], [source], [rdf(Doc,llo:url,var(source))]),
    Query
  ),
  ll_sparql_select(Query, [[Source]]), !.
% The data document derives from an archive entry.
ll_doc_source(Doc, Source) :-
  atom_phrase(
    sparql_build_select(
      [llo],
      [parent,path],
      [rdf(Doc,llo:path,var(path)),rdf(var(parent),llo:containsEntry,Doc)]
    ),
    Query
  ),
  ll_sparql_select(Query, [[Parent,PathLit]]),
  q_literal_val(PathLit, Path),
  ll_doc_source(Parent, ParentSource),
  atomic_list_concat([ParentSource,Path], ' ', Source).



%! ll_doc_stats(+Doc, -Stats) is det.

ll_doc_stats(Doc, Stats) :-
  ll_index0([join], Doc, Stats).



%! ll_docs(-Docs) is det.

ll_docs(Docs) :-
  atom_phrase(
    sparql_build_select(
      [llo],
      [doc],
      [rdf(var(doc),llo:triples,var(numTriples))]
    ),
    Query
  ),
  findall(
    Docs,
    (
      ll_sparql_select(Query, Rows),
      maplist(singleton_list, Docs, Rows)
    ),
    Docss
  ),
  append(Docss, Docs).



%! ll_docs(?S, ?P, ?O, -Docs) is nondet.
%
% Enumerates the LOD Laundromat data documents in which the triple
% pattern 〈S,P,O〉 is satisfied.

ll_docs(S, P, O, Docs) :-
  include(rdf_is_iri, [S,P,O], Iris),
  (   Iris == []
  ->  ll_docs(Docs)
  ;   concurrent_maplist(ll_iri_to_docs, Iris, Docss),
      ord_intersection(Docss, Docs)
  ).



%! ll_docs_by_size(?Low, ?High, -Docs) is det.
%
% Enumerates the LOD Laundromat data documents that contain between
% Low and High number of statements.

ll_docs_by_size(Low, High, Docs) :-
  ll_docs_by_size0(Low, High, Pairs),
  pairs_values(Pairs, Docs).



%! ll_iri_doc(+Iri, -Doc) is nondet.

ll_iri_doc(Iri, Doc) :-
  ll_iri_docs(Iri, Docs),
  member(Doc, Docs).



%! ll_iri_docs(+Iri, -Docs) is nondet.

ll_iri_docs(Iri, Docs) :-
  ll_index0([r2d], Iri, Results1),
  maplist(atom_string, Results2, Results1),
  maplist(ll_doc_name0, Docs, Results2).



%! ll_iri_to_doc(+Iri, -Doc) is nondet.
%
% Non-deterministic wrapper around iri_to_docs/2.

ll_iri_to_doc(Iri, Doc) :-
  ll_iri_to_docs(Iri, Docs),
  member(Doc, Docs).



%! ll_iri_to_docs(+Iri, -Docs) is det.

ll_iri_to_docs(Iri, Docs) :-
  call_collect_messages(memo(ll_iri_docs(Iri, Docs))).



%! ll_iri_to_numdocs(+Iri, -NumDocs) is det.
%
% NumDocs is the number of LOD Laundromat data documents in which IRI
% appears as a term at least once.

ll_iri_to_numdocs(Iri, NumDocs) :-
  ll_iri_to_docs(Iri, Docs),
  length(Docs, NumDocs).



%! ll_ldf(?S, ?P, ?O) is nondet.
%! ll_ldf(?S, ?P, ?O, ?Doc) is nondet.

ll_ldf(S, P, O) :-
  ll_ldf(S, P, O, _).


ll_ldf(S, P, O, Doc) :-
  ll_doc(S, P, O, Doc),
  ll_doc_name0(Doc, Name),
  atomic_list_concat(['',Name], /, Path),
  uri_components(Endpoint, uri_components(http,'ldf.lodlaundromat.org',Path,_,_)),
  call_or_exception(ldf(S, P, O, Endpoint)).



%! ll_ldf_guess_size(?S, ?P, ?O, -Pairs, -NumTriples) is det.
%
% Returns the number of triples that match the given triple pattern
% 〈S,P,O〉.
%
% Pair is a list of pairs `Doc-NumTriples`, where Doc is a LOD
% Laundromat document identifier and `NumTriples` is a positive
% integer.

ll_ldf_guess_size(S, P, O, Pairs, NumTriples) :-
  ll_docs(S, P, O, Docs),
  maplist(ll_ldf_guess_size_goal0(S, P, O), Docs, Goals, NumTripless),
  concurrent(100, Goals, []),
  pairs_keys_values(Pairs, Docs, NumTripless),
  sum_list(NumTripless, NumTriples).


ll_ldf_guess_size_goal0(
  S,
  P,
  O,
  Doc,
  ldf_guess_size(S, P, O, Doc, NumTriples),
  NumTriples
).



%! ll_ldf2gml(?S, ?P, ?O) is nondet.
%! ll_ldf2gml(?S, ?P, ?O, ?Doc) is nondet.
%! ll_ldf2gml(?S, ?P, ?O, ?Doc, +Opts) is nondet.

ll_ldf2gml(S, P, O) :-
  ll_ldf2gml(S, P, O, _).


ll_ldf2gml(S, P, O, Doc) :-
  ll_ldf2gml(S, P, O, Doc, []).


ll_ldf2gml(S, P, O, Doc, Opts0) :-
  md5(rdf(S,P,O,Doc), Base),
  merge_options([base_name(Base)], Opts0, Opts),
  rdf2gml_start(Opts, NFile, EFile, GFile, ExportOpts),
  call_to_streams(NFile, EFile, ll_ldf2gml0(S, P, O, Doc, ExportOpts), Opts),
  rdf2gml_end(NFile, EFile, GFile, Opts).


ll_ldf2gml0(S, P, O, Doc, ExportOpts, NOut, EOut) :-
  forall(
    ll_ldf(S, P, O, Doc),
    rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts)
  ).



%! ll_metadata(?Doc, ?P, ?O) is nondet.
%
% Simple triple pattern matching with the document IRI in the subject
% position.

ll_metadata(Doc, P, O) :-
  ll_metadata_triples(Doc, Triples),
  % NONDET.
  member(rdf(Doc,P,O), Triples).


%! ll_metadata_triples(?Doc, -Triples) is det.
%
% Returns the RDF metadata description of the given data Document.

ll_metadata_triples(Doc, Triples) :-
  ll_doc(Doc),
  atom_phrase(
    sparql_build_select([], [p,o], [rdf(iri(Doc),var(p),var(o))]),
    Query
  ),
  ll_sparql_select(Query, Rows),
  maplist(row_to_triple0(Doc), Rows, Triples).


row_to_triple0(Doc, [P,O], rdf(Doc,P,O)).



%! ll_predicate_stats(+Doc, +P, -Stats) is det.

ll_predicate_stats(Doc, P, Stats) :-
  ll_index0([join,Doc], P, Stats).



%! ll_prefix_doc(+Prefix, -Doc) is nondet.
%! ll_prefix_docs(+Prefix, -Docs) is nondet.

ll_prefix_doc(Prefix, Doc) :-
  ll_prefix_docs(Prefix, Docs),
  member(Doc, Docs).


ll_prefix_docs(Prefix, Docs) :-
  ll_index0([ns2d], Prefix, Results1),
  maplist(atom_string, Results2, Results1),
  maplist(ll_doc_name0, Docs, Results2).



%! ll_sparql_select(+Query, -Rows) is nondet.

ll_sparql_select(Query, Rows) :-
  ll_endpoint0(Endpoint, Opts),
  sparql_select(Endpoint, Query, Rows, Opts), !.



%! ll_stream(:Goal_5) is nondet.
%! ll_stream(?S, ?P, ?O, :Goal_5) is nondet.
%! ll_stream(?S, ?P, ?O, ?Doc, :Goal_5) is nondet.
%! ll_stream(?S, ?P, ?O, ?Doc, :Goal_5, +Opts) is nondet.
%
% Retrieve triples from LOD Laundromat files.

ll_stream(Goal_5) :-
  ll_stream(_, _, _, Goal_5).


ll_stream(S, P, O, Goal_5) :-
  ll_stream(S, P, O, _, Goal_5).


ll_stream(S, P, O, Doc, Goal_5) :-
  ll_stream(S, P, O, Doc, Goal_5, []).


ll_stream(S, P, O, Doc, Goal_5, Opts1) :-
  ll_doc(S, P, O, Doc),
  ll_doc_download0(Doc, Iri),
  merge_options(
    [rdf_media_type(application/'n-quads'),retry(1000)],
    Opts1,
    Opts2
  ),
  call_collect_messages(
    rdf_call_on_tuples(Iri, Goal_5, Opts2),
    Status,
    Msgs
  ),
  % @tbd Make this more generic and part of PLC.
  (   (Status \== true ; Msgs \== [])
  ->  format(user_error, "BUGGY DATA DOCUMENT: ~a~n", [Doc]),
      print_message(warning, Status),
      maplist(print_message(warning), Msgs)
  ;   true
  ).





% HELPERS %

%! ll_docs_by_size_query0(?Lower, ?Higher, -Query) is det.
%
% Constructs a SPARQL query for retrieving LOD Laundromat documents
% whose size is in between Lower and Higher.

ll_docs_by_size_query0(Low, High, Query) :-
  % Lower bound.
  (positive_integer(Low) -> true ; Low = 1),
  Query1 = [filter(>=(var(ntrips),Low))],

  % Upper bound.
  (   positive_integer(High)
  ->  Query2 = [filter(<=(var(ntrips),High))]
  ;   Query2 = []
  ),

  % The number of triples is needed irrespective of there being a
  % bound.  This excludes documents with less than one triple.
  Query3 = [
    rdf(var(doc),llo:triples,var(ntrips)),
    rdf(var(doc),llo:statementsType,var(type))
  ],

  append([Query3,Query1,Query2,[rdf(var(doc),llo:md5,var(md5))]], Query).



%! ll_doc_download0(+Doc, -Iri) is det.

ll_doc_download0(Doc, Iri) :-
  nonvar(Doc), !,
  ll_doc_name0(Doc, Name),
  rdf_prefix_iri(lld:Name, Iri).
ll_doc_download0(Doc, Iri) :-
  nonvar(Iri), !,
  rdf_prefix_iri(lld:Name, Iri),
  ll_doc_name0(Doc, Name).



%! ll_doc_name0(?Doc, ?Name) is det.

ll_doc_name0(Doc, Name) :-
  rdf_prefix_iri(llr:Name, Doc).



%! ll_doc_query0(-Rows) is nondet.

ll_doc_query0(Rows) :-
  atom_phrase(
    sparql_build_select(
      [llo],
      [doc],
      [rdf(var(doc),llo:triples,var(numTriples))]
    ),
    Query
  ),
  ll_sparql_select(Query, Rows).



%! ll_docs_by_size0(?Low, ?High, -Pair) is det.

ll_docs_by_size0(Low, High, SortedPairs) :-
  ll_docs_by_size_query0(Low, High, Bgp),
  atom_phrase(sparql_build_select([llo], [doc,ntrips], Bgp), Query),
  findall(Rows, ll_sparql_select(Query, Rows), Rowss),
  findall(
    NTriples-Doc,
    (
      member(Rows, Rowss),
      member([Doc,NTriplesLit], Rows),
      NTriplesLit = literal(type(_,NTriplesA)),
      atom_number(NTriplesA, NTriples)
    ),
    Pairs
  ),
  keysort(Pairs, SortedPairs).



%! ll_endpoint0(-Endpoint, -Opts) is det.

ll_endpoint0(
  'http://sparql.backend.lodlaundromat.org',
  [manufacturer(virtuoso)]
).



%! ll_index0(+PathComps, +Iri, -Results) is det.

ll_index0(PathComps, Iri, Results) :-
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, [limit(0),uri(Iri)]),
  uri_components(
    RequestIri,
    uri_components(http,'index.lodlaundromat.org',Path,Query,_)
  ),
  json_read_any(RequestIri, Results).
