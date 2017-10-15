:- module(
  rdf_stream,
  [
    rdf_deref_quad/2,    % +Uri, -Quad
    rdf_deref_quad/3,    % +Uri, -Quad, +Options
    rdf_deref_triple/2,  % +Uri, -Triple
    rdf_deref_triple/3,  % +Uri, -Triple, +Options
    rdf_reserialize/2,   % +Uri, +File
    rdf_reserialize/3,   % +Kind, +Uri, +File
    rdf_reserialize/4,   % +Kind, +Uri, +File, +Options
    rdf_to_hdt/2         % +UriSpec, +HdtFile
  ]
).

/** <module> RDF stream

Streamed processing of RDF data.

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(file_ext)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin), []).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    call_on_rdf(+, 2),
    call_on_rdf(+, 2, +).

:- rdf_meta
   rdf_deref_quad(r, t),
   rdf_deref_quad(r, t, +),
   rdf_deref_triple(r, t),
   rdf_deref_triple(r, t, +).





%! rdf_deref_quad(+UriSpec:compound, -Quad:compound) is nondet.
%! rdf_deref_quad(+UriSpec:compound, -Quad:compound,
%!                +Options:list(compound)) is nondet.

rdf_deref_quad(UriSpec, Quad) :-
  rdf_deref_quad(UriSpec, Quad, []).


rdf_deref_quad(UriSpec, Quad, Options) :-
  call_on_rdf(UriSpec, rdf_deref_quad_(Quad), Options).

rdf_deref_quad_(Quad, Tuples, _) :-
  member(Tuple, Tuples),
  rdf_clean_tuple(Tuple, Quad).



%! rdf_deref_triple(+UriSpec:term, -Triple:compound) is nondet.
%! rdf_deref_triple(+UriSpec:term, -Triple:compound,
%!                  +Options:list(compound)) is nondet.
%
% Options are passed to rdf_deref_quad/3.

rdf_deref_triple(UriSpec, Triple) :-
  rdf_deref_triple(UriSpec, Triple, []).


rdf_deref_triple(UriSpec, Triple, Options) :-
  rdf_deref_quad(UriSpec, Quad, Options),
  rdf_tuple_triple(Quad, Triple).



%! rdf_reserialize(+File1:atom, +File2:atom) is nondet.
%! rdf_reserialize(+Kind:oneof([quads,triples]), +File1:atom,
%!                 +File2:atom) is nondet.
%! rdf_reserialize(+Kind:oneof([quads,triples]), +File1:atom,
%!                 +File2:atom, +Options:list(compound)) is nondet.
%
% Reserializes RDF data from URI to N-Quads File.

rdf_reserialize(File1, File2) :-
  rdf_reserialize(quads, File1, File2).


rdf_reserialize(Kind, File1, File2) :-
  rdf_reserialize(Kind, File1, File2, []).


rdf_reserialize(Kind, File1, File2, Options) :-
  setup_call_cleanup(
    gzopen(File1, write, Out),
    forall(
      call_on_rdf(UriSpec, write_ntuples_(Kind, Out), Options),
      true
    ),
    close(Out)
  ).

write_ntuples_(Kind, Out, Tuples, _) :-
  convlist(rdf_clean_tuple, Tuples, Quads),
  maplist(write_ntuple_(Kind, Out), Quads).

write_ntuple_(quads, Out, Quad) :- !,
  write_nquad(Out, Quad).
write_ntuple_(triples, Out, Quad) :-
  write_ntriple(Out, Quad).



%! rdf_to_hdt(+UriSpec:term, +File:term) is det.

rdf_to_hdt(UriSpec, File) :-
  absolute_file_name(File, File, [access(write)]),

  % Convert to uncompressed N-Triples.
  debug(semweb(rdf_to_hdt), "Creating uncompressed N-Triples…", []),
  create_temporary_file1(TriplesFileTmp),
  rdf_reserialize(triples, UriSpec, TriplesFileTmp, [compression(none)]),
  debug(semweb(rdf_to_hdt), "…uncompressed N-Triples created.", []),

  % Create HDT file.
  debug(semweb(rdf_to_hdt), "Creating HDT…", []),
  file_name_extension(File, working, HdtFile),
  hdt_create(TriplesFileTmp, [hdt_file(HdtFile)]),
  with_mutex(rdf_to_hdt,
    (   % Somebody else was earlier.
        exists_file(File)
    ->  delete_file(HdtFile)
    ;   rename_file(HdtFile, File)
    )
  ),
  delete_file(TriplesFileTmp),
  debug(semweb(rdf_to_hdt), "…HDT created.", []),

  % Create HDT index file.
  debug(semweb(rdf_to_hdt), "Creating HDT index…", []),
  hdt_call_on_file(File, [Hdt]>>once(hdt(_,_,_,Hdt))),
  debug(semweb(rdf_to_hdt), "…HDT index created.", []).
  
create_temporary_file1(File) :-
  uuid(Uuid),
  absolute_file_name(Uuid, File, [access(write),extensions([tmp])]).
