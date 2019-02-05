:- module(
  rdf_reply,
  [
    rdf_reply/2 % +MediaTypes:list(media_type), +Triples:list(rdf_triple)
  ]
).

/** <module> RDF reply

@author Wouter Beek
@version 2019
*/

:- use_module(library(apply)).
:- use_module(library(http/http_json)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- use_module(library(file_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).

:- setting(tmp_directory, atom, ., "").





%! rdf_reply(+MediaTypes:list(media_type), +Triples:list(rdf_triple)) is det.

rdf_reply(MediaTypes, Triples) :-
  member(MediaType, MediaTypes),
  current_output(Out),
  rdf_reply(Out, MediaType, Triples), !.

% application/n-quads
rdf_reply(Out, media(application/'n-quads',_), Triples) :-
  format(Out, "Content-Type: application/n-quads\n\n", []),
  maplist({G}/[Triple]>>rdf_write_quad(Out, Triple, G), Triples).
% application/n-triples
rdf_reply(Out, media(application/'n-triples',_), Triples) :-
  format(Out, "Content-Type: application/n-triples\n\n", []),
  maplist(rdf_write_triple(Out), Triples).
% application/trig
rdf_reply(Out, media(application/trig,_), Triples) :-
  format(Out, "Content-Type: application/trig\n\n", []),
  maplist(rdf_write_triple(Out), Triples).
% application/rdf+xml
rdf_reply(Out, media(application/'rdf+xml',_), Triples) :-
  setting(tmp_directory, Dir),
  uuid(Local),
  directory_file_path(Dir, Local, File),
  format(Out, "Content-Type: application/rdf+xml; charset=utf-8\n\n", []),
  rdf_transaction(
    call_cleanup(
      (
        maplist(assert_triple(mem(Local)), Triples),
        rdf_save_file(File, [graph(Local),media_type(media(application/'rdf+xml'))]),
        read_from_file(File, {Out}/[In]>>copy_stream_data(In, Out))
      ),
      delete_file(File)
    )
  ).
% text/turtle
rdf_reply(Out, media(text/turtle,_), Triples) :-
  setting(tmp_directory, Dir),
  uuid(Local),
  directory_file_path(Dir, Local, File),
  format(Out, "Content-Type: text/turtle\n\n", []),
  rdf_transaction(
    call_cleanup(
      (
        maplist(assert_triple(mem(Local)), Triples),
        rdf_save_file(File, [graph(Local),media_type(media(text/turtle,[]))]),
        read_from_file(File, {Out}/[In]>>copy_stream_data(In, Out))
      ),
      delete_file(File)
    )
  ).
% 406 Unacceptable
rdf_reply(Out, _, _) :-
  with_output_to(
    Out,
    reply_json_dict(
      _{
        error: "Unacceptable",
        reason: "The requested Accept header does not contain a supported RDF serialization format."
      },
      [status(406)]
    )
  ).
