:- module(
  rdf_stream,
  [
    rdf_read_from_stream/2, % +Source, :Goal_2
    rdf_read_from_stream/3, % +Source
                            % :Goal_2
                            % +Options:list(compound)
    rdf_write_to_stream/2, % +Source, :Goal_2
    rdf_write_to_stream/3 % +Source
                          % :Goal_2
                          % +Options:list(compound)    
  ]
).

/** <module> RDF stream

@author Wouter Beek
@version 2015/08-2015/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/call_on_stream)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_read_from_stream(+,2)).
:- meta_predicate(rdf_read_from_stream(+,2,+)).
:- meta_predicate(rdf_write_to_stream(+,2)).
:- meta_predicate(rdf_write_to_stream(+,2,+)).

:- predicate_options(rdf_read_from_stream/3, 3, [
     pass_to(rdf_read_from_stream0/4, 2),
     pass_to(rdf_http_plugin:rdf_extra_headers/2, 2),
     pass_to(read_from_stream/4, 4)
   ]).
:- predicate_options(rdf_read_from_stream0/4, 2, [
     format(+oneof([nquads,ntriples,trig,triples,turtle,xml]))
   ]).
:- predicate_options(rdf_write_to_stream/3, 3, [
     pass_to(write_to_stream/3, 3)
   ]).





%! rdf_read_from_stream(+Source, :Goal_2) is det.
% Wrapper around rdf_read_from_stream/3 with default options.

rdf_read_from_stream(Source, Goal_2):-
  rdf_read_from_stream(Source, Goal_2, []).


%! rdf_read_from_stream(+Source, :Goal_2, +Options:list(compound)) is det.
% Goal_2 is applied to a metadata dictionary and a stream (in that order).

rdf_read_from_stream(Source, Goal_2, Opts1):-
  % Accept headers for RDF are specified in `library(semweb/rdf_http_plugin))'.
  rdf_http_plugin:rdf_extra_headers(DefaultRdfOpts, Opts1),
  merge_options(DefaultRdfOpts, Opts1, Opts2),
  
  % Remove option format/1 as it will confuse archive_open/3.
  % Archive format ↔ RDF serialization format
  (select_option(format(_), Opts2, Opts3) -> true ; Opts3 = Opts2),
  
  read_from_stream(Source, rdf_read_from_stream0(Goal_2, Opts2), Opts3).


rdf_read_from_stream0(Goal_2, Opts, M1, Read):-
  % Guess the RDF serialization format in case option `format(+)'
  % is not given.
  (   option(format(Format), Opts),
      ground(Format)
  ->  true
  ;   rdf_guess_format(Read, Format)
  ),
  % `Format' is now instantiated.
  put_dict(rdf, M1, metadata{format: Format}, M2),
  call(Goal_2, M2, Read).



%! rdf_write_to_stream(+Out, :Goal_2, +Options:list(compound)) is det.
% Wrapper around rdf_write_to_stream/3 with default options.

rdf_write_to_stream(Out, Goal_2):-
  rdf_write_to_stream(Out, Goal_2, []).


%! rdf_write_to_stream(+Out, :Goal_2, +Options:list(compound)) is det.

rdf_write_to_stream(Out, Goal_2, Opts):-
  write_to_stream(Out, Goal_2, Opts).
