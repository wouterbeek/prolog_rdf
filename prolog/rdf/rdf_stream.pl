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
@version 2015/08-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/call_on_stream)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_read_from_stream(+,2)).
:- meta_predicate(rdf_read_from_stream(+,2,+)).
:- meta_predicate(rdf_write_to_stream(+,2)).
:- meta_predicate(rdf_write_to_stream(+,2,+)).
:- meta_predicate(rdf_read_from_stream0(2,+,+,+)).

:- predicate_options(rdf_read_from_stream/3, 3, [
     pass_to(rdf_read_from_stream0/4, 2),
     pass_to(rdf_http_plugin:rdf_extra_headers/2, 2),
     pass_to(read_from_stream/4, 4)
   ]).
:- predicate_options(rdf_read_from_stream0/4, 2, [
     format(+oneof([nquads,ntriples,trig,triples,turtle,xml])),
     pass_to(rdf_guess_format/3, 3)
   ]).
:- predicate_options(rdf_write_to_stream/3, 3, [
     pass_to(write_to_stream/3, 3)
   ]).





%! rdf_read_from_stream(+Source, :Goal_2) is det.
% Wrapper around rdf_read_from_stream/3 with default options.

rdf_read_from_stream(Source, Goal_2) :-
  rdf_read_from_stream(Source, Goal_2, []).


%! rdf_read_from_stream(+Source, :Goal_2, +Options:list(compound)) is det.
% Goal_2 is applied to a metadata dictionary and a stream (in that order).

rdf_read_from_stream(Source, Goal_2, Opts1) :-
  % Accept headers for RDF are specified in `library(semweb/rdf_http_plugin))'.
  rdf_http_plugin:rdf_extra_headers(DefaultRdfOpts, Opts1),
  merge_options(DefaultRdfOpts, Opts1, Opts2),
  
  % Remove option format/1 as it will confuse archive_open/3.
  % Archive format ↔ RDF serialization format
  (select_option(format(_), Opts2, Opts3) -> true ; Opts3 = Opts2),
  
  read_from_stream(Source, rdf_read_from_stream0(Goal_2, Opts2), Opts3).


rdf_read_from_stream0(Goal_2, Opts1, M1, Read) :-
  % Guess the RDF serialization format in case option `format(+)'
  % is not given.
  (   option(format(Format), Opts1),
      ground(Format)
  ->  true
  ;   rdf_guess_format_options(M1, Opts1, Opts2),
      rdf_guess_format(Read, Format, Opts2)
  ),
  % `Format' is now instantiated.
  put_dict(rdf, M1, metadata{format: Format}, M2),
  call(Goal_2, M2, Read).



%! rdf_write_to_stream(+Out, :Goal_2, +Options:list(compound)) is det.
% Wrapper around rdf_write_to_stream/3 with default options.

rdf_write_to_stream(Out, Goal_2) :-
  rdf_write_to_stream(Out, Goal_2, []).


%! rdf_write_to_stream(+Out, :Goal_2, +Options:list(compound)) is det.

rdf_write_to_stream(Out, Goal_2, Opts) :-
  write_to_stream(Out, Goal_2, Opts).





% HELPERS %

%! rdf_guess_format_options(
%!   +Metadata:dict,
%!   +Options1:list(compound),
%!   -Options2:list(compound)
%! ) is det.

rdf_guess_format_options(M, Opts1, Opts2) :-
  uri_file_extensions(M.base_iri, Exts1),
  reverse(Exts1, Exts2),
  member(Ext, Exts2),
  rdf_file_extension(Ext, DefFormat), !,
  merge_options(Opts1, [default_format(DefFormat)], Opts2).
rdf_guess_format_options(_, Opts, Opts).
