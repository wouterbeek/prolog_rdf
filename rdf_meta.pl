:- module(
  rdf_meta,
  [
    rdf_setup_call_cleanup/3, % +From:or([atom,list(atom)])
                              % :Goal
                              % +LoadOptions:list(nvpair)
    rdf_setup_call_cleanup/5 % +From:or([atom,list(atom)])
                             % :Goal
                             % ?ToFile:atom
                             % +LoadOptions:list(nvpair)
                             % +SaveOptions:list(nvpair)
  ]
).

/** <module> RDF meta

Meta-callings on an RDF graph.

@author Wouter Beek
@version 2014/01-2014/02, 2014/05, 2014/07
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(io/file_ext)).

:- use_module(plRdf(debug/rdf_deb)).
:- use_module(plRdf(graph/rdf_graph_name)).
:- use_module(plRdf(management/rdf_file)).
:- use_module(plRdf(management/rdf_file_db)).
:- use_module(plRdf(management/rdf_load_any)).

:- meta_predicate(rdf_setup_call_cleanup(+,1,+)).
:- meta_predicate(rdf_setup_call_cleanup(+,1,?,+,+)).

:- predicate_options(rdf_setup_call_cleanup/3, 3, [
  pass_to(rdf_load_any/2, 2)
]).
:- predicate_options(rdf_setup_call_cleanup/5, 5, [
  pass_to(output_file_based_on_input_file/3, 3),
  pass_to(rdf_load_any/2, 2),
  pass_to(rdf_save/2, 2)
]).
:- predicate_options(output_file_based_on_input_file/3, 3, [
  pass_to(ensure_format/3, 3)
]).
:- predicate_options(ensure_format/3, 3, [
  format(+atom)
]).





%! rdf_setup_call_cleanup(
%!   +FromFile:atom,
%!   :Goal,
%!   +LoadOptions:list(nvpair)
%! ) is det.
% Do not save graph to file.
%
% @arg Goal Take one argument, which is the atomic name of an RDF graph.

% Load RDF files, process goal, no output.
rdf_setup_call_cleanup(From, Goal, LoadOptions1):-
  setup_call_cleanup(
    (
      rdf_new_graph(temp, Graph),
      merge_options([graph(Graph)], LoadOptions1, LoadOptions2),
      rdf_load_any(From, LoadOptions2)
    ),
    call(Goal, Graph),
    rdf_unload_graph_deb(Graph)
  ).


%! rdf_setup_call_cleanup(
%!   +FromFile:atom,
%!   :Goal,
%!   ?ToFile:atom,
%!   +LoadOptions:list(nvpair),
%!   +SaveOptions:list(nvpair)
%! ) is det.
% Save graph to file.
%
% @arg Goal Take one argument, which is the atomic name of an RDF graph.

rdf_setup_call_cleanup(FromFile, Goal, ToFile, LoadOptions1, SaveOptions1):-
  output_file_based_on_input_file(FromFile, ToFile, SaveOptions1),
  setup_call_cleanup(
    (
      rdf_new_graph(temp, TempGraph),
      merge_options([graph(TempGraph)], LoadOptions1, LoadOptions2),
      rdf_load_any(FromFile, LoadOptions2)
    ),
    call(Goal, TempGraph),
    (
      merge_options([graph(TempGraph)], SaveOptions1, SaveOptions2),
      rdf_save(ToFile, SaveOptions2),
      rdf_unload_graph(TempGraph)
    )
  ).



% Helpers

%! ensure_format(+File:atom, -Format:atom, +Options:list(nvpair)) is det.
% Try the best we can to extract some serialization format for exporting RDF.
%
% The format is established in the following way (in order of precedence):
%    1. The value of the `format` option.
%    2. The serialization format associated with the extension of `File`.
%    3. N-Triples.

ensure_format(_, Format, Options):-
  option(format(Format), Options), !.
ensure_format(File, Format, _):-
  nonvar(File),
  file_name_extension(_, Extension, File),
  rdf_file_extension_format(Extension, Format).
ensure_format(_, ntriples, _).


%! output_file_based_on_input_file(
%!   +FromFile:atom,
%!   ?ToFile:atom,
%!   +SaveOptions:list(nvpair)
%! ) is det.
% Returns the name of the output file, based on the given input file
% and sae options.

% If the output file is already given, then we are done.
output_file_based_on_input_file(_, ToFile, _):-
  nonvar(ToFile),
  is_absolute_file_name(ToFile), !.
% If the output file is not given, then it is based on the input file.
output_file_based_on_input_file(FromFile, ToFile, SaveOptions):-
  ensure_format(ToFile, ToFormat, SaveOptions),
  rdf_file_extension_format(ToExtension, ToFormat),
  % If the from file is a directory, then call the to file `output`.
  (
    exists_directory(FromFile)
  ->
    file_components(ToFile, FromFile, output, ToExtension)
  ;
    file_alternative(FromFile, _, _, ToExtension, ToFile)
  ).

