:- module(
  rdf_meta,
  [
    rdf_setup_call_cleanup/3, % +LoadOptions:list(nvpair)
                              % +From:or([atom,list(atom)])
                              % :Goal
    rdf_setup_call_cleanup/5 % +LoadOptions:list(nvpair)
                             % +From:or([atom,list(atom)])
                             % :Goal
                             % +SaveOptions:list(nvpair)
                             % ?ToFile:atom
  ]
).

/** <module> RDF meta

Meta-callings on an RDF graph.

@author Wouter Beek
@version 2014/01-2014/02, 2014/05
*/

:- use_module(os(file_ext)).

:- use_module(plRdf(rdf_deb)).
:- use_module(plRdf(rdf_graph_name)).
:- use_module(plRdf_ser(rdf_file)).
:- use_module(plRdf_ser(rdf_file_db)).
:- use_module(plRdf_ser(rdf_serial)).

:- meta_predicate(rdf_setup_call_cleanup(+,+,1)).
:- meta_predicate(rdf_setup_call_cleanup(+,+,1,+,?)).



%! rdf_setup_call_cleanup(
%!   +LoadOptions:list(nvpair),
%!   +FromFile:atom,
%!   :Goal
%! ) is det.
% Do not save graph to file.
%
% @arg Goal Take one argument, which is the atomic name of an RDF graph.

% Load RDF files, process goal, no output.
rdf_setup_call_cleanup(O1_Load, From, Goal):-
  setup_call_cleanup(
    (
      rdf_new_graph(temp, Graph),
      rdf_load_any([graph(Graph)|O1_Load], From)
    ),
    call(Goal, Graph),
    rdf_unload_graph_deb(Graph)
  ).


%! rdf_setup_call_cleanup(
%!   +LoadOptions:list(nvpair),
%!   +FromFile:atom,
%!   :Goal,
%!   +SaveOptions:list(nvpair),
%!   ?ToFile:atom
%! ) is det.
% Save graph to file.
%
% @arg Goal Take one argument, which is the atomic name of an RDF graph.

rdf_setup_call_cleanup(O1_Load, From, Goal, O1_Save, ToFile):-
  % If the output file is not given,
  % then it is based on the input file.
  (
    nonvar(ToFile), is_absolute_file_name(ToFile), !
  ;
    ensure_format(O1_Save, ToFile, ToFormat),
    once(rdf_serialization(ToExt, _, ToFormat, _, _)),
    (
      exists_directory(From)
    ->
      file_name(ToFile, From, output, ttl)
    ;
      From = [FromFile|_]
    ->
      file_alternative(FromFile, _, _, ToExt, ToFile)
    ;
      FromFile = From,
      file_alternative(FromFile, _, _, ToExt, ToFile)
    )
  ),

  setup_call_cleanup(
    (
      rdf_new_graph(temp, Graph),
      rdf_load_any([graph(Graph)|O1_Load], From)
    ),
    call(Goal, Graph),
    (
      rdf_save(O1_Save, Graph, ToFile),
      rdf_unload_graph_deb(Graph)
    )
  ).


%! ensure_format(+Options:list(nvpair), +File:atom, -Format:atom) is det.
% Try the best we can to extract some serialization format for exporting RDF.
%
% The format is established in the following way (in order of precedence):
%    1. The value of the `format` option.
%    2. The serialization format associated with the extension of `File`.
%    3. N-Triples.

ensure_format(Options, _, Format):-
  option(format(Format), Options), !.
ensure_format(_, File, Format):-
  nonvar(File),
  file_name_extension(_, Extension, File),
  rdf_serialization(Extension, _, Format, _, _).
ensure_format(_, _, ntriples).

