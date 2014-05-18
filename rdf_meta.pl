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
@version 2014/01-2014/02
*/

:- use_module(os(file_ext)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf_file(rdf_file)).
:- use_module(rdf_file(rdf_serial)).

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
    rdf_unload_graph_debug(Graph)
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
    rdf_serial:ensure_format(O1_Save, ToFile, ToFormat),
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
      rdf_unload_graph_debug(Graph)
    )
  ).

