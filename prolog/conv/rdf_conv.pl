:- module(
  rdf_conv,
  [
    rdf_conv/1, % +Name
    rdf_conv/2  % +Name, +Opts
  ]
).

/** <module> RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dict_ext)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(vocab/void)).





%! rdf_conv(+Name) is det.
%! rdf_conv(+Name, +Opts) is det.
%
% The resultant data file is called `<NAME>_data.<EXT>`.  The
% resultant VoID file, if any, is called `<NAME>_void.<EXT>`.
%
% The calls that are made are `call(<NAME>_load_data, +G)` and
% `call(<NAME>_load_void, +G)`.
%
% The followning options are defined:
%
%   * alias(+atom) The alias of the RDF prefix that acts as the
%   default IRI prefix.  By default this is Name.
%
%   * format(+rdf_format) The RDF serialization format that is used to
%   store the data in.  The default is `ntriples`.
%
%   * module(+atom) The name of the module which defines the goals.
%   By default this is Name.
%
%   * search_path(+atom) The file search path where data files should
%   be read/written from/to.  The default is Alias.
%
%   * vocab(+boolean) Whether or not a vocabulary is generates as
%   well.  The default is `false`.
%
%   * void(+boolean) Whether or not a VoID description is generates as
%   well.  The default is `false`.


rdf_conv(Name) :-
  rdf_conv(Name, _{}).


rdf_conv(Name, Opts1) :-
  dict_get(alias, Opts1, Name, Alias),
  dict_get(format, Opts1, ntriples, Format),
  dict_get(module, Opts1, Name, Mod),
  dict_get(search_path, Opts1, Alias, Search),
  merge_dict(
    Opts1,
    _{alias: Alias, format: Format, module: Mod, search_path: Search},
    Opts2
  ),
  rdf_conv_data(Name, Opts2),
  (get_dict(vocab, Opts1, true) -> rdf_conv_vocab(Name, Opts2) ; true),
  (get_dict(void, Opts1, true) -> rdf_conv_void(Name, Opts2) ; true).


rdf_conv_data(Name, Opts) :-
  rdf_conv_spec(Name, data, Opts, Spec),
  rdf_conv_goal(Name, data, Pred_1),
  Goal_1 = Opts.module:Pred_1,
  rdf_conv_graph(Name, data, Opts, G),
  rdf_load_file_or_call(Spec, Goal_1, G, [rdf_format(Opts.format)]).


rdf_conv_vocab(Name, Opts) :-
  rdf_conv_spec(Name, vocab, Opts, Spec),
  rdf_conv_goal(Name, vocab, Pred_1),
  Goal_1 = Opts.module:Pred_1,
  rdf_conv_graph(Name, vocab, Opts, G),
  rdf_load_file_or_call(Spec, Goal_1, G, [rdf_format(Opts.format)]).


rdf_conv_void(Name, Opts) :-
  rdf_conv_spec(Name, data, Opts, Spec0),
  rdf_conv_spec(Name, void, Opts, Spec),
  rdf_conv_goal(Name, void, Pred_1),
  Goal_1 = Opts.module:Pred_1,
  rdf_conv_graph(Name, void, Opts, G),
  rdf_load_file_or_call(Spec, source_to_void(Spec0, Goal_1), G, [rdf_format(Opts.format)]).





% HELPERS %

%! rdf_conv_base(+Name, +Kind, -Base) is det.

rdf_conv_base(Name, Kind, Base) :-
  atomic_list_concat([Name,Kind], '_', Base).



%! rdf_conv_file(+Name, +Kind, +Opts, -File) is det.

rdf_conv_file(Name, Kind, Opts, File) :-
  rdf_conv_base(Name, Kind, Base),
  rdf_file_extension(Ext, Opts.format),
  file_name_extension(Base, Ext, File).



%! rdf_conv_goal(+Name, +Kind, -Goal_1) is det.

rdf_conv_goal(Name, Kind, Goal_1) :-
  atomic_list_concat([Name,load,Kind], '_', Goal_1).



%! rdf_conv_graph(+Name, +Kind, +Opts, -G) is det.

rdf_conv_graph(Name, Kind, Opts, G) :-
  rdf_conv_base(Name, Kind, Base),
  rdf_global_id(Opts.alias:Base, G).



%! rdf_conv_spec(+Name, +Kind, +Opts, -Spec) is det.

rdf_conv_spec(Name, Kind, Opts, Spec) :-
  rdf_conv_file(Name, Kind, Opts, File0),
  Spec =.. [Opts.search_path,File0].
