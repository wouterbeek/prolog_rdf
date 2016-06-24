:- module(
  rdf_conv,
  [
    rdf_conv/1,              % +Alias
    rdf_conv/2,              % +Alias, +Opts
    rdf_conv_alias_options/2 % +Opts1, -Opts2
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

:- meta_predicate
    rdf_conv_call(1, +, +, +).





%! rdf_conv(+Alias) is det.
%! rdf_conv(+Alias, +Opts) is det.
%
% The resultant data file is called `<NAME>_data.<EXT>`.  The
% resultant VoID file, if any, is called `<NAME>_void.<EXT>`.
%
% The calls that are made are `call(<NAME>_load_data, +Sink, +G)`,
% `call(<NAME>_load_vocab, +Sink, +G)` and `call(<NAME>_load_void,
% +Sink, +G)`.
%
% The followning options are defined:
%
%   * format(+rdf_format) The RDF serialization format that is used to
%   store the data in.  The default is `ntriples`.
%
%   * mode(+oneof([disk,mem])) Whether the result of data conversion
%   is stored in memory (RDF graph) or on disk (HDT archive).  The
%   default is `mem`.
%
%   * module(+atom) The name of the module which defines the goals.
%   The default is Alias.
%
%   * search_path(+atom) The file search path where data files should
%   be read/written from/to.  The default is Alias.
%
%   * vocab(+boolean) Whether or not a vocabulary is generates as
%   well.  The default is `false`.
%
%   * void(+boolean) Whether or not a VoID description is generates as
%   well.  The default is `false`.

rdf_conv(Alias) :-
  rdf_conv(Alias, _{}).


rdf_conv(Alias, Opts1) :-
  dict_get(format, Opts1, ntriples, Format),
  dict_get(module, Opts1, Alias, Mod),
  dict_get(search_path, Opts1, Alias, Search),
  merge_dicts(
    Opts1,
    _{alias: Alias, format: Format, module: Mod, search_path: Search},
    Opts2
  ),
  rdf_conv_data(Alias, Opts2),
  (get_dict(vocab, Opts1, true) -> rdf_conv_vocab(Alias, Opts2) ; true),
  (get_dict(void, Opts1, true) -> rdf_conv_void(Alias, Opts2) ; true).


rdf_conv_data(Alias, Opts) :-
  rdf_conv_spec(Alias, data, Opts, Spec),
  rdf_conv_goal(Alias, data, Pred_1),
  Goal_1 = Opts.module:Pred_1,
  rdf_conv_graph(Alias, data, G),
  rdf_conv_call(Goal_1, Spec, G, Opts).


rdf_conv_vocab(Alias, Opts) :-
  rdf_conv_spec(Alias, vocab, Opts, Spec),
  rdf_conv_goal(Alias, vocab, Pred_1),
  Goal_1 = Opts.module:Pred_1,
  rdf_conv_graph(Alias, vocab, G),
  rdf_conv_call(Goal_1, Spec, G, Opts).


rdf_conv_void(Alias, Opts) :-
  rdf_conv_spec(Alias, data, Opts, Spec0),
  rdf_conv_spec(Alias, void, Opts, Spec),
  rdf_conv_goal(Alias, void, Pred_1),
  Goal_1 = Opts.module:Pred_1,
  rdf_conv_graph(Alias, void, G),
  rdf_conv_call(source_to_void(Spec0, Goal_1), Spec, G, Opts).



%! rdf_conv_alias_options(+Opts1, -Opts2) is det.

rdf_conv_alias_options(Opts1, Opts3) :-
  del_dict(alias, Opts1, Alias, Opts2), !,
  Opts3 = Opts2.put(_{abox_alias: Alias, tbox_alias: Alias}).
rdf_conv_alias_options(Opts1, Opts3) :-
  dict_put_def(abox_alias, Opts1, ex, Opts2),
  dict_put_def(tbox_alias, Opts2, ex, Opts3).





% HELPERS %

%! rdf_conv_base(+Alias, +Kind, -Base) is det.

rdf_conv_base(Alias, Kind, Base) :-
  atomic_list_concat([Alias,Kind], '_', Base).



%! rdf_conv_call(:Goal_1, +Spec, +G, +Opts) is det.

rdf_conv_call(Goal_1, Spec, _, Opts) :-
  get_dict(mode, Opts, hdt), !,
  rdf_load_file_or_write_to_disk(Spec, Goal_1, [rdf_format(Opts.format)]).
rdf_conv_call(Goal_1, Spec, G, Opts) :-
  rdf_load_file_or_write_to_mem(Spec, Goal_1, G, [rdf_format(Opts.format)]).



%! rdf_conv_file(+Alias, +Kind, +Opts, -File) is det.

rdf_conv_file(Alias, Kind, Opts, File) :-
  rdf_conv_base(Alias, Kind, Base),
  rdf_file_extension(Ext, Opts.format),
  file_name_extension(Base, Ext, File).



%! rdf_conv_goal(+Alias, +Kind, -Goal_1) is det.

rdf_conv_goal(Alias, Kind, Goal_1) :-
  atomic_list_concat([Alias,load,Kind], '_', Goal_1).



%! rdf_conv_graph(+Alias, +Kind, -G) is det.

rdf_conv_graph(Alias, Kind, G) :-
  rdf_global_id(Alias:Kind, G).



%! rdf_conv_spec(+Alias, +Kind, +Opts, -Spec) is det.

rdf_conv_spec(Alias, Kind, Opts, Spec) :-
  rdf_conv_file(Alias, Kind, Opts, File0),
  Spec =.. [Opts.search_path,File0].
