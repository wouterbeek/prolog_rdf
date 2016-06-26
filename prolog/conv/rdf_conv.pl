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
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(vocab/void)).
:- use_module(library(z/z_ext)).

:- rdf_meta
   rdf_conv(r),
   rdf_conv(r, +).





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
%   * mode(+oneof([disk,memory])) Whether the result of data
%   conversion is stored in memory (RDF graph) or on disk (HDT
%   archive).  The default is `memory`.
%
%   * module(+atom) The name of the module which defines the goals.
%   The default is Alias.
%
%   * vocab(+boolean) Whether or not a vocabulary is generates as
%   well.  The default is `false`.
%
%   * void(+boolean) Whether or not a VoID description is generates as
%   well.  The default is `false`.

rdf_conv(Alias) :-
  rdf_conv(Alias, _{}).


rdf_conv(Alias, Opts1) :-
  merge_dicts(_{mode: disk, module: Alias}, Opts1, Opts2),
  rdf_conv_data(Alias, Opts2),
  (get_dict(vocab, Opts1, true) -> rdf_conv_vocab(Alias, Opts2) ; true),
  (get_dict(void, Opts1, true) -> rdf_conv_void(Alias, Opts2) ; true).


rdf_conv_data(Alias, Opts) :-
  rdf_global_id(Alias:data, G),
  atomic_list_concat([Alias,load,data], '_', Pred_1),
  Goal_1 = Opts.module:Pred_1,
  z_load_or_call(Opts.mode, Goal_1, G).


rdf_conv_vocab(Alias, Opts) :-
  rdf_global_id(Alias:vocab, G),
  atomic_list_concat([Alias,load,vocab], '_', Pred_1),
  Goal_1 = Opts.module:Pred_1,
  z_load_or_call(Opts.mode, Goal_1, G).


rdf_conv_void(Alias, Opts) :-
  rdf_global_id(Alias:data, DataG),
  z_graph_to_file(DataG, [nt,gz], DataFile),
  rdf_global_id(Alias:void, VoidG),
  atomic_list_concat([Alias,load,void], '_', Pred_1),
  Goal_1 = Opts.module:Pred_1,
  z_load_or_call(Opts.mode, source_to_void0(DataFile, Goal_1), VoidG).


source_to_void0(DataFile, Goal_1, VoidG) :-
  source_to_void(DataFile, Goal_1, VoidG),
  z_graph_to_file(VoidG, [nt,gz], VoidFile),
  rdf_write_to_sink(VoidFile, VoidG, [compression(gzip),rdf_format(ntriples)]).



%! rdf_conv_alias_options(+Opts1, -Opts2) is det.

rdf_conv_alias_options(Opts1, Opts3) :-
  del_dict(alias, Opts1, Alias, Opts2), !,
  Opts3 = Opts2.put(_{abox_alias: Alias, tbox_alias: Alias}).
rdf_conv_alias_options(Opts1, Opts3) :-
  dict_put_def(abox_alias, Opts1, ex, Opts2),
  dict_put_def(tbox_alias, Opts2, ex, Opts3).
