:- module(
  q_conv,
  [
    conv_alias_options/2, % +Opts1, -Opts2
    q_conv/1,             % +Alias
    q_conv/2              % +Alias, +Opts
  ]
).

/** <module> RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(dict_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_term)).
:- use_module(library(vocab/void)).

:- rdf_meta
   q_conv(r),
   q_conv(r, +).





%! q_conv(+Alias) is det.
%! q_conv(+Alias, +Opts) is det.
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
%   * mode(+oneof([hdt,rdf])) Whether the result of data conversion is
%   stored in memory (`rdf`) or on disk (`hdt`).  The default is
%   `hdt`.
%
%   * module(+atom) The name of the module which defines the goals.
%   The default is Alias.
%
%   * vocab(+boolean) Whether or not a vocabulary is generates as
%   well.  The default is `false`.
%
%   * void(+boolean) Whether or not a VoID description is generates as
%   well.  The default is `false`.

q_conv(Alias) :-
  q_conv(Alias, _{}).


q_conv(Alias, Opts1) :-
  merge_dicts(_{mode: hdt, module: Alias}, Opts1, Opts2),
  q_conv_data(Alias, Opts2),
  (get_dict(vocab, Opts1, true) -> q_conv_vocab(Alias, Opts2) ; true),
  (get_dict(void, Opts1, true) -> q_conv_void(Alias, Opts2) ; true).


q_conv_data(Alias, Opts) :-
  rdf_global_id(Alias:data, G),
  atomic_list_concat([Alias,load,data], '_', Pred_1),
  Goal_1 = Opts.module:Pred_1,
  q_load_or_call(Opts.mode, Goal_1, G).


q_conv_vocab(Alias, Opts) :-
  rdf_global_id(Alias:vocab, G),
  atomic_list_concat([Alias,load,vocab], '_', Pred_1),
  Goal_1 = Opts.module:Pred_1,
  q_load_or_call(Opts.mode, Goal_1, G).


q_conv_void(Alias, Opts) :-
  rdf_global_id(Alias:data, DataG),
  q_graph_to_file(DataG, [nt,gz], DataFile),
  rdf_global_id(Alias:void, VoidG),
  atomic_list_concat([Alias,load,void], '_', Pred_1),
  Goal_1 = Opts.module:Pred_1,
  q_load_or_call(Opts.mode, source_to_void0(DataFile, Goal_1), VoidG).


source_to_void0(DataFile, Goal_1, VoidG) :-
  source_to_void(DataFile, Goal_1, VoidG),
  q_graph_to_file(VoidG, [nt,gz], VoidFile),
  rdf_write_to_sink(VoidFile, VoidG, [compression(gzip),rdf_format(ntriples)]),
  q_unload_graph(VoidG).



%! conv_alias_options(+Opts1, -Opts2) is det.

conv_alias_options(Opts1, Opts3) :-
  del_dict(alias, Opts1, Alias, Opts2), !,
  Opts3 = Opts2.put(_{abox_alias: Alias, tbox_alias: Alias}).
conv_alias_options(Opts1, Opts3) :-
  dict_put_def(abox_alias, Opts1, ex, Opts2),
  dict_put_def(tbox_alias, Opts2, ex, Opts3).
