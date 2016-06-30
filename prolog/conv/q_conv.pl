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

:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(vocab/void)).

:- rdf_meta
   q_conv(r),
   q_conv(r, +).

:- debug(q(conv)).





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
  merge_dicts(_{module: Alias}, Opts1, Opts2),
  q_conv(Alias, data, Opts2),
  (get_dict(vocab, Opts1, true) -> q_conv(Alias, vocab, Opts2) ; true),
  (get_dict(void, Opts1, true) -> q_conv_void(Alias, Opts2) ; true).


q_conv(Alias, Name, Opts) :-
  rdf_global_id(Alias:Name, G),
  (   q_exists(G)
  ->  debug(q(conv), "Graph ~a already exists in store.", [G])
  ;   atomic_list_concat([Alias,load,Name], '_', Pred),
      Goal_1 = Opts.module:Pred,
      call(Goal_1, G),
      q_save(G),
      q_unload(G),
      debug(q(conv), "Graph ~a converted to store.", [G])
  ).


q_conv_void(Alias, Opts) :-
  rdf_global_id(Alias:void, VoidG),
  (   q_exists(VoidG)
  ->  debug(q(conv), "Graph ~a already exists in store.", [VoidG])
  ;   rdf_global_id(Alias:data, DataG),
      atomic_list_concat([Alias,load,void], '_', Pred),
      Goal_3 = Opts.module:Pred,
      q_load(hdt, DataG),
      source_to_void(DataG, Goal_3, VoidG),
      q_unload(DataG),
      debug(q(conv), "Graph ~a converted to store.", [VoidG])
  ).



%! conv_alias_options(+Opts1, -Opts2) is det.

conv_alias_options(Opts1, Opts3) :-
  del_dict(alias, Opts1, Alias, Opts2), !,
  Opts3 = Opts2.put(_{abox_alias: Alias, tbox_alias: Alias}).
conv_alias_options(Opts1, Opts3) :-
  dict_put_def(abox_alias, Opts1, ex, Opts2),
  dict_put_def(tbox_alias, Opts2, ex, Opts3).
