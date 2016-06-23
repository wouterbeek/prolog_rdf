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
%   * void(+boolean) Whether or not a VoID description is generates as
%   well.  The default is `false`.


rdf_conv(Name) :-
  rdf_conv(Name, _{}).


rdf_conv(Name, Opts) :-
  dict_get(alias, Opts, Name, Alias),
  dict_get(module, Opts, Name, Mod),
  rdf_conv_data(Alias, Mod, Name, Opts),
  (get_dict(void, Opts, true) -> rdf_conv_void(Alias, Mod, Name, Opts) ; true).


rdf_conv_data(Alias, Mod, Name, Opts) :-
  dict_get(format, Opts, ntriples, Format),
  rdf_conv_spec(Alias, Name, data, Format, Spec),
  rdf_conv_goal(Name, data, Pred_1),
  Goal_1 = Mod:Pred_1,
  rdf_conv_graph(Alias, Name, data, G),
  rdf_load_file_or_call(Spec, Goal_1, G, [rdf_format(Format)]).


rdf_conv_void(Alias, Mod, Name, Opts) :-
  dict_get(format, Opts, ntriples, Format),
  rdf_conv_spec(Alias, Name, data, Format, Spec0),
  rdf_conv_spec(Alias, Name, void, Format, Spec),
  rdf_conv_goal(Name, void, Pred_1),
  Goal_1 = Mod:Pred_1,
  rdf_conv_graph(Alias, Name, void, G),
  rdf_load_file_or_call(
    Spec,
    source_to_void(Spec0, Goal_1),
    G,
    [rdf_format(Format)]
  ).





% HELPERS %

%! rdf_conv_base(+Name, +Kind, -Base) is det.

rdf_conv_base(Name, Kind, Base) :-
  atomic_list_concat([Name,Kind], '_', Base).



%! rdf_conv_file(+Name, +Kind, +Format, -File) is det.

rdf_conv_file(Name, Kind, Format, File) :-
  rdf_conv_base(Name, Kind, Base),
  rdf_file_extension(Ext, Format),
  file_name_extension(Base, Ext, File).



%! rdf_conv_goal(+Name, +Kind, -Goal_1) is det.

rdf_conv_goal(Name, Kind, Goal_1) :-
  atomic_list_concat([Name,load,Kind], '_', Goal_1).



%! rdf_conv_graph(+Alias, +Name, +Kind, -G) is det.

rdf_conv_graph(Alias, Name, Kind, G) :-
  rdf_conv_base(Name, Kind, Base),
  rdf_global_id(Alias:Base, G).



%! rdf_conv_spec(+Alias, +Name, +Kind, +Format, -Spec) is det.

rdf_conv_spec(Alias, Name, Kind, Format, Spec) :-
  rdf_conv_file(Name, Kind, Format, File0),
  Spec =.. [Alias,File0].
