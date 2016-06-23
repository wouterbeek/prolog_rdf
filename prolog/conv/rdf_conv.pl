:- module(
  rdf_conv,
  [
    rdf_conv/3, % +Alias, +Mod, +Name
    rdf_conv/4  % +Alias, +Mod, +Name, +Format
  ]
).

/** <module> RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(vocab/void)).





%! rdf_conv(+Alias, +Mod, +Name) is det.
%! rdf_conv(+Alias, +Mod, +Name, +Format) is det.

rdf_conv(Alias, Mod, Name) :-
  rdf_conv(Alias, Mod, Name, ntriples).


rdf_conv(Alias, Mod, Name, Format) :-
  rdf_conv_data(Alias, Mod, Name, Format),
  rdf_conv_void(Alias, Mod, Name, Format).



rdf_conv_data(Alias, Mod, Name, Format) :-
  rdf_conv_spec(Alias, Name, data, Format, Spec),
  rdf_conv_goal(Name, data, Pred_1),
  rdf_conv_graph(Alias, Name, data, G),
  Goal_1 = Mod:Pred_1,
  rdf_load_file_or_call(Spec, Goal_1, G, [rdf_format(Format)]).



rdf_conv_void(Alias, Mod, Name, Format) :-
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
