%! rdf_choose_media_type(+Primaries, +Secondaries, -MediaType) is det.
%
% Picks one MediaType from the list of Primaries, where the
% Secondaries are used in case there are multiple options.
%
% Chooses TriG if no Media Type can be determined.

rdf_choose_media_type([MediaType], _, MediaType) :- !.
rdf_choose_media_type(MediaTypes1, MediaTypes2, MediaType1) :-
  member(MediaType1, MediaTypes1),
  member(MediaType2, MediaTypes2),
  generalization(MediaType1, MediaType2), !.
rdf_choose_media_type(_, _, media(application/trig,[])).

generalization(MT, MT) :- !.
generalization(MT1, MT3) :-
  direct_generalization(MT1, MT2),
  generalization(MT2, MT3).

direct_generalization(
  media(application/trig,Params),
  media(text/turtle,Params)
).
direct_generalization(
  media(application/'n-quads',Params),
  media(application/'n-triples',Params)
).
direct_generalization(
  media(text/turtle,Params),
  media(application/'n-triples',Params)
).
