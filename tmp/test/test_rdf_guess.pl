:- module(test_rdf_guess, []).

:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_guess)).

:- begin_tests(rdf_guess).

test('n-quads', [forall(doc(MT,Doc))]) :-
  open_string(Doc, In),
  rdf_guess_media_type(In, MTs),
  MTs = [MT].

:- end_tests(rdf_guess).

doc(media(application/'n-quads',[]), "<a> <b> <c> <d> .").
doc(media(application/'n-quads',[]), "<a> <b> <c> <d>.").
doc(media(application/'n-quads',[]), "<a> <b> <c> .").
doc(media(application/'n-quads',[]), "<a> <b> <c>.").
doc(media(application/trig,[]), "<a> <b> <c> . <d> { }").
doc(media(application/trig,[]), "<a> a <c>.").
doc(media(application/trig,[]), "ex:a <b> <c> .").
doc(media(application/trig,[]), "<a> <b> <c> ; <d> <e> .").
