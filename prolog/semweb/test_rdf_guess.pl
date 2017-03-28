:- module(test_rdf_guess, []).

:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_guess)).

:- begin_tests(rdf_guess).

test('n-quads', [forall(doc('n-quads',Doc))]) :-
  open_string(Doc, In),
  rdf_guess_media_type(In, MT),
  correct_media_type(MT, 'n-quads').

test('n-triples', [forall(doc('n-triples',Doc))]) :-
  open_string(Doc, In),
  rdf_guess_media_type(In, MT),
  correct_media_type(MT, 'n-triples').

:- end_tests(rdf_guess).

correct_media_type(media(_/Subtype1,_), Subtype2) :-
  correct_subtype(Subtype1, Subtype2).

correct_subtype(Subtype, Subtype) :- !.
correct_subtype(Subtype1, Subtype3) :-
  rdf_guess:generalization(Subtype1, Subtype2),
  correct_subtype(Subtype2, Subtype3).

doc('n-quads', "<a> <b> <c> <d> .").
doc('n-quads', "<a> <b> <c> <d>.").
doc('n-triples', "<a> <b> <c> .").
doc('n-triples', "<a> <b> <c>.").
