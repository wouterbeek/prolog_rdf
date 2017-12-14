:- module(
  rdf_update,
  [
    rdf_call_update/2,        % :Find_0, Transform_0
    rdf_call_update/3,        % +Message, :Find_0, Transform_0
    rdf_update2/4,            % ?S, ?P, ?O, +Action
    rdf_update2/5,            % ?S, ?P, ?O, ?G, +Action
    rdf_update_language_tag/3 % +P, +LTag, +G
  ]
).

/** <module> RDF update

@author Wouter Beek
@version 2017/08-2017/10
*/

:- use_module(library(dict_ext)).
:- use_module(library(semweb/rdf_api)).

:- meta_predicate
    rdf_call_update(0, 0),
    rdf_call_update(+, 0, 0),
    rdf_call_update_(+, 0, 0, +).

:- rdf_meta
   rdf_call_update(t, t),
   rdf_call_update(+, t, t),
   rdf_update2(r, r, o, +),
   rdf_update2(r, r, o, r, +),
   rdf_update_language_tag(r, +, r).





%! rdf_call_update(:Find_0, Transform_0) is det.
%! rdf_call_update(+Message:string, :Find_0, Transform_0) is det.
%
% Generic data transformation call:
%
%   - Find_0 matches a single candidate for transformation.
%
%   - Transform_0 acts on a single matched candidate to effectuate the
%     transformation.
%
% If Transform_0 fails the debugger is opened.

rdf_call_update(Find_0, Transform_0) :-
  rdf_call_update(_, Find_0, Transform_0).


rdf_call_update(Message, Find_0, Transform_0) :-
  rdf_transaction(
    rdf_call_update_(Message, Find_0, Transform_0, _{count: 0})
  ).

rdf_call_update_(Message, Find_0, Transform_0, State) :-
  Find_0, % NONDET
  (   Transform_0
  ->  true
  ;   (ground(Message) -> true ; format(string(Message), "~w", [Transform_0])),
      print_message(warning, rdf_update_fail(State.count,Message))
  ),
  dict_inc(count, State),
  fail.
rdf_call_update_(Message, Find_0, _, State) :-
  (ground(Message) -> true ; format(string(Message), "~w", [Find_0])),
  print_message(informational, rdf_update_succeed(State.count,Message)).



%! rdf_update2(?S, ?P, ?O, +Action) is det.
%! rdf_update2(?S, ?P, ?O, ?G, +Action) is det.

rdf_update2(S, P, O, Action) :-
  rdf_update2(S, P, O, _, Action).


rdf_update2(S, P, O, G, Action) :-
  forall(
    rdf(S, P, O, G),
    rdf11:rdf_update(S, P, O, G, Action)
  ).



%! rdf_update_language_tag(+P:atom, +LTag:atom, +G:atom) is det.

rdf_update_language_tag(P, LTag, G) :-
  rdf_call_update(
    rdf(S, P, literal(type(xsd:string,Lex)), G),
    rdf_update2(S, P, literal(type(xsd:string,Lex)), G, object(literal(lang(LTag,Lex))))
  ).
