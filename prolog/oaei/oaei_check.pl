:- module(
  oaei_check,
  [
    oaei_check/2 % +ReferenceAlignments:ordset(pair(iri))
                 % +RawAlignments:ordset(pair(iri))
  ]
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI): Check

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(ordsets)).
:- use_module(library(msg_ext)).





%! oaei_check(
%!   +ReferenceAlignments:list(pair),
%!   +RawAlignments:list(pair)
%! ) is det.
% Tests the quality of a raw alignment relative to the given reference.

oaei_check(RefAs, RawAs) :-
  t1_error(RefAs, RawAs, FalsePos),
  t2_error(RefAs, RawAs, FalseNeg),
  ord_intersection(RefAs, RawAs, X),
  length(X, Overlap),
  % Write the results to user output.
  msg_notification("~t~w~t~w~t~w~n", [Overlap,FalsePos,FalseNeg]).





% HELPERS %

%! t1_error(+True:ordset, +Verified:ordset, T1_Error:nonneg) is det.
% The number of cases in which the hypothesis is actually false but
% is said to be true.
%
% # Synonyms
%     * false alarm.
%     * false positive error
%     * asserting something that is absent
%     * false hit


t1_error(HypothesisTrue, HypothesisVerified, T1_Error) :-
  ord_subtract(HypothesisVerified, HypothesisTrue, X),
  length(X, T1_Error).



%! t2_error(+True:ordset, +Verified:ordset, T2_Error:nonneg) is det.
% The number of cases in which the hypothesis is actually true but
% is said to be false.
%
% # Synonyms
%     * false negative error
%     * failing to assert what is present
%     * miss

t2_error(HypothesisTrue, HypothesisVerified, T2_Error) :-
  ord_subtract(HypothesisTrue, HypothesisVerified, X),
  length(X, T2_Error).
