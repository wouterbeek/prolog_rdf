:- module(
  mi_script,
  [
    run/4 % ?Low, ?High, ?Max, +NumWorkers
  ]
).

/** <module> Mutual Information: script

  - x and y are datasets
  - xy is the concatenation of x and y
  - C(x) is the length of the compressed version of dataset x

| Name | Number of statements | Number of subject terms
| MI types | MI predsets | MI path types | MI path presets

@author Wouter Beek
@author Frank van Harmelen
@version 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(csv_ext)).
:- use_module(library(debug)).
:- use_module(library(llapi/llapi)).
:- use_module(library(pool)).

:- use_module(mi).

:- debug(pool(_)).





run(Low, High, Max, NumWorkers) :-
  (   var(Max)
  ->  llapi_doc:docs_size0(Low, High, Pairs)
  ;   findnsols(
        Max,
        NumTriples-Doc,
        doc_size(Low, High, Doc, NumTriples),
        Pairs
      )
  ), !,
  maplist(add_resource(mi), Pairs),
  forall(
    between(1, NumWorkers, _),
    add_worker(mi, mi_worker('results.csv'))
  ).


mi_worker(File, NumTriples-Doc, []) :-
  % Obtain raw values.
  mi(Doc, none,      predset, NumSubjects, Cx1p, Cy1p, Cxy1p),
  mi(Doc, none,      typeset, NumSubjects, Cx1t, Cy1t, Cxy1t),
  mi(Doc, no_prefix, predset, NumSubjects, Cx2p, Cy2p, Cxy2p),
  mi(Doc, no_prefix, typeset, NumSubjects, Cx2t, Cy2t, Cxy2t),
  mi(Doc, rnd_iri,   predset, NumSubjects, Cx3p, Cy3p, Cxy3p),
  mi(Doc, rnd_iri,   typeset, NumSubjects, Cx3t, Cy3t, Cxy3t),

  % Calculate magic value based on raw values.
  RowCx  = [ Cx1p,  Cx1t,  Cx2p,  Cx2t,  Cx3p,  Cx3t],
  RowCy  = [ Cy1p,  Cy1t,  Cy2p,  Cy2t,  Cy3p,  Cy3t],
  RowCxy = [Cxy1p, Cxy1t, Cxy2p, Cxy2t, Cxy3p, Cxy3t],
  RowF   = [  F1p,   F1t,   F2p,   F2t,   F3p,   F3t],
  RowW   = [  W1p,   W1t,   W2p,   W2t,   W3p,   W3t],
  maplist(frank, RowCx, RowCy, RowCxy, RowF),
  maplist(wouter(NumSubjects), RowCx, RowCy, RowCxy, RowW),

  % Store results.
  doc_name(Doc, Name),
  Row = row(
          Name, NumTriples, NumSubjects, % | Transformation | Meaning proxy |
          Cx1p, Cy1p, Cxy1p, F1p, W1p,   % |     1:none     |   p:predset   |
          Cx1t, Cy1t, Cxy1t, F1t, W1t,   % |     1:none     |   t:typeset   |
          Cx2p, Cy2p, Cxy2p, F2p, W2p,   % |   2:no_prefix  |   p:predset   |
          Cx2t, Cy2t, Cxy2t, F2t, W2t,   % |   2:no_prefix  |   t:typeset   |
          Cx3p, Cy3p, Cxy3p, F3p, W3p,   % |    3:rnd_iri   |   p:predset   |
          Cx3t, Cy3t, Cxy3t, F3t, W3t    % |    3:rnd_iri   |   t:typeset   |
        ),
  csv_to_file(File, [Row], [mode(append)]).


frank(Cx, Cy, Cxy, V) :-
  Max is max(Cx, Cy),
  V is (Cx + Cy - Cxy) / Max.


wouter(NumSubjects, Cx, Cy, Cxy, V) :-
  V is (Cx + Cy - Cxy) / NumSubjects.
