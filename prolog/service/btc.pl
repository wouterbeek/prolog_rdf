:- module(
  btc,
  [
    load_btc/0
  ]
).

/** <module> Billion Triple Challenge (BTC)

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(os/io)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(thread)).
:- use_module(library(yall)).





%! load_btc is det.

load_btc:-
  call_on_stream(
    'http://km.aifb.kit.edu/projects/btc-2012/000-CONTENTS',
    {Iris}/[In,Meta,Meta]>>findall(Iri, read_stream_to_atom(In, Iri), Iris)
  ),
  concurrent_maplist(rdf_load_file, Iris).
