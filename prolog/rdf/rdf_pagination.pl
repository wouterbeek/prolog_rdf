:- module(
  rdf_pagination,
  [
    rdf_read_tuple/2 % +Source, -Tuple
  ]
).

/** <module> RDF pagination

@author Wouter Beek
@version 2016/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(http/rfc5988)).
:- use_module(library(semweb/rdf_ntriples)).





rdf_read_tuple(Uri, Tuple) :-
  Opts = [
    header(link,LinkAtom),
    request_header('Accept'='application/n-quads')
  ],
  setup_call_cleanup(
    http_open(Uri, In, Opts),
    (
      repeat,
      read_ntuple(In, Tuple),
      (Tuple == end_of_file -> ! ; true)
    ),
    close(In)
  ),
  gtrace,
  atom_phrase(link(Link), LinkAtom),
  writeln(Link).
