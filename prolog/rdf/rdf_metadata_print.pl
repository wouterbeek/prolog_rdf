:- module(
  rdf_metadata_print,
  [
    rdf_metadata_print/1 % +Metadata
  ]
).

/** <module> RDF Clean: Metadata

@author Wouter Beek
@version 2015/10-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_cardinal)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(clpfd)).
:- use_module(library(http/http_print)).





%! rdf_metadata_print(+M) is det.

rdf_metadata_print(M) :-
  dcg_with_output_to(user_output, rdf_metadata(M)).

rdf_metadata(M) -->
  rdf_metadata(0, M).

rdf_metadata(I, M) -->
  ({M.source_type == http_iri} -> http_iri_metadata(I, M) ; ""),
  ({get_dict(stream, M, MStream)} -> stream_metadata(I, MStream) ; ""),
  ({get_dict(rdf, M, MRdf)} -> rdf_metadata0(I, MRdf) ; "").

rdf_metadata0(I1, M) -->
  {
    I2 is I1 + 1,
    I3 is I2 + 1
  },
  section(I1, "RDF metadata:", (
    tab_nl(I2, nvpair("Serialization format", atom(M.format))),
    tab_nl(I2, nvpair("Number of statements", thousands_integer(M.statements))),
    tab_nl(I3, nvpair("Number of quadruples", thousands_integer(M.quadruples))),
    tab_nl(I3, nvpair("Number of triples", thousands_integer(M.triples)))
  )).



stream_metadata(I1, M) -->
  {
    I2 is I1 + 1,
    dict_pairs(M, _, L)
  },
  section(I1, "Stream metadata:", *(nvpair0(I2), L)).



nvpair0(I, X) --> indent(I, nvpair(X)).
