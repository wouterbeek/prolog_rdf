:- module(
  jsonld_metadata,
  [
    jsonld_metadata/2,                % +M,       -Jsonld
    jsonld_metadata_abbreviate_iri/2, % +Full,    -Compact
    jsonld_metadata_context/1,        % -Context
    jsonld_metadata_expand_iri/2      % +Compact, -Full
  ]
).

/** <module> JSON-LD metadata

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(jsonld/jsonld_generics)).





%! jsonld_metadata(+M, -Jsonld) is det.

jsonld_metadata(M, Jsonld) :-
  jsonld_metadata_context(Context),
  M0 = M.put(_{'@context': Context}),
  atom_json_dict(A, M0),
  atom_json_dict(A, Jsonld).



%! jsonld_metadata_abbreviate_iri(+Full, -Compact) is det.

jsonld_metadata_abbreviate_iri(Full, Compact) :-
  jsonld_metadata_context(Context),
  jsonld_abbreviate_iri(Context, Full, Compact).



%! jsonld_metadata_context(-Context) is det.

jsonld_metadata_context(
  _{
    '@vocab':          'http://lodlaundromat.org/ontology/',
    base_uri:          _{'@type': 'xsd:anyURI'},
    duplicate_tuples:  _{'@type': 'xsd:nonNegativeInteger'},
    formats:           'http://www.w3.org/ns/formats/',
    processed_quads:   _{'@type': 'xsd:nonNegativeInteger'},
    processed_triples: _{'@type': 'xsd:nonNegativeInteger'},
    processed_tuples:  _{'@type': 'xsd:nonNegativeInteger'},
    rdf:               'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    unique_tuples:     _{'@type': 'xsd:nonNegativeInteger'},
    xsd:               'http://www.w3.org/2001/XMLSchema#'
  }
).



%! jsonld_metadata_expand_iri(+Compact, -Full) is det.

jsonld_metadata_expand_iri(Compact, Full) :-
  jsonld_metadata_context(Context),
  jsonld_expand_term(Context, Compact, Full).
