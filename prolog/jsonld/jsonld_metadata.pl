:- module(
  jsonld_metadata,
  [
    jsonld_metadata/2,                % +Metadata, -Jsonld
    jsonld_metadata_abbreviate_iri/2, % +Full, -Compact
    jsonld_metadata_context/1,        % -Context
    jsonld_metadata_expand_iri/2      % +Compact, -Full
  ]
).

/** <module> JSON-LD metadata

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(rdf/rdf_print)).





%! jsonld_metadata(+Metadata, -Jsonld) is det.
%! jsonld_metadata(+S, +Metadata, -Jsonld) is det.

jsonld_metadata(D1, D3) :-
  jsonld_metadata_context(Context),
  D2 = D1.put(_{'@context': Context}),
  atom_json_dict(A, D2),
  atom_json_dict(A, D3).



%! jsonld_metadata_abbreviate_iri(+Full, -Compact) is det.

jsonld_metadata_abbreviate_iri(Full, Compact) :-
  jsonld_metadata_context(Context),
  jsonld_abbreviate_iri(Context, Full, Compact).



%! jsonld_metadata_context(-Context) is det.

jsonld_metadata_context(
  _{
    formats: 'http://www.w3.org/ns/formats/',
    llo: 'http://lodlaundromat.org/ontology/',
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
    'llo:access-control-allow-origin': _{'@container': '@list'},
    'llo:base-iri': _{'@type': '@id'},
    'llo:connection': _{'@container': '@list'},
    'llo:content-length': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:content-type': _{'@container': '@list'},
    'llo:date': _{'@container': '@list'},
    'llo:duplicate-statements': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:filetype': _{'@type': 'xsd:string'},
    'llo:filters': _{'@container': '@list'},
    'llo:final-iri': _{'@type': '@id'},
    'llo:format': _{'@type': 'xsd:string'},
    'llo:major': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:minor': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:mode': _{'@type': 'xsd:string'},
    'llo:mtime': _{'@type': 'xsd:float'},
    'llo:name': _{'@type': 'xsd:string'},
    'llo:parser': _{'@type': 'xsd:string'},
    'llo:processed-quadruples': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:processed-statements': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:processed-triples': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:raw': _{'@type': 'xsd:string'},
    'llo:serialization-format': _{'@type': '@id'},
    'llo:server': _{'@container': '@list'},
    'llo:size': _{'@container': '@list'},
    'llo:status-code': _{'@type': 'xsd:nonNegativeInteger'},
    'llo:source-type': _{'@type': 'xsd:string'},
    'llo:subtype': _{'@type': 'xsd:string'},
    'llo:transfer-encoding': _{'@container': '@list'},
    'llo:type': _{'@type': 'xsd:string'},
    'llo:unique-statements': _{'@type': 'xsd:nonNegativeInteger'}
  }
).



%! jsonld_metadata_expand_iri(+Compact, -Full) is det.

jsonld_metadata_expand_iri(Compact, Full) :-
  jsonld_metadata_context(Context),
  jsonld_expand_iri(Context, Compact, Full).
