# CHANGELOG

## 1.0.4 (2021-03-20)

This released brings the following changes:

  - Use of dictionaries to represent options in all places.  Options
    dictionaries are translated into options lists where needed to
    interact with the SWI standard library.

  - Added `assert_prefixes/1` to assert the prefixes declared in a
    given N-Quads, N-Triples, Turtle, or TriG file.

  - Improved RDF pretty-printing to also use prefix declarations for
    IRIs that only appear as datatype IRIs in typed literals.

  - Improved RDF pretty-printing to preserve backslash escape
    sequences present in the RDF syntax.

  - Improved the Prolog type declarations for RDF, and applied them
    throughout this library.

  - Removed library `rdf_media_type` which is no longer needed due to
    improved Media Type support in Prolog Library Collection.

  - Simplified the RDF pretty-printing API in library `rdf_print`.

  - Two new predicates `rdf_atom_node/2` and `rdf_atom_predicate/2`
    that replace the old `rdf_atom_term/2` predicate.

  - Enhanced `rdf_create_iri/3` to allow the creation of IRIs with
    paths consisting of multiple subpaths.

  - Removed support for geopsatial and URI literals.  These require
    the installation of external C/C++ libraries and are somewhat
    specialized functionalities to begin with.
