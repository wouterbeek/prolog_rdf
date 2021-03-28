# CHANGELOG

## 1.0.5 (2021-03-28)

This release brings the following changes:

### rdf_clean

- Allow the calling context to determine what to do with RDF errors
  detected during cleaning.

### rdf_prefix

- Added `rdf_prefix_map/1` to allow all current prefix declarations to
  be conveniently stored in one object.

### rdf_print

- Added `rdf_dcg_var//2` for printing variables in the SPARQL syntax.

- Allow variables to be printed as part of `rdf_dcg_tp//2` (triple
  patterns).

### rdf_term

- Added print statements for some of the emitted syntax errors.

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

## 1.0.3 (2021-01-08)

## `rdf_download`

  - Added `rdf_download/3` wich allows options to be passed on to the
    IRI dereferencing predicates.

## `rdf_print`

  - ENHANCED: When printing multiple object terms with the same
    subject/predicate term pair, emit the object terms of separate
    lines.

  - ENHANCED: When printing multiple triples with the same subject
    term, print the instance-of triples first.

  - CLEANUP: Removed support for option `rdfs:label`, since this
    relied on an RDF backend being available.

## `rdf_media_type`

  - CLEANUP: Removed `rdf_media_type_extension/2`, since popular file
    name extensions are already handled by the Prolog Library
    Collection library.

## `rdf_term`

  - NEW: `rdf_hash_iri/3` makes it easy to create content-based IRIs.
    Content-based IRIs can be used when no naming schema can be
    determined, or when determining a naming schema is considered too
    costly.

  - ENHANCED: taught the DWIM support about Boolean literals.

  - ENHANCED: Allow a maximum to be set for the RDF container
    membership properties.  Without this limit, the generative mode
    for RDF container membership properties will have infinite
    solutions.
