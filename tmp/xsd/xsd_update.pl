:- module(
  xsd_update,
  [
    xsd_lexical_canonical_map/3, % +Datatype:iri
                                 % +LexicalForm:atom
                                 % -CanonicalLexicalForm:atom
    xsd_lexical_canonical_map/4 % +FromDatatype:iri
                                % +LexicalForm:atom
                                % +ToDatatype:iri
                                % -CanonicalLexicalForm:atom
  ]
).

/** <module> XSD update

Predicates for cleaning XML Scheme 1.1 datatypes.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(xsd/xsd)).

:- rdf_meta(xsd_lexical_canonical_map(r,+,-)).
:- rdf_meta(xsd_lexical_canonical_map(r,+,r,-)).





%! xsd_lexical_canonical_map(
%!   +Datatype:iri,
%!   +LexicalForm:atom,
%!   -CanonicalLexicalForm:atom
%! ) is det.
% Reads a datatype lexical expression and converts it into its canonical form.

xsd_lexical_canonical_map(D, Lex, CLex):-
  xsd_lexical_canonical_map(D, Lex, D, CLex).


%! xsd_lexical_canonical_map(
%!   +FromDatatype:iri,
%!   +LexicalForm:atom,
%!   +ToDatatype:iri,
%!   -CanonicalLexicalForm:atom
%! ) is det.

xsd_lexical_canonical_map(FromD, FromLex, ToD, ToLex):-
  xsd_lexical_map(FromD, FromLex, Val),
  xsd_canonical_map(ToD, Val, ToLex).
