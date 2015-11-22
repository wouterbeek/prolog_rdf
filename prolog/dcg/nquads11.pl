:- module(
  nquads11,
  [
  ]
).
:- reexport(library(dcg/ntriples11_code), [
     'EOL'//0,
     'UCHAR'//1 % ?Code:code
   ]).
:- reexport(library(dcg/turtle11_token), [
     'IRIREF'//1, % ?Iri:atom
     'STRING_LITERAL_QUOTE'//1 % ?String:atom
   ]).

/** <module> N-Quads 1.1: Codes

@author Wouter Beek
@compat N-Quads 1.1
@version 2015/11
*/
