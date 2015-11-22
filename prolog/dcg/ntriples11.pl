:- module(
  ntriples11,
  [
    'EOL'//0,
    literal//1 % ?Literal:compound
  ]
).
:- reexport(library(dcg/turtle11_code), [
     'UCHAR'//1 % ?Code:code
   ]).
:- reexport(library(dcg/turtle11_token), [
     'IRIREF'//1, % ?Iri:atom
     'STRING_LITERAL_QUOTE'//1 % ?String:atom
   ]).

/** <module> N-Triples 1.1: Codes

@author Wouter Beek
@compat N-Triples 1.1
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).





%! 'EOL'// .
%! 'EOL'(?Codes:list(code))// .
% ```abnf
% EOL ::= [#xD#xA]+
% ```

'EOL' --> +(eol_code).
eol_code --> [0xD].
eol_code --> [0xA].



%! literal(?Literal:compound)// .
% ```ebnf
% literal ::= STRING_LITERAL_QUOTE ( '^^' IRIREF | LANGTAG )?
% ```

literal(Lit) -->
  'STRING_LITERAL_QUOTE'(Lex),
  (   "^^"
  ->  'IRIREF'(D), {Lit = literal(type(D,Lex))}
  ;   'LANGTAG'(LTag) -> {Lit = literal(lang(LTag,Lex))}
  ;   {Lit = literal(Lex)}
  ).



%! 'PN_LOCAL_ESC'(?Code:code)// .
% ```bnf
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | * | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ```
%
% @compat SPARQL 1.1 Query [173].
% @compat Turtle 1.1 [172s].

'PN_LOCAL_ESC'(C) --> "\\", pn_local_esc_code(C).
pn_local_esc_code(0'_) --> "_".
pn_local_esc_code(0'~) --> "~".
pn_local_esc_code(0'.) --> ".".
pn_local_esc_code(0'-) --> "-".
pn_local_esc_code(0'!) --> "!".
pn_local_esc_code(0'$) --> "$".
pn_local_esc_code(0'&) --> "&".
pn_local_esc_code(0'') --> "'".
pn_local_esc_code(0'() --> "(".
pn_local_esc_code(0')) --> ")".
pn_local_esc_code(0'*) --> "*".
pn_local_esc_code(0'+) --> "+".
pn_local_esc_code(0',) --> ",".
pn_local_esc_code(0';) --> ";".
pn_local_esc_code(0'=) --> "=".
pn_local_esc_code(0'/) --> "/".
pn_local_esc_code(0'?) --> "?".
pn_local_esc_code(0'#) --> "#".
pn_local_esc_code(0'@) --> "@".
pn_local_esc_code(0'%) --> "%".



%! white_space// .
%! white_space(?Codes:list(code))// .
% White space is a sequence of:
%   - N-Quads 1.1, N-Triples 1.1
%   - OWL 2 Web Ontology Language Manchester Syntax (Second Edition):
%     - blanks (U+20)
%     - tabs (U+9)
%     - line feeds (U+A)
%     - carriage returns (U+D)
%     - comments
%
% @compat N-Triples 1.1
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

white_space --> white_space(_).
white_space([C]) --> 'WS'(C).
white_space(Cs) --> comment(Cs).
white_space([0'\t]) --> "\t".
white_space([0' ]) --> " ".
