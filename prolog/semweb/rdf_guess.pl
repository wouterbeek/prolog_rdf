:- module(
  rdf_guess,
  [
    rdf_guess_file/2,   % +File, -MediaType
    rdf_guess_stream/2, % +In, -MediaType
    rdf_guess_stream/3  % +In, +Size, -MediaType
  ]
).

/** <module> RDF guess

This module heuristically guesses the RDF serialization format that is
stored in the input stream In.

RDF/XML can be distinguished from Turtle-family, because it is not
possible to define valid RDF/XML without XML namespaces.  At the same
time, it is not possible to define a valid absolute Turtle-family IRI
(`<…>'-notation) with a valid `xmlns' declaration[?].

@author Wouter Beek
@author Jan Wielemaker
@version 2017/04-2017/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(media_type)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_prefixes)).
:- use_module(library(sgml)).

:- meta_predicate
    n3_lexical_form_codes(//, ?, ?).

:- setting(maximum_peek_size, nonneg, 100000,
           "The maximum number of codes that is peeked from the input stream.").
:- setting(minimum_peek_size, nonneg, 1000,
           "The initial number of codes that is peeked from the input stream.").





%! rdf_guess_file(+File:atom, -MediaType:compound) is det.

rdf_guess_file(File, MediaType) :-
  rdf_guess_file0(File, Ext),
  media_type_extension(MediaType, Ext).

% JSON-LD
rdf_guess_file0(File, jsonld) :-
  phrase_from_file(jsonld_format, File), !.
% N-Quads, N-Triples, TriG, Turtle
rdf_guess_file0(File, Ext) :-
  phrase_from_file(n3_format(false, [nq,trig], Ext), File).
% RDF/XML
rdf_guess_file0(File, Ext) :-
  setup_call_cleanup(
    open(File, read, In),
    sgml_format(In, Ext),
    close(In)
  ).



%! rdf_guess_stream(+In:stream, -MediaType:compound) is det.
%! rdf_guess_stream(+In:stream, +Size:nonneg, -MediaType:compound) is det.
%
% @arg Size The initial number of codes that is read from the input
%      stream In, on which the guess is based.  This number is doubled
%      while backtracking, until either the end of the stream is
%      reached or the maximum peek size, as indicated by the input
%      stream `In', is exceeded.
%
% @arg MediaType is a compound term of the form
%      `media(Supertype/Subtype,Params)'.  This is how Media Types are
%      represented in the HTTP package (see
%      http_parse_header_value/3).
%
%      There is one JSON-family Media Type:
%
%        * media(application/ld+json,[]) for JSON-LD
%
%      There are four Turtle-family Media Types:
%
%        * media(application/trig,[]) for TriG
%
%          This includes the Media Type for Turtle, i.e.,
%          `media(text/turtle,[])'.
%
%        * media(application/'n-nquads',[]) for N-Quads
%
%          This includes the Media Type for N-Triples, i.e.,
%          `media(application/'n-triples',[])'.
%
%      There are two SGML-family Media Types that denote RDF:
%
%        * media(application/'rdf+xml',[]) for RDF/XML
%
%        * media(text/html,_) for RDFa
%
% Non-determinism causes an increasingly longer prefix to be read.

rdf_guess_stream(In, MediaType) :-
  setting(minimum_peek_size, Min),
  rdf_guess_stream(In, Min, MediaType).


rdf_guess_stream(In, Size, MediaType) :-
  must_be(positive_integer, Size),
  setting(maximum_peek_size, Max),
  rdf_guess_stream0(In, Size, Max, Ext),
  media_type_extension(MediaType, Ext).

% Guess the RDF serialization format based on the current peek size.
rdf_guess_stream0(In, Size, Max, Ext) :-
  Size =< Max,
  peek_string(In, Size, String),
  debug(rdf_guess, "[PEEK ~D CHARS] ~s", [Size,String]),
  string_length(String, Length),
  % Keep track of whether or not the entire stream has been peeked.
  (Length < Size -> !, EoS = true ; EoS = false),
  rdf_guess_string(String, EoS, Ext), !.
% Unable to determine the RDF serialization format within the maximum
% peek size.
rdf_guess_stream0(_, Size, Size, _) :- !, fail.
% Increase the peek size
rdf_guess_stream0(In, Size1, Max, Ext) :-
  Size2 is min(Size1 * 2,Max),
  rdf_guess_stream0(In, Size2, Max, Ext).

rdf_guess_string(String, _, jsonld) :-
  string_phrase(jsonld_format, String), !.
rdf_guess_string(String, EoS, Ext) :-
  % We use the information as to whether or not the end of the stream
  % has been reached.
  string_phrase(n3_format(EoS, [nq,trig], Ext), String, _).
rdf_guess_string(String, _, Ext) :-
  setup_call_cleanup(
    new_memory_file(MFile),
    (
      setup_call_cleanup(
        open_memory_file(MFile, write, Write),
        format(Write, "~s", [String]),
        close(Write)
      ),
      setup_call_cleanup(
        open_memory_file(MFile, read, In),
        sgml_format(In, Ext),
        close(In)
      )
    ),
    free_memory_file(MFile)
  ).



% JSON-LD %

%! jsonld_format// .
%
% It is not clear to me whether a JSON document is a single array, a
% single object, or a sequence of arrays and objects.  The JSON
% grammars that I could find online only define JSON terms, not JSON
% documents.
%
% Whatever the case may be, if a document start with the square
% opening bracket, it can be neither belong to the Turtle- nor
% to the SGML-family, so it must be JSON-LD.
%
% If a document start with a curly opening bracket, it can still be
% TriG or JSON-LD.  However, if the opening curly bracket is directly
% followed by a JSON-conformant string and a colon (the key/value
% separator in JSON).

jsonld_format -->
  blanks,
  optional_arrays,
  "{",
  blanks,
  json_string,
  blanks,
  ":", !,
  remainder(_).

optional_arrays -->
  "[", !,
  blanks,
  optional_arrays.
optional_arrays --> "".

% JSON strings start with a double quote.
json_string -->
  "\"",
  json_string_rest.

% JSON strings may contain escaped double quotes.
json_string_rest -->
  "\\\"", !,
  json_string_rest.
% An unescaped double quote ends a JSON string.
json_string_rest -->
  "\"", !.
% Skip other JSON string content.
json_string_rest -->
  [_], !,
  json_string_rest.



% N3 FAMILY %

%! n3_format(+EoS:boolean, +Extensions:ordset(oneof([nq,trig])),
%!           -Extension:oneof([nq,trig]))// is semidet.
%
% Succeeds on a list of codes that match the beginning of a document
% in the Turtle-family.
%
% @arg EoS Whether or not the end of stream has been reached on the
%      parsed input.  This matters for determining the RDF
%      serialization format.  For example, if we have not seen any
%      graph terms yet it cannot be TriG or N-Quads.

% done: unique format
n3_format(_, [Ext], Ext) --> !.
% done: end-of-stream; take a _most specific_ format
%
% Empty files or files that only consist of N3 comments are also
% classified as N3 at this step.
%
% At end-of-stream the format cannot be TriG, because it should have
% been the only option by now.
n3_format(true, Exts, nq) -->
  eos, !,
  {assertion(Exts == [nq])}.
% skip blanks
n3_format(EoS, Exts, Ext) -->
  n3_blank, !,
  n3_blanks,
  n3_format(EoS, Exts, Ext).
% N-Quads, N-Triples, TriG, Turtle comment
n3_format(EoS, Exts, Ext) -->
  n3_comment, !,
  n3_format(EoS, Exts, Ext).
% Turtle, TriG base or prefix declaration
n3_format(EoS, Exts1, Ext) -->
  ("@base" ; "base" ; "@prefix" ; "prefix"), !,
  {ord_subtract(Exts1, [nq], Exts2)},
  n3_format(EoS, Exts2, Ext).
% TriG default graph
n3_format(EoS, Exts1, Ext) -->
  "{", !,
  {ord_subtract(Exts1, [nq], Exts2)},
  n3_format(EoS, Exts2, Ext).
% N-Quads, N-Triples TriG, Turtle triple or quadruple
n3_format(EoS, Exts1, Ext) -->
  n3_subject(Exts1, Exts2),
  n3_blanks,
  n3_predicate(Exts2, Exts3), !,
  n3_blanks,
  n3_object(Exts3, Exts4),
  n3_blanks,
  (   % end of a triple
      "."
  ->  {Exts6 = Exts4}
  ;   % TriG, Turtle object list notation
      ";"
  ->  {ord_subtract(Exts4, [nq], Exts6)}
  ;   % TriG, Turtle predicate-object pairs list notation
      ","
  ->  {ord_subtract(Exts4, [nq], Exts6)}
  ;   % N-Quads end of a quadruple
      n3_graph(Exts4, Exts5),
      n3_blanks,
      "."
  ->  {ord_subtract(Exts5, [trig], Exts6)}
  ),
  n3_format(EoS, Exts6, Ext).
% TriG, Turtle anonymous blank node
n3_format(EoS, Exts1, Ext) -->
  "[", !,
  {ord_subtract(Exts1, [nq], Exts2)},
  ns_format(EoS, Exts2, Ext).
% TriG, Turtle collection
n3_format(EoS, Exts1, Ext) -->
  "(", !,
  {ord_subtract(Exts1, [nq], Exts2)},
  n3_format(EoS, Exts2, Ext).
% TriG named graph
n3_format(EoS, Exts1, Ext) -->
  n3_graph(Exts1, Exts2),
  n3_blanks,
  "{",
  {ord_subtract(Exts2, [nq], Exts3)},
  n3_format(EoS, Exts3, Ext).

% N3 only allows horizontal tab and space, but we skip other blank
% characters as well, since they may appear in non-conforming
% documents without telling us anything about which N3 subtype we are
% parsing.
n3_blank --> blank.
n3_blank --> n3_comment.

n3_blanks -->
  n3_blank, !,
  n3_blanks.
n3_blanks --> "".

n3_bnode -->
  "_:",
  nonblanks.

n3_comment -->
  "#",
  string(_),
  (eol ; eos).

n3_graph(L1, L2) -->
  n3_iriref(L1, L2).

% N-Quads, N-Triples, TriG, Turtle full IRI
n3_iriref(L, L) -->
  "<", !,
  n3_iriref0.
% TriG, Turtle prefixed IRI
n3_iriref(L1, L2) -->
  n3_iriref_prefix,
  ":",
  nonblanks,
  {ord_subtract(L1, [nq], L2)}.

n3_iriref0 -->
  ">", !.
n3_iriref0 -->
  [Code],
  {\+ non_iri_code(Code)},
  n3_iriref0.

non_iri_code(Code) :-
  between(0x0, 0x20, Code).
non_iri_code(0'<).
non_iri_code(0'>).
non_iri_code(0'").%"
non_iri_code(0'{).
non_iri_code(0'}).
non_iri_code(0'|).
non_iri_code(0'^).
non_iri_code(0'`).
non_iri_code(0'\\).

n3_iriref_prefix -->
  ":", !,
  {fail}.
n3_iriref_prefix -->
  blank, !,
  {fail}.
n3_iriref_prefix -->
  nonblank, !,
  n3_iriref_prefix.
n3_iriref_prefix --> "".

% TriG, Turtle lexical form with triple single quotes
n3_lexical_form(L1, L2) -->
  "'''", !,
  n3_lexical_form_codes([0'',0'',0'']),
  {ord_subtract(L1, [nq], L2)}.
% TriG, Turtle lexical form with single single quotes
n3_lexical_form(L1, L2) -->
  "'", !,
  n3_lexical_form_codes([0'']),
  {ord_subtract(L1, [nq], L2)}.
% TriG, Turtle lexical form with triple double quotes
n3_lexical_form(L1, L2) -->
  "\"\"\"", !,
  n3_lexical_form_codes([0'",0'",0'"]), %"
  {ord_subtract(L1, [nq], L2)}.
% N-Quads, N-Triples, TriG, Turtle lexical form with single double
% quotes
n3_lexical_form(L, L) -->
  "\"",
  n3_lexical_form_codes([0'"]). %"

% Escaped single quote.
n3_lexical_form_codes(End) -->
  "\\\'", !,
  n3_lexical_form_codes(End).
% Escaped double quote.
n3_lexical_form_codes(End) -->
  "\\\"", !,
  n3_lexical_form_codes(End).
% End of string.
n3_lexical_form_codes(End) -->
  End, !.
% Content.
n3_lexical_form_codes(End) -->
  [_], !,
  n3_lexical_form_codes(End).
% End of stream.
n3_lexical_form_codes(_) --> "".

% TriG, Turtle abbreviated form for XSD boolean, decimal, double, and
% integer literals
n3_literal(L1, L2) -->
  ("false" ; "true" ; "+" ; "-" ; "." ; digit(_)), !,
  {ord_subtract(L1, [nq], L2)}.
n3_literal(L1, L3) -->
  n3_lexical_form(L1, L2),
  n3_blanks,
  (   "^^"
  ->  n3_blanks,
      n3_iriref(L2, L3)
  ;   "@"
  ->  n3_ltag,
      {L3 = L2}
  ;   % TriG, Turtle abbreviated form for XSD string literals.
      {ord_subtract(L2, [nq], L3)}
  ).

n3_ltag -->
  nonblanks.

n3_object(L1, L2) -->
  n3_iriref(L1, L2), !.
n3_object(L, L) -->
  n3_bnode, !.
n3_object(L1, L2) -->
  n3_literal(L1, L2).

n3_predicate(L1, L2) -->
  n3_iriref(L1, L2), !.
% TriG, Turtle abbreviation for `rdf:type'
n3_predicate(L1, L2) -->
  "a",
  {ord_subtract(L1, [nq], L2)}.

n3_subject(L1, L2) -->
  n3_iriref(L1, L2), !.
n3_subject(L, L) -->
  n3_bnode.



% SGML FAMILY %

%! sgml_format(+In:stream, -Extension:atom) is semidet.
%
% Try to see whether the document is some form of HTML or XML and in
% particular whether it is RDF/XML.  The latter is basically
% impossible because it is not obligatory for an RDF/XML document to
% have an rdf:RDF top level element, and when using a typed node, just
% about anything can qualify for RDF.  The only real demand is that
% the XML document must use XML namespaces, because these are both
% required to define `<rdf:Description>' and a valid type IRI from a
% typed node.
%
% If the toplevel element is detected as HTML we guess that the
% document contains RDFa.

sgml_format(In, Ext) :-
  sgml_doctype(In, Dialect, DocType, Attributes),
  doc_content_type(Dialect, DocType, Attributes, Ext).


%! sgml_doctype(+In:stream, -Dialect:atom, -Doctype:atom,
%!              -Attributes:list(compound)) is semidet.
%
% Parse a _repositional_ stream and get the name of the first SGML
% element *and* demand that this element defines XML namespaces.
% Fails if the document is illegal SGML before the first element.
%
% Note that it is not possible to define valid RDF/XML without
% namespaces, while it is not possible to define a valid absolute
% Turtle IRI (using `<...>`-notation) with a valid xmlns declaration.

sgml_doctype(In, Dialect, DocType, Attributes) :-
  setup_call_cleanup(
    make_parser(In, Parser, State),
    catch(
      sgml_parse(
        Parser,
        [
          call(begin, on_begin),
          call(cdata, on_cdata),
          max_errors(-1),
          source(In),
          syntax_errors(quiet)
        ]
      ),
      E,
      true
    ),
    clean_parser(In, Parser, State)
  ),
  nonvar(E),
  E = tag(Dialect, DocType, Attributes).

make_parser(In, Parser, state(Position)):-
  stream_property(In, position(Position)),
  new_sgml_parser(Parser, []).

clean_parser(In, Parser, state(Position)):-
  free_sgml_parser(Parser),
  set_stream_position(In, Position).

on_begin(Tag, Attributes, Parser) :-
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Tag, Attributes)).

on_cdata(_, _) :-
  throw(error(cdata)).


%! doc_content_type(+Dialect:atom, +Doctype:atom, +Attributes:list(compound),
%!                  -Extension:atom) is det.

doc_content_type(_, html, _, rdfa) :- !.
doc_content_type(html, _, _, rdfa) :- !.
doc_content_type(xhtml, _, _, rdfa) :- !.
doc_content_type(html5, _, _, rdfa) :- !.
doc_content_type(xhtml5, _, _, rdfa) :- !.
doc_content_type(xml, rss, _, rdfa) :- !.
doc_content_type(Dialect, Top,  Attributes, rdfxml) :-
  % Extract the namespace from the doctype.
  dialect_local_name(Dialect, LocalName),
  atomic_list_concat([NS,LocalName], :, Top),

  % Look up the RDF namespace in the attributes list.
  atomic_list_concat([xmlns,NS], :, Attribute),
  memberchk(Attribute=RDFNS, Attributes),

  % Ensure it is indeed the RDF namespace.
  rdf_current_prefix(rdf, RDFNS).

dialect_local_name(sgml, rdf).
dialect_local_name(xml, 'RDF').
