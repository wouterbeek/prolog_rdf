:- encoding(utf8).
:- module(
  rdf_guess,
  [
    rdf_guess_file/3,   % +FileSpec, +Size, -MediaType
    rdf_guess_stream/3, % +In, +Size, -MediaType
    rdf_guess_string/2  % +String, -MediaType
  ]
).

/** <module> RDF guess

This module heuristically guesses the RDF serialization format that is
stored in the input stream In.

RDF/XML can be distinguished from Turtle-family serializations,
because it is not possible to define valid RDF/XML without declaring
XML namespaces.

*/

:- use_module(library(sgml)).
:- use_module(library(yall)).

:- use_module(library(archive_ext)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(media_type)).
:- use_module(library(rdf_prefix)).

:- meta_predicate
    n3_lexical_form_codes(//, ?, ?).





%! rdf_guess_file(+FileSpec:term,
%!                +Size:positiveInteger,
%!                -MediaType:media_type) is semidet.

rdf_guess_file(Spec, Size, MediaType) :-
  read_from_file(
    Spec,
    {Size,MediaType}/[In]>>rdf_guess_stream(In, Size, MediaType)
  ).



%! rdf_guess_stream(+In:istream, +Size:nonneg, -MediaType:media_type) is semidet.
%
% @arg Size is the number of codes that is read from the input stream
% In, on which the guess is based.  This number is doubled while
% backtracking, until either the end of the stream is reached or the
% maximum peek size, as indicated by the input stream `In', is
% exceeded.
%
% @arg MediaType is a compound term of the form
% `media(Supertype/Subtype,Params)'.  This is how Media Types are
% represented in the HTTP package (see http_parse_header_value/3).
%
% There is one JSON-family Media Type:
%
%   * media(application/ld+json,[]) for JSON-LD
%
% There are two/four Turtle-family Media Types:
%
%   * media(application/trig,[]) for TriG
%
%     This includes the Media Type for Turtle, i.e.,
%     `media(text/turtle,[])'.
%
%   * media(application/'n-nquads',[]) for N-Quads
%
%     This includes the Media Type for N-Triples, i.e.,
%     `media(application/'n-triples',[])'.
%
% There are two SGML-family Media Types that denote RDF:
%
%   * media(application/'rdf+xml',[]) for RDF/XML
%
%   * media(application/'xhtml+xml',[]) for RDFa
%
%   * media(text/html,_) for RDFa

rdf_guess_stream(In, Size, MediaType) :-
  peek_string(In, Size, String),
  rdf_guess_string(String, MediaType).



%! rdf_guess_string(+String:string, -MediaType:media_type) is semidet.

rdf_guess_string(String, MediaType) :-
  rdf_guess_string_(String, Ext),
  media_type_extension(MediaType, Ext).

rdf_guess_string_(String, jsonld) :-
  string_phrase(jsonld_format, String), !.
rdf_guess_string_(String, Ext) :-
  % We use the information as to whether or not the end of the stream
  % has been reached.
  string_phrase(n3_format(Ext), String, _), !.
rdf_guess_string_(String, Ext) :-
  setup_call_cleanup(
    open_string(String, In),
    sgml_format(In, Ext),
    close(In)
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

%! n3_format(-Extension:oneof([nq,trig]))// is semidet.
%
% Succeeds on a list of codes that match the beginning of a document
% in the Turtle-family.

% skip blanks
n3_format(Ext) -->
  n3_blank, !,
  n3_blanks,
  n3_format(Ext).
% N-Quads, N-Triples, TriG, Turtle comment
n3_format(Ext) -->
  n3_comment, !,
  n3_format(Ext).
% Turtle, TriG base or prefix declaration
n3_format(trig) -->
  ("@base" ; "base" ; "@prefix" ; "prefix"), !.
% TriG default graph
n3_format(trig) -->
  "{", !.
% N-Quads, N-Triples TriG, Turtle triple or quadruple
n3_format(Ext) -->
  n3_subject(Ext),
  n3_blanks,
  n3_predicate(Ext),
  n3_blanks,
  n3_object(Ext),
  n3_blanks,
  (   % end of a triple
      "."
  ->  ""
  ;   % TriG, Turtle object list notation
      ";"
  ->  {Ext = trig}
  ;   % TriG, Turtle predicate-object pairs list notation
      ","
  ->  {Ext = trig}
  ;   % N-Quads end of a quadruple
      n3_graph(Ext),
      n3_blanks,
      "."
  ->  {Ext = nq}
  ), !,
  {(var(Ext) -> Ext = nq ; true)}.
% TriG, Turtle anonymous blank node
n3_format(trig) -->
  "[", !.
% TriG, Turtle collection
n3_format(trig) -->
  "(", !.
% TriG named graph
n3_format(trig) -->
  n3_graph(_),
  n3_blanks,
  "{", !.

% N3 only allows horizontal tab and space, but we skip other blank
% characters as well, since they may appear in non-conforming
% documents without telling us anything about which N3 subtype we are
% parsing.
n3_blank -->
  blank.
n3_blank -->
  n3_comment.

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

eol --> "\n".
eol --> "\r\n".

n3_graph(Ext) -->
  n3_iriref(Ext).

% N-Quads, N-Triples, TriG, Turtle full IRI
n3_iriref(_) -->
  "<", !,
  n3_iriref0.
% TriG, Turtle prefixed IRI
n3_iriref(trig) -->
  n3_iriref_prefix,
  ":",
  nonblanks.

n3_iriref0 -->
  ">", !.
n3_iriref0 -->
  "\\", !,
  (   "u"
  ->  xdigit(_), xdigit(_), xdigit(_), xdigit(_)
  ;   "U"
  ->  xdigit(_), xdigit(_), xdigit(_), xdigit(_), xdigit(_), xdigit(_), xdigit(_), xdigit(_)
  ),
  n3_iriref0.
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
n3_lexical_form(trig) -->
  "'''", !,
  n3_lexical_form_codes([0'',0'',0'']).
% TriG, Turtle lexical form with single single quotes
n3_lexical_form(trig) -->
  "'", !,
  n3_lexical_form_codes([0'']).
% TriG, Turtle lexical form with triple double quotes
n3_lexical_form(trig) -->
  "\"\"\"", !,
  n3_lexical_form_codes([0'",0'",0'"]).
% N-Quads, N-Triples, TriG, Turtle lexical form with single double
% quotes
n3_lexical_form(_) -->
  "\"",
  n3_lexical_form_codes([0'"]).

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
n3_literal(trig) -->
  ("false" ; "true" ; "+" ; "-" ; "." ; digit(_)), !.
n3_literal(Ext) -->
  n3_lexical_form(Ext),
  n3_blanks,
  (   % Literal with an explicit datatype IRI (previously: typed
      % literal).
      "^^"
  ->  n3_blanks,
      n3_iriref(Ext)
  ;   % Language-tagged string (datatype IRI `rdf:langString').
      "@"
  ->  n3_ltag
  ;   % Abbreviated notation for `xsd:string' (previously: simple
      % literal).
      ""
  ).

n3_ltag -->
  nonblanks.

n3_object(Ext) -->
  n3_iriref(Ext), !.
n3_object(_) -->
  n3_bnode, !.
n3_object(Ext) -->
  n3_literal(Ext).

n3_predicate(Ext) -->
  n3_iriref(Ext), !.
% TriG, Turtle abbreviation for `rdf:type'
n3_predicate(trig) -->
  "a".

n3_subject(Ext) -->
  n3_iriref(Ext), !.
n3_subject(_) -->
  n3_bnode.



% SGML FAMILY %

%! sgml_format(+In:istream, -Extension:atom) is semidet.
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


%! sgml_doctype(+In:istream,
%!              -Dialect:atom,
%!              -Doctype:atom,
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
          call(decl, on_declaration),
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

on_declaration(Text, Parser) :-
  atomic_list_concat(Components, ' ', Text),
  Components = ['DOCTYPE',Doctype],
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Doctype, [])).


%! doc_content_type(+Dialect:atom, +Doctype:atom, +Attributes:list(compound),
%!                  -Extension:atom) is det.

doc_content_type(_, html, _, html) :- !.
doc_content_type(html, _, _, html) :- !.
doc_content_type(xhtml, _, _, xhtml) :- !.
doc_content_type(html5, _, _, html) :- !.
doc_content_type(xhtml5, _, _, xhtml) :- !.
doc_content_type(xml, rss, _, rdf) :- !.
doc_content_type(Dialect, Top,  Attributes, rdf) :-
  % Extract the namespace from the doctype.
  dialect_local_name(Dialect, LocalName),
  atomic_list_concat([NS,LocalName], :, Top),

  % Look up the RDF namespace in the attributes list.
  atomic_list_concat([xmlns,NS], :, Attribute),
  memberchk(Attribute=RDFNS, Attributes),

  % Ensure it is indeed the RDF namespace.
  rdf_prefix(rdf, RDFNS).

dialect_local_name(sgml, rdf).
dialect_local_name(xml, 'RDF').
