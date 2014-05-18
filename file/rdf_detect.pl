:- module(
  rdf_detect,
  [
    rdf_guess_format/3 % +Stream:stream
                       % -ContentType:atom
                       % +Options:list(nvpair)
  ]
).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).

/** <module> Detect RDF document format from stream content
*/

%%  rdf_guess_format(+Stream, -ContentType, +Options) is semidet.
%
%  True when Stream is  thought  to   contain  RDF  data  using the
%  indicated content type.  Options processed:
%
%    - look_ahead(+Bytes)
%    Look ahead the indicated amount
%    - format(+Format)
%    Guessed format from media type and/or file name

rdf_guess_format(Stream, ContentType, Options) :-
  option(look_ahead(Bytes), Options, 2000),
  peek_string(Stream, Bytes, String),
  (   string_codes(String, Codes),
      phrase(rdf_content_type(ContentType, Options), Codes, _)
  ->  true
  ;   open_binary_string_stream(String, StartStream),
      guess_xml_type(StartStream, ContentType)
  ).

rdf_content_type(ContentType, Options) -->
  turtle_like(ContentType, Options), !.

%%  turtle_like(-ContentType, +Options)// is semidet.
%
%  True if the start of the   input matches a turtle-like language.
%  There are four of them:
%
%    1. Turtle
%    2. TRiG
%    3. ntriples
%    4. nquads.
%
%  The first three can all be handled   by the turtle parser, so it
%  doesn't matter too much.

turtle_like(ContentType, Options) -->
  blank, !, blanks,
  turtle_like(ContentType, Options).
turtle_like(ContentType, Options) -->
  "#", !, skip_line,
  turtle_like(ContentType, Options).
turtle_like(Format, Options) -->
  "@", icase_keyword(Keyword), {turtle_keyword(Keyword)}, !,
  turtle_or_trig(Format, Options).
turtle_like(Format, Options) -->
  "PREFIX", blank, !,
  turtle_or_trig(Format, Options).
turtle_like(Format, Options) -->
  "BASE", blank, !,
  turtle_or_trig(Format, Options).
turtle_like(Format, Options) -->
  iriref, 'nt_whites+', iriref_pred, 'nt_whites+', nt_object,
  'nt_whites+',
  (   "."
  ->  nt_end,
      nt_turtle_like(Format, Options)
  ;   ";"
  ->  nt_end,
      nt_turtle_like(Format, Options)
  ;   iriref, nt_end
  ->  {Format = nquads}
  ).
turtle_like(Format, Options) -->    % starts with a blank node
  "[", !,
  turtle_or_trig(Format, Options).
turtle_like(Format, Options) -->      % starts with a collection
  "(", !,
  turtle_or_trig(Format, Options).

turtle_keyword(base).
turtle_keyword(prefix).

%%  turtle_or_trig(-Format, +Options)//
%
%  The file starts with a Turtle construct.   It can still be TriG.
%  We trust the content type and otherwise  we assume TriG if there
%  is a "{" in the first section of the file.

turtle_or_trig(Format, Options) -->
  { option(format(Format), Options),
    turtle_or_trig(Format)
  }, !.
turtle_or_trig(Format, _Options) -->
  (   ..., "{"
  ->  {Format = trig}
  ;   {Format = turtle}
  ).

turtle_or_trig(turtle).
turtle_or_trig(trig).

... --> "" | [_], ... .

%%  nt_turtle_like(-Format, +Options)//
%
%  We found a fully qualified triple.  This still can be Turtle,
%  TriG, ntriples and nquads.

nt_turtle_like(Format, Options) -->
  { option(format(Format), Options),
    nt_turtle_like(Format)
  }, !.
nt_turtle_like(ntriples, _) -->
  "".

nt_turtle_like(turtle).
nt_turtle_like(trig).
nt_turtle_like(ntriples).
nt_turtle_like(nquads).

iriref --> "<", iri_codes, ">".

iriref_pred --> `a`.
iriref_pred --> iriref.

nt_object --> iriref, !.
nt_object -->
  nt_string,
  (   "^^"
  ->  iriref
  ;   "@"
  ->  langtag
  ;
    {true}
  ).

iri_codes --> iri_code, !, iri_codes.
iri_codes --> [].

iri_code -->
  [C],
  { (   C =< 0'\s
    ;   no_iri_code(C)
    ), !, fail
  }.
iri_code -->
  "\\",
  (   "u"
  ->  xdigit4
  ;   "U"
  ->  xdigit8
  ).
iri_code -->
  [_].

langtag --> az, azs, sublangs.

sublangs --> "-", !, azd, azds, sublangs.
sublangs --> "".

az   --> [C], { between(0'a,0'z,C) ; between(0'A,0'Z,C) }, !.
azd  --> [C], { between(0'a,0'z,C) ; between(0'A,0'Z,C), between(0'0,0'9,C) }, !.
azs  --> az, !, azs | "".
azds --> azd, !, azds | "".

term_expansion(no_iri_code(x), Clauses) :-
  findall(no_iri_code(C),
    string_code(_,"<>\"{}|^`\\",C),
    Clauses).
term_expansion(echar(x), Clauses) :-
  findall(echar(C),
    string_code(_,"tbnrf\"\'\\",C),
    Clauses).

no_iri_code(x).
echar(x).

xdigit2 --> xdigit(_), xdigit(_).
xdigit4 --> xdigit2, xdigit2.
xdigit8 --> xdigit4, xdigit4.

nt_string --> "\"", nt_string_codes, "\"".

nt_string_codes --> string_code, !, nt_string_codes.
nt_string_codes --> [].

string_code --> "\"", !, {fail}.
string_code --> "\n", !, {fail}.
string_code --> "\r", !, {fail}.
string_code --> "\\", !,
  (   "u"
  ->  xdigit4
  ;   "U"
  ->  xdigit8
  ;   [C],
      {echar(C)}
  ).
string_code --> [_].

'nt_whites+' --> nt_white, 'nt_whites*'.
'nt_whites*' --> nt_white, 'nt_whites*'.
'nt_whites*' --> [].

nt_white --> [10], !.
nt_white --> [13], !.
nt_white --> white, !.
nt_white, " " --> "#", string(_), ( eol1 ; eos ), !.

nt_end -->
  (   eol
  ->  []
  ;   eos
  ).

eol --> eol1, eols.

eol1 --> "\n".
eol1 --> "\r".

eols --> eol1, !, eols.
eols --> [].


     /*******************************
     *        READ XML    *
     *******************************/

%%  guess_xml_type(+Stream, -ContentType) is semidet.
%
%  Try to see whether the document is some  form of HTML or XML and
%  in particular whether it is  RDF/XML.   The  latter is basically
%  impossible because it is not obligatory  for an XML/RDF document
%  to have an rdf:RDF top level  element,   and  when using a typed
%  node, just about anything can  qualify   for  RDF. The only real
%  demand is the XML document uses XML namespaces because these are
%  both required to define <rdf:Description> and   a valid type IRI
%  from a typed node.
%
%  If the toplevel element is detected as =HTML=, we pass =rdfa= as
%  type.

guess_xml_type(Stream, ContentType) :-
  xml_doctype(Stream, Dialect, DocType, Attributes),
  once(doc_content_type(Dialect, DocType, Attributes, ContentType)).

doc_content_type(_,   html, _, rdfa).
doc_content_type(html,   _,    _, rdfa).
doc_content_type(xhtml,   _,    _, rdfa).
doc_content_type(html5,   _,    _, rdfa).
doc_content_type(xhtml5, _,    _, rdfa).
doc_content_type(Dialect, Top, Attributes, xml) :-
  (
    Dialect == sgml
  ->
    atomic_list_concat([NS,rdf], :, Top)
  ;
    Dialect == xml
  ->
    atomic_list_concat([NS,'RDF'], :, Top)
  ),
  atomic_list_concat([xmlns,NS], :, Attr),
  memberchk(Attr=RDFNS, Attributes),
  rdf_current_prefix(rdf, RDFNS).


%%  xml_doctype(+Stream, -Dialect, -DocType, -Attributes) is semidet.
%
%  Parse a _repositional_ stream and get the  name of the first XML
%  element *and* demand that this   element defines XML namespaces.
%  Fails if the document is illegal XML before the first element.
%
%  Note that it is not  possible   to  define valid RDF/XML without
%  namespaces, while it is not possible  to define a valid absolute
%  Turtle URI (using <URI>) with a valid xmlns declaration.

xml_doctype(Stream, Dialect, DocType, Attributes) :-
  catch(setup_call_cleanup(
      make_parser(Stream, Parser, State),
      sgml_parse(Parser,
           [ source(Stream),
             max_errors(100),
             syntax_errors(quiet),
             call(begin, on_begin),
             call(cdata, on_cdata)
           ]),
      cleanup_parser(Stream, Parser, State)),
        E, true),
  nonvar(E),
  E = tag(Dialect, DocType, Attributes).

make_parser(Stream, Parser, state(Pos)) :-
  stream_property(Stream, position(Pos)),
  new_sgml_parser(Parser, []).

cleanup_parser(Stream, Parser, state(Pos)) :-
  free_sgml_parser(Parser),
  set_stream_position(Stream, Pos).

on_begin(Tag, Attributes, Parser) :-
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Tag, Attributes)).

on_cdata(_CDATA, _Parser) :-
  throw(error(cdata)).


     /*******************************
     *      DCG BASICS    *
     *******************************/

skip_line --> eol, !.
skip_line --> [_], skip_line.

%%  icase_keyword(-Keyword)// is semidet.
%
%  True when Keyword is an atom representing a non-empty sequence of
%  alphanumeric characters, converted to lowercase.

icase_keyword(Keyword) -->
  alpha_to_lower(H),
  alpha_to_lowers(T),
  { atom_codes(Keyword, [H|T])
  }.

alpha_to_lowers([H|T]) -->
  alpha_to_lower(H), !,
  alpha_to_lowers(T).
alpha_to_lowers([]) -->
  [].

%%  open_binary_string_stream(+String, -Stream) is det.
%
%  True when Stream is  a  binary   stream  holding  the context of
%  String.

open_binary_string_stream(String, Stream) :-
  new_memory_file(MF),
  setup_call_cleanup(
      open_memory_file(MF, write, Out, [encoding(octet)]),
      format(Out, '~s', [String]),
      close(Out)),
  open_memory_file(MF, read, Stream,
       [ free_on_close(true),
         encoding(octet)
       ]).
