:- module(
  rdf_guess_format,
  [
    rdf_guess_format/2, % +File:atom
                        % -Format:rdf_format
    rdf_guess_format/4 % +Read:blob
                       % +FileExtension:atom,
                       % +ContentType:atom,
                       % -Format:rdf_format
  ]
).

/** <module> RDF detect

Detect the RDF serialization format of a given stream.

@author Jan Wielemaker
@author Wouter Beek
@version 2014/04-2014/05, 2014/07-2014/08, 2014/10
*/

:- use_module(library(dcg/basics)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(sgml)).

:- use_module(plRdf(management/rdf_file_db)).



%! rdf_guess_format(+File:atom, -Format:rdf_format) is semidet.
% True when `Source` is thought to contain RDF data using the
% indicated content type.
%
% `Source` is either a stream or a file name.
%
% The following options are processed:
%   - `format(+Format)`
%     The guessed RDF serialization format,
%     e.g. based on the media type and/or file name.
%   - `look_ahead(+NumberOfBytes:nonneg)`
%     Look ahead the indicated amount

rdf_guess_format(File0, Format):-
  % Make sure the file exists and we have read access to it.
  absolute_file_name(File0, File, [access(read)]),

  % Take the file extension into account, if any.
  file_name_extension(File, FileExtension0, _),
  (   FileExtension0 == ''
  ->  true
  ;   FileExtension = FileExtension0
  ),
  
  setup_call_cleanup(
    open(File, read, Stream),
    rdf_guess_format(Stream, FileExtension, _, Format),
    close(Stream)
  ).


%! rdf_guess_format0(
%!   +Stream:stream,
%!   -Format:rdf_format,
%!   +Options:list(nvpair)
%! ) is det.

rdf_guess_format0(Stream, Format, Options):-
  rdf_guess_format0(Stream, 1, Format, Options).

%! rdf_guess_format0(
%!   +Stream:stream,
%!   +Iteration:positive_integer,
%!   -Format:rdf_format,
%!   +Options:list(nvpair)
%! ) is det.

rdf_guess_format0(Stream, Iteration, Format, Options):-
  % Peek a given number of bytes from stream.
  option(look_ahead(Bytes0), Options, 1000),
  Bytes is Iteration * Bytes0,
  peek_string(Stream, Bytes, String),
  
  % Do not backtrack if the whole stream has been peeked.
  string_length(String, Length),
  (   Length < Bytes
  ->  !
  ;   true
  ),
  
  % Try to parse the peeked string as Turtle- or XML-like.
  (   string_codes(String, Codes),
      phrase(turtle_like(Format, Options), Codes, _)
  ->  true
  ;   setup_call_cleanup(
        new_memory_file(MemFile),
        (
          setup_call_cleanup(
            open_memory_file(MemFile, write, Write),
            format(Write, '~s', [String]),
            close(Write)
          ),
          setup_call_cleanup(
            open_memory_file(MemFile, read, Read),
            guess_xml_type(Read, Format),
            close(Read)
          )
        ),
        free_memory_file(MemFile)
      )
  ), !.
rdf_guess_format0(Stream, Iteration, Format, Options):-
  NewIteration is Iteration + 1,
  rdf_guess_format0(Stream, NewIteration, Format, Options).


%! rdf_guess_format(
%!   +Read:blob,
%!   +FileExtension:atom,
%!   +ContentType:atom,
%!   -Format:rdf_format
%! ) is semidet.
% Fails if the RDF serialization format cannot be decided on.

% Use the file extensions as the RDF serialization format suggestion.
rdf_guess_format(Read, FileExtension, _, Format):-
  nonvar(FileExtension),
  rdf_db:rdf_file_type(FileExtension, SuggestedFormat), !,
  rdf_guess_format0(Read, Format, [format(SuggestedFormat)]).
% Use the HTTP content type header as the RDF serialization format suggestion.
rdf_guess_format(Read, _, ContentType, Format):-
  nonvar(ContentType),
  rdf_media_type_format(ContentType, SuggestedFormat), !,
  rdf_guess_format0(Read, Format, [format(SuggestedFormat)]).
% Use no RDF serialization format suggestion.
rdf_guess_format(Read, _, _, Format):-
  rdf_guess_format0(Read, Format, []), !.


%! turtle_like(
%!   -Format:oneof([nquads,ntriples,turtle,trig]),
%!   +Options:list(nvpair)
%! )// is semidet.
% True if the start of the input matches a turtle-like language.
% There are four of them:
%   1. Turtle
%   2. TRiG
%   3. ntriples
%   4. nquads.
%
% The first three can all be handled by the turtle parser,
% so it oesn't matter too much.

turtle_like(Format, Options) -->
  blank, !, blanks,
  turtle_like(Format, Options).
turtle_like(Format, Options) -->
  "#", !, skip_line,
  turtle_like(Format, Options).
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

%! turtle_or_trig(-Format, +Options)//
%
%  The file starts with a Turtle construct.   It can still be TriG.
%  We trust the content type and otherwise  we assume TriG if there
%  is a "{" in the first section of the file.

turtle_or_trig(Format, Options) -->
  {
    option(format(Format), Options),
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

%! nt_turtle_like(
%!   -Format:oneof([nquads,ntriples,turtle,trig]),
%!   +Options:list(nvpair)
%! )//
% We found a fully qualified triple.
% This still can be Turtle, TriG, N-Triples or N-Quads.

nt_turtle_like(Format, Options) -->
  {
    option(format(Format), Options),
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

term_expansion(no_iri_code(x), Clauses):-
  findall(no_iri_code(C),
    string_code(_,"<>\"{}|^`\\",C),
    Clauses).
term_expansion(echar(x), Clauses):-
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
nt_white, " " -->
  "#",
  string(_),
  (   eol1
  ;   eos
  ), !.

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

%! guess_xml_type(+Stream, -Format:oneof([rdfs,xml])) is semidet.
%
%  Try to see whether the document is some  form of HTML or XML and
%  in particular whether it is  RDF/XML.   The  latter is basically
%  impossible because it is not obligatory  for an RDF/XML document
%  to have an rdf:RDF top level  element,   and  when using a typed
%  node, just about anything can  qualify   for  RDF. The only real
%  demand is the XML document uses XML namespaces because these are
%  both required to define <rdf:Description> and   a valid type IRI
%  from a typed node.
%
%  If the toplevel element is detected as =HTML=, we pass =rdfa= as
%  type.

guess_xml_type(Stream, Format):-
  xml_doctype(Stream, Dialect, DocType, Attributes),
  once(doc_content_type(Dialect, DocType, Attributes, Format)).

doc_content_type(_,   html, _, rdfa).
doc_content_type(html,   _,    _, rdfa).
doc_content_type(xhtml,   _,    _, rdfa).
doc_content_type(html5,   _,    _, rdfa).
doc_content_type(xhtml5, _,    _, rdfa).
doc_content_type(Dialect, Top, Attributes, xml):-
  (   Dialect == sgml
  ->  atomic_list_concat([NS,rdf], :, Top)
  ;   Dialect == xml
  ->  atomic_list_concat([NS,'RDF'], :, Top)
  ),
  atomic_list_concat([xmlns,NS], :, Attr),
  memberchk(Attr=RDFNS, Attributes),
  rdf_current_prefix(rdf, RDFNS).


%! xml_doctype(+Stream, -Dialect, -DocType, -Attributes) is semidet.
%
%  Parse a _repositional_ stream and get the  name of the first XML
%  element *and* demand that this   element defines XML namespaces.
%  Fails if the document is illegal XML before the first element.
%
%  Note that it is not  possible   to  define valid RDF/XML without
%  namespaces, while it is not possible  to define a valid absolute
%  Turtle URI (using <URI>) with a valid xmlns declaration.

xml_doctype(Stream, Dialect, DocType, Attributes):-
  catch(
    setup_call_cleanup(
      make_parser(Stream, Parser, State),
      (
        sgml_parse(
          Parser,
          [
            call(begin, on_begin),
            call(cdata, on_cdata),
            max_errors(-1),
            source(Stream),
            syntax_errors(quiet)
          ]
        )
      ),
      cleanup_parser(Stream, Parser, State)
    ),
    E,
    true
  ),
  nonvar(E),
  E = tag(Dialect, DocType, Attributes).

make_parser(Stream, Parser, state(Pos)):-
  stream_property(Stream, position(Pos)),
  new_sgml_parser(Parser, []).

cleanup_parser(Stream, Parser, state(Pos)):-
  free_sgml_parser(Parser),
  set_stream_position(Stream, Pos).

on_begin(Tag, Attributes, Parser):-
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Tag, Attributes)).

on_cdata(_CDATA, _Parser):-
  throw(error(cdata)).


     /*******************************
     *      DCG BASICS    *
     *******************************/

skip_line --> eol, !.
skip_line --> [_], skip_line.

%! icase_keyword(-Keyword)// is semidet.
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

