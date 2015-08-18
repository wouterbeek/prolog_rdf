:- module(
  rdf_guess_format,
  [
    rdf_guess_format/2, % +File:atom
                        % -Format:rdf_format
    rdf_guess_format/4 % +In:stream
                       % ?FileExtension:atom,
                       % ?ContentType:atom,
                       % -Format:rdf_format
  ]
).

/** <module> RDF guess format

Guesses the RDF serialization format of a given stream.

@author Jan Wielemaker
@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).

:- meta_predicate(nt_string_codes(//,?,?)).

:- multifile(error:has_type/2).

error:has_type(rdf_format, T):-
  error:has_type(oneof([nquads,ntriples,turtle,trig]), T).





%! rdf_guess_format(+File:atom, -Format:rdf_format) is det.
% True when `Source` is thought to contain RDF data using the
% indicated content type.
%
% `Source` is either a stream or a file name.
%
% The following options are processed:
%   * `format(+Format)`
%     The guessed RDF serialization format,
%     e.g. based on the media type and/or file name.
%   * `look_ahead(+NumberOfBytes:nonneg)`
%     Look ahead the indicated amount
%
% @throws no_rdf If no RDF serialization format can be recognized.

rdf_guess_format(File0, Format):-
  % Make sure the file exists and we have read access to it.
  absolute_file_name(File0, File, [access(read)]),

  % Take the file extension into account, if any.
  file_name_extension(_, FileExt0, File),
  (FileExt0 == '' ->  true ; FileExt = FileExt0),

  setup_call_cleanup(
    open(File, read, Read),
    rdf_guess_format(Read, FileExt, _, Format),
    close(Read)
  ).

%! rdf_guess_format0(
%!   +Read:stream,
%!   -Format:rdf_format,
%!   +Options:list(compound)
%! ) is det.

rdf_guess_format0(Read, Format, Opts):-
  rdf_guess_format0(Read, 0, Format, Opts).

%! rdf_guess_format0(
%!   +Read:stream,
%!   +Iteration:positive_integer,
%!   -Format:rdf_format,
%!   +Options:list(compound)
%! ) is det.

rdf_guess_format0(Read, Iteration, Format, Opts0):-
  % Peek a given number of bytes from stream.
  option(look_ahead(Bytes0), Opts0, 1000),
  Bytes is Bytes0 * 2^Iteration,
  peek_string(Read, Bytes, S),

  % Do not backtrack if the whole stream has been peeked.
  string_length(S, Length),
  (   (   Length =:= 0
      ;   Length < Bytes
      )
  ->  !,
      EoS = true
  ;   EoS = false
  ),
  merge_options([eos(EoS)], Opts0, Opts),

  % Try to parse the peeked string as Turtle- or XML-like.
  (   string_codes(S, Cs),
      phrase(turtle_like(Format, Opts), Cs, _)
  ->  true
  ;   setup_call_cleanup(
        new_memory_file(MemFile),
        (
          setup_call_cleanup(
            open_memory_file(MemFile, write, Out),
            format(Out, '~s', [String]),
            close(Out)
          ),
          setup_call_cleanup(
            open_memory_file(MemFile, read, In),
            guess_xml_type(In, Format),
            close(In)
          )
        ),
        free_memory_file(MemFile)
      )
  ), !.
rdf_guess_format0(Read, Iteration, Format, Opts):-
  Iteration < 4,
  NewIteration is Iteration + 1,
  rdf_guess_format0(Read, NewIteration, Format, Opts).


%! rdf_guess_format(
%!   +Read:stream,
%!   ?FileExtension:atom,
%!   ?ContentType:atom,
%!   -Format:rdf_format
%! ) is det.
% @throws no_rdf If no RDF serialization format can be recognized.

% Use the file extensions as the RDF serialization format suggestion.
rdf_guess_format(Read, FileExt, _, Format):-
  nonvar(FileExt),
  rdf_db:rdf_file_type(FileExt, SuggestedFormat), !,
  rdf_guess_format0(In, Format, [format(SuggestedFormat)]).
% Use the HTTP content type header as the RDF serialization format suggestion.
rdf_guess_format(In, _, ContentType, Format):-
  nonvar(ContentType),
  rdf_media_type(ContentType, SuggestedFormat), !,
  rdf_guess_format0(Read, Format, [format(SuggestedFormat)]).
% Use no RDF serialization format suggestion.
rdf_guess_format(Read, _, _, Format):-
  rdf_guess_format0(Read, Format, []), !.
rdf_guess_format(Read, _, _, _):-
  throw(error(no_rdf(Read))).

%! turtle_like(
%!   -Format:rdf_format,
%!   +Options:list(compound)
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

% Whenever the end-of-stream is reached we assume it is in a format
% belonging to the Turtle family.
% This e.g. allows Turtle files that consist of comments exclusively
% to be classified as such (and an empty file is a Turtle file
% as well).
turtle_like(Format, Options) -->
  dcg_end,
  {option(eos(true), Options)}, !,
  nt_turtle_or_trig(Format, Options).
turtle_like(Format, Options) -->
  blank, !, blanks,
  turtle_like(Format, Options).
% Turtle comment.
turtle_like(Format, Options) -->
  "#", !, skip_line,
  turtle_like(Format, Options).
% @BASE
% @PREFIX
turtle_like(Format, Options) -->
  "@", icase_keyword(Keyword), {turtle_keyword(Keyword)}, !,
  nt_turtle_or_trig(Format, Options).
% BASE
% PREFIX
turtle_like(Format, Options) -->
  icase_keyword(Keyword), {turtle_keyword(Keyword)}, blank, !,
  nt_turtle_or_trig(Format, Options).
% Turtle triple.
turtle_like(Format, Options) -->
  nt_subject,
  *('WS', []),
  nt_predicate,
  *('WS', []),
  nt_object,
  *('WS', []),
  (   "."
  ->  nt_end,
      nt_turtle_like(Format, Options)
  ;   ";"
  ->  nt_end,
      nt_turtle_or_trig(Format, Options)
  ;   nt_graph,
      *('WS', []),
      ".",
      nt_end
  ->  {Format = nquads}
  ).
% Turtle anonymous blank node.
turtle_like(Format, Options) -->
  "[", !,
  nt_turtle_or_trig(Format, Options).
% Turtle collection.
turtle_like(Format, Options) -->
  "(", !,
  nt_turtle_or_trig(Format, Options).

nt_bnode --> "_:", nt_bnode_codes.

nt_bnode_codes --> nt_bnode_code, !, nt_bnode_codes.
nt_bnode_codes --> [].

nt_bnode_code --> blank, !, {fail}.
nt_bnode_code --> [_].

nt_graph --> nt_iriref.

nt_iriref --> "<", nt_iri_codes, ">".

nt_iri_codes --> nt_iri_code, !, nt_iri_codes.
nt_iri_codes --> [].

nt_iri_code --> ">", !, {fail}.
nt_iri_code --> [_].

nt_langtag --> nt_langtag_codes.

nt_langtag_codes --> nt_langtag_code, !, nt_langtag_codes.
nt_langtag_codes --> [].

nt_langtag_code --> blank, !, {fail}.
nt_langtag_code --> [_].

nt_object --> nt_iriref, !.
nt_object --> nt_bnode, !.
nt_object -->
  nt_string,
  (   "^^"
  ->  nt_iriref
  ;   "@"
  ->  nt_langtag
  ;   ""
  ).

nt_predicate --> "a".
nt_predicate --> nt_iriref.

nt_string --> "'''", !, nt_string_codes([39,39,39]).
nt_string --> "'", !, nt_string_codes([39]).
nt_string --> "\"\"\"", !, nt_string_codes([34,34,34]).
nt_string --> "\"", !, nt_string_codes([34]).

nt_string_codes(End) --> "\\\'", nt_string_codes(End).
nt_string_codes(End) --> "\\\"", nt_string_codes(End).
nt_string_codes(End) --> End, !.
nt_string_codes(End) --> [_], !, nt_string_codes(End).
nt_string_codes(_) --> [].

nt_subject --> nt_iriref, !.
nt_subject --> nt_bnode.

'WS' --> [10], !.
'WS' --> [13], !.
'WS' --> white, !.
'WS', " " -->
  "#",
  string(_),
  (   eol1
  ;   eos
  ), !.

nt_end -->
  whites,
  (   eol
  ->  []
  ;   eos
  ).

eol --> eol1, eols.

eol1 --> "\n".
eol1 --> "\r".

eols --> eol1, !, eols.
eols --> [].

turtle_keyword(base).
turtle_keyword(prefix).

%! nt_turtle_or_trig(-Format, +Options)//
%
%  The file starts with a Turtle construct.   It can still be TriG.
%  We trust the content type and otherwise  we assume TriG if there
%  is a "{" in the first section of the file.

nt_turtle_or_trig(Format, Options) -->
  {
    option(format(Format), Options),
    nt_turtle_or_trig(Format)
  }, !.
nt_turtle_or_trig(Format, _) -->
  (   ..., "{"
  ->  {Format = trig}
  ;   {Format = turtle}
  ).

nt_turtle_or_trig(turtle).
nt_turtle_or_trig(trig).

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

... --> "" | [_], ... .

skip_line --> eol, !.
skip_line --> [_], skip_line.

%! icase_keyword(-Keyword)// is semidet.
%
%  True when Keyword is an atom representing a non-empty sequence of
%  alphanumeric characters, converted to lowercase.

icase_keyword(Keyword) -->
  alpha_to_lower(H),
  alpha_to_lowers(T),
  {atom_codes(Keyword, [H|T])}.

alpha_to_lowers([H|T]) -->
  alpha_to_lower(H), !,
  alpha_to_lowers(T).
alpha_to_lowers([]) -->
  [].
