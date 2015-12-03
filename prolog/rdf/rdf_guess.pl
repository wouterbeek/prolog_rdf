:- module(
  rdf_guess,
  [
    rdf_guess_format/2, % +Spec, -Format
    rdf_guess_format/3 % +Spec
                       % -Format:rdf_format
                       % +Options:list(compound)
  ]
).

/** <module> RDF guess

@author Wouter Beek
@author Jan Wielemaker
@version 2015/08-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(memfile)).
:- use_module(library(msg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(sgml/sgml_ext)).
:- use_module(library(typecheck)).

:- meta_predicate(nt_string_codes(//,?,?)).

:- predicate_options(rdf_guess_format/3, 3, [
     default_format(+rdf_format)
   ]).





%! rdf_guess_format(+Spec, -Format:rdf_term, +Options:list(compound)) is det.
% Wrapper around rdf_guess_format/3 with default options.

rdf_guess_format(Spec, Format):-
  rdf_guess_format(Spec, Format, []).


%! rdf_guess_format(+Spec, -Format:rdf_term, +Options:list(compound)) is det.
% The following options are supported:
%   * default_format(+rdf_format)

rdf_guess_format(Spec, Format, Opts):-
  setup_call_cleanup(
    open_any2(Spec, read, Read, Close_0),
    rdf_guess_format(Read, 0, Format, Opts),
    close_any2(Close_0)
  ).


%! rdf_guess_format(
%!   +Read:stream,
%!   +Iteration:nonneg,
%!   -Format:rdf_format,
%!   +Options:list(compound)
%! ) is det.

rdf_guess_format(Read, Iteration, Format, Opts):-
  N is 1000 * 2 ^ Iteration,
  peek_string(Read, N, S),
  debug(rdf(guess), "[RDF-GUESS] ~s", [S]),

  % Try to parse the peeked string as Turtle- or XML-like.
  (   rdf_guess_turtle(S, N, Format, Opts)
  ->  true
  ;   rdf_guess_xml(S, Format)
  ), !,

  debug(rdf(guess), "Assuming ~a based on heuristics.", [Format]).
rdf_guess_format(Read, Iteration, Format, Opts):-
  Iteration < 4,
  NewIteration is Iteration + 1,
  rdf_guess_format(Read, NewIteration, Format, Opts).

rdf_guess_turtle(S, N, Format, Opts):-
  % Do not backtrack if the whole stream has been peeked.
  string_length(S, M),
  (   (   M =:= 0
      ;   M < N
      )
  ->  !,
      EoS = true
  ;   EoS = false
  ),
  string_phrase(rdf_guess_turtle(EoS, Format, Opts), S, _).

rdf_guess_xml(S, Format):-
  setup_call_cleanup(
    new_memory_file(MFile),
    (
      setup_call_cleanup(
        open_memory_file(MFile, write, Write),
        format(Write, '~s', [S]),
        close(Write)
      ),
     setup_call_cleanup(
        open_memory_file(MFile, read, Read),
        guess_xml_type(Read, Format),
        close(Read)
      )
    ),
    free_memory_file(MFile)
  ).



%! rdf_guess_turtle(
%!   +EoS:boolean,
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
% This e.g. allows empty files or files that only consist of Turtle comments
% to be classified as such.
% as well).
rdf_guess_turtle(true, Format, Opts) -->
  eos, !,
  guess_turtle_or_trig(Format, Opts).
% Skip blanks.
rdf_guess_turtle(EoS, Format, Opts) -->
  blank, !, blanks,
  rdf_guess_turtle(EoS, Format, Opts).
% Turtle comment.
rdf_guess_turtle(EoS, Format, Opts) -->
  "#", !, skip_line,
  rdf_guess_turtle(EoS, Format, Opts).
% @BASE
% @PREFIX
rdf_guess_turtle(_, Format, Opts) -->
  "@", turtle_keyword, !,
  guess_turtle_or_trig(Format, Opts).
% BASE
% PREFIX
rdf_guess_turtle(_, Format, Opts) -->
  turtle_keyword, blank, !,
  guess_turtle_or_trig(Format, Opts).
% Turtle triple.
rdf_guess_turtle(_, Format, Opts) -->
  nt_subject(SFormat),
  *('WS'),
  nt_predicate(PFormat),
  *('WS'),
  nt_object(OFormat),
  *('WS'),
  (   % End of triple.
      "."
  ->  {exclude(var, [SFormat,PFormat,OFormat], Formats),
       guess_turtle_family(Formats, Format, Opts)}
  ;   % Object list notation.
      ";"
  ->  guess_turtle_or_trig(Format, Opts)
  ;   % Predicate-Object pairs list notation.
      ","
  ->  guess_turtle_or_trig(Format, Opts)
  ;   % End of quadruple.
      nt_graph,
      *('WS'),
      "."
  ->  {Format = nquads}
  ).
% Anonymous blank node.
rdf_guess_turtle(_, Format, Opts) -->
  "[", !,
  guess_turtle_or_trig(Format, Opts).
% RDF collection.
rdf_guess_turtle(_, Format, Opts) -->
  "(", !,
  guess_turtle_or_trig(Format, Opts).

nt_bnode --> "_:", *(nonblank).

nt_graph --> nt_iriref(_).

nt_iriref(Format) -->
  "<", !, ...(Cs), ">", !,
  {atom_codes(Iri, Cs), (is_iri(Iri) -> true ; Format = turtleOrTrig)}.
nt_iriref(_) -->
  nt_iriref_prefix, ":", *(nonblank).

nt_iriref_prefix --> ":", !, {fail}.
nt_iriref_prefix --> blank, !, {fail}.
nt_iriref_prefix --> [_], nt_iriref_prefix.
nt_iriref_prefix --> "".

nt_ltag --> *(nonblank).

nt_object(OFormat) --> nt_iriref(OFormat), !.
nt_object(_) --> nt_bnode, !.
nt_object(_) --> nt_string, ("^^" -> nt_iriref(_) ; "@" -> nt_ltag ; "").

nt_predicate(PFormat) --> nt_iriref(PFormat), !.
nt_predicate(turtleOrTrig) --> "a".

nt_string --> "'''", !, nt_string_codes([39,39,39]).
nt_string --> "'", !, nt_string_codes([39]).
nt_string --> "\"\"\"", !, nt_string_codes([34,34,34]).
nt_string --> "\"", nt_string_codes([34]).

nt_string_codes(End) --> "\\\'", nt_string_codes(End).
nt_string_codes(End) --> "\\\"", nt_string_codes(End).
nt_string_codes(End) --> End, !.
nt_string_codes(End) --> [_], !, nt_string_codes(End).
nt_string_codes(_) --> "".

nt_subject(SFormat) --> nt_iriref(SFormat), !.
nt_subject(_) --> nt_bnode.

'WS' --> blank, !.
'WS', " " --> "#", ..., (eol ; eos), !.

turtle_keyword -->
  atom_lower(A), !,
  {turtle_keyword(A)}.

turtle_keyword(base).
turtle_keyword(prefix).


%! guess_turtle_or_trig(-Format:rdf_format, +Options:list(compound))// is det.
% The file starts with a Turtle construct.
% It can still be TriG.
% We trust the content type and otherwise we assume TriG if there
% is a "{" in the first section of the file.

guess_turtle_or_trig(Format, Opts) -->
  (  {option(default_format(Format0), Opts),
      ground(Format0)}
  -> {must_be(oneof([trig,turtle]), Format0),
      Format = Format0}
  ;   ..., "{"
  -> {Format = trig}
  ;  {Format = turtle}
  ).


%! guess_turtle_family(
%!   +Formats:list(oneof([nquads,ntriples,trig,turtle])),
%!   -Format:rdf_format,
%!   +Options:list(compound)
%! )// is det.
% We found a fully qualified triple.
% This still can be Turtle, TriG, N-Triples or N-Quads.

guess_turtle_family(Formats, Format, Opts):-
  memberchk(turtleOrTrig, Formats), !,
  (   option(default_format(Format0), Opts),
      ground(Format0)
  ->  must_be(oneof([trig,turtle]), Format0),
      Format = Format0
  ;   debug(rdf(guess), 'Assuming Turtle based on heuristics.', []),
      Format = turtle
  ).
guess_turtle_family(_, Format, Opts):-
  (   option(default_format(Format0), Opts),
      ground(Format0)
  ->  must_be(oneof([nquads,ntriples,trig,turtle]), Format0),
      Format = Format0
  ;   debug(rdf(guess), 'Assuming N-Triples based on heuristics.', []),
      Format = ntriples
  ).



%! guess_xml_type(+Read:stream, -Format:rdf_format) is semidet.
% Try to see whether the document is some form of HTML or XML and in particular
% whether it is  RDF/XML.
% The latter is basically impossible because it is not obligatory for
% an RDF/XML document to have an rdf:RDF top level  element, and  when using
% a typed node, just about anything can qualify for RDF.
% The only real demand is the XML document uses XML namespaces because these
% are both required to define <rdf:Description> and a valid type IRI from a
% typed node.
%
% If the toplevel element is detected as =HTML=, we pass =rdfa= as type.

guess_xml_type(Read, Format):-
  xml_doctype(Read, Dialect, DocType, Attrs),
  once(doc_content_type(Dialect, DocType, Attrs, Format)).

doc_content_type(_     , html, _, rdfa).
doc_content_type(html  , _   , _, rdfa).
doc_content_type(xhtml , _   , _, rdfa).
doc_content_type(html5 , _   , _, rdfa).
doc_content_type(xhtml5, _   , _, rdfa).
doc_content_type(Dialect, Top, Attrs, xml):-
  (   Dialect == sgml
  ->  atomic_list_concat([NS,rdf], :, Top)
  ;   Dialect == xml
  ->  atomic_list_concat([NS,'RDF'], :, Top)
  ),
  atomic_list_concat([xmlns,NS], :, Attr),
  memberchk(Attr=RDFNS, Attrs),
  rdf_current_prefix(rdf, RDFNS).

%! xml_doctype(+Read:stream, -Dialect, -DocType, -Attributes) is semidet.
% Parse a _repositional_ stream and get the name of the first XML
% element *and* demand that this element defines XML namespaces.
% Fails if the document is illegal XML before the first element.
%
% Note that it is not possible to define valid RDF/XML without
% namespaces, while it is not possible to define a valid absolute
% Turtle IRI (using `<...>`-notation) with a valid xmlns declaration.

xml_doctype(Read, Dialect, DocType, Attrs):-
  catch(
    sgml_parser(
      Read,
      sgml_parser0([
        call(begin, on_begin),
        call(cdata, on_cdata),
        max_errors(-1),
        source(Read),
        syntax_errors(quiet)
      ])
    ),
    E,
    true
  ),
  nonvar(E),
  E = tag(Dialect, DocType, Attrs).

sgml_parser0(Opts, Parser):-
  sgml_parse(Parser, Opts).

on_begin(Tag, Attrs, Parser):-
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Tag, Attrs)).

on_cdata(_, _):-
  throw(error(cdata)).
