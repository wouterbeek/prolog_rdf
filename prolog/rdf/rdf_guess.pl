:- module(
  rdf_guess,
  [
    rdf_guess_media_type/2,        % +Source, -MT
    rdf_guess_media_type/3,        % +Source, -MT, +Opts
    rdf_guess_media_type_stream/2, % +In, -MT
    rdf_guess_media_type_stream/3  % +In, -MT, +Opts
  ]
).

/** <module> RDF guess

The following debug flag is used:
  - rdf_guess

@author Wouter Beek
@author Jan Wielemaker
@version 2016/03-2017/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(memfile)).
:- use_module(library(option_ext)).
:- use_module(library(ordsets)).
:- use_module(library(os/io)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml/sgml_ext)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- meta_predicate
    turtle_string_codes(//, ?, ?).





%! rdf_guess_media_type(+Source, -MT) is det.
%! rdf_guess_media_type(+Source, -MT, +Opts) is det.
%
% The following options are supported:
%
%   * default_rdf_media_type(+rdf_media_type)

rdf_guess_media_type(Source, MT) :-
  rdf_guess_media_type(Source, MT, []).


rdf_guess_media_type(Source, MT, Opts) :-
  call_on_stream(
    Source,
    {MT,Opts}/[In,InPath,InPath]>>rdf_guess_media_type_stream(In, MT, Opts)
  ).



%! rdf_guess_media_type_stream(+In, -MT) is det.
%! rdf_guess_media_type_stream(+In, -MT, +Opts) is det.

rdf_guess_media_type_stream(In, MT) :-
  rdf_guess_media_type_stream(In, MT, []).


rdf_guess_media_type_stream(In, MT, Opts) :-
  rdf_guess_media_type_stream(In, 1, MT, Opts).

rdf_guess_media_type_stream(In, I, MT, Opts) :-
  N is 1000 * I,
  peek_string(In, N, Str),
  debug(rdf_guess, "[RDF-GUESS] ~s", [Str]),
  % Try to parse the peeked string as Turtle- or XML-family.
  (   rdf_guess_jsonld(Str, N),
      MT = application/'ld+json'
  ;   rdf_guess_turtle(Str, N, MT, Opts)
  ;   rdf_guess_xml(Str, MT)
  ), !,
  debug(rdf_guess, "Assuming ~w based on heuristics.", [MT]).
rdf_guess_media_type_stream(In, I1, MT, Opts) :-
  I1 =< 4,
  I2 is I1 + 1,
  rdf_guess_media_type_stream(In, I2, MT, Opts).

% For Turtle-family media_types it matters whether or not the end of
% stream has been reached.
rdf_guess_turtle(Str, N, MT, Opts) :-
  % Do not backtrack if the whole stream has been peeked.
  string_length(Str, M),
  ((M =:= 0 ; M < N) -> !, EoS = true ; EoS = false),
  string_phrase(rdf_guess_turtle(EoS, MT, Opts), Str, _).





% JSON-LD %

rdf_guess_jsonld(S, N) :-
  % Do not backtrack if the whole stream has been peeked.
  string_length(S, M),
  ((M =:= 0 ; M < N) -> !, true ; true),
  string_phrase(rdf_guess_jsonld_grammar, S, _).

rdf_guess_jsonld_grammar -->
  *(bs),
  optional_arrays,
  "{",
  *(bs),
  jsonld_string,
  *(bs),
  ":", !.

optional_arrays -->
  "[", !,
  *(bs),
  optional_arrays.
optional_arrays --> "".

% Start of jsonld_string.
jsonld_string --> "\"", jsonld_string0.

% Escaped double quote.
jsonld_string0 --> "\\\"", !, jsonld_string0.
% End of jsonld_string.
jsonld_string0 --> "\"", !.
% Middle of jsonld_string.
jsonld_string0 --> [_], !, jsonld_string0.




% TURTLE FAMILY %

%! rdf_guess_turtle(+EoS:boolean, -MT, +Opts)// is semidet.
%
% True if the start of the input matches a Turtle-like language.
%
% These are Turtle-like Media Types:
%
%   1. application/turtle
%
%   2. application/trig
%
%   3. application/'n-triples'
%
%   4. application/'n-quads'
%
% The first three can all be handled by the turtle parser,
% so it oesn't matter too much.

rdf_guess_turtle(EoS, application/Subtype, Opts) -->
  rdf_guess_turtle_subtype(EoS, Subtype, Opts).

% Whenever the end-of-stream is reached we assume it is in a format
% belonging to the Turtle family.  This e.g. allows empty files or
% files that only consist of Turtle comments to be classified as
% Turtle-family files as well.
rdf_guess_turtle_subtype(true, Subtype, Opts) -->
  eos, !,
  {guess_turtle_subtype([], Subtype, Opts)}.
% Skip blanks.
rdf_guess_turtle_subtype(EoS, Subtype, Opts) -->
  blank, !, blanks,
  rdf_guess_turtle_subtype(EoS, Subtype, Opts).
% Turtle comment.
rdf_guess_turtle_subtype(EoS, Subtype, Opts) -->
  "#", !, skip_line,
  rdf_guess_turtle_subtype(EoS, Subtype, Opts).
% @BASE
% @PREFIX
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "@", turtle_keyword, !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.
% BASE
% PREFIX
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  turtle_keyword, blank, !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.
% Default graph.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "{", !,
  {guess_turtle_subtype([trig], Subtype, Opts)}.
% Named graph.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  turtle_iriref(_), *(bs), "{", !,
  {guess_turtle_subtype([trig], Subtype, Opts)}.
% Tuple.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  turtle_subject(Subtypes1),
  *(bs),
  turtle_predicate(Subtypes2),
  *(bs),
  turtle_object(Subtypes3),
  *(bs),
  (   % End of a triple.
      "."
  ->  {ord_union([Subtypes1,Subtypes2,Subtypes3], Subtypes)}
  ;   % Object list notation.
      ";"
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,['n-quads','n-triples']],
          Subtypes
        )
      }
  ;   % Predicate-Object pairs list notation.
      ","
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,['n-quads','n-triples']],
          Subtypes
        )
      }
  ;   % End of a quad.
      turtle_graph(Subtypes4),
      *(bs),
      "."
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,Subtypes4,['n-triples',trig,turtle]],
          Subtypes
        )
      }
  ),
  {guess_turtle_subtype(Subtypes, Subtype, Opts)}.
% Anonymous blank node.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "[", !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.
% RDF collection.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "(", !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.

turtle_bnode --> "_:", *(nonblank).

turtle_graph(Subtypes) --> turtle_iriref(Subtypes).

turtle_iriref([]) -->
  "<", !, ..., ">", !.
turtle_iriref(['n-quads','n-triples']) -->
  turtle_iriref_prefix, ":", *(nonblank).

turtle_iriref_prefix --> ":", !, {fail}.
turtle_iriref_prefix --> blank, !, {fail}.
turtle_iriref_prefix --> [_], turtle_iriref_prefix.
turtle_iriref_prefix --> "".

turtle_ltag --> *(nonblank).

turtle_object(Subtypes) --> turtle_iriref(Subtypes), !.
turtle_object([]) --> turtle_bnode, !.
turtle_object(Subtypes) -->
  turtle_string(Subtypes1), *(bs),
  (   "^^"
  ->  *(bs),
      turtle_iriref(Subtypes2)
  ;   "@"
  ->  turtle_ltag,
      {Subtypes2 = []}
  ;   {Subtypes2 = []}
  ),
  {ord_union([Subtypes1,Subtypes2], Subtypes)}.

turtle_predicate(Subtypes) --> turtle_iriref(Subtypes), !.
turtle_predicate(['n-quads','n-triples']) --> "a".

% Triple single quotes.
turtle_string(['n-quads','n-triples']) -->
  "'''", !,
  turtle_string_codes([0'',0'',0'']).
% Single single quotes.
turtle_string(['n-quads','n-triples']) -->
  "'", !,
  turtle_string_codes([0'']).
% Triple double quotes.
turtle_string(['n-quads','n-triples']) -->
  "\"\"\"", !,
  turtle_string_codes([0'",0'",0'"]). %"
% Single double quotes.
turtle_string([]) -->
  "\"",
  turtle_string_codes([0'"]). %"

% Escaped single quote.
turtle_string_codes(End) --> "\\\'", !, turtle_string_codes(End).
% Escaped double quote.
turtle_string_codes(End) --> "\\\"", !, turtle_string_codes(End).
% End of string.
turtle_string_codes(End) --> End,    !.
% Content.
turtle_string_codes(End) --> [_],    !, turtle_string_codes(End).
% End of stream.
turtle_string_codes(_)   --> "".

turtle_subject(Subtypes) --> turtle_iriref(Subtypes), !.
turtle_subject([]) --> turtle_bnode.

bs0 --> bs, !.
bs0, " " --> "#", ..., (eol ; eos), !.

turtle_keyword --> "base".
turtle_keyword --> "prefix".

%! guess_turtle_subtype(+Excluded:ordset, -Subtype, +Opts)// is det.
%
% We found a fully qualified triple.  Determine whether it is Turtle,
% TriG, N-Triples or N-Quads.

guess_turtle_subtype(Excluded, Subtype, Opts) :-
  ord_subtract(['n-quads','n-triples',trig,turtle], Excluded, Subtypes),
  (   % Narrowed down to one.
      Subtypes = [Subtype]
  ->  true
  ;   % Multiple options: pick the one specified as an option, if any.
      option(default_rdf_media_type(application/Subtype0), Opts),
      ground(Subtype0),
      memberchk(Subtype0, Subtypes)
  ->  Subtype = Subtype0
  ;   % Multiple options: Trig is more general than Turtle.
      Subtype = trig,
      memberchk(Subtype, Subtypes)
  ;   % Multiple options: N-Quads is more general than N-Triples.
      Subtype = 'n-quads',
      memberchk(Subtype, Subtypes)
  ).





% XML FAMILY %

%! rdf_guess_xml(+Snippet, -MT) is det.

rdf_guess_xml(S, MT) :-
  setup_call_cleanup(
    new_memory_file(MFile),
    (
      setup_call_cleanup(
        open_memory_file(MFile, write, Write),
        format(Write, "~s", [S]),
        close(Write)
      ),
     setup_call_cleanup(
        open_memory_file(MFile, read, Read),
        guess_xml_type(Read, MT),
        close(Read)
      )
    ),
    free_memory_file(MFile)
  ).



%! guess_xml_type(+Read, -MT) is semidet.
%
% Try to see whether the document is some form of HTML or XML and in
% particular whether it is RDF/XML.  The latter is basically
% impossible because it is not obligatory for an RDF/XML document to
% have an rdf:RDF top level element, and when using a typed node, just
% about anything can qualify for RDF.  The only real demand is the XML
% document uses XML namespaces because these are both required to
% define <rdf:Description> and a valid type IRI from a typed node.
%
% If the toplevel element is detected as =HTML=, we pass =rdfa= as type.

guess_xml_type(Read, MT) :-
  xml_doctype(Read, Dialect, DocType, Attrs),
  doc_content_type(Dialect, DocType, Attrs, MT).



%! xml_doctype(+Read, -Dialect:atom, -DocType:atom, -Attrs) is semidet.
%
% Parse a _repositional_ stream and get the name of the first XML
% element *and* demand that this element defines XML namespaces.
% Fails if the document is illegal XML before the first element.
%
% Note that it is not possible to define valid RDF/XML without
% namespaces, while it is not possible to define a valid absolute
% Turtle IRI (using `<...>`-notation) with a valid xmlns declaration.

xml_doctype(Read, Dialect, DocType, Attrs) :-
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

sgml_parser0(Opts, Parser) :-
  sgml_parse(Parser, Opts).

on_begin(Tag, Attrs, Parser) :-
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Tag, Attrs)).

on_cdata(_, _) :-
  throw(error(cdata)).



%! doc_content_type(+Dialect, +Doctype, +Attrs, -MT) is det.

doc_content_type(_, html, _, text/html) :- !.
doc_content_type(html, _, _, text/html) :- !.
doc_content_type(xhtml, _, _, application/'xhtml+xml') :- !.
doc_content_type(html5, _, _, text/html) :- !.
doc_content_type(xhtml5, _, _, application/'xhtml+xml') :- !.
doc_content_type(xml, rss, _, application/'rss+xml') :- !.
doc_content_type(Dialect, Top,  Attrs, application/'rdf+xml') :-
  % Extract the namespace from the doctype.
  (   Dialect == sgml
  ->  LocalName = rdf
  ;   Dialect == xml
  ->  LocalName = 'RDF'
  ),
  atomic_list_concat([NS,LocalName], :, Top),

  % Look up the RDF namespace in the attributes list.
  atomic_list_concat([xmlns,NS], :, Attr),
  memberchk(Attr=RDFNS, Attrs),

  % Ensure it is indeed the RDF namespace.
  rdf_alias_prefix(rdf, RDFNS).
