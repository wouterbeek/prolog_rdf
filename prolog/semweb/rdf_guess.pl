:- module(
  rdf_guess,
  [
    rdf_guess_media_type/2 % +In, -MediaType
  ]
).

/** <module> RDF guess

The module guess the RDF serialization format that is stored in the
input stream In.

The peek length is currently set to 1000, 2000, 3000, and -- finally
-- 4000 characters.  Does it make sense to turn these into settings?

We sometimes cannot decide between N-Quads and TriG, is it OK to go
with N-Quads in those cases?

@author Wouter Beek
@author Jan Wielemaker
@version 2017/03
*/

:- use_module(library(dcg/basics)).
:- use_module(library(memfile)).
:- use_module(library(ordsets)).
:- use_module(library(sgml)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- meta_predicate
    turtle_string_codes(//, ?, ?).





%! rdf_guess_media_type(+In, -MediaType) is det.
%
% MediaType is a compound terms of the form
% `media(Supertype/Subtype,Params)`.  This is how Media Types are
% represented in the HTTP package, e.g., http_parse_header_value/3.
%
% There is one JSON-LD Media Type:
%
%   * media(application/ld+json,[]) for JSON-LD
%
% There are four Turtle-family Media Types:
%
%   * media(application/trig,[]) for TriG
%
%   * media(application/'n-triples',[]) for N-Triples
%
%   * media(application/'n-quads',[]) for N-Quads
%
%   * media(text/turtle,[]) for Turtle
%
% There are two SGML-family Media Types that denote RDF:
%
%   * media(application/'rdf+xml',[]) for RDF/XML
%
%   * media(text/html,_) for RDFa


rdf_guess_media_type(In, MediaType) :-
  rdf_guess_media_type0(In, 1000, MediaType).

rdf_guess_media_type0(In, PeekLen, MediaType) :-
  peek_string(In, PeekLen, Str),
  string_codes(Str, Cs),
  length(Cs, Len),
  % Do not extended the peek length if the previous peek size already
  % surpassed the length of the content.
  ((Len =:= 0 ; Len < PeekLen) -> !, EoS = true ; EoS = false),
  (   % JSON-LD
      phrase(rdf_guess_jsonld, Cs, _)
  ->  MediaType = media(application/'ld+json',[])
  ;   % Turtle-family
      %
      % We use the information as to whether or not the end of the
      % stream has been reached.
      phrase(rdf_guess_turtle(EoS, MediaType), Cs, _)
  ;   % SGML-family
      rdf_guess_sgml(Cs, MediaType)
  ), !.
rdf_guess_media_type0(In, PeekLen1, MediaType) :-
  PeekLen1 =< 3000,
  PeekLen2 is PeekLen1 + 1000,
  rdf_guess_media_type0(In, PeekLen2, MediaType).





% JSON-LD %

%! rdf_guess_jsonld// .
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

rdf_guess_jsonld -->
  'bs*',
  optional_arrays,
  "{",
  'bs*',
  json_string,
  'bs*',
  ":", !.

optional_arrays -->
  "[", !,
  'bs*',
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

'bs*' -->
  blank, !,
  'bs*'.
'bs*' --> "".





% TURTLE FAMILY %

%! rdf_guess_turtle(+EoS:boolean, -MediaType)// .
%
% Succeeds on a list of codes that match the beginning of a document
% in the Turtle-family.

rdf_guess_turtle(EoS, media(Supertype/Subtype,[])) -->
  rdf_guess_turtle_subtype(EoS, Subtype),
  {(Subtype == turtle -> Supertype = text ; Supertype = application)}.

% Whenever the end-of-stream is reached we assume it is in a format
% belonging to the Turtle family.  This e.g. allows empty files or
% files that only consist of Turtle comments to be classified as
% Turtle-family files as well.
rdf_guess_turtle_subtype(true, Subtype) -->
  eos, !,
  {guess_turtle_subtype(true, [], Subtype)}.
% Skip blanks.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  blank, !, blanks,
  rdf_guess_turtle_subtype(EoS, Subtype).
% Turtle-family comment.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  turtle_comment, !,
  rdf_guess_turtle_subtype(EoS, Subtype).
% Keyword `@BASE' or `@PREFIX': excludes N-Quads and N-Triples.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  "@", decl_keyword, !,
  {guess_turtle_subtype(EoS, ['n-quads','n-triples'], Subtype)}.
% Keyword `BASE' or `PREFIX': excludes N-Quads and N-Triples.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  decl_keyword, !,
  {guess_turtle_subtype(EoS, ['n-quads','n-triples'], Subtype)}.
% Default graph: can only appear in TriG.
rdf_guess_turtle_subtype(_, trig) -->
  "{", !.
% Named graph: can only appear in TriG.
rdf_guess_turtle_subtype(_, trig) -->
  turtle_iriref(_),
  'turtle_bs*',
  "{", !.
% Tuple: each of the terms possibly allows us to exclude Turtle
% subtypes.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  turtle_subject(Subtypes1),
  'turtle_bs*',
  turtle_predicate(Subtypes2),
  'turtle_bs*',
  turtle_object(Subtypes3),
  'turtle_bs*',
  (   % End of a triple.
      "."
  ->  {ord_union([Subtypes1,Subtypes2,Subtypes3], Subtypes)}
  ;   % Object list notation: excludes: N-Quads and N-Triples.
      ";"
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,['n-quads','n-triples']],
          Subtypes
        )
      }
  ;   % Predicate-Object pairs list notation: excludes: N-Quads and
      % N-Triples.
      ","
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,['n-quads','n-triples']],
          Subtypes
        )
      }
  ;   % End of a quadruple: excludes N-Triples, TriG and Turtle.
      turtle_graph(Subtypes4),
      'turtle_bs*',
      "."
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,Subtypes4,['n-triples',trig,turtle]],
          Subtypes
        )
      }
  ),
  {guess_turtle_subtype(EoS, Subtypes, Subtype)}.
% Anonymous blank node: exlcudes N-Quads and N-Triples.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  "[", !,
  {guess_turtle_subtype(EoS, ['n-quads','n-triples'], Subtype)}.
% RDF collection: excludes N-Quads and N-Triples.
rdf_guess_turtle_subtype(EoS, Subtype) -->
  "(", !,
  {guess_turtle_subtype(EoS, ['n-quads','n-triples'], Subtype)}.

turtle_bnode -->
  "_:",
  'nonblank*'.

turtle_graph(Subtypes) -->
  turtle_iriref(Subtypes).

% Full IRI: can appear in any Turtle-family subtype.
turtle_iriref([]) -->
  "<", !,
  string(_),
  ">", !.
% Prefixed IRI: excludes N-Quads and N-Triples.
turtle_iriref(['n-quads','n-triples']) -->
  turtle_iriref_prefix,
  ":",
  'nonblank*'.

turtle_iriref_prefix -->
  ":", !,
  {fail}.
turtle_iriref_prefix -->
  blank, !,
  {fail}.
turtle_iriref_prefix -->
  [_],
  turtle_iriref_prefix.
turtle_iriref_prefix --> "".

turtle_ltag -->
  'nonblank*'.

'nonblank*' -->
  nonblank(_),
  'nonblank*'.
'nonblank*' --> "".

turtle_object(Subtypes) -->
  turtle_iriref(Subtypes), !.
turtle_object([]) -->
  turtle_bnode, !.
turtle_object(Subtypes) -->
  turtle_string(Subtypes1), 'turtle_bs*',
  (   "^^"
  ->  'turtle_bs*',
      turtle_iriref(Subtypes2)
  ;   "@"
  ->  turtle_ltag,
      {Subtypes2 = []}
  ;   {Subtypes2 = []}
  ),
  {ord_union([Subtypes1,Subtypes2], Subtypes)}.

turtle_predicate(Subtypes) -->
  turtle_iriref(Subtypes), !.
% Abbreviation for `rdf:type': excludes N-Quads and N-Triples.
turtle_predicate(['n-quads','n-triples']) -->
  "a".

% String that starts with three single quotes: excludes N-Quads and
% N-Triples.
turtle_string(['n-quads','n-triples']) -->
  "'''", !,
  turtle_string_codes([0'',0'',0'']).
% String that starts with one single quote: excludes N-Quads and
% N-Triples.
turtle_string(['n-quads','n-triples']) -->
  "'", !,
  turtle_string_codes([0'']).
% String that starts with three double quotes: excludes N-Quads and
% N-Triples.
turtle_string(['n-quads','n-triples']) -->
  "\"\"\"", !,
  turtle_string_codes([0'",0'",0'"]).
% String that starts with single double quote: can be any Turtle
% subtype.
turtle_string([]) -->
  "\"",
  turtle_string_codes([0'"]).

% Escaped single quote.
turtle_string_codes(End) -->
  "\\\'", !,
  turtle_string_codes(End).
% Escaped double quote.
turtle_string_codes(End) -->
  "\\\"", !,
  turtle_string_codes(End).
% End of string.
turtle_string_codes(End) -->
  End, !.
% Content.
turtle_string_codes(End) -->
  [_], !,
  turtle_string_codes(End).
% End of stream.
turtle_string_codes(_) --> "".

turtle_subject(Subtypes) -->
  turtle_iriref(Subtypes), !.
turtle_subject([]) -->
  turtle_bnode.

decl_keyword --> "base".
decl_keyword --> "prefix".

'turtle_bs*' -->
  turtle_bs, !,
  'turtle_bs*'.
'turtle_bs*' --> "".

% Turtle only allows horizontal tab and space, but we skip other blank
% characters as well, since they may appear in non-conforming
% documents without telling us anything about which Turtle subtype we
% are parsing.
turtle_bs -->
  blank.
turtle_bs -->
  turtle_comment.

turtle_comment -->
  "#",
  string(_),
  (eol ; eos), !.

eol --> "\n".
eol --> "\r\n".

%! guess_turtle_subtype(+EoS:boolean, +Excluded:ordset, -Subtype)// is det.
%
% Sometimes we just have to make a decision as to which Turtle-family
% format we have in front of us.  Based on the parsed code list, we
% have a list of excluded subtypes that informs our guess.
%
% After applying generalization (see generalize/2), we still may have
% the situation in which a document can be either N-Quads or TriG.
% Specifically, this is the case for the following combinations of
% subtypes:
%
%   * ['n-quads', 'n-triples', trig, turtle]
%
%   * ['n-quads', 'n-triples', trig]
%
%   * ['n-quads', 'n-triples', turtle]
%
%   * ['n-quads', trig, turtle]
%
%   * ['n-quads', trig]
%
%   * ['n-quads', turtle]
%
% For these cases we choose N-Quads over TriG.  We make this choice
% because we assume it less likely that TriG syntax is introduced
% later in the file.  Whether this assumption is correct or not will
% be determined by use of this module.
%
% If the end of stream (EoS) has been reached, then we can apply
% generalization the other way round: to remove more generic subtypes
% for which we have not found evidence (see specialize/2).

guess_turtle_subtype(EoS, Excluded, Subtype) :-
  ord_subtract(['n-quads','n-triples',trig,turtle], Excluded, Subtypes1),
  (   EoS == true
  ->  specialize(Subtypes1, Subtypes2)
  ;   EoS == false
  ->  generalize(Subtypes1, Subtypes2)
  ),
  (   Subtypes2 = [Subtype]
  ->  true
  ;   EoS == true
  ->  % If we are the end of the stream, and we have not seen any
      % evidence of TriG syntax, we should.
      print_message(warning, could_be(Subtypes2)),
      Subtype = 'n-quads'
  ).

%! specialize(+Subtypes1, -Subtypes2) is det.

specialize(L1, L3) :-
  generalization(X, Y),
  memberchk(X, L1),
  memberchk(Y, L1), !,
  ord_del_element(L1, X, L2),
  specialize(L2, L3).
specialize(L, L).

%! generalize(+Subtypes1, -Subtypes2) is det.
%
% Further reduce the number of Turtle-family subtypes by using the following
% generalization relations:
%
%   1. TriG is a generalization of Turtle.
%
%   2. Turtle is a generalization of N-Triples.
%
%   3. N-Quads is a generalization of N-Triples

generalize(L1, L3) :-
  generalization(X, Y),
  memberchk(X, L1),
  memberchk(Y, L1), !,
  ord_del_element(L1, Y, L2),
  generalize(L2, L3).
generalize(L, L).

generalization(trig, turtle).
generalization(turtle, 'n-triples').
generalization('n-quads', 'n-triples').





% SGML FAMILY %

%! rdf_guess_sgml(+Cs, -MediaType) is det.

rdf_guess_sgml(Cs, MediaType) :-
  setup_call_cleanup(
    new_memory_file(MFile),
    (
      setup_call_cleanup(
        open_memory_file(MFile, write, Write),
        format(Write, "~s", [Cs]),
        close(Write)
      ),
     setup_call_cleanup(
        open_memory_file(MFile, read, Read),
        guess_sgml_type(Read, MediaType),
        close(Read)
      )
    ),
    free_memory_file(MFile)
  ).



%! guess_sgml_type(+Read, -MediaType) is semidet.
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
% If the toplevel element is detected as `HTML` we guess that the
% document contains RDFa.

guess_sgml_type(Read, MediaType) :-
  sgml_doctype(Read, Dialect, DocType, Attrs),
  doc_content_type(Dialect, DocType, Attrs, MediaType).



%! sgml_doctype(+Read, -Dialect:atom, -DocType:atom, -Attrs) is semidet.
%
% Parse a _repositional_ stream and get the name of the first SGML
% element *and* demand that this element defines XML namespaces.
% Fails if the document is illegal SGML before the first element.
%
% Note that it is not possible to define valid RDF/XML without
% namespaces, while it is not possible to define a valid absolute
% Turtle IRI (using `<...>`-notation) with a valid xmlns declaration.

sgml_doctype(Read, Dialect, DocType, Attrs) :-
  setup_call_cleanup(
    make_parser(Read, Parser, State),
    catch(
      sgml_parse(
        Parser,
        [
          call(begin, on_begin),
          call(cdata, on_cdata),
          max_errors(-1),
          source(Read),
          syntax_errors(quiet)
        ]
      ),
      E,
      true
    ),
    clean_parser(Read, Parser, State)
  ),
  nonvar(E),
  E = tag(Dialect, DocType, Attrs).

make_parser(Read, Parser, state(Pos)):-
  stream_property(Read, position(Pos)),
  new_sgml_parser(Parser, []).

clean_parser(Read, Parser, state(Pos)):-
  free_sgml_parser(Parser),
  set_stream_position(Read, Pos).

on_begin(Tag, Attrs, Parser) :-
  get_sgml_parser(Parser, dialect(Dialect)),
  throw(tag(Dialect, Tag, Attrs)).

on_cdata(_, _) :-
  throw(error(cdata)).



%! doc_content_type(+Dialect, +Doctype, +Attrs, -MediaType) is det.

doc_content_type(_, html, _, media(text/html,[])) :- !.
doc_content_type(html, _, _, media(text/html,[])) :- !.
doc_content_type(xhtml, _, _, media(application/'xhtml+xml',[])) :- !.
doc_content_type(html5, _, _, media(text/html,[])) :- !.
doc_content_type(xhtml5, _, _, media(application/'xhtml+xml',[])) :- !.
doc_content_type(xml, rss, _, media(application/'rss+xml',[])) :- !.
doc_content_type(Dialect, Top,  Attrs, media(application/'rdf+xml',[])) :-
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
  rdf_current_prefix(rdf, RDFNS).
