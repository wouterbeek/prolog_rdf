:- module(
  rdf_guess,
  [
    rdf_guess/2, % +In, -MediaTypes
    rdf_guess/3  % +In, -MediaTypes, +Options
  ]
).

/** <module> RDF guess

This module heuristically guesses the RDF serialization format that is
stored in the input stream In.

@author Wouter Beek
@author Jan Wielemaker
@version 2017/04-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(sgml)).

:- meta_predicate
    turtle_lexical_form_codes(//, ?, ?).

:- setting(maximum_peek_size, nonneg, 100000,
           "The maximum number of codes that is peeked from the input stream.").
:- setting(minimum_peek_size, nonneg, 1000,
           "The initial number of codes that is peeked from the input stream.").





%! rdf_guess(+In:stream, -MediaTypes:list(compound)) is det.
%! rdf_guess(+In:stream, -MediaTypes:list(compound),
%!           +Options:list(compound)) is det.
%
% @arg MediaTypes is a list of compound term of the form
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
%        * media(application/'n-quads',[]) for N-Quads
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
% @arg Options The following options are supported:
%
%   * peek_size(+positive_integer)
%
%     The initial number of codes that is read from the input stream
%     In, on which the guess is based.  This number is doubled while
%     backtracking, until either the end of the stream is reached or
%     the maximum peek size, as indicated by the input stream `In', is
%     exceeded.
%
% Non-determinism causes an increasingly longer prefix to be read.

rdf_guess(In, MediaTypes) :-
  rdf_guess(In, MediaTypes, []).


rdf_guess(In, MediaTypes, Options) :-
  setting(minimum_peek_size, Min),
  option(peek_size(Size), Options, Min),
  must_be(positive_integer, Size),
  setting(maximum_peek_size, Max),
  rdf_guess_range(In, Size-Max, MediaTypes).

% Guess the RDF serialization format based on the current peek size.
rdf_guess_range(In, Size-Max, MediaTypes) :-
  Size =< Max,
  peek_string(In, Size, String),
  debug(rdf_guess, "[PEEK ~D CHARS] ~s", [Size,String]),
  string_length(String, Length),
  % Keep track of whether or not the entire stream has been peeked.
  (Length < Size -> !, EoS = true ; EoS = false),
  (   % JSON-LD
      string_phrase(rdf_guess_jsonld, String, _Rest)
  ->  MediaTypes = [media(application/'ld+json',[])]
  ;   % Turtle-family
      %
      % We use the information as to whether or not the end of the
      % stream has been reached.
      string_phrase(rdf_guess_turtle(EoS, MediaTypes), String, _Rest)
  ->  true
  ;   % SGML-family
      rdf_guess_sgml(String, MediaType)
  ->  MediaTypes = [MediaType]
  ), !.
% Unable to determine the RDF serialization format within the maximum
% peek size.
rdf_guess_range(_, Size-Size, []) :- !.
% Increase the peek size
rdf_guess_range(In, Size1-Max, MediaTypes) :-
  Size2 is min(Size1 * 2,Max),
  rdf_guess_range(In, Size2-Max, MediaTypes).





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
  blanks,
  optional_arrays,
  "{",
  blanks,
  json_string,
  blanks,
  ":", !.

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





% TURTLE FAMILY %

%! rdf_guess_turtle(+EoS:boolean, -MediaTypes:list(compound))// is semidet.
%
% Succeeds on a list of codes that match the beginning of a document
% in the Turtle-family.
%
% @arg EoS Whether or not the end of stream has been reached on the
%      parsed input.  This matters for determining the RDF
%      serialization format.  For example, if we have not seen any
%      graph terms yet it cannot be TriG or N-Quads.
%
% @arg MediaTypes A list of compound terms of the form
%      `media(Supertype/Subtype,Parameters)'.

rdf_guess_turtle(EoS, MediaTypes) -->
  rdf_guess_turtle_subtypes(EoS, Subtypes),
  {maplist(subtype_media_type, Subtypes, MediaTypes)}.

subtype_media_type(Subtype, media(application/Subtype,[])).

% Whenever the end-of-stream is reached we assume it is in a format
% belonging to the Turtle family.  This e.g. allows empty files or
% files that only consist of Turtle comments to be classified as
% Turtle-family files as well.
rdf_guess_turtle_subtypes(true, Subtypes) -->
  eos, !,
  {guess_turtle_subtypes(true, ['n-quads',trig], Subtypes)}.
% skip blanks
rdf_guess_turtle_subtypes(EoS, Subtypes) -->
  blank, !, blanks,
  rdf_guess_turtle_subtypes(EoS, Subtypes).
% Turtle-family comment
rdf_guess_turtle_subtypes(EoS, Subtypes) -->
  turtle_comment, !,
  rdf_guess_turtle_subtypes(EoS, Subtypes).
% Turtle/TriG `@base', `base', `@prefix', or `prefix' declaration
rdf_guess_turtle_subtypes(_, [trig]) -->
  ("@" ; ""), decl_keyword, !.
% TriG default graph
rdf_guess_turtle_subtypes(_, [trig]) -->
  "{", !.
% TriG named graph
rdf_guess_turtle_subtypes(_, [trig]) -->
  turtle_graph,
  *(turtle_blank),
  "{", !.
rdf_guess_turtle_subtypes(EoS, Subtypes5) -->
  turtle_subject(Subtypes1),
  *(turtle_blank),
  turtle_predicate(Subtypes2),
  *(turtle_blank),
  turtle_object(Subtypes3),
  *(turtle_blank),
  (   % end of a triple
      "."
  ->  {ord_intersection([Subtypes1,Subtypes2,Subtypes3], Subtypes4)}
  ;   % object list notation: Turtle/TriG.
      ";"
  ->  {Subtypes4 = [trig]}
  ;   % Predicate-Object pairs list notation: Turtle/TriG.
      ","
  ->  {Subtypes4 = [trig]}
  ;   % end of a quadruple: N-Quads
      turtle_graph,
      *(turtle_blank),
      "."
  ->  {Subtypes4 = ['n-quads']}
  ),
  {guess_turtle_subtypes(EoS, Subtypes4, Subtypes5)}.
% Turtle/TriG anonymous blank node
rdf_guess_turtle_subtypes(_, [trig]) -->
  "[", !.
% Turtle/TriG collection
rdf_guess_turtle_subtypes(_, [trig]) -->
  "(", !.

turtle_bnode(['n-quads',trig]) -->
  "_:",
  *(nonblank).

turtle_graph -->
  turtle_iriref(_).

% Turtle-family full IRI
turtle_iriref(['n-quads',trig]) -->
  "<", !,
  string(_),
  ">", !.
% Turtle/TriG prefixed IRI
turtle_iriref([trig]) -->
  turtle_iriref_prefix,
  ":",
  *(nonblank).

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
  *(nonblank).

turtle_object(Subtypes) -->
  turtle_iriref(Subtypes), !.
turtle_object(Subtypes) -->
  turtle_bnode(Subtypes), !.
turtle_object(Subtypes) -->
  turtle_literal(Subtypes).

% Turtle/TriG abbreviated form for XSD decimal, double, and integer
% literals.
turtle_literal([trig]) -->
  ("+" ; "-" ; "." ; digit(_)), !.
% Turtle/TriG abbreviated form for XSD boolean literals.
turtle_literal([trig]) -->
  ("false" ; "true"), !.
turtle_literal(Subtypes3) -->
  turtle_lexical_form(Subtypes1), *(turtle_blank),
  (   "^^"
  ->  *(turtle_blank),
      turtle_iriref(Subtypes2)
  ;   "@"
  ->  turtle_ltag,
      {Subtypes2 = ['n-quads',trig]}
  ;   {Subtypes2 = ['n-quads',trig]}
  ),
  {ord_intersection([Subtypes1,Subtypes2], Subtypes3)}.

turtle_predicate(Subtypes) -->
  turtle_iriref(Subtypes), !.
% Turtle/TriG abbreviation for `rdf:type'
turtle_predicate([trig]) -->
  "a".

% Turtle/TriG triple single quoted lexical form
turtle_lexical_form([trig]) -->
  "'''", !,
  turtle_lexical_form_codes([0'',0'',0'']).
% Turtle/TriG single single quoted lexical form
turtle_lexical_form([trig]) -->
  "'", !,
  turtle_lexical_form_codes([0'']).
% Turtle/TriG triple double quoted lexical form
turtle_lexical_form([trig]) -->
  "\"\"\"", !,
  turtle_lexical_form_codes([0'",0'",0'"]).
% String that starts with a single double quote.
turtle_lexical_form(['n-quads',trig]) -->
  "\"",
  turtle_lexical_form_codes([0'"]).

% Escaped single quote.
turtle_lexical_form_codes(End) -->
  "\\\'", !,
  turtle_lexical_form_codes(End).
% Escaped double quote.
turtle_lexical_form_codes(End) -->
  "\\\"", !,
  turtle_lexical_form_codes(End).
% End of string.
turtle_lexical_form_codes(End) -->
  End, !.
% Content.
turtle_lexical_form_codes(End) -->
  [_], !,
  turtle_lexical_form_codes(End).
% End of stream.
turtle_lexical_form_codes(_) --> "".

turtle_subject(Subtypes) -->
  turtle_iriref(Subtypes), !.
turtle_subject(Subtypes) -->
  turtle_bnode(Subtypes).

decl_keyword --> "base".
decl_keyword --> "prefix".

% Turtle only allows horizontal tab and space, but we skip other blank
% characters as well, since they may appear in non-conforming
% documents without telling us anything about which Turtle subtype we
% are parsing.
turtle_blank -->
  blank.
turtle_blank -->
  turtle_comment.

turtle_comment -->
  "#",
  string(_),
  (eol ; eos), !.

%! guess_turtle_subtypes(+EoS:boolean, +Subtypes1, -Subtypes2)// is det.
%
% If we still cannot choose between TriG or N-Quads based on the
% syntax encountered, and we have also reached the end of the input
% stream (EoS), we can use the latter fact to filter out the
% possibility of TriG.

guess_turtle_subtypes(_, [Subtype], [Subtype]) :- !.
guess_turtle_subtypes(EoS, Subtypes1, Subtypes2) :-
  EoS == true, !,
  selectchk(trig, Subtypes1, Subtypes2).
guess_turtle_subtypes(_, Subtypes, Subtypes).





% SGML FAMILY %

%! rdf_guess_sgml(+String:string, -MediaType:compound) is det.

rdf_guess_sgml(String, MediaType) :-
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
        guess_sgml_type(In, MediaType),
        close(In)
      )
    ),
    free_memory_file(MFile)
  ).



%! guess_sgml_type(+In:stream, -MediaType:compound) is semidet.
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

guess_sgml_type(In, MediaType) :-
  sgml_doctype(In, Dialect, DocType, Attributes),
  doc_content_type(Dialect, DocType, Attributes, MediaType).



%! sgml_doctype(+In:stream, -Dialect:atom, -DocType:atom,
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
%!                  -MediaType:compound) is det.

doc_content_type(_, html, _, media(text/html,[])) :- !.
doc_content_type(html, _, _, media(text/html,[])) :- !.
doc_content_type(xhtml, _, _, media(application/'xhtml+xml',[])) :- !.
doc_content_type(html5, _, _, media(text/html,[])) :- !.
doc_content_type(xhtml5, _, _, media(application/'xhtml+xml',[])) :- !.
doc_content_type(xml, rss, _, media(application/'rss+xml',[])) :- !.
doc_content_type(Dialect, Top,  Attributes, media(application/'rdf+xml',[])) :-
  % Extract the namespace from the doctype.
  (   Dialect == sgml
  ->  LocalName = rdf
  ;   Dialect == xml
  ->  LocalName = 'RDF'
  ),
  atomic_list_concat([NS,LocalName], :, Top),

  % Look up the RDF namespace in the attributes list.
  atomic_list_concat([xmlns,NS], :, Attribute),
  memberchk(Attribute=RDFNS, Attributes),

  % Ensure it is indeed the RDF namespace.
  rdf_current_prefix(rdf, RDFNS).
