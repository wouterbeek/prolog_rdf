:- module(
  sparql_functions,
  [
    'fn:concat'/2 % +Literals, -Literal
  ]
).

:- use_module(library(apply)).
:- use_module(library(error)).

:- use_module(library(semweb/rdf_prefix)).

:- rdf_meta
   'fn:concat'(t, t),
   string_literal(t, r, -, -).





% 'fn:concat'(+Literals:list(string_literal), -Literal:string_literal) is det.

%! 'fn:concat'(+Literals:list(sparql_literal), -Literal:sparql_literal) is det.
%
% The function accepts string literals as arguments.
%
% The lexical form of the returned literal is obtained by
% concatenating the lexical forms of its inputs.  If all input
% literals are typed literals of type `xsd:string', then the returned
% literal is also of type `xsd:string', if all input literals are plain
% literals with identical language tag, then the returned literal is a
% plain literal with the same language tag, in all other cases, the
% returned literal is a simple literal.

'fn:concat'(Literals, Literal) :-
  maplist(string_literal, Literals, Ds, LTags, Strings), !,
  atomic_list_concat(Strings, Lex),
  (   % xsd:string
      rdf_equal(D, xsd:string),
      maplist(==(D), Ds)
  ->  rdf_prefix_term(literal(type(D,Lex)), Literal)
  ;   % language-tagged string
      ground(LTags),
      LTags = [LTag|LTagsT],
      maplist(==(LTag), LTagsT)
  ->  rdf_prefix_term(literal(lang(LTag,Lex)), Literal)
  ;   % plain literal
      rdf_prefix_term(literal(Lex), Literal)
  ).
'fn:concat'(_, type_error).





% GENERICS %

%! string_literal(+Literal:string_literal, -D:iri, -LTag:atom, -String:string) is det.
%! string_literal(-Literal:string_literal, +D:iri, +LTag:atom, +String:string) is det.

string_literal(literal(lang(LTag,Lex)), rdf:langString, LTag, String) :-
  ground(LTag), !,
  atom_string(Lex, String).
string_literal(literal(type(Lex)), simple, _, String) :- !,
  atom_string(Lex, String).
string_literal(literal(type(xsd:string,Lex)), xsd:string, _, String) :- !,
  atom_string(Lex, String).
string_literal(Literal, D, _, _) :-
  (   nonvar(Literal)
  ->  domain_error(string_literal, Literal)
  ;   nonvar(D)
  ->  domain_error(datatype_iri, D)
  ;   instantiation_error(args([Literal,D]))
  ).
