:- module(
  nquads11,
  [
    graphLabel//1, % ?G:rdf_graph
    statement//1   % ?Stmt:rdf_tuple
  ]
).
:- reexport(library(dcg/ntriples11), [
     'EOL'//0,
     object//1,    % ?O:rdf_object
     predicate//1, % ?P:rdf_predicate
     subject//1,   % ?S:rdf_subject
     ws//0
   ]).
:- reexport(library(dcg/turtle11), [
     'IRIREF'//1,              % ?Iri:atom
     'STRING_LITERAL_QUOTE'//1 % ?Lex:atom
   ]).

/** <module> N-Quads 1.1

@author Wouter Beek
@compat N-Quads 1.1
@version 2015/11, 2016/03, 2016/12
*/

:- use_module(library(dcg)).





%! graphLabel(?G:rdf_graph)// is det.
%
% ```abnf
% graphLabel ::= IRIREF | BLANK_NODE_LABEL
% ```

graphLabel(Iri) -->
  'IRIREF'(Iri), !.
graphLabel(BNode) -->
  'BLANK_NODE_LABEL'(BNode).



%! statement(?Tuple:rdf_tuple)// is det.
%
% ```abnf
% statement ::= subject predicate object graphLabel? '.'
% ```

statement(Tuple) -->
  subject(S),
  +(ws), !,
  predicate(P),
  +(ws), !,
  object(O),
  +(ws), !,
  (   graphLabel(G)
  ->  +(ws), !,
      {Tuple = rdf(S,P,O,G)}
  ;   {Tuple = rdf(S,P,O)}
  ),
  ".".
