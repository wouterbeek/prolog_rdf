:- module(
  rdf_meta_auto_expand,
  [
    (rdf_meta_expand)/1,
    op(1150, fx, (rdf_meta_expand))
  ]
).

/** <module> RDF_META_AUTO_EXPAND

Module created to automate the insertion
of rdf_global_id/2 statements in predicates
that need these conversions.

### Usage

The directive rdf_meta_expand/1 can be used
to register a predicate to be auto-expanded.
Each argument of this predicate is replaced
by either:
  * =e=
    Expand this term.
  * =i=
    Do not expand this term.

### Example

~~~{.pl}
:- rdf_meta_expand pred(e,e,i).

pred(Converted, AlsoConverted, NotConvertedLabel):-
  rdf(Converted, rdf:type, rdfs:Class),
  rdf(Converted, rdf:type, rdfs:Class),
  writeln(NotConvertedLabel).

:- X = rdfs:domains, pred(X, rdfs:range, label).
~~~

The resulting listing of pred/3 will be:

~~~{.pl}
pred(Converted2, AlsoConverted2, NotConvertedLabel):-
  rdf_global_id(Converted2, Converted),
  rdf_global_id(AlsoConverted2, AlsoConverted),
  rdf(Converted, rdf:type, rdfs:Class),
  rdf(Converted, rdf:type, rdfs:Class),
  writeln(NotConvertedLabel).
~~~

--

@author Sander Latour wrote this module in 2011.
@author Wouter Beek made lots of use of this module and updated it
        slightly in 2013/08.
@version 2011, 2013/08
*/

:- use_module(generics(db_ext)).
:- use_module(library(lists)).

:- dynamic(rdf_meta_expand_db/1).



rdf_meta_expand(Term):-
  db_add_novel(rdf_meta_expand_db(Term)).

%! expand_body(+Expansions, +Body, -ExpandedBody)
% Expands the original body with each element in
% the list of expansions by combining them in
% a conjunction using the '=|,|=' operator.

expand_body([], Body, Body).
expand_body([Expansion|Expansions], Body, (Expansion, ExpandedBody)):-
  expand_body(Expansions, Body, ExpandedBody).

%! rdf_meta_expand_all(
%!   +ArgumentTypes,
%!   +OriginalArguments,
%!   -NewArguments,
%!   -Expansions
%! )
% For each argument in `OriginalArgument` that
% has the corresponding argument type `e`:
%   1. Add a new variable to the new arguments.
%   2. Add a rdf_global_id/2 statement to the expansions.

rdf_meta_expand_all([], [], [], []).
% Non-variables do not need expansion.
rdf_meta_expand_all([e|ArgTypes], [Arg|Args], [Arg|NewArgs], Expansions):-
  nonvar(Arg), !,
  rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions).
rdf_meta_expand_all(
  [e|ArgTypes],
  [Arg|Args],
  [NewArg|NewArgs],
  [rdf_global_id(NewArg,Arg)|Expansions]
):-
  rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions).
% Anything that is not `e` does not have to be expanded.
rdf_meta_expand_all([i|ArgTypes], [Arg|Args], [Arg|NewArgs], Expansions):-
  rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions).

system:term_expansion(TermIn, TermOut):-
  (
    % If `TermIn` is a predicate...
    TermIn =.. [:-,Head,Body],
    % ... separate `Functor` from `Args`.
    Head =.. [Functor|Args],

    % Create a list of unbounded variables that has the same length.
    same_length(Args, ArgTypes),

    % Create a query term that can be used
    % to match `TermIn` on the list of
    % predicates set to auto-expand.
    MatchTerm =.. [Functor|ArgTypes],

    % Match query to auto-expand fact base.
    % Notice the module prefix.
    rdf_meta_expand_db(_Mod:MatchTerm),

    % Create lists of renamed arguments and
    % expansions to the body.
    rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions),

    % Expand the original `Body` with `Expansions`.
    expand_body(Expansions, Body, ExpandedBody),

    % Create the (altered) `TermOut`.
    NewHead =.. [Functor|NewArgs],
    TermOut =.. [:-,NewHead,ExpandedBody], !
  ;
    % Either something went wrong
    % or `TermIn` was not a predicate
    TermOut = TermIn
  ).

