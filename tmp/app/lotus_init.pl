:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(os/io)).
:- use_module(library(q/q_term)).
:- use_module(library(service/llapi)).

:- debug(http(error)).
:- debug(http(receive_reply)).
:- debug(http(send_request)).





run :-
  call_to_stream('rdfs_labels.ssv.gz', run, [mode(write)]).


run(Out):-
  forall(
    (
      ll_ldf(Iri, rdfs:label, Lit),
      % @note Some values for ‘rdfs:label’ are IRIs.
      q_is_literal(Lit)
    ),
    (
      q_literal(Lit, D, Lex, LTag),
      defval(null, LTag),
      format(Out, "~a ~a ~a ~a~n", [Iri,D,LTag,Lex])
    )
  ).
