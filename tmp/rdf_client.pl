:- module(
  rdf_client,
  [
    print_triples/1, % +Uri
    print_uri/1,     % +Uri
    print_uri/2,     % +MT, +Uri
    read_quad/2,     % +Uri, -Quad
    read_triple/2,   % +Uri, -Triple
    test0/1,         % -Name
    write_triple/2   % +Out, +Triple
  ]
).

/** <module> RDF client

@author Wouter Beek
@version 2016/11-2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(http/rfc5988)).
:- use_module(library(os/io)).
:- use_module(library(q/rdf_print)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle), []).

:- debug(rdf(client)).

test0("read/write triples") :-
  setup_call_cleanup(
    open('local.nt', write, Out),
    forall(
      read_triple('http://geonovum.triply.cc/graph', Triple),
      write_triple(Out, Triple)
    ),
    close(Out)
  ).





%! print_triples(+Uri) is det.

print_triples(Uri) :-
  forall(
    read_triple(Uri, Triple),
    rdf_print_triple(Triple)
  ).



%! print_uri(+Uri) is det.
%! print_uri(+MT, +Uri) is det.

print_uri(Uri) :-
  print_uri(_, Uri).


print_uri(MT, Uri) :-
  call_on_stream(Uri, print_uri0, [request_header('Accept'=MT)]).

print_uri0(In, Path, Path) :-
  copy_stream_data(In, user_output).



%! read_quad(+Uri, -Quad) is nondet.

read_quad(Uri, Quad) :-
  read_tuple('application/n-quads', Uri, Quad).



%! read_triple(+Uri, -Triple) is nondet.

read_triple(Uri, Triple) :-
  read_tuple('application/n-triples', Uri, Triple).



read_tuple(MT, Uri, Tuple) :-
  http_open(Uri, In, [header(link,LinkAtom),request_header('Accept'=MT)]),
  atom_phrase(link(Uri, LinkValues), LinkAtom),
  read_tuple_from_stream(LinkValues, In, Tuple).

read_tuple_from_stream(LinkValues, In, Tuple) :-
  repeat,
  read_ntuple(In, Result),
  (   Result == end_of_file
  ->  !,
      close(In),
      (   member(link(NextUri,Params), LinkValues),
          memberchk(rel-next, Params)
      ->  read_tuple(NextUri, Tuple)
      )
  ;   Tuple = Result
  ).



%! write_triple(+Out, +Triple:compound) is det.

write_triple(Out, rdf(S,P,O)) :-
  with_output_to(Out, (
    write_subject(S),
    put_char(' '),
    write_predicate(P),
    put_char(' '),
    write_object(O),
    put_char(' '),
    put_char(.),
    put_code(10)
  )).

write_bnode(BNode) :-
  write(BNode).

write_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).

write_literal(V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  write_iri(D).
write_literal(V@LTag) :-
  rdf_literal_lexical_form(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  put_char('@'),
  write(LTag).

write_object(S) :-
  write_subject(S), !.
write_object(Lit) :-
  write_literal(Lit).

write_predicate(Iri) :-
  write_iri(Iri).

write_subject(BNode) :-
  rdf_is_bnode(BNode), !,
  write_bnode(BNode).
write_subject(Iri) :-
  rdf_is_iri(Iri), !,
  write_predicate(Iri).
