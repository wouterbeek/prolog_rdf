:- module(sameAs, [run/0]).

/** <module> sameAs

@author Wouter Beek
@version 2017/05
*/

:- use_module(library(file_ext)).
:- use_module(library(zlib)).

run :-
  file_to_string('sameAs.sparql', QStr),
  setup_call_cleanup(
    gzopen('sameAs.out.gz', write, Out),
    forall(
      sparql_stream(
        %'http://localhost:3000/sparql',
        'http://websql.lod.labs.vu.nl/sparql',
        QStr,
        In,
        [result_set_format(json)]
      ),
      copy_stream_data(In, user_output)
    ),
    close(Out)
  ).
