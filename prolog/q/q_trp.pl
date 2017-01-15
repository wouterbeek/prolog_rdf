:- modile(q_trp, []).

/** <module> TRP backend for Q
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).

:- multifile
    q_io:q_cache_format_hook/2,
    q_io:q_cache2view_hook/2,
    q_io:q_store2cache_hook/4,
    q_io:q_source2store_hook/5,
    q_io:q_source_format_hook/2,
    q_io:q_view_graph_hook/3,
    q_io:q_view_rm_hook/2.

q_io:q_cache_format_hook(trp, [trp]).

q_io:q_cache2view_hook(trp, G) :-
  dcg_with_output_to(string(Msg), deb_q_io("TRP", G, "MEM")),
  q_file_graph(File, trp, G),
  indent_debug(io, Msg),
  rdf11:rdf_load_db(File).

q_io:q_source2store_hook(rdf, Source, Sink, SourceOpts, SinkOpts) :-
  rdf_reserialize(Source, Sink, SourceOpts, SinkOpts).

q_io:q_source_format_hook(rdf, Ext) :-
  rdf_media_type(_, _, Ext).

q_io:q_store2cache_hook(trp, Source, Sink, G) :-
  dcg_with_output_to(string(Msg1), deb_q_io("N-Triples", G, "MEM")),
  dcg_with_output_to(string(Msg2), deb_q_io("MEM", G, "TRP")),
  indent_debug_call(io, Msg1,
    setup_call_cleanup(
      rdf_load_file(Source, [graph(G)]),
      indent_debug_call(io, Msg2,
        rdf11:rdf_save_db(Sink, G)
      ),
      rdf_unload_graph(G)
    )
  ).

q_io:q_view_graph_hook(trp, G, false) :-
  rdf_graph(G).

q_io:q_view_rm_hook(trp, G) :-
  rdf_unload_graph(G),
  dcg_with_output_to(string(Msg), deb_q_io("TRP", G, "MEM")),
  indent_debug(io, Msg).
