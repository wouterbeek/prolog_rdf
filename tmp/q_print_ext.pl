:- module(
  rdf_print_ext,
  [
    rdf_html_list//2, % +M, +L
    rdf_html_list//3  % +M, +L, +Opts
  ]
).

/** <module> Quine print extensions

Extended Quine term printing: term printing that requires more
information than the term itself.

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(rdf/rdf_api)).

:- rdf_meta
   rdf_html_list(+, r, ?, ?),
   rdf_html_list(+, r, +, ?, ?).

% Replace IRIs with labels.
rdf_html_iri_inner0(M, Iri, Opts) -->
  {
    ground(M),
    get_dict(iri_lbl, Opts, true),
    rdfs_pref_label(M, Iri, Lbl)
  }, !,
  rdf_html_literal_inner0(Lbl, Opts).

%! rdf_html_list(+M, +L)// is det.
%! rdf_html_list(+M, +L, +Opts)// is det.

rdf_html_list(M, L) -->
  rdf_html_list(M, L, _{}).


rdf_html_list(M, L1, Opts1) -->
  {
    rdf_html_default_options(Opts1, Opts2),
    rdf_list(M, L1, L2, _)
  },
  html_list(rdf_html_term_outer0(M, Opts2), L2).
