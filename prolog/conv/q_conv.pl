:- module(
  q_conv,
  [
    q_conv_options/2 % +Opts1, -Opts2
  ]
).

/** <module> Quine conversion generics

@author Wouter Beek
@version 2016/08, 2016/11
*/

:- use_module(library(dict_ext)).
:- use_module(library(settings)).





%! q_conv_options(+Opts1, -Opts2) is det.

q_conv_options(Opts1, Opts2) :-
  setting(iri:data_scheme, Scheme),
  setting(iri:data_auth, Host),
  merge_dicts(_{concept: resource, host: Host, scheme: Scheme}, Opts1, Opts2).
