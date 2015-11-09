:- module(sparql10_helpers, []).
:- reexport(
  library(url/rfc1738_code),
  [
    digit//1 as '[0-9]', % ?Weight:between(0,9)
    digit//2 as '[0-9]', % ?Weight:between(0,9)
                         % ?Code:code
    hialpha//1 as '[A-Z]', % ?Code:code
    lowalpha//1 as '[a-z]' % ?Code:code
  ]
).

/** <module> SPARQL 1.0: Helper predicates

@author Wouter Beek
@version 2015/11
*/
