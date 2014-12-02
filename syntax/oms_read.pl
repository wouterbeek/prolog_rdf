:- module(
  oms_read,
  [
    oms_read/1 % +Stream:stream
  ]
).

/** <module> Read Manchaster Syntax for OWL

# BNF

BNF notation in the OMS grammar is mapped onto dcg_abnf.pl
 in the following way:

| **Construct** | **Syntax**      | **`dcg_abnf`** |
|:--------------|:----------------|:---------------|
| zero or more  | curly braces    | `'*'(NT, [])`  |
| zero or one   | square brackets | `'?'(NT, [])`  |
| alternative   | vertical bar    | `(NT1 ; NT2)`  |
| grouping      | parentheses     | ???            |

# Meta-productions

```
[1]   <NT>List ::= <NT> { ',' <NT> }
[2]   <NT>2List ::= <NT> ',' <NT>List
[3]   <NT>AnnotatedList ::= [annotations] <NT> { ',' [annotations] <NT> }
```

```
[1']   '+'(NT, [separator(comma)])
[2']   'm*'(2, NT, [separator(comma)])
[3']   '+'(annotations(NT), [separator(comma)])
       annotations(NT) --> '?'(annotations, []), NT.
```

# White space

White space is allowed between any two terminals or non-terminals
 except inside nonNegativeInteger//, prefixName//, IRI//, and literal//.
White space is required between two terminals or non-terminals if
 its removal could cause ambiguity.
Generally this means requiring white space except before and after
 punctuation (e.g., commas, parentheses, braces, and brackets). 

--

@author Wouter Beek
@compat [OWL 2 Web Ontology Language Manchester Syntax (Second Edition)](http://www.w3.org/TR/2012/NOTE-owl2-manchester-syntax-20121211/)
@version 2014/12
*/

:- use_module(plDcg(dcg_generics)).

:- use_module(plRdf(syntax/sw_char)).





% comment// .
% Comments are maximal sequences of Unicode characters starting with a `#`
%  and not containing a line feed or a carriage return.
% Note that comments are only recognized where white space is allowed,
%  and thus not inside the above non-terminals.

comment -->
  "#",
  dcg_until(end_of_comment, _, [end_mode(inclusive)]).

end_of_comment --> carriage_return.
end_of_comment --> line_feed.



%! white_space// .
% White space is a sequence of:
%   - blanks (U+20)
%   - tabs (U+9)
%   - line feeds (U+A)
%   - carriage returns (U+D)
%   - comments

white_space -->
  'WS',
  white_space.
white_space -->
  comments,
  white_space.
white_space --> [].
