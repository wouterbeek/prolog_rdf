:- module(
  rdfa_user,
  [
    rdfa_user_menu//2,       % +M, +G
    rdfa_user_menu_button//3 % +M, :Content_0, +G
  ]
).

/** <module> RDFa user

@author Wouter Beek
@version 2016/06-2016/08
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- html_meta
   rdfa_user_menu_button(+, html, +, ?, ?).

:- rdf_meta
   rdfs_user_menu(+, r, ?, ?),
   rdfa_user_menu_button(+, :, r, ?, ?).





%! rdfa_user_menu(+M, +G)// is det.

rdfa_user_menu(M, G) -->
  user_menu(
    {M,G}/[Agent,Name]>>agent_name(M, Agent, Name, G),
    {M,G}/[Agent,Img]>>agent_image(M, Agent, Img, G)
  ).



%! rdfa_user_menu_button(+M, :Content_0, +G)// is det.
%
% The user menu shown as a button.

rdfa_user_menu_button(M, Content_0, G) -->
  % @hack
  {user_db:current_user(User)}, !,
  html(
    div([class=dropdown,id='user-menu'], [
      button([
        class=[btn,'btn-default','dropdown-toggle','navbar-btn'],
        'data-toggle'=dropdown,
        id='user-button',
        type=button
      ], [
        \agent_image(M, User, G),
        span(class=caret, [])
      ]),
      Content_0
    ])
  ).
rdfa_user_menu_button(_, _, _) -->
  {login_link(Link)},
  html(
    span(id='user-menu',
      \icon_button(user, 'window.location="'+Link+'";')
    )
  ).
