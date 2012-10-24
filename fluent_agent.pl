%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dr. Breadcrumb, a wumpus avoiding agent. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adam.
andrew.
james.
kern.
owen.

:- use_module(library(time)).
%:- use_module(sets).   

agt_initialize:-
    alarm(98,out_of_time,_,[install(true)]).
    
:- dynamic go_home_plan/1.
:- dynamic breadcrumbs/1.
breadcrumbs([xy(1,1)]).
    
out_of_time:-
    not(fl_have_gold),
    update([glitter],grab),
    asserta(moved(turn(left))),
    asserta(moved(turn(left))),
    assert(fl_have_gold),
    retract(max_depth(50)),
    assert(max_depth(1)).
out_of_time.
    
actions([forward, turn(left), turn(right), shoot, grab, climb]).

%agt_act(Percept, shoot):-
%    member(stench, Percept),
%    fl_loc(Loc),
%    fl_dir(Dir),
%    next_location(Dir, Loc, Loc1),
%
%    wumpus(Loc1),
%    ops(shoot).
    
agt_act(_, climb) :-
    fl_have_gold,
    fl_loc(Loc),
    home(Loc).
    %update(Percept,climb).

agt_act(Percept,grab):-
    member(glitter,Percept),
    update(Percept,grab),
    asserta(moved(turn(left))),
    asserta(moved(turn(left))).
   
agt_act(Percept,shoot):-
    member(stench, Percept),
    update(Percept, shoot).
   
agt_act(_, Action) :-
    fl_have_gold,
%writeln(starting),
    %go_home(Action).
    %go_home1(Action).
    go_home2(Action).
    %update(Percept,Action).
    
agt_act(Percept,Action) :-
    assert_percept(Percept),
    explore(Action),
    update(Percept,Action).
    
agt_act(_, climb):-
    out_of_time.

update(Percept,Action):-
	member(Action,[forward,turn(right),turn(left)]),
	assert_percept(Percept),
	update_situation(Action),
	asserta(moved(Action)),!.

update(Percept,Action):-
    assert_percept(Percept),
    update_situation(Action).

go_home1(Action):-
    %writeln(goinghome),
	moved(Action1),
	reverse_move(Action1,Action),
	retract(moved(Action1)).
go_home1(climb).

reverse_move(forward,forward).
reverse_move(turn(right),turn(left)).
reverse_move(turn(left),turn(right)).

go_home2(Action):-
	retract(go_home_plan([Action|Rest])),
        !,
        assert(go_home_plan(Rest)).
        
go_home2(Action):-
    breadcrumbs(BC),
    fl_loc(Loc),
    fl_dir(Dir),
    breadcrumbs_actions(BC, locDir(Loc, Dir), [], Actions0),
    reverse([climb|Actions0], Actions1),
    Actions1 = [Action|Actions],
    assert(go_home_plan(Actions)).
go_home2(climb).

breadcrumbs_actions([], _, SoFar, SoFar).
breadcrumbs_actions(BC, locDir(CurLoc, CurDir), SoFar, Actions):-
    append(Rest, [CurLoc], BC),
    %writeln(BC),
    %writeln(hi),
    %writeln(CurLoc),
    breadcrumbs_actions(Rest, locDir(CurLoc, CurDir), SoFar, Actions).
    
breadcrumbs_actions(BC, locDir(CurLoc, CurDir), SoFar, Actions):-
    append(Rest, [Loc], BC),
    %writeln(BC),
    %writeln(hey),
    %writeln(CurLoc),
    next_location(CurDir, CurLoc, Loc), !,
    %writeln(hello),
    breadcrumbs_actions(Rest, locDir(Loc, CurDir), [forward|SoFar], Actions).
    
breadcrumbs_actions(BC, locDir(CurLoc, CurDir), SoFar, Actions):-
    append(Rest, [Loc], BC),
    next_location(NextDir, CurLoc, Loc),
    clockwise_next(CurDir, NextDir), !,
    %writeln(hi),
    breadcrumbs_actions(Rest, locDir(Loc, NextDir), [forward, turn(right)|SoFar], Actions).
    
breadcrumbs_actions(BC, locDir(CurLoc, CurDir), SoFar, Actions):-
    append(Rest, [Loc], BC),
    next_location(NextDir, CurLoc, Loc),
    clockwise_next(NextDir, CurDir), !,
    %writeln(hey),
    breadcrumbs_actions(Rest, locDir(Loc, NextDir), [forward, turn(left)|SoFar], Actions).
    
breadcrumbs_actions(BC, locDir(CurLoc, _), SoFar, Actions):-
    append(Rest, [Loc], BC),
    next_location(NextDir, CurLoc, Loc),
    %writeln(sup),
    breadcrumbs_actions(Rest, locDir(Loc, NextDir), [forward, turn(left), turn(left)|SoFar], Actions).

update_breadcrumbs(Loc):-
    breadcrumbs(BC),
    member(Loc, BC), !,
    retract(breadcrumbs(BC)),
    append(Start, [Loc|_], BC),
    append(Start, [Loc], BC1),
    assert(breadcrumbs(BC1)).
update_breadcrumbs(Loc):-
    retract(breadcrumbs(BC)),
    append(BC, [Loc], BC1),
    assert(breadcrumbs(BC1)).
    

go_home(Action) :-
	retract(plan([Action|Rest])),
        !,
        assert(plan(Rest)).
        
go_home(Action) :-
    %writeln(goinghome),
    fl_loc(Loc),
    fl_dir(Dir),
    idag_search(sp(locDir(Loc, Dir), succ, at_home, h_home),_, [Action|Actions]),
	assert(plan(Actions)).

at_home(locDir(Loc, _)):-
    Loc = xy(1,1).

h_home(locDir(Loc, _), Score):-
    mhdist(Loc, xy(1,1), Score).

explore(Action):-
	retract(plan([Action|Rest])),
        !,
        assert(plan(Rest)).

explore(Action):-
    fl_loc(Loc),
    fl_dir(Dir),
    %writeln(searching),
    idag_search(sp(locDir(Loc, Dir), succ, safe_unvisited, h_explore),_, [Action|Actions]),
	assert(plan(Actions)).

h_explore(locDir(Loc,_),Score):-
    setof(Loc1, (location(Loc1), safe_unvisited(locDir(Loc1, _))), SafeCells),
    h_explore_helper(SafeCells, Loc, 20,Score).

h_explore_helper([], _, Score, Score).
h_explore_helper([Loc1|Rest],Loc, Best, Score):-
    mhdist(Loc1, Loc, Dist),
    min(Dist,Best,Dist1),
    h_explore_helper(Rest, Loc, Dist1, Score).
    
min(A, B, A):-
    A =< B.
    
min(A, B, B):-
    B < A.
    
safe_unvisited(locDir(Loc, _)):-
   ok(Loc), 
   not(visited(Loc)).
	
succ(locDir(Loc0, Dir), forward, locDir(Loc1, Dir)):-
    next_location(Dir,Loc0,Loc1),
    ok(Loc1).
    
succ(locDir(Loc, Dir0), turn(LR), locDir(Loc, Dir1)):-
    next_direction(LR, Dir0, Dir1).


mhdist(xy(X1,Y1), xy(X2,Y2), Dist) :-
	Dist is abs(X2 - X1) + abs(Y2 - Y1).


% WumpusKB -- Class wumpus code Winter 2010
% Version 0.1
%   This is untested, and full of bugs.


%Fluent Stuff%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic fl_loc/1.
:- dynamic fl_dir/1.

:- dynamic fl_glitter/1.
:- dynamic fl_breezy/1.
:- dynamic fl_smelly/1.
:- dynamic fl_wumpus_dead/0.
:- dynamic fl_wumpus/1.
:- dynamic fl_visited/1.

:- dynamic fl_have_gold/0.
:- dynamic fl_have_arrow/0.

:- dynamic fl_ok/1.

fl_loc(xy(1,1)).
fl_visited(xy(1,1)).
fl_dir(north).
fl_have_arrow.

ops(forward):-
    retract(fl_loc(Loc)),
    fl_dir(Dir),
    next_location(Dir, Loc, Loc1),
    asserta(fl_visited(Loc1)),
    update_breadcrumbs(Loc1),
    asserta(fl_loc(Loc1)).
    
ops(turn(LR)):-
    retract(fl_dir(Dir)),
    next_direction(LR, Dir, Dir1),
    asserta(fl_dir(Dir1)).
    
ops(grab):-
    fl_loc(Loc),
    fl_glitter(Loc),
    asserta(fl_have_gold),
    retract(fl_glitter(Loc)).
    
ops(climb):-
    fl_loc(xy(1,1)).
    
ops(shoot):-
    fl_have_arrow,
    retract(fl_have_arrow).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% memory predicates
    
assert_percept(P) :-
    %writeln(assertingVisited),
    %writeln(Loc),
	assert_implications(P).


assert_implications(P) :-
    member(breeze, P),
    fl_loc(Loc),
    %writeln(assertingBreezy),
    assert(fl_breezy(Loc)),
    fail.
    
assert_implications(P) :-
    member(stench, P),
    fl_loc(Loc),
    %writeln(assertingSmelly),
    assert(fl_smelly(Loc)), 
    fail.
    
assert_implications(P) :-
    member(glitter, P),
    fl_loc(Loc),
    %writeln(assertingGlitter),
    assert(fl_glitter(Loc)),
    fail.
    
assert_implications(P) :-
    member(scream, P),
    %writeln(assertingDead),
    assert(fl_wumpus_dead),
    fail.
    
assert_implications(P):-
    not(member(breeze, P)),
    fl_loc(Loc),
    %writeln(assertingNotBreezy),
    assert(fl_not_breezy(Loc)),
    fail.
    
assert_implications(P):-
    not(member(stench, P)),
    fl_loc(Loc),
    %writeln(assertingNotSmelly),
    assert(fl_not_smelly(Loc)),
    fail.
assert_implications([]):-
    fl_loc(Loc),
    adj(Loc, Loc1),
    assert(fl_ok(Loc1)),
    fail.    
assert_implications(_) :- !.
    %writeln(succeeding), !.
    
%assert_implications([]) :-
%    fl_loc(Loc),
%    findall(Loc1, adj(Loc, Loc1), Adjs),
%    assert_all_ok(Adjs).
    
assert_all_ok([]).
assert_all_ok([H|T]) :-
    assert(fl_ok(H)),
    assert_all_ok(T).
    
update_situation(Action):-
    ops(Action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% environment

:- dynamic x_size/1.
:- dynamic y_size/1.

%%%%%%%%%%%%%%BLEH PUT THESE IN WHEN YOU RUN IN PROLOG MANUALLY%%%%%%%%%%%%%%%%%%%%%%%%
go:-
    assert(x_size(4)),
    assert(y_size(4)).

location(xy(X,Y)) :-
	x_size(XS),
	y_size(YS),
	between(1,XS,X),
	between(1,YS,Y).

direction(north).
direction(east).
direction(south).
direction(west).

action(forward).
action(turn(left)).
action(turn(right)).
%action(grab).
%action(climb).
%action(shoot).


home(xy(1,1)).

next_location(Dir, xy(X0,Y0), xy(X,Y)) :-
    dir_inc(Dir, Dx, Dy),
    X is X0 + Dx,
    Y is Y0 + Dy,
    location(xy(X,Y)).
   
dir_inc(east, 1, 0).
dir_inc(west,-1, 0).
dir_inc(north,0, 1).
dir_inc(south,0,-1).    
   
%next_direction(LR, Dir0, Dir1) :-
%	nth0(N, [north, east, south, west], Dir0), 
%	member(LR/Exp, [left/(N-1), right/(N+1)]),!,
%	Index is (Exp+4) mod 4,
%	nth0(Index, [north, east, south, west], Dir1).

clockwise_next(north,east).
clockwise_next(east,south).
clockwise_next(south,west).
clockwise_next(west,north).

next_direction(left, Dir0, Dir) :-
	clockwise_next(Dir,Dir0).
	
next_direction(right, Dir0, Dir) :-
	clockwise_next(Dir0, Dir).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Direct Perceptions

% breezy(?Location).
% not_breezy(?Location).
% smelly(?Location).
% not_smelly(?Location).
% visited(?Location).
% Location - xy(X,Y)

breezy(Loc):-
    fl_breezy(Loc).

not_breezy(Loc):-
    fl_visited(Loc), not(fl_breezy(Loc)).

smelly(Loc):-
	fl_smelly(Loc).

not_smelly(Loc):-
	fl_visited(Loc), not(fl_smelly(Loc)).

visited(Loc):-
	fl_visited(Loc).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% High-level inferences

% Gets an adjacent square
adj(Loc1, Loc2):-
    next_location(_, Loc1, Loc2).
 
adjacentToAll([], _).
adjacentToAll([H|T], Loc) :-
    adj(Loc, H),
    adjacentToAll(T, Loc). 

% If any neighbor squares are not stinky, the square is not a wumpus
not_wumpus(xy(1,1)) :- !.
not_wumpus(Loc) :-
    adj(Loc, Loc1),
    not_smelly(Loc1).

% If any neighbor squares are not breezy, the square is not a pit
not_pit(xy(1,1)) :- !.
not_pit(Loc) :-
    adj(Loc, Loc1),
    not_breezy(Loc1).

% Square is safe for travel if it isn't a pit or wumpus    
ok(xy(1,1)):-!.
ok(Loc) :-
    fl_ok(Loc), !.
ok(Loc) :-
    not_pit(Loc), wumpus_dead, !.
ok(Loc) :-
    not_pit(Loc), not_wumpus(Loc).

% The wumpus is dead if we have ever heard a scream.
wumpus_dead :-
    fl_wumpus_dead.


wumpus(Loc) :-
	location(Loc),
	setof(L, (adj(Loc, L), smelly(L)), SmellyAdj),
	SmellyAdj \= [],
	setof(L, (adjacentToAll(SmellyAdj, L), not(not_wumpus(L))), CouldBeWumpus),
	CouldBeWumpus = [Loc].

    
% pit(Loc) :-
%    adj(Loc, Loc1), breezy(Loc1),
%    setof(L,(adj, (L,L1), not(not_pit(L1))), CouldBePit),
%    CouldBePit = [Loc].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEARCHING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph search (cycle checking).
:- dynamic max_depth/1.
max_depth(100).
	
%------------------------------------------------------------
% version for graph search (cycle checking)

idag_search(sp(Start, SuccPred,GoalTest, HPred), Final, Ops) :-
	max_depth(MaxD),
	call(HPred, Start, StartH),
	gen_bound(StartH, MaxD, Depth),
	%writeln(Depth),
	list_to_set(Start, Closed),
	path(SuccPred, HPred, Start, StartH, Depth, Closed, Final, Ops),
	call(GoalTest, Final).

path(_, _, S, _, _, _, S, []).
path(SuccPred, HPred, S0, H0, StepsLeft, Closed0, S, [Op|Ops]) :-
	StepsLeft >= H0,
	!,
	StepsLeft1 is StepsLeft - 1,
	call(SuccPred, S0, Op, S1),
	%writeln(S1),
	set_add_new(Closed0, S1, Closed),
	call(HPred, S1, H1),
	path(SuccPred, HPred, S1, H1, StepsLeft1, Closed, S, Ops).
path(_, _, _, H, StepsLeft, _, _, _) :-   % pruned. update bound, then fail.
	% H > StepsLeft
	Needed is H-StepsLeft,
	update_next_bound(Needed),
	fail.
	
:- dynamic bound_increment/1.

gen_bound(Start, Max, Start):-
	retractall(bound_increment(_)),
	asserta(bound_increment(Max)).  % set increment to "infinity"
gen_bound(Bound, Max, Bound1) :-
	bound_increment(Inc),
	NewBound is Bound + Inc,
	NewBound < Max,
	gen_bound(NewBound, Max, Bound1).

% Save the smallest increment necessary to usefully expand search
update_next_bound(ThisInc) :-
	bound_increment(Inc),
	ThisInc < Inc,
	!,
	retractall(bound_increment(_)),
	asserta(bound_increment(ThisInc)).
update_next_bound(_).

% sets.pl -- efficient sets using assoc library
% Programmer: John Zelle

%:-module(sets), [set_empty/1, set_add/3, set_add_new/3, set_member/2,
%s		set_memberchk/2, set_add_list/3, list_to_set/2, set_to_list/2]).

:- use_module(library(assoc)).

%---------------------------------------------------------------------------
/*  
   Operations:
       set_empty(?Set) 
       set_add(+Set0, +Element, -Set)
       set_add_new(+Set0, +Element, -Set)
            Fails if Element is already in Set0
       set_member(-Element, +Set)
            used to enumerate the set
       set_memberchk(+Element, +Set)
       set_add_list(+Set0, +List, -Set)
       list_to_set(+List, -Set)
       set_to_list(+Set, -List)
*/
%---------------------------------------------------------------------------
set_empty(S) :- empty_assoc(S).

%---------------------------------------------------------------------------
set_add(Set0, Element, Set1) :- put_assoc(Element, Set0, t, Set1).

%---------------------------------------------------------------------------
set_add_new(Set0, Element, Set1) :-
	\+ get_assoc(Element, Set0, _),
	put_assoc(Element, Set0, t, Set1).

%---------------------------------------------------------------------------
set_member(Element, Set) :- gen_assoc(Element, Set, _).

%---------------------------------------------------------------------------
set_memberchk(Element, Set) :-
	get_assoc(Element, Set, _).

%---------------------------------------------------------------------------
set_add_list(Set0, Items, Set1) :-
	add_list_helper(Items, Set0, Set1).

add_list_helper([], Set, Set).
add_list_helper([H|T], Set0, Set) :-
	set_add(H, Set0, Set1),
	add_list_helper(T, Set1, Set).

%---------------------------------------------------------------------------
list_to_set(List, S) :-
	findall(Key-t, member(Key,List), Pairs),
	list_to_assoc(Pairs, S).

%---------------------------------------------------------------------------
set_to_list(Set, List) :- assoc_to_keys(Set, List).

armadillo.
velociraptor.
wombat.
