% pylogic.pl
%    This is the Prolog server from the Pylogic module by John Zelle

%    The pylogic module is a Python module communicating with a prolog
%    process. It is comprised of this server (pylogic.pl) and a corresponding
%    Python wrapper module (pylogic.py).

%    This package has been written for SWI-Prolog, but should work with other 
%      relatively ISO compliant Prolog interpreters.

%    See the pylogic.py file for information on the Python interface.

/* launch command:
    pl -f pylogic.pl -t pylogic:server -g pylogic:server 2> /dev/null
*/

:-module(pylogic,[]).

:- nodebug.

/* Server protocol

All interactions are initiated by the Python side and result in a response
from the Prolog side having the form:

    status:data\n

The data portion of the string might be empty, but the response will always
have a status followed by a colon. When fired up, the server responds:

    ready:

In response to a query, the possible results are:

  yes:bindings
  no:
  error:error message

If the response is yes:bindings, the server waits for the message:

  next.

and then returns either another solution (via yes:) or no:
Sending "ok." instead of next. resets the server for the next query.

The server can be shutdown by sending the query "halt."

Example interaction:
client                  server
------------------------------
Start Server
                        ready:
append(X,Y, [1,2,3]).
                        yes:["X=[]","Y=[1, 2, 3]"]
next.
                        yes:["X=[1]","Y=[2, 3]"]
ok.
                        ready:
foo.
                        error: error(existence_error(procedure, pylogicKB...
bleeg(.
                        error: syntax
member(1,[2,3,4]).
                        no:
member(X,[2,3,4]).
                        yes:["X=2"]
next.
                        yes:["X=3"]
next.
                        yes:["X=4"]
next.
                        no:
halt.

*/

server:-
	prompt(_,''),
	write('ready:'), nl, flush,
	server_loop.

server_loop:-
	repeat,
        get_term(Goal, Vars),
	catch( output_solution(Goal, Vars, More), E, error_fail(E) ),
	More = false,
	!, 
	server_loop.

get_term(Goal, Vars) :-
        read_term(Goal, [syntax_errors(quiet), variable_names(Vars)]),
        !.
get_term(_,_):-
	error_fail('syntax').

error_fail(E):-
	write('error: '), write(E), nl, flush, fail.
	
output_solution(Goal, Vars, More) :-
	get_solution(Goal, GotSolution),
	output_result(GotSolution, Vars, More).

get_solution(Goal, true) :- pylogicKB:Goal.
get_solution(_, false).

output_result(true, Vars, More) :-
	!,
	write('yes:'),
	write_bindings(Vars),
	read(R),
	( R = next ->
	    More = true
	;
	  More = false,
	  write('ready:'), nl, flush
        ).
output_result(false, _, false) :-
	write('no:'), nl, flush.

write_bindings(Vars):-
	write('['),
	write_each_binding(Vars),
	write(']'),
	nl,
	flush.

write_each_binding([]):- !.
write_each_binding([B]) :- 
	!,
	write_binding(B).
write_each_binding([B|Bs]) :-
	write_binding(B),
	write('\,'),
	write_each_binding(Bs).

write_binding(B) :-
	write('\"'),
	write(B),
	write('\"'),
	flush.
