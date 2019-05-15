%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Zmienne:
% State  		stan
% Goals 		lista celów
% InitState 	stan początkowy
% Plan 			skonstruowany plan
% FinalState 	stan końcowy
% Goal 			cel wybrany z listy celów
% RestGoals 	pozostałe cele
% Action 		akcja osiągająca zadany cel
% CondGoals		warunki dla akcji, które stają się nowymi celami
% PrePlan		skonstruowany preplan
% State1		stan pośredni 1, osiągany po wykonaniu preplanu
% InstAction	akcja ukonkretniona przed wykonaniem
% State2 		stan pośredni 2, osiągany po wykonaniu akcji w stanie pośrednim 1
% PostPlan		skonstruowany postplan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wariant 1 planu, który wykonuje procedurę 
% goals_achieved(Goals, State), gdy stan początkowy 
% jest równy stanowi finalnemu

plan(State, Goals, [], State) :-
	goals_achieved(Goals, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Wariant 2 planu, który wykonuje określone akcje
% aby dojść do stanu finalnego.

plan(InitState, Goals, Plan, FinalState) :-

	choose_goal(Goal, Goals, RestGoals, InitState),

	achieves(Goal, Action),

	requires(Action, CondGoals),

	plan(InitState, CondGoals, PrePlan, State1),

	inst_action(Action, Goal, State1, InstAction),

	perform_action(State1, InstAction, State2), !,

	plan(State2, RestGoals, PostPlan, FinalState),

	conc(PrePlan, [InstAction|PostPlan], Plan).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wszystkie procedury, które nie są ściśle powiązane z innymi
% ale potrzebne są, aby procedury poprawnie działały
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementacja procedury standardowej member(X, Y).


part_of(Member, Member). %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LISTA UŻYTYCH
	
part_of(Member, [Member|_]).

part_of(Member, [_|ListRest]) :-
	part_of(Member, ListRest).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura sprawdzająca, czy zmienna nie posiada
% dodatkowego warunku, który musi spełnić.

no_slash(X) :-
	var(X).
	
no_slash(X) :-
	X \= _/_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura sprawdzająca spełnienie celu w zadanym stanie.

goal_achieved(clear(X), State) :- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LISTA UŻYTYCH
	no_slash(X),
	member(clear(X), State).
	
goal_achieved(clear(X/Cond), State) :-
	nonvar(Cond),
	goal_achieved(Cond, State),
	member(clear(X), State).
	
goal_achieved(on(X,Y), State) :-
	no_slash(X),
	no_slash(Y),
	member(on(X,Y), State).
	
goal_achieved(on(X/Cond, Y), State) :-
	no_slash(Y),
	nonvar(Cond),
	goal_achieved(Cond, State),
	member(on(X,Y), State).

goal_achieved(on(X, Y/Cond), State) :-
	no_slash(X),
	nonvar(Cond),
	goal_achieved(Cond, State),
	member(on(X,Y), State).
	
goal_achieved(on(X/Cond, Y/Cond2), State) :-
	nonvar(Cond),
	nonvar(Cond2),
	goal_achieved(Cond, State),
	goal_achieved(Cond2, State),
	member(on(X,Y), State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura usuwająca elementy z listy

remove([First|Rest], X, Rest) :-
	part_of(X, First).

remove([First|RestList], X, [First|Rest]) :-
	remove(RestList, X, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura łącząca

conc([], X, X).

conc([Head|Tail], X, [Head|Tail2]) :-
	conc(Tail, X, Tail2).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura czy zmienne są różne

diff(X/_, Y/_) :-
	% dif(X, Y).
	X \= Y.
	
diff(X/_, Y) :-
	% dif(X, Y).
	X \= Y.

diff(X, Y/_) :-
	% dif(X, Y).
	X \= Y.

diff(X, Y) :-
	% dif(X, Y).
	X \= Y.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura sprawdzająca, czy wszystkie cele z Goals
% są spełnione w State.
% Cele mogą być zarówno ukonkretnione, jak i nie.
% goals_achieved(Goals, State).

goals_achieved([], _).

goals_achieved([Goal|Rest], State) :-
	goal_achieved(Goal, State),
	goals_achieved(Rest, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z listy Goals wybiera do przetwarzania cel (Goal), 
% który nie jest spełniony w aktualnym stanie (State).
% Pozostałe cele zapisuje do RestGoals.
% choose_goal(Goal, Goals, RestGoals, State).

%choose_goal(Goal, [], Rest, State) :-

choose_goal(Goal, [Goal|Rest], Rest, State) :-
	\+goal_achieved(Goal, State).
	
choose_goal(Goal, [X|RestGoals], [X|Rest], State) :-
	choose_goal(Goal, RestGoals, Rest, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Określa akcję (Action), która osiąga podany cel (Goal).
% Cel może być zarówno ukonkretniony, jak i nie.
% achieves(Goal, Action).

achieves(on(X, Y), move(X, Z/(on(X,Z)), Y)).

achieves(clear(X), move(Y/on(Y,X), X, Z/clear(Z))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Określa warunki (CondGoals) wykonania podanej akcji (Action),
% które stają się celami dla następnego kroku algorytmu.
% requires(Action, CondGoals).

requires(move(X, _, Z), [clear(X), clear(Z)]) :-
	no_slash(X).
	
requires(move(X/C, _, _), [clear(X/C)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ukonkretnia akcję (Action) przed wykonaniem. 
% inst_action(Action, Goal, State, InstAction).

inst_action(move(X, Y, Z), Cond, State, move(InstX, InstY, InstZ)) :-
    inst1(X, Cond, State, InstX),
	inst2(Y, Cond, State, InstY),
	inst3(Z, Cond, State, InstZ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dla warunku on() - X ukonkretnione
inst1(X, on(X,_), _, X).

% Dla warunku clear() - jest struktura
inst1(X, clear(_), State, X) :-
	goal_achieved(X, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dla warunku clear() - Y ukonkretnione
inst2(Y, clear(_), _, Y):-
	no_slash(Y).

% struktura
inst2(Y, clear(_), State, Y):-
	goal_achieved(Y, State).

% Dla warunku on() - Y jest struktura
inst2(Y, on(Y,_), State, Y) :-
	goal_achieved(Y, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dla warunku clear() -
% Z ukonkretnione
inst3(Z, on(Z,_), _, Z).

% Z jest strukturą
inst3(Z, clear(_), State, Z):-
    goal_achieved(Z, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Określa stan (State2) osiągany ze stanu (State1)
% podanego przez wykonanie podanej akcji (Action).
% perform_action(State1, Action, State2).

perform_action(State, move(X, Y, Z), [on(X,Z), clear(Y)|Rest]) :-
	part_of(clear(Z), State),
	part_of(on(X, Y), State),
	remove(State, clear(Z), RestState),
	remove(RestState, on(X, Y), Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%