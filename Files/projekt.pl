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
	my_trace(1,plan, 1,['State'/State, 'Goals'/Goals]),
	my_trace(2,plan, 1,goals_achieved),
	goals_achieved(Goals, State),
	my_trace(3,plan, 1,goals_achieved, ['State'/State, 'Goals'/Goals]),
	my_trace(4,plan, 1, ['State'/State, 'Goals'/Goals]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Wariant 2 planu, który wykonuje określone akcje
% aby dojść do stanu finalnego.

plan(InitState, Goals, Plan, FinalState) :-
	my_trace(1,plan, 2,['InitState'/InitState, 'Goals'/Goals]),
	my_trace(2,plan, 2,choose_goal),
	choose_goal(Goal, Goals, RestGoals, InitState),
	my_trace(3,plan, 2,choose_goal, ['Goal'/Goal, 'Goals'/Goals]),
	my_trace(1,plan, 2,['Goal'/Goal]),
	my_trace(2,plan, 2,achieves),
	achieves(Goal, Action),
	my_trace(3,plan, 2,achieves, ['Goal'/Goal, 'Action'/Action]),
	my_trace(1,plan, 2,['Action'/Action]),
	my_trace(2,plan, 2,requires),
	requires(Action, CondGoals),
	my_trace(3,plan, 2,requires, ['Action'/Action, 'CondGoals'/CondGoals]),
	my_trace(1,plan, 2,['CondGoals'/CondGoals]),
	my_trace(2,plan, 2,plan),
	plan(InitState, CondGoals, PrePlan, State1),
	my_trace(3,plan, 2,plan, ['CondGoals'/CondGoals, 'PrePlan'/PrePlan, 'State1'/State1]),
	my_trace(1,plan, 2,['Action'/Action]),
	my_trace(2,plan, 2,inst_action),
	inst_action(Action, CondGoals, State1, InstAction),
	my_trace(3,plan, 2,inst_action, ['Action'/Action, 'Goal'/Goal, 'State1'/State1, 'InstAction'/InstAction]),
	my_trace(1,plan, 2,['State1'/State1, 'InstAction'/InstAction]),
	my_trace(2,plan, 2,perform_action),
	perform_action(State1, InstAction, State2), !,
	my_trace(3,plan, 2,perform_action, ['State2'/State2]),
	my_trace(1,plan, 2,['State2'/State2]),
	my_trace(2,plan, 2,plan),
	plan(State2, RestGoals, PostPlan, FinalState),
	my_trace(3,plan, 2,plan, ['State2XD'/State2]),
	my_trace(1,plan, 2,['XDPostPlan'/PostPlan]),
	my_trace(2,plan, 2,conc),
	conc(PrePlan, [InstAction|PostPlan], Plan),
	my_trace(3,plan, 2,conc, ['PrePlan'/PrePlan]),
	my_trace(4,plan, 2, ['PrePlan'/PrePlan, 'PostPlan'/PostPlan, 'Plan'/Plan]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wszystkie procedury, które nie są ściśle powiązane z innymi
% ale potrzebne są, aby procedury poprawnie działały
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementacja procedury standardowej member(X, Y).


part_of(Member, Member, _). %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LISTA UŻYTYCH
	
part_of(Member, [Member|_], Gowno):-
	/+part_of(Member, Gowno, []).

part_of(Member, [_|ListRest], Gowno) :-
	part_of(Member, ListRest, Gowno).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura sprawdzająca, czy zmienna nie posiada
% dodatkowego warunku, który musi spełnić.

no_slash(X) :-
	var(X).
	
no_slash(X) :-
	X \= _/_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura sprawdzająca spełnienie celu w zadanym stanie.

goal_achieved(clear(X), State, [clear(X)|XD]) :- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LISTA UŻYTYCH
	my_trace(1,goal_achieved, 0,[]),
	no_slash(X),
	part_of(clear(X), State, XD).
	
goal_achieved(clear(X/Cond), State) :-
	my_trace(1,goal_achieved, 1,[]),
	nonvar(Cond),
	goal_achieved(Cond, State),
	part_of(clear(X), State).
	
goal_achieved(on(X,Y), State) :-
	my_trace(1,goal_achieved, 2,[]),
	no_slash(X),
	no_slash(Y),
	part_of(on(X,Y), State).
	
goal_achieved(on(X/Cond, Y), State) :-
	my_trace(1,goal_achieved, 3,[]),
	no_slash(Y),
	nonvar(Cond),
	goal_achieved(Cond, State),
	part_of(on(X,Y), State).

goal_achieved(on(X, Y/Cond), State) :-
	my_trace(1,goal_achieved, 4,[]),
	no_slash(X),
	nonvar(Cond),
	goal_achieved(Cond, State),
	part_of(on(X,Y), State).
	
goal_achieved(on(X/Cond, Y/Cond2), State) :-
	my_trace(1,goal_achieved, 5,[]),
	nonvar(Cond),
	nonvar(Cond2),
	goal_achieved(Cond, State),
	goal_achieved(Cond2, State),
	part_of(on(X,Y), State).

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
% Procedura sprawdzająca spełnianie warunków

conds_achieved([], _).

conds_achieved([diff(X, Y)|Rest], State) :-
    no_slash(X),
    no_slash(Y),
    conds_achieved(Rest, State),!,
	diff(X, Y).
	
conds_achieved([diff(X, Y/Z)|Rest], State) :-
    no_slash(X),
	goal_achieved(Z, State),
	conds_achieved(Rest, State),!,
	diff(X, Y).

conds_achieved([diff(X/Z, Y)|Rest], State) :-
    no_slash(Y),
	goal_achieved(Z, State),
	conds_achieved(Rest, State),!,
	diff(X, Y).

conds_achieved([diff(X/Z, Y/Z2)|Rest], State) :-
	goal_achieved(Z, State),
	goal_achieved(Z2, State),
	conds_achieved(Rest, State),!,
	diff(X, Y).

conds_achieved([HeadGoal|Rest], State) :-
	HeadGoal \= diff(_, _),
	goal_achieved(HeadGoal, State),
	conds_achieved(Rest, State).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura konkretyzująca zmienne

inst(X/Y, _, X) :-
	var(X),
	var(Y),
	fail.
	
inst(X/Y, State, X) :-
	nonvar(Y),
	goal_achieved(Y, State).
	
inst(X, _, X) :-
	no_slash(X).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura czy zmienne są różne

diff(X/_, Y/_) :-
	dif(X, Y).
	% X \= Y.
	
diff(X/_, Y) :-
	dif(X, Y).
	% X \= Y.

diff(X, Y/_) :-
	dif(X, Y).
	% X \= Y.

diff(X, Y) :-
	dif(X, Y).
	% X \= Y.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura sprawdzająca, czy wszystkie cele z Goals
% są spełnione w State.
% Cele mogą być zarówno ukonkretnione, jak i nie.
% goals_achieved(Goals, State).

goals_achieved([], _) :-
	my_trace(1,goals_achieved, 1,[]).

goals_achieved([Goal|Rest], State) :-
	my_trace(1,goals_achieved, 2,['Goal'/Goal, 'State'/State]),
	goal_achieved(Goal, State),
	my_trace(3,goals_achieved, 2, goals_achieved,['Goal'/Goal, 'State'/State]),
	goals_achieved(Rest, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z listy Goals wybiera do przetwarzania cel (Goal), 
% który nie jest spełniony w aktualnym stanie (State).
% Pozostałe cele zapisuje do RestGoals.
% choose_goal(Goal, Goals, RestGoals, State).

%choose_goal(Goal, [], Rest, State) :-
%	my_trace(1,choose_goal, 1,['State'/State]).

choose_goal(Goal, [Goal|Rest], Rest, State) :-
	%my_trace(1,choose_goal, 2,['Goal'/Goal, 'State'/State]),
	%my_trace(2,choose_goal, 2,goal_achieved),
	\+goal_achieved(Goal, State).
	%my_trace(3,choose_goal, 2,goal_achieved, ['Goal'/Goal, 'State'/State]),
	%my_trace(4,choose_goal, 2, ['Goal'/Goal, 'State'/State]).
	
choose_goal(Goal, [X|RestGoals], [X|Rest], State) :-
	%my_trace(1,choose_goal, 3,['Goal'/Goal, 'State'/State, 'X'/X]),
	%my_trace(2,choose_goal, 3,choose_goal),
	choose_goal(Goal, RestGoals, Rest, State).
	%my_trace(3,choose_goal, 3,goal_achieved, ['Goal'/Goal, 'State'/State, 'X'/X]),
	%my_trace(4,choose_goal, 3, ['Goal'/Goal, 'State'/State, 'X'/X]).

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

requires(move(X, Y, Z), [clear(X), clear(Z), on(X, Y)]) :-
	my_trace(1,requires, 1,['move(X, Y, Z)'/move(X, Y, Z), '[clear(X), clear(Z), on(X, Y)]'/[clear(X), clear(Z), on(X, Y)]]),
	my_trace(1,requires, 1,['Y'/Y]),	
	my_trace(2,requires, 1,no_slash),
	no_slash(Y),
	my_trace(3,requires, 1,no_slash, []),
	my_trace(1,requires, 1,['X'/X]),		
	my_trace(2,requires, 1,nonvar),
	nonvar(X),
	my_trace(3,requires, 1,nonvar, []),
	my_trace(1,requires, 1,['Z'/Z]),		
	my_trace(2,requires, 1,nonvar),	
	nonvar(Z),
	my_trace(3,requires, 1,nonvar, []),
	my_trace(4,requires, 1, []).
	
requires(move(X, Y/on(X,Y), Z), [clear(X), clear(Z), on(X, Y)]) :-
	my_trace(1,requires, 2,['move(X, Y/on(X,Y), Z)'/move(X, Y/on(X,Y), Z), '[clear(X), clear(Z), on(X, Y)]'/[clear(X), clear(Z), on(X, Y)]]),
	my_trace(2,requires, 2,nonvar),
	nonvar(X),
	my_trace(2,requires, 2,nonvar),
	nonvar(Z),
	my_trace(4,requires, 2, []).

requires(move(X/on(X,Y), Y, Z), [clear(X), clear(Z), on(X,Y)]) :-
	my_trace(1,requires, 3,['move(X/on(X,Y), Y, Z)'/move(X/on(X,Y), Y, Z), '[clear(X), clear(Z), on(X, Y)]'/[clear(X), clear(Z), on(X, Y)]]),
	my_trace(2,requires, 3,nonvar),
	nonvar(Y),
	my_trace(2,requires, 3,nonvar),
	nonvar(Z),
	my_trace(4,requires, 3, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ukonkretnia akcję (Action) przed wykonaniem. 
% inst_action(Action, Goal, State, InstAction).

inst_action(move(X, Y, Z), Conds, State, move(InstX, InstY, InstZ)) :-
	my_trace(1,inst_action, 1,['move(X, Y, Z)'/move(X, Y, Z), 'Conds'/Conds, 'State'/State]),
	inst(X, State, InstX),
	inst(Y, State, InstY),
	inst(Z, State, InstZ),
	conds_achieved(Conds, State).

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