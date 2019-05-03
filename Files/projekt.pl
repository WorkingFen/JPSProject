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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wszystkie procedury, które nie są ściśle powiązane z innymi
% ale potrzebne są, aby procedury poprawnie działały
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementacja procedury standardowej member(X, Y).

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

goal_achieved(clear(X), State) :-
	no_slash(X),
	part_of(clear(X), State).
	
goal_achieved(clear(X/Cond), State) :-
	nonvar(Cond),
	goal_achieved(Cond, State),
	part_of(clear(X), State).
	
goal_achieved(on(X,Y), State) :-
	no_slash(X),
	no_slash(Y),
	part_of(on(X,Y), State).
	
goal_achieved(on(X/Cond, Y), State) :-
	no_slash(Y),
	nonvar(Cond),
	goal_achieved(Cond, State),
	part_of(on(X,Y), State).

goal_achieved(on(X, Y/Cond), State) :-
	no_slash(X),
	nonvar(Cond),
	goal_achieved(Cond, State),
	part_of(on(X,Y), State).
	
goal_achieved(on(X/Cond, Y/Cond2), State) :-
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

inst(X/Y, State, X) :-
	var(X),
	var(Y),
	fail.
	
inst(X/Y, State, X) :-
	nonvar(Y),
	goal_achieved(Y, State).
	
inst(X, State, X) :-
	no_slash(X).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedura czy zmienne są różne

diff(X/_, Y/_) :-
	X /= Y.
	
diff(X/_, Y) :-
	X /= Y.

diff(X, Y/_) :-
	X /= Y.

diff(X, Y) :-
	X /= Y.
	
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

choose_goal(Goal, [Goal|Rest], Rest, State) :-
	\+goal_achieved(Goal, State).
	
choose_goal(Goal, [X|RestGoals], [X|Rest], State) :-
	choose_goal(Goal, RestGoals, Rest, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Określa akcję (Action), która osiąga podany cel (Goal).
% Cel może być zarówno ukonkretniony, jak i nie.
% achieves(Goal, Action).

achieves(on(X, Y), move(X, Z/(on(X,Z)), Y)).

achieves(clear(X), move(X/on(X,Y), Y, Z)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Określa warunki (CondGoals) wykonania podanej akcji (Action),
% które stają się celami dla następnego kroku algorytmu.
% requires(Action, CondGoals).

requires(move(X, Y, Z), [clear(X), clear(Z), on(X, Y)]) :-
	no_slash(Y),
	nonvar(X),
	nonvar(Z).
	
requires(move(X, Y/on(X,Y), Z), [clear(X), clear(Z), on(X, Y)]) :-
	nonvar(X),
	nonvar(Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ukonkretnia akcję (Action) przed wykonaniem. 
% inst_action(Action, Goal, State, InstAction).

inst_action(move(X, Y, Z), Conds, State, move(InstX, InstY, InstZ)) :-
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