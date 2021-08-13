

unstable_states(TVL, Keys, Result) :-
    unstables_(Keys, [], States),
    intersection(TVL, States, Result).
    
stable_states(TVL, Keys, Result) :-
    unstable_states(TVL, Keys, Unstables),
    subtract(TVL, Unstables, Result).
    
unstables_([(ZState, State) | Keys], ZAcc, States) :-
    dict_create(S01, _, [ZState:0, State:1]),
    dict_create(S10, _, [ZState:1, State:0]),
    append([term(S01), term(S10)], ZAcc, Acc),
    unstables_(Keys, Acc, States).
unstables_([], States, States).
    
         
from_state(TVL, Keys, To, From) :-
    state_(from_, TVL, Keys, To, From).

to_state(TVL, Keys, From, To) :-
    state_(to_, TVL, Keys, From, To).    
    
state_(Dir, TVL, Keys, [A], B) :-
    tvl:delete_term(A, TVL, TVLd),
    A =.. [term | [Args]],
    states_(Keys, Args, Dir, States),
    intersection(TVLd, [term(States)], B).

states_(Keys, Dict, Dir, Result) :-
    states_data(Keys, Dict, Dir, Values),
    dict_create(Result, _, Values).

states_data([Key | Keys], Dict, Dir, [State:Val | Values]) :-
    call(Dir, Key, (ZState, State)),
    get_dict(ZState, Dict, Val),
    states_data(Keys, Dict, Dir, Values).
states_data([], _Dict, _Dir, []).
      
from_((ZState, State), (State, ZState)).
to_((ZState, State), (ZState, State)).
   

       
    

    
    

    
                      
