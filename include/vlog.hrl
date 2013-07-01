%%
%% vlog.hrl
%% Kevin Lynx
%% 06.05.2013
%% 
-ifndef(VLOGHRL).
-define(VLOGHRL, true).

-define(TRACE, 0).
-define(INFO, 1).
-define(WARN, 2).
-define(ERROR, 3).
-define(OFF, 4).

-define(LVLS(L), 
	case L of
		?TRACE -> "trac";
		?INFO -> "info";
		?WARN -> "warn";
		?ERROR -> "error"
	end).
-define(LOG(X, Lvl), 
    vlog:format(Lvl, "~s [~s] {~p, ~p}: ~p~n", 
    	[?LVLS(Lvl), vlog:time_string(), ?MODULE, ?LINE, X])).
-define(T(X), ?LOG(X, ?TRACE)).
-define(I(X), ?LOG(X, ?INFO)).
-define(W(X), ?LOG(X, ?WARN)).
-define(E(X), ?LOG(X, ?ERROR)).

-define(FMT(S, A), lists:flatten(io_lib:format(S, A))).

-endif.
