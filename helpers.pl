% criação de helpers que possam ser reutilizados ao longo do projeto

:- module(helpers, [
           pertenceData/3,
           dataDiff/3,
           dataStamp/2,
           countX/3,
           sumMap/2
                ]).

pertenceData(DataInicial, DataFinal, Data) :- dataDiff(Data, DataInicial, N1),
                                              dataDiff(Data, DataFinal, N2),
                                              N1 >= 0,
                                              N2 =< 0.

dataDiff(Data1,Data2, N) :- dataStamp(Data1, Stamp1),
                            dataStamp(Data2, Stamp2),
                            N is Stamp1 - Stamp2.

dataStamp(DD/MM/YY, Stamp) :- date_time_stamp(date(YY,MM,DD,0,0,0,0,-,-), Stamp).

countX(_, [], 0).
countX(X, [X|T], N) :- countX(X,T,N1),
                       N is N1 + 1, !.
countX(X, [_|T], N) :- countX(X,T,N).


plusFloat(X,Y,R) :- R is X + Y.

sumMap(L, N) :- foldl(plusFloat, L, 0, N).
