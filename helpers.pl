% criação de helpers que possam ser reutilizados ao longo do projeto

:- module(helpers, [
           pertenceData/3,
           dataDiff/3,
           dataStamp/2,
           countX/3,
           sumMap/2,
           plusFloat/3,
           removeRepetidos/2,
           removeElem/3,
           maxElem/3,
	   maxOcor/3,
	   getEstafetasMax/3,
           calcParOcorr/2,
           calcParOcorrAux/3,
           ordDecrescente/2,
           removePar/3,
           isEqual/2,
           takeTopN/3,
           printListaPares/1,
           clear/0
                ]).

pertenceData(DataInicial, DataFinal, Data) :- dataDiff(Data, DataInicial, N1),
                                              dataDiff(Data, DataFinal, N2),
                                              N1 >= 0,
                                              N2 =< 0.

dataDiff(Data1,Data2, N) :- dataStamp(Data1, Stamp1),
                            dataStamp(Data2, Stamp2),
                            N is Stamp1 - Stamp2.

dataStamp(DD/MM/YY/HH, Stamp) :- date_time_stamp(date(YY,MM,DD,HH,0,0,0,-,-), Stamp).

countX(_, [], 0).
countX(X, [X|T], N) :- countX(X,T,N1),
                       N is N1 + 1, !.
countX(X, [_|T], N) :- countX(X,T,N).


plusFloat(X,Y,R) :- R is X + Y.

sumMap(L, N) :- foldl(plusFloat, L, 0, N).

/*
Nome: removeRepetidos(Lista A,Lista B).
Descrição: dada uma lista A, constŕoi uma lista B com os elementos de A sem repetir.
Ex: removeRepetidos([1,2,2,3,4,3],B).
    B = [1,2,3,4]
*/

removeRepetidos([],[]).
removeRepetidos([Elem|T],[Elem|T2]) :- removeElem(Elem,T,ListSemRepetidos),
				         removeRepetidos(ListSemRepetidos,T2).

/*
Nome: removeElem(Elem,Lista A,Lista B).
Descrição: dado um elemento, constrói uma lista B com os elementos de A diferentes do elemento dado. Por outras palavras, B é o resultado de remover de A os elementos iguais ao elemento dado.
Ex: removeElem(2,[1,2,3,2],B).
    B = [1,3]
*/

removeElem(_,[],[]).
removeElem(Elem,[Elem|T],L) :-removeElem(Elem,T,L).
removeElem(Elem,[OutroElem|T],[OutroElem|Res]) :- removeElem(Elem,T,Res).

/*
Nome: maxElem(Lista de Pares <A,B>, Par <Elem mais comum até ao momento, Nº de vezes que ocorre>, Res).
Descrição: dada uma lista de pares <A,B>, retorna o elem A com mais ocorrências (B). 
Ex: maxElem([Inteligencia/3, Artificial/2, Trabalho/4], Prático/1, Res). 
    Res = Trabalho.
*/

maxElem([],Zona/_,Zona).
maxElem([Zona/N|T],_/N2,Res) :- N > N2, maxElem(T, Zona/N, Res).
maxElem([_/N|T],ZonaMaior/N2,Res) :- N =< N2, maxElem(T, ZonaMaior/N2, Res).


/*
*/

maxOcor([],_/Max,Max).
maxOcor([Zona/N|T],_/N2,Max) :- N > N2, maxOcor(T, Zona/N, Max).
maxOcor([_/N|T],ZonaMaior/N2,Max) :- N =< N2, maxOcor(T, ZonaMaior/N2, Max).

getEstafetasMax([],_,[]).
getEstafetasMax([CodEstafeta/N|T1],Max,[CodEstafeta|T2]) :- N == Max, getEstafetasMax(T1,Max,T2). 
getEstafetasMax([_/N|T1],Max,Res) :- N =\= Max, getEstafetasMax(T1,Max,Res). 

/*
Nome: calcParOcorr(Lista A <Lista de elementos>, Lista de pares <Elementos, nº de ocorrências associadas>)
Descrição: calcula quantas vezes o elemento aparece na lista A. Este passo é feito pela regra "calcParOcorrAux". 
O resultado será colocado na variável "NElem", na forma de par "<Elem,Numero de ocorrencias>. 
Repare que este valor é incorporado na cabeça da lista resultante em cada recursão. 
Após termos calculado quantas vezes o elemento que está a cabeça da lista repete-se, temos de fazer a próxima recursão sem ele. 
Por isso existe a regra "removeElem" que será responsável por criar uma lista (a partir da lista de elementos) sem a cabeça anterior que já foi analisada. 
Por fim, invocamos a recursão com a lista sem o elemento que já foi analisado (e seus dados já foram incorporados na lista resultante através de [NElem|LResultante]).
*/
	
calcParOcorr([],[]).
calcParOcorr([Elem|T],[NElem|LResultante]) :-
	calcParOcorrAux(Elem,T,NElem),
	removeElem(Elem,T,ListSemElem),
	calcParOcorr(ListSemElem,LResultante).

/*
Nome: calcParOcorrAux(Elem, Lista de Elementos, par <Elem,Nº de vezes que o estafeta fez entregas>).
Raciocínio:
    1º Passo: Se a lista dada for vazia, quer dizer que o elemento não se repete nela, ou seja, quer dizer que só há uma ocorrência (pois a lista dada é corresponde ao Tail da lista de estafetas).
    2º Passo: Se o elemento surge na lista, temos então de invocar a recursão (com o Tail da lista) e preparar uma variável N para armazenar o "resultado da recursão + 1". Caso o elemento não surja na cabeça da lista, 
    invocamos a recursão com o Tail da lista, sem fazer nada com o resultado R.
*/

calcParOcorrAux(Elem,[],Elem/1).
calcParOcorrAux(Elem,[Elem|T],Elem/N) :- calcParOcorrAux(Elem,T,Elem/N2),N is N2 + 1.
calcParOcorrAux(Elem,[_|T],R) :- calcParOcorrAux(Elem,T,R).

/*
 * Nome: ordDecrescente(Lista Pares, Lista de Elementos Resultante)
 * Descrição: Recebe uma lista de Pares <Elem, Número de Ocorrências> e cria uma lista de Elem's ordenados
 * 
 * Raciocínio: 1º Passo - Encontrar o elemento com maior frequência na lista (através da maxElem) 
 			   2º Passo - Se esse elemento for a cabeça, adicionar o elemento à lista resultante e chamar recursivamente 
 						para o resto da lista 
			   3º Passo - Se esse elemento não for a cabeça, remover da cauda o elemento, adicioná-lo à lista resultante 
 						e chamar recursivamente para a cabeça e a cauda após remoção do par.
*/
ordDecrescente([],[]).
ordDecrescente([H|T], [Elem|Res]) :- 
    maxElem(T,H,Elem),
    (isEqual(H,Elem) -> ordDecrescente(T,Res), !; removePar(Elem,T,ListSemElem), ordDecrescente([H|ListSemElem], Res)).

/*
 * Nome: removePar(Elem, Lista de Pares <Elemento,Frequência>, Lista de Pares Resultante)
 * Descrição: Quando encontra o par com elemento Elem, remove o par <Elem,FrequenciaElem>
*/
removePar(_,[],[]).
removePar(Elem, [Elem/_|T], L) :- removePar(Elem, T, L).
removePar(Elem, [OutroElem|T], [OutroElem|Res]) :- removePar(Elem, T, Res).

/*
 * Nome: isEqual(Elem/Frequencia, Outro Elem) 
 * Descrição: Verifica se o primeiro parâmetro do par é igual a um dado elemento.
*/
isEqual(A/_,A).

/*
 * Nome: takeTopN(N, Lista, Lista Resultante)
 * Descrição: Recebe uma lista e devolve a lista resultante de selecionar os primeiros N elementos 
*/
takeTopN(_, [], []).
takeTopN(0, _, []).
takeTopN(N, [H|T], [H|Res]) :- B is N - 1, takeTopN(B, T, Res). 

/* 
 * Nome: printListaPares(Lista de Pares <Nome,Frequência>)
 * Descrição: Recebe uma lista de pares <Nome,Frequência> e imprime-os de forma separada.
*/
printListaPares([]).
printListaPares([Nome/Freq|T]) :- 
	write('Nome: '), write(Nome), write(', Frequência: '), write(Freq), nl,
	printListaPares(T).	

clear() :- nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl.
