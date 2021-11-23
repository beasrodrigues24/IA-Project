:- use_module(helpers).
/* 
A explicação desse facto "tMaisEcologico" será útil para flexibilidade na primeira query (explicação adiante)
*/
tMaisEcologico(bicicleta).

%-encomenda(TempMax,
%-	codCliente,
%-	codEstafeta,
%-	Peso,
%-	Volume,
%-	Estado,
%-	Transporte,
%- 	PrecoBase,
%- 	DataEntrega,
%-  ZonaEntrega).

encomenda(40,4,7,34,23,entregue,moto, 19.95, 21/11/2021, gothamCity). 
encomenda(20,2,2,34,23,entregue,bicicleta, 20.1, 21/11/2021, centralCity). %-- CodE:2 - CodC:2
encomenda(50,3,3,34,23,entregue,carro, 23, 1/6/2021, gothamCity).
encomenda(10,2,4,34,23,entregue,bicicleta, 32, 30/5/2021, centralCity). %-- CodE:4 - CodC:2
encomenda(10,2,5,34,23,entregue,bicicleta, 43, 11/9/2021, centralCity). %-- CodE:5 - CodC:2
encomenda(30,6,6,34,23,entregue,moto, 10, 23/2/2018, wonderland).
encomenda(40,2,4,34,23,entregue,bicicleta, 2, 12/2/2019, narnia).  %-- CodE:4 - CodC:2
encomenda(50,8,8,34,23,entregue,carro,2, 21/11/2021, centralCity).
encomenda(60,2,2,34,23,entregue,bicicleta, 28, 24/12/2020, narnia).  %-- CodE:2 - CodC:2
encomenda(10,5,10,34,23,entregue,moto, 40, 26/12/2020, narnia).    
encomenda(20,3,7,34,23,entregue,moto, 42, 1/1/2021, centralCity). 
encomenda(70,2,4,34,23,entregue,bicicleta, 24.5, 21/11/2021, narnia).   %-- CodE:4 - CodC:2

/* 
Optamos pelo código do estafeta para permitir a existência de estafetas com o mesmo nome, mas códigos de identificação distintos
*/
%-estafeta(Codigo,Nome).

estafeta(1,homemaranha).
estafeta(2,homemdeferro).
estafeta(3,hulk).
estafeta(4,doutorestranho).
estafeta(5,wanda).
estafeta(6,homemformiga).
estafeta(7,loki).
estafeta(8,capitamarvel).

%- cliente(Codigo,Cliente).

cliente(1,superhomem).
cliente(2,mulhermaravilha).
cliente(3,flash).
cliente(4,zeus).
cliente(5,ares).
cliente(6,batman).

%- precoEncomenda(Base, TempMax, Veiculo, Preco).

precoEncomenda(Base, 2, bicicleta, Base + 5).
precoEncomenda(Base, 6, bicicleta, Base + 4).
precoEncomenda(Base, 24, bicicleta, Base + 3).
precoEncomenda(Base, 2, moto, Base + 5).
precoEncomenda(Base, 6, moto, Base + 4).
precoEncomenda(Base, 24,moto, Base + 3).
precoEncomenda(Base, 2, carro, Base + 6).
precoEncomenda(Base, 6, carro, Base + 4).
precoEncomenda(Base, 24, carro, Base + 3).

%------------------------- Regras auxiliares -------------------------------%

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


%------------------------------ Query 1 -------------------------------------%

/*
Nome: query1().
Descrição: identifica o estafeta que utilizou mais vezes um meio de transporte mais ecológico. Caso existir um novo meio de transporte mais ecológico do que a bicicleta, basta alterar o facto "tMaisEscologico".
Ex: consideramos, atualmente em nossa base de conhecimento, a bicicleta sendo o transporte mais ecológico. Assim, o resultado desta query será a identificação do estafeta que mais vezes realizou entregas usando bicicleta. 

Raciocínio: 
	1º Passo: identificar todos os estafetas que possuem encomendas com estado de "entregue" e que utilizaram bicicleta como meio de transporte. Este passo é feito através do "findall", sendo que a condição "encomenda(_,_,X,_,_,entregue,Transporte),tMaisEcologico(Transporte)" permite traduzir o que foi descrito anteriormente. Após este passo, temos uma lista com os códigos dos estafetas que satisfazem o que foi dito. Detalhe: essa lista terá códigos repetidos, isto é, caso o estafeta com código 2 tenha entregue 3 encomendas de bicicleta, o seu código vai aparecer 3 vezes nessa lista. Logo o próximo passo será justamente calcular quantas encomendas fez cada estafeta.
	2º Passo: criar uma lista em que cada elemento consiste no par (estafeta,número de entregas feitas com o transporte mais ecológico). Este passo é feito pela regra "calcParOcorr". A explicação dessa regra encontra-se nela.
	3º Passo: filtrar o estafeta que fez mais entregas a partir da lista que foi anteriormente obtida. Repare que nomeamos [H|T] nessa situação, para facilitar a invocação da regra "maxElem", apenas por isso. Isso porque, o 2º parâmetro dessa regra é o parâmetro temporário corresponde ao "estafeta que fez mais entregas". Dando a cabeça da lista a ele na primeira invocação, estamos de certa forma a inicar o processo apenas. O estafeta que tiver feito mais entregas será colocado na variável "Estafeta".
	Detalhe: Se existir mais que um estafeta no topo do ranking de entregas, essa regra ainda não lida com isso, devolvendo apenas um deles. Porém para corrigir isso basta a regra "maxElem" devolver o número máximo de entregas e depois filtrar da Lista obtida em "calcParOcorr" OS os estafetas que tiverem efetuado esse número de entregas. 
*/ 

query1() :- findall(X,(
	encomenda(_,_,X,_,_,entregue,Transporte,_,_,_),
	tMaisEcologico(Transporte)
	),ListEstafetas),
	calcParOcorr(ListEstafetas,[H|T]),
	!,
	maxElem(T,H,Estafeta),
	estafeta(Estafeta,Nome),
	showQuery1(Nome,Estafeta).

showQuery1(Nome,Estafeta) :-
	write('\n'),
	write('Estafeta que mais andou no transporte mais ecologico'),
	write('\n'),
	write('----------------------------------------------------'),
	write('\n'),
	write('Nome: '),write(Nome),
	write('\n'),
	write('Código: '),write(Estafeta).


	


%------------------------------------ Query 2 -----------------------------------------------%
/*
Nome: query2(Código de cliente)
Descrição: Identifica  que  estafetas  entregaram encomenda(s) a  um determinado cliente; 
*/
query2(CodCliente) :- findall(X,encomenda(_,CodCliente,X,_,_,_,_,_,_,_),ListEstafetas),
	removeRepetidos(ListEstafetas,ListSemRepetidos),
	!,
	showQuery2(ListSemRepetidos).

	
showQuery2([]).
showQuery2([CodEstafeta|T]) :- write('Estafeta: '),
				estafeta(CodEstafeta,Nome),
				write(Nome),
				write(' - Código:'),
				write(CodEstafeta),
				write('\n'),
				showQuery2(T).

%------------------------------------- Query 3 ---------------------------------------------%
/*
Nome: query2(Código de estafeta)
Descrição: Identifica que clientes foram servidos por um determinado estafeta
*/
query3(CodEstafeta) :-	findall(X,encomenda(_,X,CodEstafeta,_,_,_,_,_,_,_),ListClientes),
	removeRepetidos(ListClientes,ListSemRepetidos),
	!,
	showQuery3(ListSemRepetidos).		

	
showQuery3([]).
showQuery3([CodCliente|T]) :- write('Cliente: '),
				cliente(CodCliente,Nome),
				write(Nome),
				write(' - Código:'),
				write(CodCliente),
				write('\n'),
				showQuery3(T).

%------------------------------------- Query 4 ---------------------------------------------%

%- A função predefinida plus só aceita inteiros, provavelmente há uma maneira 
%- melhor de fazer isto.
plusFloat(X,Y,R) :- R is X + Y.
/*
 * Nome: query4(DataEntrega) ou query(DataEntrega, Preco)
 * Descroção: Calcula o valor faturado pela Green Distribution num dado dia
 */

%- query4(DataEntrega, Preco) :- 
%-      findall(X, encomenda(_,_,_,_,_,_,_,X,DataEntrega), Found),
%- 	foldl(plus, Found, 0, Preco).

query4(DataEntrega) :- findall(X, encomenda(_,_,_,_,_,_,_,X,DataEntrega,_), Found),
	foldl(plusFloat, Found, 0, Preco), showQuery4(DataEntrega, Preco).

showQuery4(DataEntrega, Preco) :- write('DataEntrega: '),
                                  write(DataEntrega),
				  write(' -> Preco acumulado: '),
                                  write(Preco),
                                  write('\n').

%------------------------------------- Query 5 ---------------------------------------------%

query5(Top) :- findall(X, encomenda(_,_,_,_,_,_,_,_,_,X), ListaZonas),
	calcParOcorr(ListaZonas, ListaPares),
	!,
	ordDecrescente(ListaPares, Result),
	takeTopN(Top, Result, ResultTruncado),
	!,
	showQuery5(ResultTruncado).
	
showQuery5(Result) :- 
	write('\n'),
	write('Zona com maior volume de entregas'),
	write('\n'),
	write('----------------------------------------------------'),
	write('\n'),
	write(Result),
	write('\n').

ordDecrescente([],[]).
ordDecrescente([H|T], [Elem|Res]) :- 
    maxElem(T,H,Elem),
    (isEqual(H,Elem) -> ordDecrescente(T,Res), !; removePar(Elem,T,ListSemElem), ordDecrescente([H|ListSemElem], Res)).

removePar(_,[],[]).
removePar(Elem, [Elem/_|T], L) :- removePar(Elem, T, L).
removePar(Elem, [OutroElem|T], [OutroElem|Res]) :- removePar(Elem, T, Res).

isEqual(A/_,A).

takeTopN(_, [], []).
takeTopN(0, _, []).
takeTopN(N, [H|T], [H|Res]) :- B is N - 1, takeTopN(B, T, Res). 


%------------------------------------- Query 9 ---------------------------------------------%

query9(DataInicial, DataFinal, Entregues/NEntregues) :-
	findall(Estado,
	(encomenda(_,_,_,_,_,Estado,_,_,DataEntrega,_), pertenceData(DataInicial, DataFinal, DataEntrega)),
	Found),
	countX(entregue, Found, Entregues),
	countX(espera, Found, NEntregues).

query9(DataInicial, DataFinal) :-
	findall(Estado,
	(encomenda(_,_,_,_,_,Estado,_,_,DataEntrega,_), pertenceData(DataInicial, DataFinal, DataEntrega)),
	Found),
	countX(entregue, Found, Entregues),
	countX(espera, Found, NEntregues),
	showQuery9(DataInicial, DataFinal, Entregues, NEntregues).

showQuery9(DataInicial, DataFinal, Entregues, NEntregues) :- write('Data Incial: '),
															 write(DataInicial),
															 write(' ; Data Final: '),
															 write(DataFinal),
															 write('\nEntregues: '),
															 write(Entregues),
															 write("; Não Entregues: "),
															 write(NEntregues),
															 write('\n').



%-------------------------------  Menu  -------------------------------------%
menu :- repeat,
		nl,nl,
		write('|-----------------------------------------------------    Green Distribution    ------------------------------------------------------|'),nl,
		write('|                                                                                                                                     |'),nl,
		write('|  1.  Identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico.                                           |'),nl,
		write('|  2.  Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente.                                     |'),nl,
		write('|  3.  Identificar os clientes servidos por um determinado estafeta.                                                                  |'),nl,
		write('|  4.  Calcular o valor faturado pela Green Distribution num determinado dia.                                                         |'),nl,
		write('|  5.  Identificar quais as zonas com maior volume de entregas por parte da Green Distribution.                                       |'),nl,
		write('|  6.  Calcular a classificação média de satisfação de cliente para um determinado estafeta.                                          |'),nl,
		write('|  7.  Identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo.               |'),nl,
		write('|  8.  Identificar  o  número  total  de  entregas  pelos  estafetas,  num  determinado intervalo de tempo.                           |'),nl,
		write('|  9.  Calcular  o  número  de  encomendas  entregues  e  não  entregues  pela  Green Distribution, num determinado período de tempo. |'),nl,
		write('| 10.  Calcular o peso total transportado por estafeta num determinado dia.                                                            |'),nl,
		write('|                                                                                                                                     |'),nl,
		write('|-------------------------------------------------------------------------------------------------------------------------------------|'),nl,nl,
		write('Insira a Query pretendida: '), nl,
		read(Choice), Choice > 0, Choice =< 10,
		doit(Choice), Choice = 0, !.
		doit(1) :- query1().
		doit(2) :- write('Insira o código do cliente pretendido: '), read(Cod), query2(Cod).
		doit(3) :- write('Insira o código do estafeta pretendido: '), read(Cod), query3(Cod).
		doit(4) :- write('Insira a data de entrega pretendida: '), read(Data), query4(Data).
		doit(5) :- write('Insira o número de resultados pretendidos: '), read(Num), query5(Num).