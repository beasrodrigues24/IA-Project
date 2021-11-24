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
%-  ZonaEntrega
%-  Classificação).

:- dynamic(encomenda/11). 

encomenda(40,4,7,34,23,entregue,moto, 19.95, 21/11/2021, gothamCity, 4). 
encomenda(20,2,2,34,23,entregue,bicicleta, 20.1, 21/11/2021, centralCity, 2). %-- CodE:2 - CodC:2
encomenda(50,3,3,34,23,entregue,carro, 23, 01/06/2021, gothamCity, 3).
encomenda(10,2,4,34,23,entregue,bicicleta, 32, 30/05/2021, centralCity, 5). %-- CodE:4 - CodC:2
encomenda(10,2,5,34,23,entregue,bicicleta, 43, 11/09/2021, centralCity, 5). %-- CodE:5 - CodC:2
encomenda(30,6,6,34,23,entregue,moto, 10, 23/02/2018, wonderland, 3).
encomenda(40,2,4,34,23,entregue,bicicleta, 02, 12/02/2019, narnia, 4).  %-- CodE:4 - CodC:2
encomenda(50,8,8,34,23,entregue,carro,2, 21/11/2021, centralCity, 1).
encomenda(60,2,2,34,23,entregue,bicicleta, 28, 24/12/2020, narnia, 5).  %-- CodE:2 - CodC:2
encomenda(10,5,1,34,23,entregue,moto, 40, 26/12/2020, narnia, 4).    
encomenda(20,3,7,34,23,entregue,moto, 42, 01/01/2021, centralCity, 3). 
encomenda(70,2,4,34,23,entregue,bicicleta, 24.5, 21/11/2021, narnia, 2).   %-- CodE:4 - CodC:2

/* 
Optamos pelo código do estafeta para permitir a existência de estafetas com o mesmo nome, mas códigos de identificação distintos
*/
%-estafeta(Codigo,Nome).

:- dynamic(estafeta/2). 

estafeta(1,homemaranha).
estafeta(2,homemdeferro).
estafeta(3,hulk).
estafeta(4,doutorestranho).
estafeta(5,wanda).
estafeta(6,homemformiga).
estafeta(7,loki).
estafeta(8,capitamarvel).

%- cliente(Codigo,Cliente).

:- dynamic(cliente/2).

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
	encomenda(_,_,X,_,_,entregue,Transporte,_,_,_,_),
	tMaisEcologico(Transporte)
	),ListEstafetas),
	calcParOcorr(ListEstafetas,[H|T]),
	!,
	maxElem(T,H,Estafeta),
	estafeta(Estafeta,Nome),
	showQuery1(Nome,Estafeta),
	write('Insira n. para avançar'),read(_).

showQuery1(Nome,Estafeta) :- clear(),
	write('\n'),
	write('Estafeta que mais andou no transporte mais ecologico'),
	write('\n'),
	write('----------------------------------------------------'),
	write('\n'),
	write('Nome: '),write(Nome),
	write('\n'),
	write('Código: '),write(Estafeta),nl.

	


%------------------------------------ Query 2 -----------------------------------------------%
/*
Nome: query2(Código de cliente)
Descrição: Identifica  que  estafetas  entregaram encomenda(s) a  um determinado cliente; 
*/
query2(CodCliente) :- findall(X,encomenda(_,CodCliente,X,_,_,_,_,_,_,_,_),ListEstafetas),
	removeRepetidos(ListEstafetas,ListSemRepetidos),
	!,
	showQuery2(ListSemRepetidos),
	write('Insira n. para avançar'),read(_).

	
showQuery2([]).
showQuery2([CodEstafeta|T]) :-
				clear(),
				write('Estafeta: '),
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
query3(CodEstafeta) :-	findall(X,encomenda(_,X,CodEstafeta,_,_,_,_,_,_,_,_),ListClientes),
	removeRepetidos(ListClientes,ListSemRepetidos),
	!,
	showQuery3(ListSemRepetidos),
	write('Insira n. para avançar'), read(_).
		

	
showQuery3([]).
showQuery3([CodCliente|T]) :- 
				clear(),
				write('Cliente: '),
				cliente(CodCliente,Nome),
				write(Nome),
				write(' - Código:'),
				write(CodCliente),
				write('\n'),
				showQuery3(T).

%------------------------------------- Query 4 ---------------------------------------------%

/*
 * Nome: query4(DataEntrega) ou query(DataEntrega, Preco)
 * Descrição: Calcula o valor faturado pela Green Distribution num dado dia
 */

%- query4(DataEntrega, Preco) :- 
%-      findall(X, encomenda(_,_,_,_,_,_,_,X,DataEntrega), Found),
%- 	foldl(plus, Found, 0, Preco).

query4(DataEntrega) :- findall(X, encomenda(_,_,_,_,_,_,_,X,DataEntrega,_,_), Found),
	sumMap(Found, Preco), showQuery4(DataEntrega, Preco), write('Insira n. para avançar'), read(_).


showQuery4(DataEntrega, Preco) :- 
								clear(),
								write('DataEntrega: '),
                                write(DataEntrega),
				 				write(' -> Preço acumulado: '),
                                write(Preco),
                                write('\n').

%------------------------------------- Query 5 ---------------------------------------------%

/*
 * Nome: query5(Top) 
 * Descrição: Calcula os Top número de zonas com mais volume de encomendas
 * 
 * Raciocínio: 1º Passo - Criar uma lista de pares <Zona,Frequência da Zona> (calcParOcorr)
			   2º Passo - Ordenar os pares por ordem crescente de frequência (ordDecrescente)
			   3º Passo - Selecionar os Top primeiros elementos dessa lista (takeTopN)
*/
query5(Top) :- findall(X, encomenda(_,_,_,_,_,_,_,_,_,X,_), ListaZonas),
	calcParOcorr(ListaZonas, ListaPares),
	!,
	ordDecrescente(ListaPares, Result),
	takeTopN(Top, Result, ResultTruncado),
	!,
	showQuery5(ResultTruncado),
	write('Insira n. para avançar'), read(_).
	
showQuery5(Result) :-
	clear(),
	write('\n'),
	write('Zona com maior volume de entregas'),
	write('\n'),
	write('----------------------------------------------------'),
	write('\n'),
	write(Result),
	write('\n').



%------------------------------------- Query 6 ---------------------------------------------%

/*
 * Nome: query6(CodEstafeta)
 * Descrição: Calcula a classificação média de um estafeta. 
 * 
 * Raciocínio: 1º Passo - Recolhe para a ListaClassificacoes todas as classificações de um dado estafeta 
 			   2º Passo - Calcula o número de classificações através do comprimento da lista 
			   3º Passo - Soma todas as classificações da lista para a variável total 
			   4º Passo - Se o número de classificações for > 0, então calcula-se a classificação média dividindo o total pelo número, 
 						caso contrário é 0 (impede divisões por 0). 
			   5º Passo - Para efeitos de imprimir no ecrã, recolher o nome do estafeta. 
			   6º Passo - Imprimir a classificação média e o nome do estafeta.
*/
query6(CodEstafeta) :- findall(X, encomenda(_,CodEstafeta,_,_,_,_,_,_,_,_,X), ListaClassificacoes), 
					   length(ListaClassificacoes, NumeroClassif), 
					   foldl(plus, ListaClassificacoes, 0, Total),
					   (NumeroClassif > 0 -> ClassifMedia is Total/NumeroClassif; ClassifMedia is 0),
					   estafeta(CodEstafeta, Estafeta),
					   showQuery6(ClassifMedia, Estafeta),
					   write('Insira n. para avançar'),read(_).


showQuery6(Classif,Estafeta) :- 
	clear(),
	nl, write('----------------------------------------------------'),nl,
	write('Classificação média do estafeta '),
	write(Estafeta),
	write(' : '),
	write(Classif), nl,
	write('----------------------------------------------------'),nl.

%------------------------------------- Query 7 ---------------------------------------------%

/* 
 * Nome: query7(DataInicial,DataFinal)
 * Descrição: Calcula o número de entregas totais feitas por cada transporte num intervalo de tempo.
 * 
 * Raciocínio: 1º Passo - Recolher para uma ListaTransportes o transporte utilizado numa dada DataEntrega, desde que a DataEntregue 
 						  esteja dentro do intervalo de tempo.
 			   2º Passo - Criar uma lista de pares <Transporte, Frequência> (através da calcParOcorr).
  			   3º Passo - Imprimir os pares
*/
query7(DataInicial,DataFinal) :- findall(X, (encomenda(_,_,_,_,_,_,X,_,DataEntrega,_,_), 
										pertenceData(DataInicial, DataFinal, DataEntrega)), ListaTransportes), 
								calcParOcorr(ListaTransportes, ParesTransporteOcorr), 
								!, 
								showQuery7(ParesTransporteOcorr),
								write('Insira n. para avançar'),read(_).


showQuery7(Pares) :- 
	clear(),
	nl,
	printListaPares(Pares).

%------------------------------------- Query 8 ---------------------------------------------%

/* 
 * Nome: query8(DataInicial,DataFinal)
 * Descrição: Calcula o número total de entregas feitas por cada estafeta num intervalo de tempo
 * 
 * Raciocínio: 1º Passo - Filtrar os códigos dos estafetas que fizeram entregas numa DataEntrega contida no intervalo para uma ListaCodEstafetas 
 			   2º Passo - Converter em pares <CodEstafeta, Número de ocorrências> (através da calcParOcorr). 
			   3º Passo - Criar uma lista que crie uma lista semelhante em que os códigos do estafeta passam a ser o seu nome
			   4º Passo - Imprimir a nova lista de pares
*/
query8(DataInicial,DataFinal) :- findall(X, (encomenda(_,_,X,_,_,_,_,_,DataEntrega,_,_), 
									pertenceData(DataInicial, DataFinal, DataEntrega)), ListaCodEstafetas), 
								 calcParOcorr(ListaCodEstafetas, ParesCodEstafetaOcorr), 
								 !, 
								 encontraNomeEstafetas(ParesCodEstafetaOcorr, ParesNomeEstafetaOcorr),
								 showQuery8(ParesNomeEstafetaOcorr),
								 write('Insira n. para avançar'),read(_).


showQuery8(Pares) :- 
	clear(),
	nl,
	printListaPares(Pares).	

/*
 * Nome: encontraNomeEstafetas(Lista de pares <Codigo,Frequencia>, Lista de pares <Nome,Frequencia>).
 * Descrição: Substitui na primeira lista as ocorrências de codigo pelo nome correspondente
*/
encontraNomeEstafetas([],[]).
encontraNomeEstafetas([Cod/N|T], [Nome/N|Res]) :- estafeta(Cod,Nome), encontraNomeEstafetas(T,Res).


%------------------------------------- Query 9 ---------------------------------------------%

query9(DataInicial, DataFinal, Entregues/NEntregues) :-
	findall(Estado,
	(encomenda(_,_,_,_,_,Estado,_,_,DataEntrega,_,_), pertenceData(DataInicial, DataFinal, DataEntrega)),
	Found),
	countX(entregue, Found, Entregues),
	countX(espera, Found, NEntregues).

query9(DataInicial, DataFinal) :-
	findall(Estado,
	(encomenda(_,_,_,_,_,Estado,_,_,DataEntrega,_,_), pertenceData(DataInicial, DataFinal, DataEntrega)),
	Found),
	countX(entregue, Found, Entregues),
	countX(espera, Found, NEntregues),
	showQuery9(DataInicial, DataFinal, Entregues, NEntregues),
	write('Insira n. para avançar'),read(_).

showQuery9(DataInicial, DataFinal, Entregues, NEntregues) :- clear(),
															 write('Data Incial: '),
															 write(DataInicial),
															 write(' ; Data Final: '),
															 write(DataFinal),
															 write('\nEntregues: '),
															 write(Entregues),
															 write("; Não Entregues: "),
															 write(NEntregues),
															 write('\n').


%------------------------------------- Query 10 ---------------------------------------------%

query10(Data, PesoTotal) :-
    findall(Peso,
            encomenda(_,_,_,Peso,_,_,_,_,Data,_,_),
            Pesos),
    sumMap(Pesos, PesoTotal).

query10(Data) :-
    query10(Data, PesoTotal),
    showQuery10(Data, PesoTotal),
	write('Insira n. para avançar'),read(_).


showQuery10(Data, PesoTotal) :- clear(),
								write('Peso Total em '),
                                write(Data),
                                write(': '),
                                write(PesoTotal),
                                nl.


%-------------------------------  Leitura de ficheiros  ---------------------%

carregaFicheiro(Name) :- open(Name,read,Str),
					 leFicheiro(Str,Data),
					 maplist(asserta, Data).								   

leFicheiro(Stream,[]) :-
	at_end_of_stream(Stream).


leFicheiro(Stream,[X|L]) :-
	\+ at_end_of_stream(Stream),
	read(Stream,X),
	leFicheiro(Stream,L).

%-------------------------------  Menu  -------------------------------------%
menu :- repeat,
		clear(),
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
		write('| 10.  Calcular o peso total transportado por estafeta num determinado dia.                                                           |'),nl,
		write('| 11.  Carregar ficheiro.																											 |'),nl,
		write('|                                                                                                                                     |'),nl,
		write('|-------------------------------------------------------------------------------------------------------------------------------------|'),nl,nl,
		write('Insira a Query pretendida: '), nl,
		read(Choice), Choice > 0, Choice =< 11,
		doit(Choice), Choice = 0, !.
		doit(1) :- query1().
		doit(2) :- write('Insira o código do cliente pretendido: '), read(Cod), query2(Cod).
		doit(3) :- write('Insira o código do estafeta pretendido: '), read(Cod), query3(Cod).
		doit(4) :- write('Insira a data de entrega pretendida: '), read(Data), query4(Data).
		doit(5) :- write('Insira o número de resultados pretendidos: '), read(Num), query5(Num).
		doit(6) :- write('Insira o código do estafeta pretendido: '), read(Cod), query6(Cod).
		doit(7) :- write('Insira a data inicial: '), read(DataInicial), nl, 
				   write('Insira a data final  : '), read(DataFinal), 
				   query7(DataInicial, DataFinal).
		doit(8) :- write('Insira a data inicial: '), read(DataInicial), nl, 
				   write('Insira a data final  : '), read(DataFinal), 
				   query8(DataInicial,DataFinal).
		doit(9) :- write('Insira a data inicial: '), read(DataInicial), nl, 
				   write('Insira a data final  : '), read(DataFinal), 
		           query9(DataInicial,DataFinal).
		doit(10) :- write('Insira a data       : '), read(Data), query10(Data).
		doit(11) :- write('Insira o nome do ficheiro: '), read(Data), term_to_atom(Data, Name), carregaFicheiro(Name).
		
