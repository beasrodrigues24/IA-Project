:- use_module(helpers).
:- use_module(knowledgeBase).

:-set_prolog_flag(discontiguous_warnings,off).

%------------------------------ Query 1 -------------------------------------%

/*
Nome: query1().
Descrição: identifica o estafeta que utilizou mais vezes um meio de transporte mais ecológico. Caso existir um novo meio de transporte mais ecológico do que a bicicleta, basta alterar o facto "tMaisEscologico".
Ex: consideramos, atualmente em nossa base de conhecimento, a bicicleta sendo o transporte mais ecológico. Assim, o resultado desta query será a identificação do estafeta que mais vezes realizou entregas usando bicicleta. 
*/ 

query1() :- query1(Estafetas),showQuery1(Estafetas),write('Insira n. para avançar'),read(_).

query1(Estafetas) :- 
	findall(CodEstafeta,(
			encomenda(X,_,_),
			encomenda(X,_,_,CodEstafeta,_,_,Transporte,_,_,_),
			tMaisEcologico(Transporte)
		),ListEstafetas),
	!,
	calcParOcorr(ListEstafetas,[H|T]),
	maxOcor(T,H,Max),
	getEstafetasMax([H|T],Max,EstafetasCod),
	getEstafetasNome(EstafetasCod,Estafetas).

showQuery1([]) :- write('\n').
showQuery1([EstafetaCod/EstafetaNome|T]) :-
	write('\n'),
	write('Nome: '),write(EstafetaNome),
	write('\n'),
	write('Código: '),write(EstafetaCod),
	showQuery1(T).

/* 
 * Nome: getEstafetasNome(Lista de Códigos de Estafetas, Lista de Pares <CodEstafeta,NomeEstafeta>)
 * Descrição: Recebe uma lista Códigos de Estefetas e preenche a segunda lista com o Código + nome do Estafeta.
*/
getEstafetasNome([],[]).
getEstafetasNome([EstafetaCod|T],[EstafetaCod/EstafetaNome|T2]) :- estafeta(EstafetaCod,EstafetaNome), getEstafetasNome(T,T2).


%------------------------------------ Query 2 -----------------------------------------------%
/*
Nome: query2(Código de cliente)
Descrição: Identifica  que  estafetas  entregaram encomenda(s) a  um determinado cliente; 
*/

query2(CodCliente) :- query2(CodCliente,Estafetas),showQuery2(Estafetas),write('Insira n. para avançar'),read(_).

query2(CodCliente,Estafetas) :- findall(CodEstafeta,(
			encomenda(X,_,_),
			encomenda(X,_,CodCliente,CodEstafeta,_,_,_,_,_,_)
		),ListEstafetas),
	!,
	removeRepetidos(ListEstafetas,EstafetasCod),
	getEstafetasNome(EstafetasCod,Estafetas).
	
showQuery2([]) :- write('\n').
showQuery2([CodEstafeta/NomeEstafeta|T]) :-  write('Estafeta: '),write(NomeEstafeta),
				write(' - Código:'),
				write(CodEstafeta),
				write('\n'),
				showQuery2(T).

%------------------------------------- Query 3 ---------------------------------------------%
/*
Nome: query2(Código de estafeta)
Descrição: Identifica que clientes foram servidos por um determinado estafeta
*/

query3(CodEstafeta) :- query3(CodEstafeta,Clientes),showQuery3(Clientes),write('Insira n. para avançar'), read(_).

query3(CodEstafeta,Clientes) :-	findall(CodCliente,(
			encomenda(X,_,_),
			encomenda(X,_,CodCliente,CodEstafeta,_,_,_,_,_,_)
		),ListClientes),
	!,
	removeRepetidos(ListClientes,ListSemRepetidos),
	getClientesNome(ListSemRepetidos,Clientes).	

getClientesNome([],[]).
getClientesNome([ClienteCod|T],[ClienteCod/ClienteNome|T2]) :- cliente(ClienteCod,ClienteNome), getClientesNome(T,T2).
	
showQuery3([]).
showQuery3([CodCliente/NomeCliente|T]) :-   write('Cliente: '),
				write(NomeCliente),
				write(' - Código:'),
				write(CodCliente),
				write('\n'),
				showQuery3(T).

%------------------------------------- Query 4 ---------------------------------------------%

/*
 * Nome: query4(DataEntrega) ou query(DataEntrega, Preco)
 * Descrição: Calcula o valor faturado pela Green Distribution num dado dia
 */

query4(D/M/Y/_,Preco) :-
    findall(CodEnc,
            encomenda(CodEnc, D/M/Y/_,_),
            LEntregues),
    findall(Preco,
            (encomenda(CodEnc,_,_,_,_,_,_,Preco,_,_),
             member(CodEnc, LEntregues)),
            LPrecos),
    sumMap(LPrecos, Preco).

query4(Data) :-
    query4(Data, Preco),
    showQuery4(Data, Preco),
	write('Insira n. para avançar'), read(_).


showQuery4(DataEntrega, Preco) :- 
								write('DataEntrega: '),
                                write(DataEntrega),
				 				write(' -> Preço acumulado: '),
                                write(Preco),
                                write('\n').

%------------------------------------- Query 5 ---------------------------------------------%

/*
 * Nome: query5(Top,ResultTruncado) 
 * Descrição: Calcula os Top número de zonas com mais volume de encomendas
 * 
 * Raciocínio: 1º Passo - Criar uma lista de pares <Zona,Frequência da Zona> (calcParOcorr)
			   2º Passo - Ordenar os pares por ordem crescente de frequência (ordDecrescente)
			   3º Passo - Selecionar os Top primeiros elementos dessa lista (takeTopN)
*/
query5(Top,ResultTruncado) :- findall(X, (encomenda(Cod,_,_,_,_,_,_,_,_,X), encomenda(Cod,_,_)), ListaZonas),
							calcParOcorr(ListaZonas, ListaPares),
							!,
							ordDecrescente(ListaPares, Result),
							takeTopN(Top, Result, ResultTruncado),
							!.

	
/*
 * Nome: showQuery5(Result)
 * Descrição: Imprime Result no terminal
*/
showQuery5(Result) :-
	write('\n'),
	write('Zona com maior volume de entregas'),
	write('\n'),
	write('----------------------------------------------------'),
	write('\n'),
	write(Result),
	write('\n').


/*
 * Nome: query5(Top)
 * Descrição: Análoga à query5(Top,ResultTruncado). 
*/	
query5(Top) :- query5(Top,ResultTruncado),
			showQuery5(ResultTruncado),
			write('Insira n. para avançar'), read(_).

%------------------------------------- Query 6 ---------------------------------------------%

/*
 * Nome: query6(CodEstafeta,ClassifMedia)
 * Descrição: Calcula a classificação média de um estafeta. 
 * 
 * Raciocínio: 1º Passo - Recolhe para a ListaClassificacoes todas as classificações de um dado estafeta 
 			   2º Passo - Calcula o número de classificações através do comprimento da lista 
			   3º Passo - Soma todas as classificações da lista para a variável total 
			   4º Passo - Se o número de classificações for > 0, então calcula-se a classificação média dividindo o total pelo número, 
 						caso contrário é 0 (impede divisões por 0).  
*/
query6(CodEstafeta, ClassifMedia) :- findall(X, (encomenda(CodEncomenda,_,_,CodEstafeta,_,_,_,_,_,_), 
													encomenda(CodEncomenda,_,X)), 
											ListaClassificacoes), 
									length(ListaClassificacoes, NumeroClassif), 
									foldl(plus, ListaClassificacoes, 0, Total),
									(NumeroClassif > 0 -> ClassifMedia is Total/NumeroClassif; ClassifMedia is 0).

/*
 * Nome: showQuery6(Classif,Estafeta)
 * Descrição: Imprime para o ecrã a informação
*/ 
showQuery6(Classif,Estafeta) :- 
						nl, write('----------------------------------------------------'),nl,
						write('Classificação média do estafeta '),
						write(Estafeta),
						write(' : '),
						write(Classif), nl,
						write('--------------------------------------------------------'),nl.

/*
 * Nome: query6(CodEstafeta,ClassifMedia)
 * Descrição: Análoga à query6(CodEstafeta,ClassifMedia). 
*/
query6(CodEstafeta) :- query6(CodEstafeta,ClassifMedia),
					   estafeta(CodEstafeta, Estafeta),
					   showQuery6(ClassifMedia, Estafeta),
					   write('Insira n. para avançar'),read(_).


%------------------------------------- Query 7 ---------------------------------------------%

/* 
 * Nome: query7(DataInicial,DataFinal,ParesTransporteOcorr)
 * Descrição: Calcula o número de entregas totais feitas por cada transporte num intervalo de tempo.
 * 
 * Raciocínio: 1º Passo - Recolher para uma ListaTransportes o transporte utilizado numa dada DataEntrega, desde que a DataEntregue 
 						  esteja dentro do intervalo de tempo.
 			   2º Passo - Criar uma lista de pares <Transporte, Frequência> (através da calcParOcorr).
*/
query7(DataInicial,DataFinal,ParesTransporteOcorr) :- findall(X, (encomenda(CodEnc,_,_,_,_,_,X,_,_,_), 
																encomenda(CodEnc,DataEntrega,_),
		   														pertenceData(DataInicial, DataFinal, DataEntrega)), 
															ListaTransportes), 
   													  calcParOcorr(ListaTransportes, ParesTransporteOcorr), 
   													  !.


/* 
 * Nome: showQuery7(Pares)
 * Descrição: Imprime a informação para o ecrã
*/
showQuery7(Pares) :- 
	nl,
	printListaPares(Pares).
							
/*
 * Nome: query7(DataInicial,DataFinal)
 * Descrição: Análoga à query7(DataInicial,DataFinal,ParesTransporteOcorr). 
*/
query7(DataInicial,DataFinal) :- query7(DataInicial,DataFinal,ParesTransporteOcorr),
								showQuery7(ParesTransporteOcorr),
								write('Insira n. para avançar'),read(_).


%------------------------------------- Query 8 ---------------------------------------------%

/* 
 * Nome: query8(DataInicial,DataFinal,ParesNomeEstafetaOcorr)
 * Descrição: Calcula o número total de entregas feitas por cada estafeta num intervalo de tempo
 * 
 * Raciocínio: 1º Passo - Filtrar os códigos dos estafetas que fizeram entregas numa DataEntrega contida no intervalo para uma ListaCodEstafetas 
 			   2º Passo - Converter em pares <CodEstafeta, Número de ocorrências> (através da calcParOcorr). 
			   3º Passo - Criar uma lista que crie uma lista semelhante em que os códigos do estafeta passam a ser o seu nome
*/
query8(DataInicial,DataFinal,ParesNomeEstafetaOcorr) :- findall(X, (encomenda(CodEnc,_,_,X,_,_,_,_,_,_), 
								 encomenda(CodEnc,DataEntrega,_),
								pertenceData(DataInicial, DataFinal, DataEntrega)), ListaCodEstafetas), 
							 calcParOcorr(ListaCodEstafetas, ParesCodEstafetaOcorr), 
							 !, 
							 encontraNomeEstafetas(ParesCodEstafetaOcorr, ParesNomeEstafetaOcorr).

/*
 * Nome: showQuery8(Pares)
 * Descrição: Imprime o resultado para o ecrã
*/
showQuery8(Pares) :- 
	nl,
	printListaPares(Pares).	

/*
 * Nome: query8(DataInicial,DataFinal)
 * Descrição: Análoga à query8(DataInicial,DataFinal,ParesNomeEstafetaOcorr).
*/
					
query8(DataInicial,DataFinal) :- query8(DataInicial,DataFinal,ParesNomeEstafetaOcorr),
							  showQuery8(ParesNomeEstafetaOcorr),
							  write('Insira n. para avançar'), read(_).

/*
 * Nome: encontraNomeEstafetas(Lista de pares <Codigo,Frequencia>, Lista de pares <Nome,Frequencia>).
 * Descrição: Substitui na primeira lista as ocorrências de codigo pelo nome correspondente
*/
encontraNomeEstafetas([],[]).
encontraNomeEstafetas([Cod/N|T], [Nome/N|Res]) :- estafeta(Cod,Nome), encontraNomeEstafetas(T,Res).


%------------------------------------- Query 9 ---------------------------------------------%

% Estou a supor que o pedido da encomenda, mesmo quando ele é entregue, não
% é apagado.
%
query9(DataInicial, DataFinal, Entregues,NEntregues) :-
    findall(CodEnc,
            (encomenda(CodEnc,_,_,_,_,_,_,_,DataCriacao,_),
            pertenceData(DataInicial, DataFinal, DataCriacao)),
            LDatas),
    findall(CodEnc,
            (encomenda(CodEnc,DataEntrega,_),
             pertenceData(DataInicial, DataFinal, DataEntrega),
             member(CodEnc, LDatas)),
            LEntregues),
    length(LEntregues, Entregues),
    length(LDatas, Todos),
    NEntregues is Todos - Entregues.


query9(DataInicial, DataFinal) :-
    query9(DataInicial, DataFinal, Entregues, NEntregues),
	showQuery9(DataInicial, DataFinal, Entregues, NEntregues),
	write('Insira n. para avançar'),read(_).

showQuery9(DataInicial, DataFinal, Entregues, NEntregues) :-
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
            encomenda(_,_,_,_,Peso,_,_,_,Data,_),
            Pesos),
    sumMap(Pesos, PesoTotal).

query10(Data) :-
    query10(Data, PesoTotal),
    showQuery10(Data, PesoTotal),
	write('Insira n. para avançar'),read(_).


showQuery10(Data, PesoTotal) :- 
								write('Peso Total em '),
                                write(Data),
                                write(': '),
                                write(PesoTotal),
                                nl.

%------------------------------------- Query 11 ---------------------------------------------%

query11(Clientes) :- findall(X/Y,cliente(X,Y),Clientes).

%------------------------------------- Query 12 ---------------------------------------------%

query12(Estafetas) :- findall(X/Y,estafeta(X,Y),Estafetas).

%-------------------------------  Leitura de ficheiros  ---------------------%


carregaFicheiro(Name,Data) :- open(Name,read,Str),
					 leFicheiro(Str,Data).

carregaFicheiro(Name) :- open(Name,read,Str),
					 leFicheiro(Str,Data),
					 aplicaEvolucao(Data).

aplicaEvolucao([]).
aplicaEvolucao([H|T]) :- evolucao(H),aplicaEvolucao(T).
aplicaEvolucao([_|T]) :- aplicaEvolucao(T).


leFicheiro(Stream,[]) :-
	at_end_of_stream(Stream).

leFicheiro(Stream,[X|L]) :-
	\+ at_end_of_stream(Stream),
	read(Stream,X),
	leFicheiro(Stream,L).

%-------------------------------  Menu  -------------------------------------%
menu :- repeat,
		clear(),
		writeln('-----------------------------------------------------    Green Distribution    ------------------------------------------------------'),
		nl,
		writeln('  1.  Identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico.'),
		writeln('  2.  Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente.'),
		writeln('  3.  Identificar os clientes servidos por um determinado estafeta.'),
		writeln('  4.  Calcular o valor faturado pela Green Distribution num determinado dia.'),
		writeln('  5.  Identificar quais as zonas com maior volume de entregas por parte da Green Distribution.'),
		writeln('  6.  Calcular a classificação média de satisfação de cliente para um determinado estafeta.'),
		writeln('  7.  Identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo.'),
		writeln('  8.  Identificar  o  número  total  de  entregas  pelos  estafetas,  num  determinado intervalo de tempo.'),
		writeln('  9.  Calcular  o  número  de  encomendas  entregues  e  não  entregues  pela  Green Distribution, num determinado período de tempo.'),
		writeln(' 10.  Calcular o peso total transportado por estafeta num determinado dia.'),
		writeln(' 11.  Imprimir os clientes.'),
		writeln(' 12.  Imprimir os estafetas.'),
		writeln(' 13.  Inserir conhecimento.'),
		writeln(' 14.  Carregar conhecimento a partir de um ficheiro.'),
		writeln('  0.  Sair do menu.'),
		nl,																																	  
		writeln('-------------------------------------------------------------------------------------------------------------------------------------'),nl,
		writeln('Insira a Query pretendida: '),
		read(Choice), Choice >= 0, Choice =< 14,
		(Choice = 0 -> !,fail;true), 
		case(Choice),fail.

case(0) :- fail.
case(1) :- query1(),!.
case(2) :- write('Insira o código do cliente pretendido: '), read(Cod), query2(Cod),!.
case(3) :- write('Insira o código do estafeta pretendido: '), read(Cod), query3(Cod),!.
case(4) :- write('Insira a data de entrega pretendida: '), read(Data), query4(Data),!.
case(5) :- write('Insira o número de resultados pretendidos: '), read(Num), query5(Num),!.
case(6) :- write('Insira o código do estafeta pretendido: '), read(Cod), query6(Cod),!.
case(7) :- write('Insira a data inicial: '), read(DataInicial), nl, 
		   write('Insira a data final  : '), read(DataFinal), 
		   query7(DataInicial, DataFinal), !.
case(8) :- write('Insira a data inicial: '), read(DataInicial), nl, 
		   write('Insira a data final  : '), read(DataFinal), 
		   query8(DataInicial,DataFinal),!.
case(9) :- write('Insira a data inicial: '), read(DataInicial), nl, 
		   write('Insira a data final  : '), read(DataFinal), 
           query9(DataInicial,DataFinal),!.
case(10) :- write('Insira a data: '), read(Data), query10(Data),!.
case(11) :- query11(Clientes), writeln(Clientes), write('Insira n. para avançar'),read(_), !.
case(12) :- query12(Estafetas), writeln(Estafetas), write('Insira n. para avançar'), read(_), !.
case(13) :- write('Insira o termo: '), read(Termo), evolucao(Termo), write(Termo), !.
case(14) :- write('Insira o nome do ficheiro: '), read(Data), term_to_atom(Data,Nome), carregaFicheiro(Nome),!.