:- use_module(helpers).
:- use_module(knowledgeBase).
:- use_module(graph_queries).

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
Nome: query3(Código de estafeta)
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

%
% Nome: query4(DataEntrega, Preco)
% Descrição: Calcula o valor faturado pela Green Distribution num dado dia
% Raciocínio: 1º Passo - Verifica as encomendas entregues num determinado dia
%             2º Passo - Cria uma lista com os preços de cada encomenda pertence
%             à lista do 1º Passo.
%             3º Passo - Soma os preços todos.
%

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
/*
 * Nome: query9(DataInicial, DataFinal, Entregues,NEntregues)
 *
 * Descrição: Calcula o número de encomendas entregues e não entregues, com base num
 * intervalo de tempo. Assim dada uma DataInicial uma entrega é entregue se houver
 * um registo de encomenda no intervalo de DataInicial
 * e DataFinal e se a entrega for feita até DataFinal.
 * Uma entrega é não entregue se houver um registo de encomenda no intervalo de
 * DataInicial e DataFinal, no entanto não haja registo de entrega ou então a data
 * de entrega seja superior a DataFinal.
 *
 * Raciocínio: 1º Passo - Filtrar os códigos de encomendas que estão associados a uma
 *                          data de registo entre DataInicial e DataFinal.
 *             2º Passo - Filtrar os códigos de encomendas entregues entre DataInicial e DataFinal
 *                          e pertençam à lista do 1º Passo.
 *             3º Passo - Calcular tamanho da lista do 2º Passo
 *             4º Passo - Calular tamamho da lista 1º Passo
 *             5º Passo - Entregues são o resultado 3º Passo e não entregues a diferença entre
 *                          os resultados 4º e 3º Passos
*/
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

%
% Nome : query10(Data, PesoTotais)
%
% Descrição: Calcula o peso total transportado/entregue por cada estafeta numa determinada data
%
% Raciocínio: 1º Passo - Recolher todas as entradas de códigos de estafetas e peso de
%                           encomendas entregues em Data.
%             2º Passo - Somar todos os Pesos relativos ao mesmo código de Estafeta.

query10(Data, PesoTotais) :-
    findall(CodEstafeta/Peso,
            (encomenda(CodEncomenda,Data,_),
             encomenda(CodEncomenda,_,_,CodEstafeta,Peso,_,_,_,_,_)),
            PesosEstafeta),
    joinPesos(PesosEstafeta, PesoTotais).

query10(Data) :-
    query10(Data, PesoTotais),
    showQuery10(Data, PesoTotais),
	write('Insira n. para avançar'),read(_).


showQuery10(Data, PesoTotais) :-
    write('Pesos Totais por Estafeta em '),
    write(Data),
    write(' :'), nl,
    showQuery10List(PesoTotais).

showQuery10List([]).
showQuery10List([CodEstafeta/PesoTotal|T]) :-
    write('Codigo Estafeta: '),
    write(CodEstafeta),
    write('; Peso Total: '),
    write(PesoTotal),
    nl,
    showQuery10List(T).

% Nome : joinPesos(List, JoinedList)
% Descrição : Soma os Pesos de códigos iguais, para que fique apenas uma lista com códigos todos distintos
% Raciocínio : Faz-se uma inserção especial de todos os regitos a partir de uma lista vazia.
joinPesos([],[]).
joinPesos([H|T], Joined) :- joinPesos(T, JoinedTail), insertPeso(H, JoinedTail, Joined).

% Nome: insertPeso(Registos, List, JoinedList)
% Descrição: Insere um registo (Codigo/Peso) numa JoinedList ordenada por códigos de estafeta.
%               Se a JoinedList for vazia insere apenas o registo.
%               Se o valor do código do estafeta à cabeça da JoinedList for menor que o registo
%               que se pretende inserir, insere-se esse registo na tail da JoinedList.
%               Se o valor do código do estafeta à cabeça da JoinesList for maior que o registo
%               que se pretende inserir, insere-se esse registo na posição da entrada à cabeça
%               da JoinedList.
insertPeso(Cod/Peso, [] ,[Cod/Peso]).
insertPeso(Cod/Peso, [Cod/PesoJ|T] , [Cod/PesoR|T]) :- PesoR is Peso + PesoJ.
insertPeso(Cod/Peso, [CodJ/PesoJ|T] , [CodJ/PesoJ|R]) :- Cod > CodJ, insertPeso(Cod/Peso, T, R).
insertPeso(Cod/Peso, [CodJ/PesoJ|T] ,[Cod/Peso|[CodJ/PesoJ|T]]) :- Cod < CodJ.
%------------------------------------- Query 11 ---------------------------------------------%

query11(Clientes) :- findall(X/Y,cliente(X,Y),Clientes).

%------------------------------------- Query 12 ---------------------------------------------%

query12(Estafetas) :- findall(X/Y,estafeta(X,Y),Estafetas).


%------------------------------------- Query 13 ---------------------------------------------%

query13(Encomendas) :- findall(X/Y/Z,encomenda(X,Y,Z),Encomendas).


%------------------------------------- Query 14 ---------------------------------------------%

query14(Encomendas) :- findall(A/B/C/D/E/F/G/H/I/J,encomenda(A,B,C,D,E,F,G,H,I,J),Encomendas).

%-------------------------------  Leitura de ficheiros  ---------------------%

/*
 * Nome: carregaFicheiro(Name,Data)
 * Descrição: Permite carregar um ficheiro linha a linha para uma lista
*/
carregaFicheiro(Name,Data) :- open(Name,read,Str),
					 leFicheiro(Str,Data).

/*
 * Nome: carregaFicheiro(Name)
 * Descrição: Análoga a carregaFicheiro(Name,Data)
*/
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

%% Permite a inserção de uma associação entre uma encomenda e um caminho
insereEncomendaCaminho(CodC, CodE) :- atom_concat('encomendaCaminho(', CodC, Parte), atom_concat(Parte, ',', Parte2), atom_concat(Parte2, CodE, Parte3),
                                      atom_concat(Parte3, ').', Final), (evolucao(Final) -> write(Final) ; write('falso')).

%-------------------------------  Menu  -------------------------------------%

printIndividuos([]).
printIndividuos([Cod/Name|T]) :- write('Codigo: '), write(Cod), write('   '), write('Nome: '), writeln(Name), printIndividuos(T).

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
		writeln(' 15.  Gerar todos os circuitos possíveis.'),
		writeln(' 16.  Gerar circuitos através de DFS.'),
		writeln(' 17.  Gerar circuitos através de BFS.'),
		writeln(' 18.  Gerar circuitos através de DFS iterativa.'),
		writeln(' 19.  Gerar circutios através de Pesquisa Gulosa com base na distância.'),
		writeln(' 20.  Gerar circuitos através de Pesquisa Gulosa com base no trânsito.'),
		writeln(' 21.  Gerar circuitos através de Pesquisa A* com base na distância.'),
		writeln(' 22.  Gerar circuitos através de Pesquisa A* com base no trânsito.'),
		writeln(' 23.  Associar o código de uma encomenda ao código de um circuito.'),
		writeln(' 24.  Obter o top N de circuitos com base no Peso.'),
		writeln(' 25.  Obter o top N de circuitos com base no Volume.'),
		writeln(' 26.  Comparar circuitos com base na distância.'),
		writeln(' 27.  Comparar circuitos com base no tempo.'),
		writeln(' 28.  Dada uma lista de circuitos escolher o mais rápido.'),
		writeln(' 29.  Dada uma lista de circuitos e a data de início de uma entrega, fornecer o circuito mais ecológico.'),
		writeln(' 30.  Imprimir todos os circuitos possíveis.'),
		writeln(' 31.  Imprimir todas as associações entre códigos de circuito e códigos de encomenda.'),
		writeln('  0.  Sair do menu.'),
		nl,																																	  
		writeln('-------------------------------------------------------------------------------------------------------------------------------------'),nl,
		writeln('Insira a Query pretendida: '),
		read(Choice), Choice >= 0, Choice =< 31,
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
case(11) :- query11(Clientes), printIndividuos(Clientes), write('Insira n. para avançar'),read(_), !.
case(12) :- query12(Estafetas), printIndividuos(Estafetas), write('Insira n. para avançar'), read(_), !.
case(13) :- write('Insira o termo: '), read(Termo), evolucao(Termo), write(Termo), !.
case(14) :- write('Insira o nome do ficheiro: '), read(Data), term_to_atom(Data,Nome), carregaFicheiro(Nome),!.
case(15) :- gerarCircuitos2(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(16) :- gerarDFS2Q(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(17) :- gerarBFS2Q(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(18) :- gerarDFSI2Q(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(19) :- gerarGulosaDis2tQ(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(20) :- gerarGulosaTran2Q(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(21) :- gerarAEstrelaDist2Q(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(22) :- gerarAEstrelaTran2Q(_), writeln('Os circuitos foram gerados com sucesso.'), writeln('Insira n. para avançar.'), read(_), !.
case(23) :- write('Insira o termo [encomendaCaminho(cod_circuito,cod_encomenda).]: '), read(EncCam), evolucao(EncCam), writeln(EncCam),
			writeln('Insira n. para avançar'), read(_), !.
case(24) :- writeln('Quantos circuitos pretende obter? '), read(N), ordenaCircuitosPeso(N), writeln('Insira n. para avançar.'), read(_), !.
case(25) :- writeln('Quantos circuitos pretende obter? '), read(N), ordenaCircuitosVolume(N), writeln('Insira n. para avançar.'), read(_), !.
case(26) :- writeln('Insira o código do primeiro caminho: '), read(Cod1), nl, writeln('Insira o código do segundo caminho'), read(Cod2), nl,
			comparaCaminho(Cod1, Cod2, Res), (Res > 0 -> write('O caminho '), write(Cod1), write(' é '), write(Res), writeln(' unidades maior em distância.'); 
											  (Res < 0 -> getAbs(Res, AbsR), write('O caminho '), write(Cod2), write(' é '), write(AbsR), writeln(' unidades maior em distância.');
											  writeln('Os caminhos são idênticos em termos de distância.'))),
											  writeln('Insira n. para avançar.'), read(_), !.
case(27) :- writeln('Insira o código do primeiro caminho: '), read(Cod1), nl, writeln('Insira o código do segundo caminho: '), read(Cod2), nl, writeln('Insira a data base: '),
			read(Data), comparaCaminho(Cod1, Cod2, Data, Res), write('Hello!'), (Res > 0 -> write('O caminho '), write(Cod1), write(' é '), write(Res), writeln(' unidades maior em duração.'); 
											  (Res < 0 -> getAbs(Res, AbsR), write('O caminho '), write(Cod2), write(' é '), write(AbsR), writeln(' unidades maior em duração.');
											  writeln('Os caminhos são idênticos em termos de tempo.'))),
											  writeln('Insira n. para avançar.'), read(_), !.
case(28) :- writeln('Insira a lista de códigos: '), read(List), nl, minCaminho(List, Res), write('O caminho '), write(Res), writeln(' é o mais rápido.'), 
			writeln('Insira n. para avançar.'), read(_), !.
case(29) :- writeln('Insira a lista de códigos: '), read(List), nl, writeln('Insira a data base: '), read(Data), nl, minCaminho(List, Data, Res), write('O caminho '), write(Res), writeln(' é o mais ecológico.'),
			writeln('Insira n. para avançar.'), read(_), !.
case(30) :- printCircuitos(), writeln('Insira n. para avançar.'), read(_), !.
case(31) :- printEncomendaCaminho(), writeln('Insira n. para avançar'), read(_), !.
