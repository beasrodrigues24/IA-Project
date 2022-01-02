:- module(graph_queries, [
	gerarBFSQ/1,
	gerarDFSQ/1,
    gerarDFSIQ/1,
    gerarGulosaDistQ/1,
    gerarGulosaTranQ/1,
    gerarAEstrelaDistQ/1,
    gerarAEstrelaTranQ/1,
    gerarCircuitos/1,
    ordenaCircuitosPeso/1,
    ordenaCircuitosVolume/1,
    insereEncomendaCaminho/2
]).

:- use_module(graph_helpers).
:- use_module(knowledgeBase).
:- use_module(helpers).


% PROCURAS NÃO INFORMADAS

% DFS

dfsQ(Origem,Destino,Caminho) :- profundidade(Origem,Destino,Caminho).

% DFS Iterativa

dfsIQ(Origem,Destino,Caminho) :- profundidadeIterativa(Origem,Destino,Caminho).

% BFS

bfsQ(Origem,Destino,Caminho) :- largura(Origem,Destino,Caminho).

% PROCURAS INFORMADAS

% Gulosa

gulosaQ(Origem,Destino,CaminhoDist,CaminhoTran) :- gulosa(Origem,Destino,CaminhoDist,CaminhoTran).

% AEstrela

aestrelaQ(Origem,Destino,CaminhoDist,CaminhoTran) :- aestrela(Origem,Destino,CaminhoDist,CaminhoTran).

% Gerar Circuitos

gerarBFSQ(Circuitos) :- gerarBFS(Circuitos).

gerarDFSQ(Circuitos) :- gerarDFS(Circuitos).

gerarDFSIQ(Circuitos) :- gerarDFSIterativa(Circuitos).

gerarGulosaDistQ(Circuitos) :- gerarGulosaDist(Circuitos).

gerarGulosaTranQ(Circuitos) :- gerarGulosaTran(Circuitos).

gerarAEstrelaDistQ(Circuitos) :- gerarAestrelaDist(Circuitos).

gerarAEstrelaTranQ(Circuitos) :- gerarAestrelaTran(Circuitos).

gerarCircuitos([CircuitosGD,CircuitosGT,CircuitosAD,CircuitosAT,CircuitosBFS,CircuitosDFS,CircuitosDFSI]) :- 
	gerarGulosaDist(CircuitosGD),
	gerarGulosaTran(CircuitosGT),
	gerarAestrelaDist(CircuitosAD),
	gerarAestrelaTran(CircuitosAT),
	gerarBFS(CircuitosBFS),
	gerarDFS(CircuitosDFS),
	gerarDFSIterativa(CircuitosDFSI).

% Calcular tempo de um percurso dado peso e transporte

tempo([],_,_,0).
tempo([_],_,_,0).
tempo([Nodo1,Nodo2|OutrosNodos], Transporte, Peso, Tempo) :- 
    edge(Nodo1,Nodo2,_,Distancia,_),
    transporte(Transporte,_,VelocidadeMedia),
    velocidadeDiminui(Transporte, Diminuicao),
    Velocidade is VelocidadeMedia - Diminuicao*Peso,
    tempo([Nodo2|OutrosNodos], Transporte, Peso, Tempo2), 
    Tempo is Distancia/Velocidade + Tempo2.

% Identificar circuitos com maior número de entregas (por volume e peso). Recebem número de parâmetros pretendidos.

% Permite escrever para o terminal uma lista com os códigos do circuito
writeTopNCodigos([]).
writeTopNCodigos([H|T]) :- write('Código do circuito: '),
                           writeln(H),
                           writeTopNCodigos(T).

%% Por peso  

sumPesos([],0).
sumPesos([CodEnc|T], PesoTotal) :- 
    encomenda(CodEnc,_,_,_,Peso,_,_,_,_,_),
    sumPesos(T, PesoR), 
    PesoTotal is Peso + PesoR.

ligaCircuitosAoPesoEncomendas([],[]).
ligaCircuitosAoPesoEncomendas([CodCaminho|OutrosCodigos], [CodCaminho/PesoTotal|Res]) :-
    findall(CodEncomenda, encomendaCaminho(CodCaminho, CodEncomenda), ListaEncomendas),
    sumPesos(ListaEncomendas, PesoTotal),
    ligaCircuitosAoPesoEncomendas(OutrosCodigos, Res).

ordenaCircuitosPeso(N) :- ordenaCircuitosPeso(N, Ordenados),
                          writeTopNCodigos(Ordenados).

ordenaCircuitosPeso(N, Ordenados) :-
    findall(CodCaminho, encomendaCaminho(CodCaminho,_), CodigosCaminhoRepetidos), 
    removeRepetidos(CodigosCaminhoRepetidos, CodigosCaminho),
    !,
    ligaCircuitosAoPesoEncomendas(CodigosCaminho, CircuitosMaisValor),
    ordDecrescente(CircuitosMaisValor, OrdenadosTodos),
    takeTopN(N,OrdenadosTodos,Ordenados).

%% Por volume
sumVolumes([],0).
sumVolumes([CodEnc|T], VolumeTotal) :-
    encomenda(CodEnc,_,_,_,_,Volume,_,_,_,_),
    sumVolumes(T, VolumeR),
    VolumeTotal is Volume + VolumeR.

ligaCircuitosAoVolumeEncomendas([],[]).
ligaCircuitosAoVolumeEncomendas([CodCaminho|OutrosCodigos], [CodCaminho/VolumeTotal|Res]) :-
    findall(CodEncomenda, encomendaCaminho(CodCaminho, CodEncomenda), ListaEncomendas),
    sumVolumes(ListaEncomendas, VolumeTotal),
    ligaCircuitosAoVolumeEncomendas(OutrosCodigos, Res).

ordenaCircuitosVolume(N) :- ordenaCircuitosVolume(N, Ordenados),
                            writeTopNCodigos(Ordenados).

ordenaCircuitosVolume(N,Ordenados) :-
    findall(CodCaminho, encomendaCaminho(CodCaminho,_), CodigosCaminhoRepetidos), 
    removeRepetidos(CodigosCaminhoRepetidos, CodigosCaminho),
    !,
    ligaCircuitosAoVolumeEncomendas(CodigosCaminho, CircuitosMaisValor),
    ordDecrescente(CircuitosMaisValor, OrdenadosTodos),
    takeTopN(N,OrdenadosTodos,Ordenados).   

%% Permite a inserção de uma associação entre uma encomenda e um caminho
insereEncomendaCaminho(CodC, CodE) :- atom_concat('encomendaCaminho(', CodC, Parte), atom_concat(Parte, ',', Parte2), atom_concat(Parte2, CodE, Parte3),
                                      atom_concat(Parte3, ').', Final), evolucao(Final).