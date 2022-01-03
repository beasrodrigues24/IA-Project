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
    ordenaCircuitosPeso/2,
    ordenaCircuitosVolume/2,
    comparaCircuitos/3,
    comparaCircuitos/6,
    printCircuitos/0
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


% Compara dois circuitos com base em distância
comparaCircuitos(C1, C2, R) :-
    distanciaCircuito(C1, D1),
    distanciaCircuito(C2, D2),
    R is D1 - D2.

% Compara dois circuitos com base na lista de encomendas que podem ser feitas
% em relação ao tempo, sendo que escolhe sempre a opção mais ecológica para as encomendas
% Sendo que DataAtual é a data que se pretende estabelecer como base de comparação.
comparaCircuitos(C1, C2, E1, E2, DataAtual, R) :-
    tempoCircuito(C1, E1, DataAtual, T1),
    tempoCircuito(C2, E2, DataAtual, T2),
    R is T1 - T2.

% Calcula o tempo do circuito escolhendo o transporte indicado para
% as encomendas que serão transportadas assim como o peso total das
% encomendas
tempoCircuito(C, E, DataAtual, Temp) :-
    sumPesos(E, P),
    maxDuracao(E, DataAtual, MaxDuracao),
    transporteDisponiveis(P, Transportes),
    calculaMinTempo(C,MaxDuracao, P, Transportes, Temp).

calculaMinTempo(C, MaxDuracao, P, Transportes, T) :-
    calculaMinTempoAux(C, MaxDuracao, P, Transportes, [T|_]).

calculaMinTempoAux(_,_,_,[],[]).
calculaMinTempoAux(C, MaxDuracao, P, [Transporte|T1], [Temp | T2]) :-
    tempo(C, Transporte, P, Temp),
    Temp =< MaxDuracao,
    calculaMinTempoAux(C, MaxDuracao, P, T1, T2), !.
calculaMinTempoAux(C, MaxDuracao, P, [_|T1], T2) :-
    calculaMinTempoAux(C, MaxDuracao, P, T1, T2).

transporteDisponiveis(Peso, T) :-
    findall(Transporte, (tMaisEcologico(Transporte), transporte(Transporte, PesoMax,_), Peso =< PesoMax) , T).

maxDuracao(E, DataAtual, MaxDuracao) :-
    dataStamp(DataAtual, Stamp),
    findall(X,
    (encomenda(Cod,TempMax,_,_,_,_,_,_,DataCriacao,_),
     member(Cod, E),
     dataStamp(DataCriacao, Stamp1),
     X is (Stamp1 + (TempMax * 3600)) - Stamp,
     X > 0),
    R),
    length(R, LenR),
    length(E, LenE),
    LenR == LenE,
    min_list(R, Max),
    MaxDuracao is Max / 3600.

% Imprime todos os circuitos existentes na base de conhecimento.
printCircuitos() :- findall(Cod/Circ, caminho(Cod,Circ), Lista), writeCircuitos(Lista).

writeLista([]).
writeLista([T]) :- write(T).
writeLista([H|T]) :- write(H), write(', '), writeLista(T).

writeCircuitos([]).
writeCircuitos([Cod/Caminho|T]) :-  write('Circuito '), write(Cod), write(': ['), writeLista(Caminho), writeln(']'), writeCircuitos(T).


escolheCaminho(Caminhos, distancia, Res) :-
    getCircuitos(Caminhos, Circuitos),
    escolheCircuito(Circuitos, distancia, RCircuito),
    caminho(Res, RCircuito).


getCircuitos([], []).
getCircuitos([H|T], [H1|T1]) :-
    getCircuitos(T, T1),
    caminho(H, H1).

escolheCircuito(Circuitos, distancia, Res) :-
    minCircuito(Circuitos, Res).

minCircuito([C], C).
minCircuito([C|T], C) :-
    minCircuito(T, R),
    distanciaCircuito(R, Dist1),
    distanciaCircuito(C, Dist2),
    Dist2 < Dist1, !.
minCircuito([_|T], R) :-
    minCircuito(T, R).
