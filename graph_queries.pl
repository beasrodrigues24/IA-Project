:- module(graph_queries, [
	gerarBFSQ/1,
	gerarBFS2Q/1,
	gerarDFSQ/1,
	gerarDFS2Q/1,
    gerarDFSIQ/1,
    gerarDFSI2Q/1,
    gerarGulosaDistQ/1,
    gerarGulosaTranQ/1,
    gerarGulosaDist2Q/1,
    gerarGulosaTran2Q/1,
    gerarAEstrelaDistQ/1,
    gerarAEstrelaDist2Q/1,
    gerarAEstrelaTranQ/1,
    gerarAEstrelaTran2Q/1,
    gerarCircuitos/1,
    gerarCircuitos2/1,
    ordenaCircuitosPeso/1,
    ordenaCircuitosVolume/1,
    ordenaCircuitosPeso/2,
    ordenaCircuitosVolume/2,
    comparaCircuitos/3,
    comparaCircuitos/6,
    printCircuitos/0,
    minCaminho/2,
    minCaminho/3,
    minCircuito/2,
    minCircuito/3,
    comparaCaminho/3,
    comparaCaminho/4,
    printEncomendaCaminho/0
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

gerarBFS2Q(Circuitos) :- gerarBFS2(Circuitos).

gerarDFSQ(Circuitos) :- gerarDFS(Circuitos).

gerarDFS2Q(Circuitos) :- gerarDFS2(Circuitos).

gerarDFSIQ(Circuitos) :- gerarDFSIterativa(Circuitos).

gerarDFSI2Q(Circuitos) :- gerarDFSIterativa2(Circuitos).

gerarGulosaDistQ(Circuitos) :- gerarGulosaDist(Circuitos).

gerarGulosaTranQ(Circuitos) :- gerarGulosaTran(Circuitos).

gerarGulosaDist2Q(Circuitos) :- gerarGulosaDist2(Circuitos).

gerarGulosaTran2Q(Circuitos) :- gerarGulosaTran2(Circuitos).

gerarAEstrelaDistQ(Circuitos) :- gerarAestrelaDist(Circuitos).

gerarAEstrelaDist2Q(Circuitos) :- gerarAestrelaDist2(Circuitos).

gerarAEstrelaTranQ(Circuitos) :- gerarAestrelaTran(Circuitos).

gerarAEstrelaTran2Q(Circuitos) :- gerarAestrelaTran2(Circuitos).

gerarCircuitos([CircuitosGD,CircuitosGT,CircuitosAD,CircuitosAT,CircuitosBFS,CircuitosDFS,CircuitosDFSI]) :- 
	gerarGulosaDist(CircuitosGD),
	gerarGulosaTran(CircuitosGT),
	gerarAestrelaDist(CircuitosAD),
	gerarAestrelaTran(CircuitosAT),
	gerarBFS(CircuitosBFS),
	gerarDFS(CircuitosDFS),
	gerarDFSIterativa(CircuitosDFSI).

gerarCircuitos2([CircuitosGD,CircuitosGT,CircuitosAD,CircuitosAT,CircuitosBFS,CircuitosDFS,CircuitosDFSI]) :- 
	gerarGulosaDist2(CircuitosGD),
	addCodToCircuitos(CircuitosGD,0,0,N1),
	gerarGulosaTran2(CircuitosGT),
	addCodToCircuitos(CircuitosGT,N1,N1,N2),
	gerarAestrelaDist2(CircuitosAD),
	addCodToCircuitos(CircuitosAD,N2,N2,N3),
	gerarAestrelaTran2(CircuitosAT),
	addCodToCircuitos(CircuitosAT,N3,N3,N4),
	gerarBFS2(CircuitosBFS),
	addCodToCircuitos(CircuitosBFS,N4,N4,N5),
	gerarDFS2(CircuitosDFS),
	addCodToCircuitos(CircuitosDFS,N5,N5,N6),
	gerarDFSIterativa2(CircuitosDFSI),
	addCodToCircuitos(CircuitosDFSI,N6,N6,_).

% Calcular tempo de um percurso dado peso e transporte

tempo([],_,_,0).
tempo([_],_,_,0).
tempo([Nodo1,Nodo2|OutrosNodos], Transporte, Peso, Tempo) :-
    edgeDist(Nodo1,Nodo2,Distancia),
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

minCaminho([Cod],Cod).
minCaminho([Cod|T], Cod) :-
    minCaminho(T, CodR),
    distanciaCaminho(Cod, Dist1),
    distanciaCaminho(CodR, Dist2),
    Dist1 < Dist2, !.
minCaminho([_|T], R) :-
    minCaminho(T, R).

minCaminho([Cod], DataAtual, Cod) :-
    tempoCaminho(Cod, DataAtual,_). % Serve apenas para confirmar que é possível realizar este caminho
minCaminho([Cod|T], DataAtual, Cod) :-
    minCaminho(T, DataAtual, CodR),
    tempoCaminho(Cod, DataAtual, Temp1),
    tempoCaminho(CodR, DataAtual, Temp2),
    Temp1 < Temp2, !.
minCaminho([_|T], DataAtual, R) :-
    minCaminho(T, DataAtual, R).

distanciaCaminho(Cod, Dist) :-
    caminho(Cod, C),
    distanciaCircuito(C, Dist).

tempoCaminho(Cod, DataAtual, Temp) :-
    caminho(Cod, Circuito),
    getEncomendas(Cod, Encomendas),
    tempoCircuito(Circuito, Encomendas, DataAtual, Temp).

getEncomendas(Cod, Encomendas) :-
    findall(Encomenda, encomendaCaminho(Cod, Encomenda), Encomendas).

comparaCaminho(C1, C2, R) :-
    distanciaCaminho(C1, D1),
    distanciaCaminho(C2, D2),
    R is D1 - D2.

comparaCaminho(C1, C2, DataAtual, R) :-
    tempoCaminho(C1, DataAtual, T1),
    tempoCaminho(C2, DataAtual, T2),
    R is T1 - T2.

minCircuito([C/E], DataAtual,C/E) :-
    tempoCircuito(C, E, DataAtual,_).
minCircuito([C/E|T], DataAtual, C/E) :-
    minCircuito(T, DataAtual, RC/RE),
    tempoCircuito(C, E, DataAtual, Temp1),
    tempoCircuito(RC, RE, DataAtual, Temp2),
    Temp1 < Temp2, !.
minCircuito([_|T], DataAtual, R) :-
    minCircuito(T, DataAtual, R).

minCircuito([C], C) :-
    distanciaCircuito(C,_).
minCircuito([C|T], C) :-
    minCircuito(T, R),
    distanciaCircuito(C, D1),
    distanciaCircuito(R, D2),
    D1 < D2, !.
minCircuito([_|T], R) :-
    minCircuito(T,R).

addCodToCircuitos([],_,FCod,FCod).
addCodToCircuitos([Circuito|OutrosCircuitos],Cod,FCod,LastCod) :-
	evolucao(caminho(Cod,Circuito)),
	ProxCod is Cod + 1,
	addCodToCircuitos(OutrosCircuitos,ProxCod,FCod,N),
	LastCod is N + 1.

% Imprime todos os circuitos existentes na base de conhecimento.
printCircuitos() :- findall(Cod/Circ, caminho(Cod,Circ), Lista), writeCircuitos(Lista).
writeCircuitos([]).
writeCircuitos([Cod/Caminho|T]) :-  write('Circuito '), write(Cod), write(': ['), writeLista(Caminho), writeln(']'), writeCircuitos(T).

% Imprime todas as "encomendaCaminho" presentes na base de conhecimento.
printEncomendaCaminho() :- findall(CodC/CodE, encomendaCaminho(CodC,CodE), Lista), writeEncCam(Lista).
writeEncCam([]).
writeEncCam([CodC/CodE|T]) :- write('Código Circuito: '), write(CodC), write(' | '), write('Código Encomenda: '), writeln(CodE), writeEncCam(T).
