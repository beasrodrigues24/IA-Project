:- module(graph_helpers, [
   	profundidade/3,
	largura/3,
	gulosa/4
	]).


% ------------------------------------------------------------- Pesquisa não informada - Profundidade

% profundidade(Origem,Destino,Caminho)

profundidade(Orig,Dest,[Orig|Caminho]) :-
	profundidadeAux(Orig,[Orig],Caminho,Dest).

profundidadeAux(Nodo,_,[],Nodo).
profundidadeAux(Nodo,Historico,[ProxNodo|Caminho],Dest) :-
	adjacente(Nodo,ProxNodo),
	not(member(ProxNodo,Historico)),
	profundidadeAux(ProxNodo,[ProxNodo|Historico],Caminho,Dest).

% ------------------------------------------------------------- Pesquisa não informada - largura

% largura(Origem,Destino,Caminho)

largura(Orig,Dest,Caminho) :- larguraAux(Dest,[[Orig]],Caminho).

larguraAux(Dest,[[Dest|T]|_],Solucao) :- reverse([Dest|T],Solucao).
larguraAux(Dest,[EstadosA|OutrosEstados],Solucao) :-
	EstadosA = [Atual|_],
	findall([EstadoX|EstadosA],
		(edge(Atual,EstadoX,_,_),not(member(EstadoX,EstadosA))),Novos),
	append(OutrosEstados,Novos,Todos),
	larguraAux(Dest,Todos,Solucao).

% ------------------------------------------------------------- Pesquisa informada - gulosa

% gulosa(Origem,Destino,Caminho)

gulosa(Orig,Dest,CaminhoDistancia,CaminhoTransito) :-
	estima(Orig,Dest,EstimaD,EstimaT),
	gulosaAuxDist(Dest,[[Orig]/EstimaD],InvCaminhoDist),
	gulosaAuxTransito(Dest,[[Orig]/EstimaT],InvCaminhoTransito),
	reverse(InvCaminhoDist,CaminhoDistancia),
	reverse(InvCaminhoTransito,CaminhoTransito).

% gulosaAuxDist e gulosaAuxTransito são SIMETRICAS. Diferem apenas no expande_gulosa (uma anexa aos caminhos expandidos a estimativa da herística da sitância, enquanto outra anexo aos caminhos expandidos a estimativa da heurística do trânsito).

gulosaAuxDist(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|_]/_.
gulosaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_gulosa_dist(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	gulosaAuxDist(NovosCaminhos,SolucaoCaminho).

gulosaAuxTransito(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|_]/_.
gulosaAuxTransito(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_gulosa_tran(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	gulosaAuxDist(NovosCaminhos,SolucaoCaminho).

% CONTEXTO - dado uma Lista de possíveis caminhos a se seguir, escolho aquele com melhor estimativa de acordo com a heurística utilizada.
% IDEIA - como a lista de possível caminhos, enviada como parâmetro, é composta em cada elemento por: (Caminho/estimativa da heurística deste caminho), para sabermos o caminho que vamos escolher, basta comparar as estimativas !

obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/EstimaD1,_/EstimaD2|Caminhos],MelhorCaminho) :- 
	EstimaD1 =< EstimaD2,
	!,
	obtem_melhor_g([Caminho1/EstimaD1|Caminhos],MelhorCaminho).

obtem_melhor_g([_|Caminhos],MelhorCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho).

% CONTEXTO - não só encontra os nodos adjacentes do nodo mais externo do Caminho, como também devolve os nodos adjacentes já anexados ao caminho dado como parâmetro. Para além disto é anexado ao caminho também e estimativa da heurística da distância para depois já ter em conta quando formos decidir se vamos seguir por ele.

expande_gulosa_dist(Dest,Caminho,ExpCaminhos) :-
	findall(NovoCaminho,adjacenteDist(Dest,Caminho,NovoCaminho),ExpCaminhos).


% simetrica a anterior, diferindo apenas que a heurística a utilizar é a do trânsito.

expande_gulosa_tran(Dest,Caminho,ExpCaminhos) :-
	findall(NovoCaminho,adjacenteTran(Dest,Caminho,NovoCaminho),ExpCaminhos).

% ------------------------------------------------------------------------------- Pesquisa Informada - A Estrela

% Graph helpers

adjacente(Nodo,ProxNodo) :- edge(Nodo,ProxNodo,_,_).
adjacente(Nodo,ProxNodo) :- edge(ProxNodo,Nodo,_,_).

% CONTEXO - expande_gulosa com a heurística da distância

adjacenteDist(Dest,[Nodo|Caminho]/_,[NextNodo,Nodo|Caminho]/EstDist) :-
	adjacente(Nodo,NextNodo,_,_),
	not(member(NextNodo,Caminho)),
	estima(NextNodo,Dest,EstDist,_).


adjacenteTran(Dest,[Nodo|Caminho]/_,[NextNodo,Nodo|Caminho]/EstTran) :-
	adjacente(Nodo,NextNodo,_,_),
	not(member(NextNodo,Caminho)),
	estima(NextNodo,Dest,_,EstTran).

% CONTEXTO - pesquisas informadas
% IDEIA - dado um caminho E e uma lista de caminhos, retorna a lista de caminhos sem o caminho E dado

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).
