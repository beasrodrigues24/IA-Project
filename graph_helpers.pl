:- module(graph_helpers, [
   	profundidade/3,
	largura/3,
	gulosa/4,
	aestrela/4
	]).


% ------------------------------------------------------------- Pesquisa não informada - Profundidade

% profundidade(Origem,Destino,Caminho)

profundidade(Orig,Dest,[Orig|Caminho]) :-
	profundidadeAux(Orig,[Orig],Caminho,Dest).

profundidadeAux(Nodo,_,[],Nodo).
profundidadeAux(Nodo,Historico,[ProxNodo|Caminho],Dest) :-
	adjacente(Nodo,ProxNodo,_,_),
	not(member(ProxNodo,Historico)),
	profundidadeAux(ProxNodo,[ProxNodo|Historico],Caminho,Dest).

% ------------------------------------------------------------- Pesquisa não informada - largura

% largura(Origem,Destino,Caminho)

largura(Orig,Dest,Caminho) :- larguraAux(Dest,[[Orig]],Caminho).

larguraAux(Dest,[[Dest|T]|_],Solucao) :- reverse([Dest|T],Solucao).
larguraAux(Dest,[EstadosA|OutrosEstados],Solucao) :-
	EstadosA = [Atual|_],
	findall([EstadoX|EstadosA],
		(adjacente(Atual,EstadoX,_,_),not(member(EstadoX,EstadosA))),Novos),
	append(OutrosEstados,Novos,Todos),
	larguraAux(Dest,Todos,Solucao).

% ------------------------------------------------------------- Pesquisa informada - gulosa

% gulosa(Origem,Destino,Caminho Seguindo a heurística da distância/Custo de distância desse caminho, Caminho seguindo a heurística do trânsito/Custo de transito desse caminho)

gulosa(Orig,Dest,CaminhoDistancia/CustoDist,CaminhoTransito/CustoTran) :-
	estima(Orig,Dest,EstimaD,EstimaT),
	gulosaAuxDist(Dest,[[Orig]/0/EstimaD],InvCaminhoDist/CustoDist/_),
	gulosaAuxTransito(Dest,[[Orig]/0/EstimaT],InvCaminhoTransito/CustoTran/_),
	reverse(InvCaminhoDist,CaminhoDistancia),
	reverse(InvCaminhoTransito,CaminhoTransito).

% gulosaAuxDist e gulosaAuxTransito são SIMETRICAS. Diferem apenas no expande_gulosa (uma anexa aos caminhos expandidos a estimativa da herística da sitância, enquanto outra anexo aos caminhos expandidos a estimativa da heurística do trânsito).

gulosaAuxDist(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
gulosaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_dist(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	gulosaAuxDist(NovosCaminhos,SolucaoCaminho).

gulosaAuxTransito(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
gulosaAuxTransito(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_tran(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	gulosaAuxDist(NovosCaminhos,SolucaoCaminho).

% CONTEXTO - dado uma Lista de possíveis caminhos a se seguir, escolho aquele com melhor estimativa de acordo com a heurística utilizada.
% IDEIA - como a lista de possível caminhos, enviada como parâmetro, é composta em cada elemento por: (Caminho/estimativa da heurística deste caminho), para sabermos o caminho que vamos escolher, basta comparar as estimativas !

obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/EstimaD1,_/_/EstimaD2|Caminhos],MelhorCaminho) :- 
	EstimaD1 =< EstimaD2,
	!,
	obtem_melhor_g([Caminho1/Custo1/EstimaD1|Caminhos],MelhorCaminho).

obtem_melhor_g([_|Caminhos],MelhorCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho).

% ------------------------------------------------------------------------------- Pesquisa Informada - A Estrela

% aestrela(Origem,Destino,Caminho Seguindo a heurística da distância/Custo de distância desse caminho, Caminho seguindo a heurística do trânsito/Custo de transito desse caminho)
% difere da gulosa apenas no obtem_melhor - nodo a ser expandido (que agora considera também o custo percorrido total).

aestrela(Orig,Dest,CaminhoDistancia/CustoDist,CaminhoTransito/CustoTran) :-
	estima(Orig,Dest,EstimaD,EstimaT),
	aestrelaAuxDist(Dest,[[Orig]/0/EstimaD],InvCaminhoDist/CustoDist/_),
	aestrelaAuxTransito(Dest,[[Orig]/0/EstimaT],InvCaminhoTransito/CustoTran/_),
	reverse(InvCaminhoDist,CaminhoDistancia),
	reverse(InvCaminhoTransito,CaminhoTransito).

%-------

aestrelaAuxDist(Dest,Caminhos,Caminho) :-
	obtem_melhor_a(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
aestrelaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_a(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_aestrela_dist(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	aestrelaAuxDist(NovosCaminhos,SolucaoCaminho).


aestrelaAuxTransito(Dest,Caminhos,Caminho) :-
	obtem_melhor_a(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
aestrelaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_a(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_aestrela_tran(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	aestrelaAuxTransito(NovosCaminhos,SolucaoCaminho).

%--------

obtem_melhor_a([Caminho],Caminho) :- !.
obtem_melhor_a([Caminho1/Custo1/EstimaD1,_/Custo2/EstimaD2|Caminhos],MelhorCaminho) :- 
	Custo1 + EstimaD1 =< Custo2 + EstimaD2,
	!,
	obtem_melhor_a([Caminho1/Custo1/EstimaD1|Caminhos],MelhorCaminho).

obtem_melhor_a([_|Caminhos],MelhorCaminho) :-
	obtem_melhor_a(Caminhos,MelhorCaminho).

% ------------------------------------------------------------------------------- Helpers

adjacente(Nodo,ProxNodo,PassoDist,PassoTran) :- edge(Nodo,ProxNodo,_,PassoDist,PassoTran).
adjacente(Nodo,ProxNodo,PassoDist,PassoTran) :- edge(ProxNodo,Nodo,_,PassoDist,PassoTran).

% CONTEXO - expande_gulosa e expande_aestrela com a heurística da distância
% RETORNO - Nodo adjacente já incorporado na lista do caminho até ele + Custo total do caminho até esse nodo adjacente + valor da heuristica da distância desse nodo adjacente.

adjacenteDist(Dest,[Nodo|Caminho]/Custo/_,[NextNodo,Nodo|Caminho]/NovoCusto/EstDist) :-
	adjacente(Nodo,NextNodo,PassoCusto,_),
	not(member(NextNodo,Caminho)),
	NovoCusto is Custo + PassoCusto,
	estima(NextNodo,Dest,_,EstDist,_).

% simetrico ao anterior mas para a heurística do trânsito.

adjacenteTran(Dest,[Nodo|Caminho]/Custo/_,[NextNodo,Nodo|Caminho]/NovoCusto/EstTran) :-
	adjacente(Nodo,NextNodo,_,PassoCusto),
	not(member(NextNodo,Caminho)),
	NovoCusto is Custo + PassoCusto,
	estima(NextNodo,Dest,_,_,EstTran).

% CONTEXTO - pesquisas informadas
% IDEIA - dado um caminho E e uma lista de caminhos, retorna a lista de caminhos sem o caminho E dado

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).


% CONTEXTO - não só encontra os nodos adjacentes do nodo mais externo do Caminho, como também devolve os nodos adjacentes já anexados ao caminho dado como parâmetro. Para além disto é anexado ao caminho também o custo total da distância percorrida pelo caminho todo e também a estimativa da heurística da distância para depois já ter em conta quando formos decidir se vamos seguir por ele.

expande_dist(Dest,Caminho,ExpCaminhos) :-
	findall(NovoCaminho,adjacenteDist(Dest,Caminho,NovoCaminho),ExpCaminhos).

% simetrica a anterior, diferindo apenas que a heurística a utilizar é a do trânsito.

expande_tran(Dest,Caminho,ExpCaminhos) :-
	findall(NovoCaminho,adjacenteTran(Dest,Caminho,NovoCaminho),ExpCaminhos).
