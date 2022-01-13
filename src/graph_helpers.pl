:- module(graph_helpers, [
   	profundidade/3,
	largura/3,
	gulosa/4,
	aestrela/4,
	gerarGulosaDist/1,
	gerarGulosaTran/1,
	gerarGulosaDist2/1,
	gerarGulosaTran2/1,
	gerarAestrelaDist/1,
	gerarAestrelaDist2/1,
	gerarAestrelaTran/1,
	gerarAestrelaTran2/1,
	gerarBFS/1,
	gerarBFS2/1,
	gerarDFS/1,
	gerarDFS2/1,
	profundidadeIterativa/3,
	gerarDFSIterativa/1,
	gerarDFSIterativa2/1,
	distanciaCircuito/2,
	edgeDist/3
	]).

:- use_module(helpers).
:- use_module(knowledgeBase).

% ------------------------------------------------------------ Gera todos os circuitos usando BFS da origem para todos os destinos (otimizando sub-percursos de percursos).

gerarBFS(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarBFSAux(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarBFS2(Circuitos) :-
	getDests(TodosDests),
	gerarBFS2Aux(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando o algoritmo BFS.

gerarBFSAux([],[]).
gerarBFSAux([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	largura(Orig,Dest,Circuito),
	gerarBFSAux(OutrosCircuitos,OutrosDest).

gerarBFS2Aux([],[]).
gerarBFS2Aux([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	largura(Orig,Dest,CircuitoIda),
	largura(Dest,Orig,[_|CircuitoVolta]),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarBFS2Aux(OutrosCircuitos,OutrosDest).

% ------------------------------------------------------------ Gera todos os circuitos usando DFS da origem para todos os destinos (otimizando sub-percursos de percursos).

gerarDFS(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarDFSAux(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarDFS2(Circuitos) :-
	getDests(TodosDests),
	gerarDFS2Aux(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando o algoritmo DFS.

gerarDFSAux([],[]).
gerarDFSAux([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	profundidade(Orig,Dest,Circuito),
	gerarDFSAux(OutrosCircuitos,OutrosDest).

gerarDFS2Aux([],[]).
gerarDFS2Aux([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	profundidade(Orig,Dest,CircuitoIda),
	profundidade(Dest,Orig,[_|CircuitoVolta]),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarDFS2Aux(OutrosCircuitos,OutrosDest).

% ----------------------------------------------------------- Gera todos os circuitos usando DFS Iterativa para todos os destinos (otimizando sub-percursos de percursos).

gerarDFSIterativa(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarDFSIterativaAux(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarDFSIterativa2(Circuitos) :-
	getDests(TodosDests),
	gerarDFSIterativaAux2(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando o algoritmo DFS Iterativo.

gerarDFSIterativaAux([],[]).
gerarDFSIterativaAux([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	profundidadeIterativa(Orig,Dest,Circuito),
	gerarDFSIterativaAux(OutrosCircuitos,OutrosDest).

gerarDFSIterativaAux2([],[]).
gerarDFSIterativaAux2([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	profundidadeIterativa(Orig,Dest,CircuitoIda),
	profundidadeIterativa(Dest,Orig,[_|CircuitoVolta]),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarDFSIterativaAux2(OutrosCircuitos,OutrosDest).

% ------------------------------------------------------------ Gera todos os circuitos usando Aestrela da origem para todos os destinos (otimizando sub-percursos de percursos), com a heurística da distância.

gerarAestrelaDist(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarAestrelaAuxDist(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarAestrelaDist2(Circuitos) :-
	getDests(TodosDests),
	gerarAestrelaAuxDist2(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando a heurística da distância para o algoritmo Aestrela.

gerarAestrelaAuxDist([],[]).
gerarAestrelaAuxDist([CircuitoD|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	aestrela(Orig,Dest,CircuitoD,_),
	gerarAestrelaAuxDist(OutrosCircuitos,OutrosDest).

gerarAestrelaAuxDist2([],[]).
gerarAestrelaAuxDist2([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	aestrela(Orig,Dest,CircuitoIda,_),
	aestrela(Dest,Orig,[_|CircuitoVolta],_),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarAestrelaAuxDist2(OutrosCircuitos,OutrosDest).

% simetrica a gerarAestrelaDist, mas com a heurística do trânsito.

gerarAestrelaTran(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarAestrelaAuxTran(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarAestrelaTran2(Circuitos) :-
	getDests(TodosDests),
	gerarAestrelaAuxTran2(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando a heurística do trânsito para o algoritmo aestrela.

gerarAestrelaAuxTran([],[]).
gerarAestrelaAuxTran([CircuitoT|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	aestrela(Orig,Dest,_,CircuitoT),
	gerarAestrelaAuxTran(OutrosCircuitos,OutrosDest).

gerarAestrelaAuxTran2([],[]).
gerarAestrelaAuxTran2([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	aestrela(Orig,Dest,_,CircuitoIda),
	aestrela(Dest,Orig,_,[_|CircuitoVolta]),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarAestrelaAuxTran2(OutrosCircuitos,OutrosDest).

% ------------------------------------------------------------ Gera todos os circuitos usando Gulosa da origem para todos os destinos (otimizando sub-percursos de percursos), com a heurística da distância.

gerarGulosaDist(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarGulosaAuxDist(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarGulosaDist2(Circuitos) :-
	getDests(TodosDests),
	gerarGulosaAuxDist2(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando a heurística da distância para o algoritmo guloso.

gerarGulosaAuxDist([],[]).
gerarGulosaAuxDist([CircuitoD|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	gulosa(Orig,Dest,CircuitoD,_),
	gerarGulosaAuxDist(OutrosCircuitos,OutrosDest).

gerarGulosaAuxDist2([],[]).
gerarGulosaAuxDist2([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	gulosa(Orig,Dest,CircuitoIda,_),
	gulosa(Dest,Orig,[_|CircuitoVolta],_),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarGulosaAuxDist2(OutrosCircuitos,OutrosDest).

% simetrica a gerarGulosaDist, mas com a heurística do trânsito.

gerarGulosaTran(CircuitosOtimizados) :-
	getDests(TodosDests),
	gerarGulosaAuxTran(Circuitos,TodosDests),
	otimizaCircuitos(Circuitos,[],CircuitosOtimizados).

gerarGulosaTran2(Circuitos) :-
	getDests(TodosDests),
	gerarGulosaAuxTran2(Circuitos,TodosDests).

% OBJETIVO - buscar todos os caminhos (da origem) para todos os destinos, utilizando a heurística do trânsito para o algoritmo guloso.

gerarGulosaAuxTran([],[]).
gerarGulosaAuxTran([CircuitoT|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	gulosa(Orig,Dest,_,CircuitoT),
	gerarGulosaAuxTran(OutrosCircuitos,OutrosDest).

gerarGulosaAuxTran2([],[]).
gerarGulosaAuxTran2([Circuito|OutrosCircuitos],[Dest|OutrosDest]) :-
	origem(Orig),
	gulosa(Orig,Dest,_,CircuitoIda),
	gulosa(Dest,Orig,_,[_|CircuitoVolta]),
	append(CircuitoIda,CircuitoVolta,Circuito),
	gerarGulosaAuxTran2(OutrosCircuitos,OutrosDest).

% --------------------------------------------------------------------------

% OBJETIVO - Dado a lista dos circuitos da origem para todos os destinos, filtrar apenas os circuitos maiores, retirando aqueles que são sub-circuitos.

otimizaCircuitos([],_,[]).

otimizaCircuitos([H|OutrosCircuitos],T2,[H|T]) :-
	otimizaCircuitosAux(H,OutrosCircuitos),
	otimizaCircuitosAux(H,T2),
	otimizaCircuitos(OutrosCircuitos,[H|T2],T).

otimizaCircuitos([_|OutrosCircuitos],HistoricoOtimizados,CircuitosOtimizados) :-
	otimizaCircuitos(OutrosCircuitos,HistoricoOtimizados,CircuitosOtimizados).


% OBJETIVO - verificar se um circuito é parte de um circuito maior. Caso não fizer parte, a regra é verdadeira.

otimizaCircuitosAux(_,[]).
otimizaCircuitosAux(C,[Cs|T]) :-
	not(prefix(C,Cs)),
	otimizaCircuitosAux(C,T).

% -----

getDests(TodosDests) :-
	findall(Nodo,(edge(Nodo,_,_,_,_),not(origem(Nodo))),Nodos),
	findall(Nodo,(edge(_,Nodo,_,_,_),not(origem(Nodo))),Nodos2),
	append(Nodos,Nodos2,Todos),
	removeRepetidos(Todos,TodosDests).

% ------------------------------------------------------------- Pesquisa não informada - Profundidade

% profundidade(Origem,Destino,Caminho)

profundidade(Orig,Dest,[Orig|Caminho]) :-
	profundidadeAux(Orig,[Orig],Caminho,Dest).

profundidadeAux(Nodo,_,[],Nodo).
profundidadeAux(Nodo,Historico,[ProxNodo|Caminho],Dest) :-
	adjacente(Nodo,ProxNodo,_,_),
	not(member(ProxNodo,Historico)),
	profundidadeAux(ProxNodo,[ProxNodo|Historico],Caminho,Dest).

% ------------------------------------------------------------- Pesquisa não informada - Profundidade Iterativa

% profundidadeIterativa(Origem,Destino,Caminho)

profundidadeIterativa(Orig,Dest,[Orig|Caminho]) :-
	profundidadeIterativaAux(Orig,Dest,0,Caminho).

profundidadeIterativaAux(Orig,Dest,NumeroIteracoes,Caminho) :-
	auxIteracao(Orig,[Orig],Caminho,NumeroIteracoes,Dest).

profundidadeIterativaAux(Orig,Dest,NumeroIteracoes,Caminho) :-
	ProxNumeroIteracoes is NumeroIteracoes + 1,
	profundidadeIterativaAux(Orig,Dest,ProxNumeroIteracoes,Caminho).

auxIteracao(Nodo,_,[],N,Nodo) :- N >= 0.
auxIteracao(Nodo,Historico,[ProxNodo|Caminho],Iteracao,Dest) :-
	Iteracao >= 0,
	adjacente(Nodo,ProxNodo,_,_),
	not(member(ProxNodo,Historico)),
	ProxIteracao is Iteracao - 1,
	auxIteracao(ProxNodo,[ProxNodo|Historico],Caminho,ProxIteracao,Dest).
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

gulosa(Orig,Dest,CaminhoDistancia,CaminhoTransito) :-
	estimaAux(Orig,Dest,EstimaD,EstimaT),
	gulosaAuxDist(Dest,[[Orig]/0/EstimaD],InvCaminhoDist/_/_),
	gulosaAuxTransito(Dest,[[Orig]/0/EstimaT],InvCaminhoTransito/_/_),
	reverse(InvCaminhoDist,CaminhoDistancia),
	reverse(InvCaminhoTransito,CaminhoTransito).

estimaAux(Orig,Dest,EstimaD,EstimaT) :- estima(Orig,Dest,EstimaD,EstimaT), !.

estimaAux(_,_,desconhecido,desconhecido).

% gulosaAuxDist e gulosaAuxTransito são SIMETRICAS. Diferem apenas no expande_gulosa (uma anexa aos caminhos expandidos a estimativa da herística da sitância, enquanto outra anexo aos caminhos expandidos a estimativa da heurística do trânsito).

gulosaAuxDist(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
gulosaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_dist(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	gulosaAuxDist(Dest,NovosCaminhos,SolucaoCaminho).

gulosaAuxTransito(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
gulosaAuxTransito(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_tran(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	gulosaAuxTransito(Dest,NovosCaminhos,SolucaoCaminho).

% CONTEXTO - dado uma Lista de possíveis caminhos a se seguir, escolho aquele com melhor estimativa de acordo com a heurística utilizada.
% IDEIA - como a lista de possível caminhos, enviada como parâmetro, é composta em cada elemento por: (Caminho/estimativa da heurística deste caminho), para sabermos o caminho que vamos escolher, basta comparar as estimativas !
% EDGE CASES - caso uma das estimativas da comparação for desconhecida, será considerado como melhor caminho aquele que tiver uma estimativa conhecida. Caso ambas forem desconhecidas, será considerado a primeira opção (nesse caso a pesquisa se assemelha a BFS).

obtem_melhor_g([Caminho],Caminho) :- !.

obtem_melhor_g([Caminho1/Custo1/EstimaD1,_/_/EstimaD2|Caminhos],MelhorCaminho) :-
	EstimaD1 \= desconhecido,
	EstimaD2 \= desconhecido,
	EstimaD1 =< EstimaD2,
	obtem_melhor_g([Caminho1/Custo1/EstimaD1|Caminhos],MelhorCaminho).


obtem_melhor_g([_/_/EstimaD1,Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho) :-
	EstimaD1 \= desconhecido,
	EstimaD2 \= desconhecido,
	EstimaD1 >= EstimaD2,
	obtem_melhor_g([Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho).


obtem_melhor_g([_/_/desconhecido,Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho) :-
	EstimaD2 \= desconhecido,
	obtem_melhor_g([Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho).

obtem_melhor_g([Caminho1/Custo1/EstimaD1,_/_/desconhecido|Caminhos],MelhorCaminho) :-
	EstimaD1 \= desconhecido,
	obtem_melhor_g([Caminho1/Custo1/EstimaD1|Caminhos],MelhorCaminho).


obtem_melhor_g([Caminho1/Custo1/desconhecido,_/_/desconhecido|Caminhos],MelhorCaminho) :-
	obtem_melhor_g([Caminho1/Custo1/desconhecido|Caminhos],MelhorCaminho).

% ------------------------------------------------------------------------------- Pesquisa Informada - A Estrela

% aestrela(Origem,Destino,Caminho Seguindo a heurística da distância/Custo de distância desse caminho, Caminho seguindo a heurística do trânsito/Custo de transito desse caminho)
% difere da gulosa apenas no obtem_melhor - nodo a ser expandido (que agora considera também o custo percorrido total).

aestrela(Orig,Dest,CaminhoDistancia,CaminhoTransito) :-
	estimaAux(Orig,Dest,EstimaD,EstimaT),
	aestrelaAuxDist(Dest,[[Orig]/0/EstimaD],InvCaminhoDist/_/_),
	aestrelaAuxTransito(Dest,[[Orig]/0/EstimaT],InvCaminhoTransito/_/_),
	reverse(InvCaminhoDist,CaminhoDistancia),
	reverse(InvCaminhoTransito,CaminhoTransito).

%-------

aestrelaAuxDist(Dest,Caminhos,Caminho) :-
	obtem_melhor_a(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
aestrelaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_a(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_dist(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	aestrelaAuxDist(Dest,NovosCaminhos,SolucaoCaminho).

aestrelaAuxTransito(Dest,Caminhos,Caminho) :-
	obtem_melhor_a(Caminhos,Caminho),
	Caminho = [Dest|_]/_/_.
aestrelaAuxTransito(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_a(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_tran(Dest,MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovosCaminhos),
	aestrelaAuxTransito(Dest,NovosCaminhos,SolucaoCaminho).

%--------

% EDGE CASES - caso uma das estimativas da comparação for desconhecida, será considerado como melhor caminho aquele que tiver uma estimativa conhecida. Caso ambas forem desconhecidas, será considerado a opção cujo custo do caminho até ela seja o menor (nesse caso a pesquisa se aproxima da pesquisa do custo uniforme).

obtem_melhor_a([Caminho],Caminho) :- !.

obtem_melhor_a([Caminho1/Custo1/EstimaD1,_/Custo2/EstimaD2|Caminhos],MelhorCaminho) :-
	EstimaD1 \= desconhecido,
	EstimaD2 \= desconhecido,
	Custo1 + EstimaD1 =< Custo2 + EstimaD2,
	obtem_melhor_a([Caminho1/Custo1/EstimaD1|Caminhos],MelhorCaminho).

obtem_melhor_a([_/Custo1/EstimaD1,Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho) :-
	EstimaD1 \= desconhecido,
	EstimaD2 \= desconhecido,
	Custo1 + EstimaD1 >= Custo2 + EstimaD2,
	obtem_melhor_a([Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho).

obtem_melhor_a([_/_/desconhecido,Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho) :-
	EstimaD2 \= desconhecido,
	obtem_melhor_a([Caminho2/Custo2/EstimaD2|Caminhos],MelhorCaminho).

obtem_melhor_a([Caminho1/Custo1/EstimaD1,_/_/desconhecido|Caminhos],MelhorCaminho) :-
	EstimaD1 \= desconhecido,
	obtem_melhor_a([Caminho1/Custo1/EstimaD1|Caminhos],MelhorCaminho).


obtem_melhor_a([Caminho1/Custo1/desconhecido,_/Custo2/desconhecido|Caminhos],MelhorCaminho) :-
	Custo1 =< Custo2, 
	obtem_melhor_a([Caminho1/Custo1/desconhecido|Caminhos],MelhorCaminho).


obtem_melhor_a([_/Custo1/desconhecido,Caminho2/Custo2/desconhecido|Caminhos],MelhorCaminho) :-
	Custo1 >= Custo2, 
	obtem_melhor_a([Caminho2/Custo2/desconhecido|Caminhos],MelhorCaminho).

% ------------------------------------------------------------------------------- Helpers

adjacente(Nodo,ProxNodo,PassoDist,PassoTran) :- edge(Nodo,ProxNodo,_,PassoDist,PassoTran).
adjacente(Nodo,ProxNodo,PassoDist,PassoTran) :- edge(ProxNodo,Nodo,_,PassoDist,PassoTran).

% CONTEXO - expande_gulosa e expande_aestrela com a heurística da distância
% RETORNO - Nodo adjacente já incorporado na lista do caminho até ele + Custo total do caminho até esse nodo adjacente + valor da heuristica da distância desse nodo adjacente. Caso o valor da heurística não for conhecido, então é inserido "desconhecido" para este valor.

adjacenteDist(Dest,[Nodo|Caminho]/Custo/_,[NextNodo,Nodo|Caminho]/NovoCusto/EstDist) :-
	adjacente(Nodo,NextNodo,PassoCusto,_),
	not(member(NextNodo,Caminho)),
	NovoCusto is Custo + PassoCusto,
	estima(NextNodo,Dest,EstDist,_).

adjacenteDist(_,[Nodo|Caminho]/Custo/_,[NextNodo,Nodo|Caminho]/NovoCusto/desconhecido) :-
	adjacente(Nodo,NextNodo,PassoCusto,_),
	not(member(NextNodo,Caminho)),
	NovoCusto is Custo + PassoCusto.

% simetrico ao anterior mas para a heurística do trânsito.

adjacenteTran(Dest,[Nodo|Caminho]/Custo/_,[NextNodo,Nodo|Caminho]/NovoCusto/EstTran) :-
	adjacente(Nodo,NextNodo,_,PassoCusto),
	not(member(NextNodo,Caminho)),
	NovoCusto is Custo + PassoCusto,
	estima(NextNodo,Dest,_,EstTran).


adjacenteTran(_,[Nodo|Caminho]/Custo/_,[NextNodo,Nodo|Caminho]/NovoCusto/desconhecido) :-
	adjacente(Nodo,NextNodo,_,PassoCusto),
	not(member(NextNodo,Caminho)),
	NovoCusto is Custo + PassoCusto.

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

distanciaCircuito([], 0).
distanciaCircuito([_], 0).
distanciaCircuito([A|[B|T]], D) :-
	edgeDist(A,B,D1),
	distanciaCircuito([B|T], D2),
	D is D1 + D2.

edgeDist(A,B, Dist) :- adjacente(A,B,Dist,_).
