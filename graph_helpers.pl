:- module(graph_helpers, [
   	profundidade/3,
	largura/3
	]).

% profundidade(Orig,Dest,Caminho)

profundidade(Orig,Dest,[Orig|Caminho]) :-
	profundidadeAux(Orig,[Orig],Caminho,Dest).

profundidadeAux(Nodo,_,[],Nodo).
profundidadeAux(Nodo,Historico,[ProxNodo|Caminho],Dest) :-
	adjacente(Nodo,ProxNodo),
	not(member(ProxNodo,Historico)),
	profundidadeAux(ProxNodo,[ProxNodo|Historico],Caminho,Dest).

% ---------------------------------------------------------------

% largura(Orig,Dest,Caminho)

largura(Orig,Dest,Caminho) :- larguraAux(Dest,[[Orig]],Caminho).

larguraAux(Dest,[[Dest|T]|_],Solucao) :- reverse([Dest|T],Solucao).
larguraAux(Dest,[EstadosA|OutrosEstados],Solucao) :-
	EstadosA = [Atual|_],
	findall([EstadoX|EstadosA],
		(edge(Atual,EstadoX,_,_),not(member(EstadoX,EstadosA))),Novos),
	append(OutrosEstados,Novos,Todos),
	larguraAux(Dest,Todos,Solucao).

% ---------------------------------------------------------------

% gulosa(Orig,Dest,Caminho)

gulosa(Orig,Dest,CaminhoDistancia,CaminhoTransito) :-
	estima(Orig,Dest,EstimaD,EstimaT),
	gulosaAuxDist(Dest,[[Orig]/EstimaD],InvCaminhoDist),
	gulosaAuxTransito(Dest,[[Orig]/EstimaT],InvCaminhoTransito),
	reverse(InvCaminhoDist,CaminhoDistancia),
	reverse(InvCaminhoTransito,CaminhoTransito).

gulosaAuxDist(Dest,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Dest|]/_.
gulosaAuxDist(Dest,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
	expande_gulosa(MelhorCaminho,ExpCaminhos),


obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/EstimaD1,_/EstimaD2|Caminhos],MelhorCaminho) :- 
	EstimaD1 =< EstimaD2,
	!,
	obtem_melhor_g([Caminho1/EstimaD1|Caminhos],MelhorCaminho).

obtem_melhor_g([_|Caminhos],MelhorCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho).

% TODO
%expande_gulosa(Caminho,ExpCaminhos) :-
%	findall(NovoCaminho,(
%			adjacente(Caminho,NovoCaminho),
%
%		)
%		,ExpCaminhos).
% Graph helpers

adjacente(Nodo,ProxNodo) :- edge(Nodo,ProxNodo,_,_).
adjacente(Nodo,ProxNodo) :- edge(ProxNodo,Nodo,_,_).

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).
