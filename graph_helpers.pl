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

% Graph helpers

adjacente(Nodo,ProxNodo) :- edge(Nodo,ProxNodo,_,_).
adjacente(Nodo,ProxNodo) :- edge(ProxNodo,Nodo,_,_).
