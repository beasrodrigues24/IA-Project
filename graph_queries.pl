:- use_module(graph_helpers).
:- use_module(knowledgeBase).

% PROCURAS NÃO INFORMADAS

% DFS

dfsQ(Origem,Destino,Caminho) :- profundidade(Origem,Destino,Caminho).

% BFS

bfsQ(Origem,Destino,Caminho) :- largura(Origem,Destino,Caminho).

% PROCURAS INFORMADAS

% Gulosa

gulosaQ(Origem,Destino,CaminhoDist,CaminhoTran) :- gulosa(Origem,Destino,CaminhoDist,CaminhoTran).


% AEstrela

aestrelaQ(Origem,Destino,CaminhoDist,CaminhoTran) :- aestrela(Origem,Destino,CaminhoDist,CaminhoTran).
