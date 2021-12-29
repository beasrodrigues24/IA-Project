:- use_module(graph_helpers).
:- use_module(knowledgeBase).

% - PROCURAS NÃO INFORMADAS

% - DFS

dfs(Origem,Destino,Caminho) :- profundidade(Origem,Destino,Caminho).

% - BFS

bfs(Origem,Destino,Caminho) :- largura(Origem,Destino,Caminho).

% - PROCURAS INFORMADAS
