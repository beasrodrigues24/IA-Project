/* Módulo de Invariantes */

:- module(invariants, [
           evolucao/1
   	]).

:- use_module(helpers).

:- op(900,xfy,'::').

/*
	Não permite a inserção de clientes com o mesmo código
*/
+cliente(Cod,_) :: (findall(Nome,cliente(Cod,Nome),R),
			length(R,N),
			N == 1
			).

/*
	Não permite a inserção de estafetas com o mesmo código
*/
+estafeta(Cod,_) :: (findall(Nome,estafeta(Cod,Nome),R),
			length(R,N),
			N == 1
			).

/*
	Não permite a atribuição de encomendas a estafetas penalizados
	Não permite a inserção de encomendas com o mesmo código
	Não permite a inserção de encomendas com estados diferentes dos possíveis casos de uso
	Detalhe do peso!
+encomenda(Cod,_,CodCliente,_,_,_,criada,_,_,DataCriacao,_,_) :: (
	findall(Cod,encomenda(Cod,_,_,_,_,_,_,_,_,criada,_,_),R),
	!,
	length(R,N),
	N == 1,
	penalizado(CodCliente,DataInicio,DataFim),
	not(pertenceData(DataInicio,DataFim,DataCriacao))
	).

+encomenda(Cod,_,_,_,_,_,entregue,_,_,_,_,_) :: (findall(Cod,encomenda(Cod,_,_,_,_,_,_,_,_,_,_,_),R),
	length(R,N),
	N == 1,
	).
*/
/*
	Não permite a criação de penalizações para Estafeta com código inexistente
	Não permite a criação de penalizações com intervalos de tempo negativos
*/
+penalizado(CodEstafeta,DataInicio,DataFim) :: (
	estafeta(CodEstafeta,_),
	dataDiff(DataInicio,DataFim,N),
	N < 0).


/*
	Garante a evolução fidedigna de conhecimento
*/

evolucao(Termo) :- findall(Invariante,+Termo::Invariante,Lista),insercao(Termo),teste(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo),!,fail.

teste([]).
teste([R|LR]) :- R,teste(LR).

