:- module(knowledgeBase, [
		tMaisEcologico/1,
		estado/1,
		encomenda/12,
		estafeta/2,
		cliente/2,
		penalizado/3,
		precoEncomenda/4,
		evolucao/1
	]).

:- use_module(helpers).

/* 
A explicação desse facto "tMaisEcologico" será útil para flexibilidade na primeira query (explicação adiante)
*/
tMaisEcologico(bicicleta).

/*
	Possíveis estados da encomenda
*/

estado(entregue).
estado(criada).

%-encomenda(
%	codEncomenda,
%	TempMax,
%-	codCliente,
%-	codEstafeta,
%-	Peso,
%-	Volume,
%-	Estado,
%-	Transporte,
%- 	PrecoBase,
%- 	Data, 
%-      ZonaEntrega
%- 	Classificação
%).

:- dynamic(encomenda/12). 

encomenda(1,40,4,7,34,23,entregue,moto, 19.95, 21/11/2021, gothamCity, 4). 
encomenda(2,20,2,2,34,23,entregue,bicicleta, 20.1, 21/11/2021, centralCity, 2). %-- CodE:2 - CodC:2
encomenda(3,50,3,3,34,23,entregue,carro, 23, 01/06/2021, gothamCity, 3).
encomenda(4,10,2,4,34,23,entregue,bicicleta, 32, 30/05/2021, centralCity, 5). %-- CodE:4 - CodC:2
encomenda(5,10,2,5,34,23,entregue,bicicleta, 43, 11/09/2021, centralCity, 5). %-- CodE:5 - CodC:2
encomenda(6,30,6,6,34,23,entregue,moto, 10, 23/02/2018, wonderland, 3).
encomenda(7,40,2,4,34,23,entregue,bicicleta, 02, 12/02/2019, narnia, 4).  %-- CodE:4 - CodC:2
encomenda(8,50,8,8,34,23,entregue,carro,2, 21/11/2021, centralCity, 1).
encomenda(9,60,2,2,34,23,entregue,bicicleta, 28, 24/12/2020, narnia, 5).  %-- CodE:2 - CodC:2
encomenda(10,10,5,1,34,23,entregue,moto, 40, 26/12/2020, narnia, 4).    
encomenda(11,20,3,2,34,23,entregue,bicicleta, 42, 01/01/2021, centralCity, 3). %-- CodE: 2 
encomenda(12,70,2,4,34,23,entregue,bicicleta, 24.5, 21/11/2021, narnia, 2).   %-- CodE:4 - CodC:2

/* 
Optamos pelo código do estafeta para permitir a existência de estafetas com o mesmo nome, mas códigos de identificação distintos
*/
%-estafeta(Codigo,Nome).

:- dynamic(estafeta/2). 

estafeta(1,homemaranha).
estafeta(2,homemdeferro).
estafeta(3,hulk).
estafeta(4,doutorestranho).
estafeta(5,wanda).
estafeta(6,homemformiga).
estafeta(7,loki).
estafeta(8,capitamarvel).

%- cliente(Codigo,Cliente).

:- dynamic(cliente/2).

cliente(1,superhomem).
cliente(2,mulhermaravilha).
cliente(3,flash).
cliente(4,zeus).
cliente(5,ares).
cliente(6,batman).

%- penalizado(CodEstafeta,DataInicio,DataFim)

:- dynamic(penalizado/3).

%- precoEncomenda(Base, TempMax, Veiculo, Preco).

precoEncomenda(Base, 2, bicicleta, Base + 5).
precoEncomenda(Base, 6, bicicleta, Base + 4).
precoEncomenda(Base, 24, bicicleta, Base + 3).
precoEncomenda(Base, 2, moto, Base + 5).
precoEncomenda(Base, 6, moto, Base + 4).
precoEncomenda(Base, 24,moto, Base + 3).
precoEncomenda(Base, 2, carro, Base + 6).
precoEncomenda(Base, 6, carro, Base + 4).
precoEncomenda(Base, 24, carro, Base + 3).

/***************** INVARIANTES ******************/

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


map( graph([gothamCity, centralCity, starCity, mordor, hogwarts, springfield, asgard, eastEgg, westEgg,
gravityFalls, capitol, bedrock, pawnee, theShire, narnia, hogsmeade, rivendell, kingsLanding, jurassicPark,
bikiniBottom, dragonstone, quahog, wonderland, westworld, neverland, themyscira, tatooine],
    [edge(gothamCity, centralCity, dcStreet, 17),
     edge(gothamCity, capitol, katnissStreet, 52),
     edge(gothamCity, westEgg, batmanAvenue, 100),
     edge(eastEgg, westEgg, gatsbyBoulevard, 20),
     edge(centralCity, starCity, fastStreet, 31),
     edge(centralCity, gravityFalls, pineStreet, 13),
     edge(westEgg, westworld, westAvenue, 5),
     edge(westworld,rivendell, elfStreet, 30),
     edge(capitol, pawnee, swansonBoulevard, 50),
     edge(capitol, hogwarts, potterStreet, 100),
     edge(gravityFalls, bikiniBottom, patrickAvenue, 4),
     edge(pawnee, bikiniBottom, squidStreet, 26),
     edge(pawnee, neverland, panAvenue, 20),
     edge(rivendell, theShire, bilboStreet, 15),
     edge(mordor, theShire, ringBoulevard, 30),
     edge(neverland, wonderland, dreamStreet, 5), 
     edge(neverland, hogwarts, wandAvenue, 10),
     edge(bikiniBottom, springfield, yellowBoulevard, 2),
     edge(springfield, quahog, griffinAvenue, 3),
     edge(springfield, bedrock, rockBoulevard, 32),
     edge(theShire, hogwarts, magicStreet, 25),
     edge(theShire, asgard, gandalfStreet, 90),
     edge(theShire, narnia, closetedStreet, 52),
     edge(hogwarts, hogsmeade, hermioneAvenue, 2),
     edge(hogsmeade, narnia, sweetBoulevard, 120),
     edge(bedrock, jurassicPark, dinosaurAvenue, 10),
     edge(jurassicPark, tatooine, skyStreet, 50),
     edge(tatooine, dragonstone, falconBoulevard, 100),
     edge(dragonstone, narnia, lionStreet, 17),
     edge(narnia, kingsLanding, aslanAvenue, 22),
     edge(kingsLanding, asgard, odinStreet, 30)]
   )).


