:- module(knowledgeBase, [
		tMaisEcologico/1,
		estado/1,
		encomenda/10,
		encomenda/3,
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
%-	Transporte,
%- PrecoBase,
%- DataCriação,
%- ZonaEntrega
%).

:- dynamic(encomenda/10).

encomenda(1,40,4,7,34,23,moto,19.95,21/11/2021/0,gothamCity).
encomenda(2,20,2,2,34,23,bicicleta,20.1,21/11/2021/0,centralCity). %-- CodE:2 - CodC:2
encomenda(3,50,3,3,34,23,carro,23,01/06/2021/0,gothamCity).
encomenda(4,10,2,4,34,23,bicicleta,32,01/06/2021/0,centralCity). %-- CodE:4 - CodC:2
encomenda(5,10,2,5,34,23,bicicleta,43,11/09/2021/0,centralCity). %-- CodE:5 - CodC:2
encomenda(6,30,6,6,34,23,moto,10,23/02/2018/0,wonderland).
encomenda(7,40,2,4,34,23,bicicleta,02,12/02/2019/0,narnia).  %-- CodE:4 - CodC:2
encomenda(8,50,8,8,34,23,carro,2,21/11/2021/0,centralCity).
encomenda(9,60,2,2,34,23,bicicleta,28,24/12/2020/0,narnia).  %-- CodE:2 - CodC:2
encomenda(10,10,5,1,34,23,moto,40,26/12/2020/0,narnia).
encomenda(11,20,3,2,34,23,bicicleta,42,01/01/2021/0,centralCity). %-- CodE: 2
encomenda(12,70,2,4,34,23,bicicleta,24.5,21/11/2021/0,narnia).   %-- CodE:4 - CodC:2


%- encomenda(
%   codEncomenda,
%   dataEntrega,
%   classificação
%) 

:- dynamic(encomenda/3).

encomenda(1,21/11/2021/0,4).
encomenda(2,21/11/2021/0,3).
encomenda(3,01/06/2021/0,2).
encomenda(4,01/06/2021/0,5).
encomenda(5,11/09/2021/0,3).
encomenda(6,23/02/2018/0,2).
encomenda(7,12/02/2019/0,3).
encomenda(8,21/11/2021/0,5).
encomenda(9,24/12/2020/0,1).
encomenda(10,26/12/2020/0,4).
encomenda(11,01/01/2021/0,5).
encomenda(12,21/11/2021/0,5).

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

penalizado(2,10/10/1980,10/10/2030).

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

%- transporte(Nome,PesoMax,VelMedia)

transporte(bicicleta,5,10).
transporte(moto,20,35).
transporte(carro,100,25).

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
	Não permite a inserção de encomendas com o mesmo código
*/

+encomenda(Cod,_,_,_,_,_,_,_,_,_,_,_) :: (
	findall(Cod,encomenda(Cod,_,_,_,_,_,_,_,_,_,_,_),R),
	length(R,N),
	N == 1
	).


/*
	Não permite a inserção de encomendas a estafetas que não estão registados
*/

+encomenda(_,_,_,CodEstafeta,_,_,_,_,_,_,_,_) :: (
	estafeta(CodEstafeta,_)
	).

/*

	Não permite a atribuição de encomendas a estafetas penalizados
*/

+encomenda(_,_,_,CodEstafeta,_,_,_,_,_,DataEnc,_,_) :: (
	findall(DataInicio/DataFim,penalizado(CodEstafeta,DataInicio,DataFim),R),
	compareDatas(R,DataEnc)
	).

compareDatas([],_).
compareDatas([Di/Df|T],De) :- not(pertenceData(Di,Df,De)), compareDatas(T,De). 

/*
	Não permite a inserção de encomendas associadas a transportes inexistentes ou que não a possam levar por conta do peso
*/

+encomenda(_,_,_,_,Peso,_,_,Transporte,_,_,_,_) :: (
	transporte(Transporte,PesoMax,_),
	Peso =< PesoMax
	).
/*
	Não permite a criação de penalizações para Estafeta com código inexistente
*/
+penalizado(CodEstafeta,_,_) :: (
	estafeta(CodEstafeta,_)
	).

/*

	Não permite a criação de penalizações com intervalos de tempo negativos
*/

+penalizado(_,DataInicio,DataFim) :: (
	dataDiff(DataInicio,DataFim,N),
	N < 0
	).
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


