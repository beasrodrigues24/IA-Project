:- module(knowledgeBase, [
		tMaisEcologico/1,
		encomenda/10,
		encomenda/3,
		estafeta/2,
		cliente/2,
		penalizado/3,
		precoEncomenda/4,
		evolucao/1,
		edge/5,
		estima/4,
		transporte/3,
		velocidadeDiminui/2,
		caminho/2,
		encomendaCaminho/2,
		origem/1
	]).

:- use_module(helpers).

:- discontiguous knowledgeBase: :: / 2.

origem(neverland).

/* 
A explicação desse facto "tMaisEcologico" será útil para flexibilidade na primeira query (explicação adiante)
*/
% Organizado do mais ecologico para o menos
tMaisEcologico(bicicleta).
tMaisEcologico(moto).
tMaisEcologico(carro).

/*
Cidade onde se localiza o armazém
*/
cidadeBase(neverland).

%-encomenda(
%	codEncomenda,
%	TempMax,
%-	codCliente,
%-	codEstafeta,
%-	Peso,
%-	Volume,
%-	Transporte,
%- 	PrecoBase,
%- 	DataCriação,
%- 	ZonaEntrega
%).

:- dynamic(encomenda/10).

encomenda(1,40,4,7,12,23,moto,19.95,21/11/2021/0,gothamCity).
encomenda(2,20,2,2,10,23,moto,20.1,21/11/2021/0,centralCity). %-- CodE:2 - CodC:2
encomenda(3,50,3,3,35,23,carro,23,01/06/2021/0,gothamCity).
encomenda(4,10,2,4,8,23,moto,32,01/06/2021/0,centralCity). %-- CodE:4 - CodC:2
encomenda(5,10,2,5,2,23,moto,43,11/09/2021/0,centralCity). %-- CodE:5 - CodC:2
encomenda(6,30,6,6,13,23,moto,10,23/02/2018/0,wonderland).
encomenda(7,40,2,4,10,23,moto,02,12/02/2019/0,narnia).  %-- CodE:4 - CodC:2
encomenda(8,50,8,8,50,23,carro,2,21/11/2021/0,centralCity).
encomenda(9,60,2,2,2,23,bicicleta,28,24/12/2020/0,narnia).  %-- CodE:2 - CodC:2
encomenda(10,10,5,1,4,23,moto,40,26/12/2020/0,narnia).
encomenda(11,20,3,2,10,23,moto,42,01/01/2021/0,centralCity). %-- CodE: 2
encomenda(12,70,2,4,2,23,moto,24.5,21/11/2021/0,narnia).   %-- CodE:4 - CodC:2


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

penalizado(2,10/10/1980/0,10/10/2030/0).

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

% - velocidadeDiminui(Transporte, Diminuição por kg)

velocidadeDiminui(bicicleta, 0.7).
velocidadeDiminui(moto, 0.5).
velocidadeDiminui(carro, 0.1).

% - caminho(CodCaminho, Caminho)

:-dynamic(caminho/2).

caminho(1000, [gravityFalls, centralCity, gothamCity]).
#caminho(2, [neverland,wonderland]).

% - encomendaCaminho(CodCaminho, CodEncomenda)

:-dynamic(encomendaCaminho/2).

encomendaCaminho(1,1).
encomendaCaminho(2,2).
% encomendaCaminho(2,6).


/***************** INVARIANTES ******************/

:- op(900,xfy,'::').

/*
 * Não permite a inserção de clientes com o mesmo código
*/
+cliente(Cod,_) :: (findall(Nome,cliente(Cod,Nome),R),
			length(R,N),
			N == 1
			).

/*
 * Não permite a inserção de estafetas com o mesmo código
*/
+estafeta(Cod,_) :: (findall(Nome,estafeta(Cod,Nome),R),
			length(R,N),
			N == 1
			).

/*
 * Não permite a inserção de encomendas com o mesmo código
*/

+encomenda(Cod,_,_,_,_,_,_,_,_,_) :: (
	findall(Cod,encomenda(Cod,_,_,_,_,_,_,_,_,_),R),
	length(R,N),
	N == 1
	).


/*
 * Não permite a inserção de encomendas a estafetas que não estão registados
*/

+encomenda(_,_,_,CodEstafeta,_,_,_,_,_,_) :: (
	estafeta(CodEstafeta,_)
	).

/*
 * Não permite a inserção de encomendas a clientes que não estão registados
*/

+encomenda(_,_,CodCliente,_,_,_,_,_,_,_) :: (
	cliente(CodCliente,_)
	).

/*
 * Não permite a atribuição de encomendas a estafetas penalizados
*/

+encomenda(_,_,_,CodEstafeta,_,_,_,_,DataEnc,_) :: (
	findall(DataInicio/DataFim,penalizado(CodEstafeta,DataInicio,DataFim),R),
	compareDatas(R,DataEnc)
	).

compareDatas([],_).
compareDatas([Di/Df|T],De) :- not(pertenceData(Di,Df,De)), compareDatas(T,De). 

/*
 * Não permite a inserção de encomendas associadas a transportes inexistentes ou que não a possam levar por conta do peso
*/

+encomenda(_,_,_,_,Peso,_,Transporte,_,_,_) :: (
	transporte(Transporte,PesoMax,_),
	Peso =< PesoMax
	).

/*
 * Não permite a inserção de encomendas registadas como entregues sem terem sido inicialmente registadas como criadas
*/

+encomenda(Cod,_,_) :: encomenda(Cod,_,_,_,_,_,_,_,_,_).


/*
 * Não permite a inserção de uma encomenda entregue 2x
 */

+encomenda(Cod,_,_) :: (
	findall(Cod,encomenda(Cod,_,_),R),
	length(R,N),
	N == 1
	).


/*
 * Não permite a inserção de uma encomenda entregue com data anterior a de criação
 */

+encomenda(Cod,Data,_) :: (
	encomenda(Cod,_,_,_,_,_,_,_,DataCriacao,_),
	dataDiff(Data,DataCriacao,N),
	N > 0
	).


 /*
 * Não permite a criação de penalizações para Estafeta com código inexistente
*/
+penalizado(CodEstafeta,_,_) :: (
	estafeta(CodEstafeta,_)
	).

/*
 * Não permite a criação de penalizações com intervalos de tempo negativos
*/

+penalizado(_,DataInicio,DataFim) :: (
	dataDiff(DataInicio,DataFim,N),
	N < 0
	).


/* 
 * Não permite a associação de uma CodEncomenda a mais de um circuito
 */

+encomendaCaminho(_,CodEncomenda) :: (
	findall(CodCaminho,encomendaCaminho(CodCaminho,CodEncomenda),Lista),
	length(Lista,N),
	N == 1
	).

/* 
 * Não permite mais de um caminho com o mesmo código de caminho
 */

+caminho(CodCaminho,_) :: (
	findall(CodCaminho,caminho(CodCaminho,_),Lista),
	length(Lista,N),
	N == 1
	).

/*
 * Garante a evolução fidedigna de conhecimento
*/

evolucao(Termo) :- findall(Invariante,+Termo::Invariante,Lista),insercao(Termo),teste(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo),!,fail.

teste([]).
teste([R|LR]) :- R,teste(LR).

/*
 * Graph
 */

edge(gothamCity, centralCity, dcStreet, 17, 20).
edge(gothamCity, capitol, katnissStreet, 52, 30).
edge(gothamCity, westEgg, batmanAvenue, 100, 45).
edge(eastEgg, westEgg, gatsbyBoulevard, 20, 13).
edge(centralCity, starCity, fastStreet, 31, 78).
edge(centralCity, gravityFalls, pineStreet, 13, 81).
edge(westEgg, westworld, westAvenue, 5, 56).
edge(westworld,rivendell, elfStreet, 30, 13).
edge(capitol, pawnee, swansonBoulevard, 20, 14).
edge(capitol, hogwarts, potterStreet, 100, 52).
edge(gravityFalls, bikiniBottom, patrickAvenue, 4, 98).
edge(pawnee, bikiniBottom, squidStreet, 26, 34).
edge(pawnee, neverland, panAvenue, 20, 67).
edge(rivendell, theShire, bilboStreet, 15, 12).
edge(mordor, theShire, ringBoulevard, 30, 29).
edge(neverland, wonderland, dreamStreet, 5, 71). 
edge(neverland, hogwarts, wandAvenue, 10, 43).
edge(bikiniBottom, springfield, yellowBoulevard, 2, 79).
edge(springfield, quahog, griffinAvenue, 3, 74).
edge(springfield, bedrock, rockBoulevard, 32, 45).
edge(theShire, hogwarts, magicStreet, 25, 47).
edge(theShire, asgard, gandalfStreet, 90, 46).
edge(theShire, narnia, closetedStreet, 52, 9).
edge(hogwarts, hogsmeade, hermioneAvenue, 2, 17).
edge(hogsmeade, narnia, sweetBoulevard, 120, 86).
edge(bedrock, jurassicPark, dinosaurAvenue, 10, 72).
edge(jurassicPark, tatooine, skyStreet, 50, 35).
edge(tatooine, dragonstone, falconBoulevard, 100, 90).
edge(dragonstone, narnia, lionStreet, 17, 23).
edge(narnia, kingsLanding, aslanAvenue, 22, 56).
edge(kingsLanding, asgard, odinStreet, 30, 89).

% estima(Orig,Dest,Distancia Linha Reta,Probabilidade de transito(valor de 0 a 100))

estima(capitol,neverland,30,37).
estima(gothamCity, hogwarts,120,13).
estima(tatooine, springfield,78,45).
estima(starCity,westworld,80,25).
estima(eastEgg,mordor,85,39).
estima(theShire,wonderland,30,56).
estima(capitol,asgard,100,9).
estima(mordor,neverland,48,21).
estima(mordor,gothamCity,94,16).
estima(mordor,bikinibottom,60,43).
estima(mordor,bedrock,74,39).

