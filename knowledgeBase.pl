
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