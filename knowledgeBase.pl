
map( graph([gothamCity, centralCity, starCity, mordor, hogwarts, springfield, asgard, eastEgg, westEgg,
gravityFalls, capitol, bedrock, pawnee, theShire, narnia, hogsmeade, rivendell, kingsLanding, jurassicPark,
bikiniBottom, dragonstone, quahog, wonderland, westworld, neverland, themyscira, tatooine],
    [edge(gothamCity, centralCity, dcStreet, 17),
     edge(gothamCity, capitol, katnissStreet, 52),
     edge(gothamCity, westEgg, batmanAvenue, 100),
     edge(eastEgg, westEgg, gatsbyBouleavard, 20),
     edge(centralCity, starCity, fastStreet, 31),
     edge(centralCity, gravityFalls, pineStreet, 13),
     edge(westEgg, westworld, westAvenue, 5),
     edge(westworld,rivendell, elfStreet, 30),
     edge(capitol, pawnee, a4, 50),
     edge(capitol, hogwarts, a24, 100),
     edge(gravityFalls, bikiniBottom,a11, 4),
     edge(pawnee, bikiniBottom, a11, 26),
     edge(pawnee, neverland, a24, 20),
     edge(rivendell, theShire,a24, 15),
     edge(mordor, theShire, a25, 30),
     edge(neverland,wonderland, a25, 5), 
     edge(neverland, hogwarts, a4, 10),
     edge(bikiniBottom, springfield,a11, 2),
     edge(springfield, quahog, a11, 3),
     edge(springfield, bedrock, a24, 32),
     edge(theShire, hogwarts, a24, 25),
     edge(theShire, asgard,a24, 90),
     edge(theShire, narnia, a25, 52),
     edge(hogwarts, hogsmeade, a25, 2),
     edge(hogsmeade, narnia, a4, 120),
     edge(bedrock, jurassicPark, a24, 10),
     edge(jurassicPark, tatooine,a11, 50),
     edge(tatooine, dragonstone, a11, 100),
     edge(dragonstone, narnia, a24, 17),
     edge(narnia, kingsLanding,a24, 22),
     edge(kingsLanding, asgard, a25, 30)]
   )).