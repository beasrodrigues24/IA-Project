import random
import datetime
from random import randrange
from datetime import timedelta
from faker import Faker

names = ("superhomem", "supermulher", "flash", "thor", "loki", "drestranho", "viuvanegra",
         "arrow", "batman", "bobesponja", "harrypotter", "barryallen", "iriswest", "lukeskywalker",
         "mandalorian", "r2d2", "hansolo", "leia", "homemformiga", "panteranegra", "lanternaverde",
         "hulk", "homemdeferro", "robin", "capitamarvel", "capitaoamerica", "batgirl", "batwoman",
         "asterix", "obelix", "peterpan", "gatsby", "katniss", "homer", "bart", "lisa", "trex",
         "bilbo", "ray", "lannister", "patrick", "anakin", "gandalf", "gollum", "dipper", "wendy",
         "cisco", "yoda", "chewbacca", "kylo", "obiwan", "darthvader", "apu", "burns", "squidward",
         "krabs", "plankton", "gary", "sandy", "hammond", "tyrion", "sansa", "arya", "cersei", "snow",
         "dumbledore", "weasley", "snape", "dobby", "bellatrix", "hagrid", "draco", "voldemort", "savitar",
         "zoom", "joe", "vibe", "caitlin", "wells", "bart", "wally", "killerfrost", "captainsnow", "venom",
         "thanos", "groot", "logan", "odin", "deadpool", "nickfury", "ultron", "shuri", "nebula", "ghostrider",
         "shangchi", "ikaris", "caspian", "aslam", "palpatine", "ares", "zeus", "wanda", "apollo"
         )

cities = ("gothamCity", "centralCity", "starCity", "mordor", "hogwarts", "springfield", "asgard", "eastEgg", "westEgg",
          "gravityFalls", "capitol", "bedrock", "pawnee", "theShire", "narnia", "hogsmeade", "rivendell",
          "kingsLanding", "jurassicPark",
          "bikiniBottom", "dragonstone", "quahog", "wonderland", "westworld", "neverland", "themyscira", "tatooine")

vehicles = ("bicicleta", "carro", "moto")


def random_date(start, end):
    delta = end - start
    int_delta = (delta.days * 24 * 60 * 60) + delta.seconds
    random_second = randrange(int_delta)
    return start + timedelta(seconds=random_second)


def get_random_date():
    fake = Faker()
    start_date = datetime.date(day=1, month=1, year=2001)
    end_date = datetime.date(day=31, month=12, year=2021)
    date_to_format = fake.date_between(start_date=start_date, end_date=end_date)
    date = datetime.datetime.strptime(str(date_to_format), '%Y-%m-%d')
    return date.strftime('%d/%m/%Y')


def get_random_name():
    return random.choice(names)


def get_random_vehicle():
    return random.choice(vehicles)


def get_random_city():
    return random.choice(cities)


def get_random_float(start, end):
    return round(random.uniform(start, end), 2)


def generate_stuff(how_many, option, begin_code, how_many_delivery, how_many_clients):
    if option == 1:
        with open('estafetas.txt', 'w') as file:
            for x in range(begin_code, how_many + begin_code):
                index = x
                tmp_str = "estafeta(" + str(index) + "," + get_random_name() + ")."
                file.write(tmp_str)
                if index + 1 != how_many + begin_code:
                    file.write("\n")
    elif option == 2:
        with open('clientes.txt', 'w') as file:
            for x in range(begin_code, how_many + begin_code):
                index = x
                tmp_str = "cliente(" + str(index) + "," + get_random_name() + ")."
                file.write(tmp_str)
                if index + 1 != how_many + begin_code:
                    file.write("\n")
    elif option == 3:
        with open('encomendas.txt', 'w') as file:
            for x in range(how_many):
                index = x
                cod_client = random.randrange(1, how_many_clients)
                cod_del = random.randrange(1, how_many_delivery)
                vehicle = get_random_vehicle()
                price = get_random_float(2.5, 150.5)
                date = get_random_date()
                weight = 0
                max_time = random.randint(1, 900)
                classification = random.randint(1, 5)
                city = get_random_city()
                volume = get_random_float(1.5, 150.5)
                status = "entregue"
                if vehicle == "bicicleta":
                    weight = random.randint(1, 5)
                elif vehicle == "mota":
                    weight = random.randint(1, 20)
                elif vehicle == "carro":
                    weight = random.randint(1, 100)
                tmp_str = "encomenda(" + str(max_time) + "," + str(cod_client) + "," + str(cod_del) + "," + str(weight) + "," + str(volume) + "," + status + "," + vehicle + "," + str(price) + "," + date + "," + city + "," + str(classification) + ")."
                file.write(tmp_str)
                if index + 1 != how_many:
                    file.write("\n")
    elif option == 4:
        with open('encomendas_n_entregues.txt', 'w') as file:
            for x in range(how_many):
                index = x
                cod_client = random.randrange(1, how_many_clients)
                cod_del = random.randrange(1, how_many_delivery)
                vehicle = get_random_vehicle()
                price = get_random_float(2.5, 150.5)
                weight = 0
                max_time = random.randint(1, 900)
                city = get_random_city()
                volume = get_random_float(1.5, 150.5)
                status = "em andamento"
                if vehicle == "bicicleta":
                    weight = random.randint(1, 5)
                elif vehicle == "mota":
                    weight = random.randint(1, 20)
                elif vehicle == "carro":
                    weight = random.randint(1, 100)
                tmp_str = "encomenda(" + str(max_time) + "," + str(cod_client) + "," + str(cod_del) + "," + str(weight) + "," + str(volume) + "," + status + "," + vehicle + "," + str(price) + "," + city + ")."
                file.write(tmp_str)
                if index + 1 != how_many:
                    file.write("\n")
    print("Ficheiro Gerado Com Sucesso.")


if __name__ == '__main__':
    while 1:
        print("1. Gerar Estafetas")
        print("2. Gerar Clientes")
        print("3. Gerar Encomendas Entregues")
        print("4. Gerar Encomendas Não Entregues")
        print("0. Sair")
        option = int(input())
        if option != 1 and option != 2 and option != 3 and option != 4 and option != 0:
            print("Opção Desconhecida. Insere novamente:")
        elif option != 0:
            print("Quantos?")
            how_many = int(input())
            if option == 1 or option == 2:
                print("Código Inicial?")
                begin_code = int(input())
                generate_stuff(how_many, option, begin_code, 0, 0)
            elif option == 3 or option == 4:
                print("Quantos clientes existem?")
                clients = int(input())
                print("Quantos estafetas existem?")
                delivery = int(input())
                generate_stuff(how_many, option, 0, clients, delivery)
        else:
            break
