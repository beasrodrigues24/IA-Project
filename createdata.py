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


def get_random_date(fake):
    start_date = datetime.date(day=1, month=1, year=2001)
    end_date = datetime.date(day=31, month=12, year=2021)
    date_to_format = fake.date_between(start_date=start_date, end_date=end_date)
    date = datetime.datetime.strptime(str(date_to_format), '%Y-%m-%d')
    random_hour = random.randint(0,23)
    return date.strftime('%d/%m/%Y') + "/" + str(random_hour)

def get_random_date_between(fake, start, end, same_day, min_hour):
    date_to_format = fake.date_between(start_date=start, end_date=end)
    date = datetime.datetime.strptime(str(date_to_format), '%Y-%m-%d')
    if same_day:
        random_hour = random.randint(min_hour,23)
    else:
        random_hour = random.randint(0, 23)
    return date.strftime('%d/%m/%Y') + "/" + str(random_hour)


def get_random_name():
    return random.choice(names)


def get_random_vehicle():
    return random.choice(vehicles)


def get_random_city():
    return random.choice(cities)


def get_random_float(start, end):
    return round(random.uniform(start, end), 2)

def get_in_time():
    bingo = random.randint(1, 4)
    return not (bingo == 4)

def was_delivered():
    bingo = random.randint(1,3)
    return bingo == 3

def get_date_on_correct_format(date):
    elems = date.split('/')
    correct_format = elems[0] + "/" + elems[1] + "/" + elems[2]
    return correct_format

def get_min_hour(date):
    elems = date.split('/')
    return elems[3]

def get_is_same_day(begin_date, end_date):
    formated_end_date = end_date.strftime('%d/%m/%Y')
    print(begin_date)
    print(formated_end_date)
    is_same_day =  begin_date == formated_end_date
    print(is_same_day)
    return begin_date == formated_end_date    


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
            fake = Faker()
            for x in range(how_many):
                index = x
                cod_client = random.randrange(1, how_many_clients)
                cod_del = random.randrange(1, how_many_delivery)
                vehicle = get_random_vehicle()
                price = get_random_float(2.5, 150.5)
                date = get_random_date(fake)
                weight = 0
                max_time = random.randint(1, 900)
                classification = random.randint(1, 5)
                city = get_random_city()
                volume = get_random_float(1.5, 150.5)
                if vehicle == "bicicleta":
                    weight = random.randint(1, 5)
                elif vehicle == "moto":
                    weight = random.randint(1, 20)
                elif vehicle == "carro":
                    weight = random.randint(1, 100)
                tmp_str = "encomenda(" + str(index) + "," + str(max_time) + "," + str(cod_client) + "," + str(cod_del) + "," + str(weight) + "," + str(volume) +  "," + vehicle + "," + str(price) + "," + date + "," + city + ")."
                file.write(tmp_str)
                delivered = was_delivered()
                if delivered:
                    file.write("\n")
                    in_time = get_in_time()
                    min_hour = int(get_min_hour(date))
                    date = get_date_on_correct_format(date)
                    if in_time:
                        begin_date = datetime.datetime.strptime(date, '%d/%m/%Y')
                        time_to_add = random.randint(1, max_time)
                        time_to_add = datetime.timedelta(hours=time_to_add)
                        end_date = begin_date + time_to_add
                        same_day = get_is_same_day(date, end_date)
                        del_date = get_random_date_between(fake, begin_date, end_date, same_day, min_hour)
                    else:
                        begin_date = datetime.datetime.strptime(date, '%d/%m/%Y')
                        time_to_add = random.randint(max_time + 1, 2000)
                        time_to_add = datetime.timedelta(hours=time_to_add)
                        end_date = begin_date + time_to_add
                        same_day = get_is_same_day(date, end_date)
                        del_date = get_random_date_between(fake, begin_date, end_date, same_day, min_hour)
                    str2 = "encomenda(" + str(index) + "," + del_date + "," + str(classification) + ")."
                    file.write(str2)
                if index + 1 != how_many:
                    file.write("\n")
    print("Ficheiro Gerado Com Sucesso.")


if __name__ == '__main__':
    while 1:
        print("1. Gerar Estafetas")
        print("2. Gerar Clientes")
        print("3. Gerar Encomendas")
        print("0. Sair")
        option = int(input())
        if option != 1 and option != 2 and option != 3 and option != 0:
            print("Opção Desconhecida. Insere novamente:")
        elif option != 0:
            print("Quantos?")
            how_many = int(input())
            if option == 1 or option == 2:
                print("Código Inicial?")
                begin_code = int(input())
                generate_stuff(how_many, option, begin_code, 0, 0)
            elif option == 3:
                print("Código Inicial?")
                begin_code = int(input())
                print("Quantos clientes existem?")
                clients = int(input())
                print("Quantos estafetas existem?")
                delivery = int(input())
                generate_stuff(how_many, option, begin_code, clients, delivery)
        else:
            break
