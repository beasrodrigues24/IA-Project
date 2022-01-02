from pyswip import Prolog
from tkinter import ttk
from tkinter import *
from tkinter import Tk, Canvas, Frame, BOTH
import os

codCircuito = StringVar()

prolog = Prolog()
prolog.consult("graph_queries.pl")
prolog.consult("queries.pl")

def clearFrame(frame):
    for widget in frame.winfo_children():
        widget.destroy()

def gerarDFS(frame):
    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarDFSQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1
        
    #table.pack()

def gerarBFS(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarBFSQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1
        

def gerarDFSI(frame):
    
    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarDFSIQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1

def gerarGulosaD(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarGulosaDistQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1
    print("oi4")

def gerarGulosaT(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarGulosaTranQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1
    print("oi5")

def gerarAEstrelaD(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarAEstrelaDistQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1
    print("oi6")

def gerarAEstrelaT(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarAEstrelaTranQ(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
            print(str(r) + "-" + str(c))
            table = ttk.Treeview(frame)
            table.grid(row = r, column = c, sticky = W, padx = 10, pady = 10)
            table['columns'] = ('Etapa','Local')

            table.column("#0",width=0,stretch=NO)
            table.column("Etapa",width=100,anchor=CENTER)
            table.column("Local",width=200,anchor=CENTER)
    
            table.heading("Etapa",text="Etapa",anchor=CENTER)
            table.heading("Local",text="Local",anchor=CENTER)
            
            for local in circuito:
                table.insert(parent='',index='end',iid=i,values=(i,local))
                i = i + 1
            #table.pack()
            
            i = 0

            if cc == 0:
                c = 1
                cc = 1
            elif cc == 1:
                c = 2
                cc = 2
            elif cc == 2:
                c = 0
                cc = 0

            if ((rr % 3) == 0):
                r = r + 1
                rr = rr + 1

            elif ((rr % 3) != 0):
                rr = rr + 1
    print("oi7")

retas = {"gothamCitycentralCity": [600,100,800,150], 
         "gothamCitycapitol": [600,100,500,250] , 
         "gothamCitywestEgg": [600,100,200,250], 
         "eastEggwestEgg": [400,125,200,250], 
         "centralCitystarCity": [800,150,1000,200],
         "centralCitygravityFalls":[800,150,900,300],
         "westEggwestworld": [200,250,300,300],
         "westworldrivendell": [300,300,400,400],
         "capitolpawnee": [500,250,700,400],
         "capitolhogwarts": [500,250,500,650],
         "gravityFallsbikiniBottom": [900,300,1100,420],
         "pawneebikiniBottom": [500,250,1100,420],
         "pawneeneverland": [500,250,1100,420],
         "rivendelltheShire": [400,400,300,600],
         "mordortheShire": [100,500,300,600],
         "neverlandwonderland": [600,550,780,550],
         "neverlandhogwarts": [600,550,500,650],
         "bikiniBottomspringfield": [1100,420,920,450],
         "springfieldquahog": [920,450,1200,500],
         "springfieldbedrock": [920,450,950,650],
         "theShirehogwarts": [300,600,500,650],
         "theShireasgard": [300,600,180,700],
         "theShirenarnia": [300,600,520,770],
         "hogwartshogsmeade": [500,650,730,700],
         "hogsmeadenarnia": [730,700,520,770],
         "bedrockjurassicPark":[950,650,970,850],
         "jurassicParktatooine": [970,850,770,810],
         "tatooinedragstone": [770,810,610,900],
         "dragstonenarnia": [610,900,520,770],
         "narniakingsLanding": [520,770,300,850],
         "kingsLanding": [300,850,180,700]
         }

def create_circle(x, y, r, canvasName): #center coordinates, radius
    x0 = x - r
    y0 = y - r
    x1 = x + r
    y1 = y + r
    return canvasName.create_oval(x0, y0, x1, y1,fill="#15f948")

def colorir(mapKey,mapKey2,canvas):
    print(mapKey)
    l = retas.get(mapKey,"Null")
    if (l == "Null"):
        print("erro")
        print(mapKey2)
        l = retas.get(mapKey2)

    canvas.create_line(l[0],l[1],l[2],l[3],fill="red")

def gerarRota(frame,canvas):
    print(codCircuito.get())
    res = list(prolog.query("caminho("+codCircuito.get()+",Circuito)", maxresult=1))
    # for local(res[0]['Circuito'])
    i = 0
    while (i < len(res[0]['Circuito']) - 1):
        print("----")
        print(res[0]['Circuito'][i])
        print(res[0]['Circuito'][i + 1])
        mapKey = str(res[0]['Circuito'][i]) + str(res[0]['Circuito'][i+1])
        mapKey2 = str(res[0]['Circuito'][i + 1]) + str(res[0]['Circuito'][i])
        colorir(mapKey,mapKey2,canvas)
        i = i + 1

def gerarMapa(frame):
    clearFrame(frame)
    
    vbar=Scrollbar(frame,orient=VERTICAL)
    vbar.pack(side=RIGHT,fill=Y)
    
    canvas = Canvas(frame,width=1280,height=520,scrollregion=(0,0,500,1500))
    vbar.config(command=canvas.yview)
    canvas.config(background="white",yscrollcommand=vbar.set)
 
    text = Label(frame,text="Codigo circuito")
    textInsert = Entry(frame,textvariable=codCircuito)
    btn = ttk.Button(frame,text="Gerar rota",command=lambda: gerarRota(frame,canvas))
    text.pack()
    textInsert.pack()
    btn.pack()
    
    #Gotham City -> Central City
    canvas.create_line(600,100,retas["gothamCitycentralCity"][2],150)
    #Gotham City -> Capitol
    canvas.create_line(600,100,500,250)
    #Gotham City -> West Egg
    canvas.create_line(600,100,200,250)
    #East Egg -> West Egg
    canvas.create_line(400,125,200,250)
    #Central City -> Star City
    canvas.create_line(800,150,1000,200)
    #Cental City -> Gravity Falls
    canvas.create_line(800,150,900,300)
    #West Egg -> West Wolrd
    canvas.create_line(200,250,300,300)
    #West World -> Rivendell
    canvas.create_line(300,300,400,400)
    #Capitol -> Pawnee
    canvas.create_line(500,250,700,400)
    #Capitol -> Hogwarts
    canvas.create_line(500,250,500,650)
    #Gravity Falls -> Bikini Bottom
    canvas.create_line(900,300,1100,420)
    #Pawnee -> Bikini Bottom
    canvas.create_line(500,250,1100,420)
    #Pawnee -> Neverland
    canvas.create_line(500,250,600,550)
    #Rivendell -> The Shire
    canvas.create_line(400,400,300,600)
    #Mordor -> The Shire
    canvas.create_line(100,500,300,600)
    #Neverland -> Wonderland
    canvas.create_line(600,550,780,550)
    #Neverland -> Hogwarts
    canvas.create_line(600,550,500,650)
    #Bikini Bottom -> Springfield
    canvas.create_line(1100,420,920,450)
    #Springfield -> Quahog
    canvas.create_line(920,450,1200,500)
    #Springfield -> Bedrock
    canvas.create_line(920,450,950,650)
    #The Shire -> Hogwarts
    canvas.create_line(300,600,500,650)
    #The Shire -> Asgard
    canvas.create_line(300,600,180,700)
    #The Shire -> Narnia
    canvas.create_line(300,600,520,770)
    #Hogwarts -> Hosmeade
    canvas.create_line(500,650,730,700)
    #Hogsmeade -> Narnia
    canvas.create_line(730,700,520,770)
    #Bedrock -> Jurassic Park
    canvas.create_line(950,650,970,850)
    #Jurassic Park -> Tatooine
    canvas.create_line(970,850,770,810)
    #Tatooine -> Dragonstone
    canvas.create_line(770,810,610,900)
    #Dragonstone -> Narnia
    canvas.create_line(610,900,520,770)
    #Narnia -> Kings Land
    canvas.create_line(520,770,300,850)
    #KingsLand -> Asgard
    canvas.create_line(300,850,180,700)



    #GothamCity
    create_circle(600,100,50,canvas)
    txt = canvas.create_text(600, 100, text='Gotham City')
    #Capitol
    create_circle(500,250,40,canvas)
    txt2 = canvas.create_text(500, 250, text='Capitol')
    #CentralCity
    create_circle(800,150,50,canvas)
    txt3 = canvas.create_text(800, 150, text='Central City')
    #GravityFalls
    create_circle(900,300,50,canvas)
    txt4 = canvas.create_text(900, 300, text='Gravity Falls')
    #Pawnee
    create_circle(700,400,30,canvas)
    txt5 = canvas.create_text(700, 400, text='Pawnee')
    #EastEgg
    create_circle(400,125,40,canvas)
    txt6 = canvas.create_text(400, 125, text='East Egg')
    #WestEgg
    create_circle(200,250,40,canvas)
    txt7 = canvas.create_text(200, 250, text='West Egg')
    #WestWorld
    create_circle(300,300,40,canvas)
    txt8 = canvas.create_text(300, 300, text='West World')
    #Rivendell
    create_circle(400,400,40,canvas)
    txt9 = canvas.create_text(400, 400, text='Rivendell')
    #StarCity
    create_circle(1000,200,40,canvas)
    txt10 = canvas.create_text(1000, 200, text='Star City')
    #BikiniBottom
    create_circle(1100,420,50,canvas)
    txt11 = canvas.create_text(1100, 420, text='Bikini Bottom')
    #Springfield
    create_circle(920,450,40,canvas)
    txt12 = canvas.create_text(920, 450, text='Springfield')
    #Quahog
    create_circle(1200,500,30,canvas)
    txt13 = canvas.create_text(1200, 500, text='Quahog')
    #Mordor
    create_circle(100,500,30,canvas)
    txt14 = canvas.create_text(100, 500, text='Mordor')
    #TheShire
    create_circle(300,600,40,canvas)
    txt15 = canvas.create_text(300, 600, text='The Shire')
    #Asgard
    create_circle(180,700,30,canvas)
    txt16 = canvas.create_text(180, 700, text='Asgard')
    #Hogwarts    
    create_circle(500,650,40,canvas)
    txt17 = canvas.create_text(500, 650, text='Hogwarts')
    #Nerveland
    create_circle(600,550,40,canvas)
    txt18 = canvas.create_text(600, 550, text='Neverland')
    #Hogsmeade
    create_circle(730,700,40,canvas)
    txt19 = canvas.create_text(730, 700, text='Hogsmeade')
    #King'sLand
    create_circle(300,850,40,canvas)
    txt20 = canvas.create_text(300, 850, text='Kings Land')
    #Narnia
    create_circle(520,770,30,canvas)
    txt21 = canvas.create_text(520, 770, text='Narnia')
    #Dragonstone
    create_circle(610,900,50,canvas)
    txt22 = canvas.create_text(610, 900, text='Dragonstone')
    #Tatooine
    create_circle(770,810,40,canvas)
    txt23 = canvas.create_text(770, 810, text='Tatooine')
    #JurassicPark
    create_circle(970,850,50,canvas)
    txt24 = canvas.create_text(970, 850, text='Jurassic Park')
    #Bedrock
    create_circle(950,650,40,canvas)
    txt25 = canvas.create_text(950, 650, text='Bedrock')
    #Wonderland
    create_circle(780,550,40,canvas)
    txt26 = canvas.create_text(780, 550, text='Wonderland')
    
    canvas.pack(side=LEFT,expand=True,fill=BOTH)
    #canvas.pack(fill=BOTH, expand=1)
    print("oi")


def gerarRec(circuitos,key):
    if len(circuitos) == 0:
        return
    st = "["
    i = 1
    for l in circuitos[0]:
        print(str(i) + "-" + str(len(circuitos[0])))
        if i < len(circuitos[0]):
            st2 = str(l) + ","
            st += st2
        elif i == len(circuitos[0]):
            st2 = str(l) + "]"
            st += st2
        i = i + 1
    print(st)
    res = bool(list(prolog.query("evolucao(caminho("+str(key)+","+st+"))")))
    if res == True:
        prolog.assertz("caminho("+str(key)+","+st+")")
        print("Inserido !")
    if len(circuitos) > 1:
        gerarRec(circuitos[1:-1],key + 1)

def gerarCircuitos():    
    res = list(prolog.query("gerarGulosaDistQ(Circuitos)", maxresult=1))
    i = 0
    j = 0
    gerarRec(res[0]['Circuitos'],0)
        #for circuito in s['Circuitos']:
        #    print("#")
        #    print(str(circuito))
        #    res2 = bool(list(prolog.query("evolucao(caminho("+str(j)+",1))")))
        #    if res2 == True:
        #        prolog.assertz("caminho("+str(j)+",1))")
        #        print("Inserido !")
        #    j = j + 1
       #     while (i < len(circuito) - 1):
        #        print("----")
        #        print(circuito[i])
        #        print(circuito[i + 1])
        #        i = i + 1
        #    i = 0
