from pyswip import Prolog
from tkinter import ttk
from tkinter import *
from tkinter import Tk, Canvas, Frame, BOTH
import os

codCircuito = StringVar()
dataBase = StringVar()
codCircuito2 = StringVar()
codEncomenda = StringVar()
topN = StringVar()
topOpt = StringVar()
produtividade = StringVar()
key_act = 0

prolog = Prolog()
prolog.consult("graph_queries.pl")
prolog.consult("queries.pl")

def clearFrame(frame):
    for widget in frame.winfo_children():
        widget.destroy()

def maisRapido(frame):
    clearFrame(frame)
    res = list(prolog.query("minCaminho("+str(codCircuito.get())+",Res)"))
    if (len(res) > 0):
        st = "O circuito " + str(res[0]['Res']) + " é o mais rápido."
        text = Label(frame,text=st)
        text.pack()

    print("Mais rapido")

def maisEcologico(frame):
    clearFrame(frame)
    res = list(prolog.query("minCaminho("+str(codCircuito.get())+","+str(dataBase.get())+",Res)"))
    if (len(res) > 0):
        st = "O circuito " + str(res[0]['Res']) + " é o mais ecológico."
        text = Label(frame,text=st)
        text.pack()

    print("Mais ecológico")

def circuitoMaisRapido(frame):
    clearFrame(frame)
    text = Label(frame,text="Codigos dos circuitos a comparar * (no formato [cod1,...,codN])")
    textInsert = Entry(frame,textvariable=codCircuito)
    text.pack()
    textInsert.pack()
    btn = ttk.Button(frame,text="Comparar",command=lambda: maisRapido(frame))
    btn.pack()
    print("Fast")

def circuitoMaisEcologico(frame):
    clearFrame(frame)
    text = Label(frame,text="Codigos dos circuitos a comparar * (no formato [cod1,...,codN]")
    textInsert = Entry(frame,textvariable=codCircuito)
    text.pack()
    textInsert.pack()
    text2 = Label(frame,text="Data base")
    text2Insert = Entry(frame,textvariable=dataBase)
    text2.pack()
    text2Insert.pack()
    btn = ttk.Button(frame,text="Comparar",command=lambda: maisEcologico(frame))
    btn.pack()
    print("Eco")

def comparar(frame):
    clearFrame(frame)
    if (produtividade.get() == "Distância"):
        res = list(prolog.query("comparaCaminho("+str(codCircuito.get())+","+str(codCircuito2.get())+",Res)"))
        print("Res:" + str(res[0]['Res']))
        if (res[0]['Res'] > 0):
            st = "O circuito " + str(codCircuito.get()) + " é " + str(res[0]['Res']) + " unidades maior que o circuito " + str(codCircuito2.get()) + " em termos de distância."
            text = Label(frame,text=st)
            text.pack()
        elif (res[0]['Res'] == 0):
            text2 = Label(frame,text="Os circuitos são iguais em termos de distância")
            text2.pack()
        elif (res[0]['Res'] < 0):
            st2 = "O circuito " + str(codCircuito2.get()) + " é " + str(abs(res[0]['Res'])) + " unidades maior que o circuito " + str(codCircuito.get()) + " em termos de distância."
            text3 = Label(frame,text=st2)
            text3.pack()

    elif (produtividade.get() == "Tempo"):
        res = list(prolog.query("comparaCaminho("+str(codCircuito.get())+","+str(codCircuito2.get())+","+dataBase.get()+",Res)"))
        print("Res:" + str(res[0]['Res']))
        if (res[0]['Res'] > 0):
            st = "O circuito " + str(codCircuito.get()) + " é " + str(res[0]['Res']) + " unidades maior que o circuito " + str(codCircuito2.get()) + " em termos de tempo."
            text = Label(frame,text=st)
            text.pack()
        elif (res[0]['Res'] == 0):
            text2 = Label(frame,text="Os circuitos são iguais em termos de tempo")
            text2.pack()
        elif (res[0]['Res'] < 0):
            st2 = "O circuito " + str(codCircuito2.get()) + " é " + str(abs(res[0]['Res'])) + " unidades maior que o circuito " + str(codCircuito.get()) + " em termos de tempo"
            text3 = Label(frame,text=st2)
            text3.pack()

def compararCircuitos(frame):
    clearFrame(frame)
    
    OPTIONS = [
    "Distância",
    "Tempo"
    ] #etc

    produtividade.set(OPTIONS[0]) # default value

    w = OptionMenu(frame, produtividade, *OPTIONS)
    w.pack()
    text = Label(frame,text="Codigo circuito 1")
    textInsert = Entry(frame,textvariable=codCircuito)
    text2 = Label(frame,text="Codigo circuito 2")
    text2Insert = Entry(frame,textvariable=codCircuito2)
    text3 = Label(frame,text="Data base * (apenas para comparar tempos)")
    text3Insert = Entry(frame,textvariable=dataBase)
    btn = ttk.Button(frame,text="Comparar",command=lambda: comparar(frame))
    text.pack()
    textInsert.pack()
    text2.pack()
    text2Insert.pack()
    text3.pack()
    text3Insert.pack()
    btn.pack()

def top(frame):
    clearFrame(frame)
    if (topOpt.get() == "Peso"):
        res = list(prolog.query("ordenaCircuitosPeso("+str(topN.get())+",Res)"))
    elif (topOpt.get() == "Volume"):
        res = list(prolog.query("ordenaCircuitosVolume("+str(topN.get())+",Res)"))

    table = ttk.Treeview(frame)
    table['columns'] = ('Posição','Codigo Circuito')
    table.column('#0',width=0,stretch=NO)
    table.column('Posição',width=300,anchor=CENTER)
    table.column('Codigo Circuito',width=400,anchor=CENTER)
    table.heading("Posição",text="Posição",anchor=CENTER)
    table.heading("Codigo Circuito",text="Codigo Circuito",anchor=CENTER)
        
    i = 1
    for answer in (res[0]['Res']):
        table.insert(parent='',index='end',iid=i,values=(i,answer))
        i = i + 1

    table.pack()
    
    print(topN.get())

def obterTopCircuitos(frame):
    clearFrame(frame)

    OPTIONS = [
    "Peso",
    "Volume"
    ] #etc

    topOpt.set(OPTIONS[0]) # default value

    w = OptionMenu(frame, topOpt, *OPTIONS)
    w.pack()
    text = Label(frame,text="Top")
    textInsert = Entry(frame,textvariable=topN)
    btn = ttk.Button(frame,text="Obter",command=lambda: top(frame))
    text.pack()
    textInsert.pack()
    btn.pack()

def associarCE():
    res = bool(list(prolog.query("evolucao(encomendaCaminho("+codCircuito.get()+","+codEncomenda.get()+"))")))
    if res == True:
        prolog.assertz("encomendaCaminho("+codCircuito.get()+","+codEncomenda.get()+")")
        print("Inserido !")

def associarEncomendaCaminho(frame):
    clearFrame(frame)
    text = Label(frame,text="Codigo circuito")
    textInsert = Entry(frame,textvariable=codCircuito)
    text2 = Label(frame,text="Codigo encomenda")
    text2Insert = Entry(frame,textvariable=codEncomenda)
    btn = ttk.Button(frame,text="Gerar rota",command=associarCE)
    text.pack()
    textInsert.pack()
    text2.pack()
    text2Insert.pack()
    btn.pack()

def gerarDFS(frame):
    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarDFS2Q(Circuitos)", maxresult=1)
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
    res = prolog.query("gerarBFS2Q(Circuitos)", maxresult=1)
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
    res = prolog.query("gerarDFSI2Q(Circuitos)", maxresult=1)
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
    res = prolog.query("gerarGulosaDist2Q(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
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

def gerarGulosaT(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarGulosaTran2Q(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
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

def gerarAEstrelaD(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarAEstrelaDist2Q(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
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

def gerarAEstrelaT(frame):

    clearFrame(frame)

    i = 0;
    r = 0;
    c = 0;
    cc = 0; rr = 1;
    res = prolog.query("gerarAEstrelaTran2Q(Circuitos)", maxresult=1)
    for s in res:
        for circuito in s['Circuitos']:
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
         "pawneebikiniBottom": [700,400,1100,420],
         "pawneeneverland": [700,400,600,550],
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
         "tatooinedragonstone": [770,810,610,900],
         "dragonstonenarnia": [610,900,520,770],
         "narniakingsLanding": [520,770,300,850],
         "kingsLandingasgard": [300,850,180,700]
         }

nodos = {"gothamCity": [600,100,50], 
        "capitol": [500,250,40],
        "centralCity":[800,150,50],
        "gravityFalls":[900,300,50],
        "pawnee":[700,400,30],
        "eastEgg":[400,125,40],
        "westEgg":[200,250,40],
        "westworld":[300,300,40],
        "rivendell":[400,400,40],
        "starCity":[1000,200,40],
        "bikiniBottom":[1100,420,50],
        "springfield":[920,450,40],
        "quahog": [1200,500,30],
        "mordor":[100,500,30],
        "theShire":[300,600,40],
        "asgard": [180,700,30],
        "hogwarts": [500,650,40],
        "neverland": [600,550,40],
        "hogsmeade": [730,700,40],
        "kingsLanding": [300,850,50],
        "narnia": [520,770,30],
        "dragonstone": [610,900,50],
        "tatooine": [770,810,40],
        "jurassicPark": [970,850,50],
        "bedrock": [950,650,40],
        "wonderland": [780,550,40]
        }

def create_circle(x, y, r, canvasName): #center coordinates, radius
    x0 = x - r
    y0 = y - r
    x1 = x + r
    y1 = y + r
    return canvasName.create_oval(x0, y0, x1, y1,fill="#15f948")

def create_circle2(x, y, r, canvasName): #center coordinates, radius
    x0 = x - r
    y0 = y - r
    x1 = x + r
    y1 = y + r
    canvasName.create_oval(x0, y0, x1, y1,fill="blue")

def write_name(x,y,name,canvasName):
    canvasName.create_text(x,y,text=name)

def colorir(mapKey,mapKey2,nodeKey1,nodeKey2,canvas):
    print(mapKey)
    l = retas.get(mapKey,"Null")
    if (l == "Null"):
        print("erro")
        print(mapKey2)
        l = retas.get(mapKey2)

    canvas.create_line(l[0],l[1],l[2],l[3],fill="blue")
    n = nodos.get(nodeKey1)
    n2 = nodos.get(nodeKey2)
    create_circle2(n[0],n[1],n[2],canvas)
    write_name(n[0],n[1],nodeKey1,canvas)
    create_circle2(n2[0],n2[1],n2[2],canvas)
    write_name(n2[0],n2[1],nodeKey2,canvas)

def gerarRota(frame,canvas):
    drawLines(frame,canvas)
    drawNodes(frame,canvas)
    print("ROTA DO CIRCUITO: " + codCircuito.get())
    res = list(prolog.query("caminho("+codCircuito.get()+",Circuito)", maxresult=1))
    # for local(res[0]['Circuito'])
    i = 0
    while (i < len(res[0]['Circuito']) - 1):
        print(">")
        print(res[0]['Circuito'][i])
        print(res[0]['Circuito'][i + 1])
        mapKey = str(res[0]['Circuito'][i]) + str(res[0]['Circuito'][i+1])
        mapKey2 = str(res[0]['Circuito'][i + 1]) + str(res[0]['Circuito'][i])
        colorir(mapKey,mapKey2,str(res[0]['Circuito'][i]),str(res[0]['Circuito'][i+1]),canvas)
        i = i + 1
    #drawNodes(frame,canvas)

def drawLines(frame,canvas): 
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
    canvas.create_line(700,400,1100,420)
    #Pawnee -> Neverland
    canvas.create_line(700,400,600,550)
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
    canvas.pack(side=LEFT,expand=True,fill=BOTH)

def drawNodes(frame,canvas):
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
    create_circle(300,850,50,canvas)
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
    drawLines(frame,canvas)
    drawNodes(frame,canvas)


def gerarRec(circuitos,key):
    global key_act
    print("KEY:" + str(key_act))
    print("LEN:" + str(len(circuitos)))
    if len(circuitos) == 0:
        return
    st = "["
    i = 1
    for l in circuitos[0]:
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
        key_act = key_act + 1
        gerarRec(circuitos[1:],key + 1)

def gerarCircuitos():   
    global key_act
    key_act = 0
    res = list(prolog.query("gerarGulosaDist2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)
    
    res = list(prolog.query("gerarGulosaTran2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)
    
    res = list(prolog.query("gerarAEstrelaDist2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)
    
    res = list(prolog.query("gerarAEstrelaTran2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)
    
    res = list(prolog.query("gerarBFS2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)

    res = list(prolog.query("gerarDFS2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)
    
    res = list(prolog.query("gerarDFSI2Q(Circuitos)",maxresult=1))
    gerarRec(res[0]['Circuitos'],key_act)
