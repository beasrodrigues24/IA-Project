from tkinter import *
import os
from pyswip import Prolog
from tkinter import ttk
from pyswip import Variable,Query
from tkinter import messagebox
from tkinter import filedialog
from PIL import ImageTk,Image  
import graph

prolog = Prolog()
prolog.consult("queries.pl")

app = Tk()
app.title("Green Distributions")
app.geometry("800x400")
app.configure(background="white")
frame = Frame(app)
frame.pack()

labelHello = Label(frame, text = 'Green Distribution')
labelHello.config(width=200)
labelHello.config(font=("Helvetica", 35),background="white")
labelHello2 = Label(frame, text = 'Bem-vindo a')
labelHello2.config(width=200)
labelHello2.config(font=("Helvetica", 15),background="white")
labelHello2.pack()
labelHello.pack()

canvas = Canvas(frame, width = 600, height = 600)  
canvas.pack()  
img = ImageTk.PhotoImage(Image.open("green3.jpg"))  
canvas.create_image(5, 5, anchor=NW, image=img) 

# GLOBAL VARIABLES

codClient = StringVar()
codEstafeta = StringVar()
nameClient = StringVar()
nameEstafeta = StringVar()
dataEntrega = StringVar()
topN = StringVar()
dataInicial = StringVar()
dataFinal = StringVar()
peso = StringVar()
volume = StringVar()
transporte = StringVar()
codEncomenda = StringVar()
zonaEntrega = StringVar()
tempMax = StringVar()
precoBase = StringVar()
classif = StringVar()

def open_popup():
   top= Toplevel(app)
   top.geometry("750x250")
   top.title("Child Window")
   Label(top, text= "Hello World!", font=('Mistral 18 bold')).place(x=150,y=80)   

def clearFrame():
    for widget in frame.winfo_children():
        widget.destroy()

# ----------------------------------------------- DONE

def query2():
    
    clearFrame()
    
    text = Label(frame ,text = "Código Cliente")
    textInsert = Entry(frame,textvariable=codClient)
    btn = ttk.Button(frame,text="Procurar",command=query2Aux)
    text.pack()
    textInsert.pack()
    btn.pack()

def query2Aux():
    cod = codClient.get()
    clearFrame()

    textString = StringVar()
    textString.set("Estafetas que serviram o Cliente: " + cod)
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código','Estafeta')

    table.column("#0",width=0,stretch=NO)
    table.column("Código",width=400,anchor=CENTER)
    table.column("Estafeta",width=400,anchor=CENTER)
   
    table.heading("Código",text="Código",anchor=CENTER)
    table.heading("Estafeta",text="Estafeta",anchor=CENTER)

    i = 0
    res = list(prolog.query("query2("+cod+",Y)"))
    for answer in (res[0]['Y']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

# -------------------------------------------------- DONE

def query3():
    
    clearFrame()
    
    text = Label(frame ,text = "Código Estafeta")
    textInsert = Entry(frame,textvariable=codEstafeta)
    btn = ttk.Button(frame,text="Procurar",command=query3Aux)
    text.pack()
    textInsert.pack()
    btn.pack()

def query3Aux():
    cod = codEstafeta.get()
    clearFrame()

    textString = StringVar()
    textString.set("Clientes servidos pelo estafeta: " + cod)
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código','Cliente')

    table.column("#0",width=0,stretch=NO)
    table.column("Código",width=400,anchor=CENTER)
    table.column("Cliente",width=400,anchor=CENTER)
   
    table.heading("Código",text="Código",anchor=CENTER)
    table.heading("Cliente",text="Cliente",anchor=CENTER)

    i = 0
    res = list(prolog.query("query3("+cod+",Y)"))
    for answer in (res[0]['Y']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

# ------------------------------------------------------ DONE

def query4():
    
    clearFrame()
    
    text = Label(frame ,text = "Data")
    textInsert = Entry(frame,textvariable=dataEntrega)
    btn = ttk.Button(frame,text="Calcular",command=query4Aux)
    text.pack()
    textInsert.pack()
    btn.pack()

def query4Aux():
    data = dataEntrega.get()
    clearFrame()

    textString = StringVar()
    textString.set("Valor faturado em: " + data)
    text = Label(frame,textvariable=textString)
    text.pack()
    textResString = StringVar()
    res = list(prolog.query("query4("+data+",Y)"))
    textRes = Label(frame,textvariable=textResString)
    textResString.set(str(res[0]['Y']) + " euros")
    textRes.pack()

# ------------------------------------------------------- DONE

def query5():
    
    clearFrame()
    
    text = Label(frame ,text = "Top")
    textInsert = Entry(frame,textvariable=topN)
    btn = ttk.Button(frame,text="Calcular",command=query5Aux)
    text.pack()
    textInsert.pack()
    btn.pack()

def query5Aux():
    top = topN.get()
    clearFrame()

    textString = StringVar()
    textString.set("Top " + top)
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame)
    table['columns'] = ('Posição','Zona')

    table.column("#0",width=0,stretch=NO)
    table.column("Posição",width=400,anchor=CENTER)
    table.column("Zona",width=400,anchor=CENTER)
   
    table.heading("Posição",text="Posição",anchor=CENTER)
    table.heading("Zona",text="Zona",anchor=CENTER)

    i = 1
    res = list(prolog.query("query5("+top+",Y)"))
    print(res)
    for answer in (res[0]['Y']):
        table.insert(parent='',index='end',iid=i,values=(i,answer))
        i = i + 1
    
    table.pack()

# -------------------------------------------------------- DONE 

def query6():
    
    clearFrame()
    
    text = Label(frame ,text = "Código Estafeta")
    textInsert = Entry(frame,textvariable=codEstafeta)
    btn = ttk.Button(frame,text="Procurar",command=query6Aux)
    text.pack()
    textInsert.pack()
    btn.pack()

def query6Aux():
    cod = codEstafeta.get()
    clearFrame()

    textString = StringVar()
    textString.set("Classificação média estafeta: " + cod)
    text = Label(frame,textvariable=textString)
    text.pack()
    
    textResString = StringVar()
    res = list(prolog.query("query6("+cod+",Y)"))
    textRes = Label(frame,textvariable=textResString)
    textResString.set("Classificação: " +str(res[0]['Y']))
    textRes.pack()

# ------------------------------------------------ DONE

def query7():
    
    clearFrame()
    
    text = Label(frame ,text = "Data inicial")
    textInsert = Entry(frame,textvariable=dataInicial)
    
    text2 = Label(frame ,text = "Data Final")
    textInsert2 = Entry(frame,textvariable=dataFinal)
    
    btn = ttk.Button(frame,text="Calcular",command=query7Aux)
    
    text.pack()
    textInsert.pack()
    text2.pack()
    textInsert2.pack()
    
    btn.pack()

def query7Aux():
    di = dataInicial.get()
    df = dataFinal.get()
    clearFrame()

    textString = StringVar()
    textString.set("Número de encomendas por transporte ")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame)
    table['columns'] = ('Transporte','Quantidade')

    table.column("#0",width=0,stretch=NO)
    table.column("Transporte",width=400,anchor=CENTER)
    table.column("Quantidade",width=400,anchor=CENTER)
   
    table.heading("Transporte",text="Transporte",anchor=CENTER)
    table.heading("Quantidade",text="Quantidade",anchor=CENTER)

    i = 1
    res = list(prolog.query("query7("+di+","+df+",Y)"))
    print(res)
    for answer in (res[0]['Y']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

# ------------------------------------------------ DONE

def query8():
    
    clearFrame()
    
    text = Label(frame ,text = "Data inicial")
    textInsert = Entry(frame,textvariable=dataInicial)
    
    text2 = Label(frame ,text = "Data Final")
    textInsert2 = Entry(frame,textvariable=dataFinal)
    
    btn = ttk.Button(frame,text="Calcular",command=query8Aux)
    
    text.pack()
    textInsert.pack()
    text2.pack()
    textInsert2.pack()
    
    btn.pack()

def query8Aux():
    di = dataInicial.get()
    df = dataFinal.get()
    clearFrame()

    text = Label(frame ,text = "Entregas por estafeta entre " + di + " e " + df)
    text.pack()

    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código Estafeta','Quantidade')

    table.column("#0",width=0,stretch=NO)
    table.column("Código Estafeta",width=400,anchor=CENTER)
    table.column("Quantidade",width=400,anchor=CENTER)
   
    table.heading("Código Estafeta",text="Código Estafeta",anchor=CENTER)
    table.heading("Quantidade",text="Quantidade",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query8("+di+","+df+",Y)"))
    for answer in (res[0]['Y']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

# ------------------------------------------------ DONE

def query9():
    
    clearFrame()
    
    text = Label(frame ,text = "Data inicial")
    textInsert = Entry(frame,textvariable=dataInicial)
    
    text2 = Label(frame ,text = "Data Final")
    textInsert2 = Entry(frame,textvariable=dataFinal)
    
    btn = ttk.Button(frame,text="Calcular",command=query9Aux)
    
    text.pack()
    textInsert.pack()
    text2.pack()
    textInsert2.pack()
    
    btn.pack()

def query9Aux():
    di = dataInicial.get()
    df = dataFinal.get()
    clearFrame()

    text = Label(frame ,text = "Número de encomendas entregues e não entregues entre " + di + " e " + df)
    text.pack()

    textResString = StringVar()
    res = list(prolog.query("query9("+di+","+df+",Y,X)"))
    textRes = Label(frame,textvariable=textResString)
    
    textResString.set("Encomendas entregues: " +str(res[0]['Y']))
    textRes.pack()
    
    textRes2String = StringVar()
    textRes2 = Label(frame,textvariable=textRes2String)
    textRes2String.set("Encomendas não entregues: " +str(res[0]['X']))
    textRes2.pack()

# ---------------------------------------------------------- DONE

def query10():
    
    clearFrame()
    
    text = Label(frame ,text = "Data")
    textInsert = Entry(frame,textvariable=dataInicial)
     
    btn = ttk.Button(frame,text="Calcular",command=query10Aux)
    
    text.pack()
    textInsert.pack()
    
    btn.pack()

def query10Aux():
    di = dataInicial.get()
    clearFrame()

    text = Label(frame ,text = "Peso Transportado pelos estafetas em " + di)
    text.pack()

    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código Estafeta','Peso Transportado')

    table.column("#0",width=0,stretch=NO)
    table.column("Código Estafeta",width=400,anchor=CENTER)
    table.column("Peso Transportado",width=400,anchor=CENTER)
   
    table.heading("Código Estafeta",text="Código Estafeta",anchor=CENTER)
    table.heading("Peso Transportado",text="Peso Transportado",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query10("+di+",X)"))
    for answer in (res[0]['X']):
        table.insert(parent='',index='end',iid=i,values=(str(answer.args[0]),str(answer.args[1]) + " Kg"))
        i = i + 1
    
    table.pack()

    


# ---------------------------------------------------------- DONE

def query11():
    clearFrame()
    
    textString = StringVar()
    textString.set("Clientes Registados")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código','Cliente')

    table.column("#0",width=0,stretch=NO)
    table.column("Código",width=400,anchor=CENTER)
    table.column("Cliente",width=400,anchor=CENTER)
   
    table.heading("Código",text="Código",anchor=CENTER)
    table.heading("Cliente",text="Cliente",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query11(Clientes)"))
    print(res[0]['Clientes'][0])
    for answer in (res[0]['Clientes']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

# --------------------------------------------------------- DONE

def query12():
    clearFrame()
    
    textString = StringVar()
    textString.set("Estafetas Registados")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código','Estafeta')
    

    table.column("#0",width=0,stretch=NO)
    table.column("Código",width=400,anchor=CENTER)
    table.column("Estafeta",width=400,anchor=CENTER)
   
    table.heading("Código",text="Código",anchor=CENTER)
    table.heading("Estafeta",text="Estafeta",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query12(Estafetas)"))
    for answer in (res[0]['Estafetas']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

# --------------------------------------------------------- DONE

def query13():
    clearFrame()
    
    textString = StringVar()
    textString.set("Encomendas Entregues")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código Encomenda','Data','Classificação')
    

    table.column("#0",width=0,stretch=NO)
    table.column("Código Encomenda",width=300,anchor=CENTER)
    table.column("Data",width=300,anchor=CENTER)
    table.column("Classificação",width=300,anchor=CENTER)
   
    table.heading("Código Encomenda",text="Código Encomenda",anchor=CENTER)
    table.heading("Data",text="Data",anchor=CENTER)
    table.heading("Classificação",text="Classificação",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query13(Encomendas)"))
    for answer in (res[0]['Encomendas']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0].args[0],str(answer.args[0].args[1].args[0].args[0].args[0]) + "/" + str(answer.args[0].args[1].args[0].args[0].args[1]) + "/" + str(answer.args[0].args[1].args[0].args[1]),answer.args[1]))
        i = i + 1
    
    table.pack()

# --------------------------------------------------------- DONE

def query14():
    clearFrame()
    
    textString = StringVar()
    textString.set("Encomendas Criadas")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código Encomenda','Tempo Max','Código Cliente','Código Estafeta','Peso','Volume','Transporte','Preço Base','Data Criação','Zona de Entrega')
    

    table.column("#0",width=0,stretch=NO)
    table.column("Código Encomenda",width=145,anchor=CENTER)
    table.column("Tempo Max",width=135,anchor=CENTER)
    table.column("Código Cliente",width=135,anchor=CENTER)
    table.column("Código Estafeta",width=135,anchor=CENTER)
    table.column("Peso",width=135,anchor=CENTER)
    table.column("Volume",width=135,anchor=CENTER)
    table.column("Transporte",width=135,anchor=CENTER)
    table.column("Preço Base",width=135,anchor=CENTER)
    table.column("Data Criação",width=135,anchor=CENTER)
    table.column("Zona de Entrega",width=135,anchor=CENTER)
   
    table.heading("Código Encomenda",text="Código Encomenda",anchor=CENTER)
    table.heading("Tempo Max",text="Tempo Max",anchor=CENTER)
    table.heading("Código Cliente",text="Código Cliente",anchor=CENTER)
    table.heading("Código Estafeta",text="Código Estafeta",anchor=CENTER)
    table.heading("Peso",text="Peso",anchor=CENTER)
    table.heading("Volume",text="Volume",anchor=CENTER)
    table.heading("Transporte",text="Transporte",anchor=CENTER)
    table.heading("Preço Base",text="Preço Base",anchor=CENTER)
    table.heading("Data Criação",text="Data Criação",anchor=CENTER)
    table.heading("Zona de Entrega",text="Zona de Entrega",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query14(Encomendas)"))
    for answer in (res[0]['Encomendas']):
        arg1 = answer.args[0].args[0].args[0].args[0].args[0].args[0].args[0].args[0].args[0]
        arg2 = answer.args[0].args[0].args[0].args[0].args[0].args[0].args[0].args[0].args[1]
        arg3 = answer.args[0].args[0].args[0].args[0].args[0].args[0].args[0].args[1]
        arg3 = answer.args[0].args[0].args[0].args[0].args[0].args[0].args[0].args[1]
        arg4 = answer.args[0].args[0].args[0].args[0].args[0].args[0].args[1]
        arg5 = answer.args[0].args[0].args[0].args[0].args[0].args[1]
        arg5 = answer.args[0].args[0].args[0].args[0].args[0].args[1]
        arg6 = answer.args[0].args[0].args[0].args[0].args[1]
        arg7 = answer.args[0].args[0].args[0].args[1]
        arg8 = answer.args[0].args[0].args[1]
        arg91 = answer.args[0].args[1].args[0].args[0].args[0]
        arg92 = answer.args[0].args[1].args[0].args[0].args[1]
        arg93 = answer.args[0].args[1].args[0].args[1]
        arg94 = answer.args[0].args[1].args[1]
        arg10 = answer.args[1]
        table.insert(parent='',index='end',iid=i,values=(str(arg1),str(arg2),str(arg3),str(arg4),str(arg5),str(arg6),arg7,str(arg8),str(arg91) + "/" + str(arg92) + "/" + str(arg93) + " às " + str(arg94) + " h", arg10))
        i = i + 1
    
    table.pack()


# ---------------------------------------------- DONE

def insertCliente():
    clearFrame()
    
    text = Label(frame ,text = "Código Cliente")
    codInsert = Entry(frame,textvariable=codClient)
    text2 = Label(frame ,text = "Nome Cliente")
    nameInsert = Entry(frame,textvariable=nameClient)

    btn = ttk.Button(frame,text="Adicionar",command=insertClienteAux)
    text.pack()
    codInsert.pack()
    text2.pack()
    nameInsert.pack()
    btn.pack()

def insertClienteAux():
    cod = codClient.get()
    name = nameClient.get()

    res = bool(list(prolog.query("evolucao(cliente("+cod+","+name+"))")))
    if res == True:
        prolog.assertz("cliente("+cod+","+name+")")
        messagebox.showinfo("Sucesso","Inserção realizada")
    if res == False:
        messagebox.showwarning("Erro", "Invariante não verificado")
# ---------------------------------------------- DONE

def insertEstafeta():
    clearFrame()
    
    text = Label(frame ,text = "Código Estafeta")
    codInsert = Entry(frame,textvariable=codEstafeta)
    text2 = Label(frame ,text = "Nome Estafeta")
    nameInsert = Entry(frame,textvariable=nameEstafeta)

    btn = ttk.Button(frame,text="Adicionar",command=insertEstafetaAux)
    text.pack()
    codInsert.pack()
    text2.pack()
    nameInsert.pack()
    btn.pack()

def insertEstafetaAux():
    cod = codEstafeta.get()
    name = nameEstafeta.get()

    res = bool(list(prolog.query("evolucao(estafeta("+cod+","+name+"))")))
    print(res)
    if res == True:
        prolog.assertz("estafeta("+cod+","+name+")")
        messagebox.showinfo("Sucesso","Inserção realizada")
    if res == False:
        messagebox.showwarning("Erro", "Invariante não verificado")

# ------------------------------------------------ DONE

def insertEncomenda():
    clearFrame()
    
    text = Label(frame ,text = "Código encomenda")
    textInsert = Entry(frame,textvariable=codEncomenda)
    
    text2 = Label(frame ,text = "Tempo máximo de entrega")
    textInsert2 = Entry(frame,textvariable=tempMax)
    
    text3 = Label(frame ,text = "Código Cliente")
    textInsert3 = Entry(frame,textvariable=codClient)
    
    text4 = Label(frame ,text = "Código estafeta")
    textInsert4 = Entry(frame,textvariable=codEstafeta)
    
    text5 = Label(frame ,text = "Peso")
    textInsert5 = Entry(frame,textvariable=peso)
    
    text6 = Label(frame ,text = "Volume")
    textInsert6 = Entry(frame,textvariable=volume)
    
    text7 = Label(frame ,text = "Transporte")
    textInsert7 = Entry(frame,textvariable=transporte)
    
    text8 = Label(frame ,text = "Preço base")
    textInsert8 = Entry(frame,textvariable=precoBase)
    
    text9 = Label(frame ,text = "Data")
    textInsert9 = Entry(frame,textvariable=dataInicial)
    
    text10 = Label(frame ,text = "Zona de entrega")
    textInsert10 = Entry(frame,textvariable=zonaEntrega)
    
    btn = ttk.Button(frame,text="Adicionar",command=insertEncomendaAux)
    
    text.pack()
    textInsert.pack()
    text2.pack()
    textInsert2.pack()
    text3.pack()
    textInsert3.pack()
    text4.pack()
    textInsert4.pack()
    text5.pack()
    textInsert5.pack()
    text6.pack()
    textInsert6.pack()
    text7.pack()
    textInsert7.pack()
    text8.pack()
    textInsert8.pack()
    text9.pack()
    textInsert9.pack()
    text10.pack()
    textInsert10.pack()
    btn.pack()

def insertEncomendaAux():

    res = bool(list(prolog.query("evolucao(encomenda("+codEncomenda.get()+","+tempMax.get()+","+codClient.get()+","+codEstafeta.get()+","+peso.get()+","+volume.get()+","+transporte.get()+","+precoBase.get()+","+dataInicial.get()+","+zonaEntrega.get()+"))")))
    
    if res == True:
        prolog.assertz("encomenda("+codEncomenda.get()+","+tempMax.get()+","+codClient.get()+","+codEstafeta.get()+","+peso.get()+","+volume.get()+","+transporte.get()+","+precoBase.get()+","+dataInicial.get()+","+zonaEntrega.get()+")")
        messagebox.showinfo("Sucesso","Inserção realizada")
    if res == False:
        messagebox.showwarning("Erro", "Invariante não verificado")

# ---------------------------------------------------------------- DONE
def insertEncomendaEntregue():
    clearFrame()
    
    text = Label(frame ,text = "Código encomenda")
    textInsert = Entry(frame,textvariable=codEncomenda)
    
    text2 = Label(frame ,text = "Data de entrega")
    textInsert2 = Entry(frame,textvariable=dataFinal)
    
    text3 = Label(frame ,text = "Classificação")
    textInsert3 = Entry(frame,textvariable=classif)
     
    btn = ttk.Button(frame,text="Adicionar",command=insertEncomendaEntregueAux)
    
    text.pack()
    textInsert.pack()
    text2.pack()
    textInsert2.pack()
    text3.pack()
    textInsert3.pack()
    btn.pack()

def insertEncomendaEntregueAux():

    res = bool(list(prolog.query("evolucao(encomenda("+codEncomenda.get()+","+dataFinal.get()+","+classif.get()+"))")))
    
    if res == True:
        prolog.assertz("encomenda("+codEncomenda.get()+","+dataFinal.get()+","+classif.get()+")")
        messagebox.showinfo("Sucesso","Inserção realizada")
    if res == False:
        messagebox.showwarning("Erro", "Invariante não verificado")

# ----------------------------------------------------------------------- DONE

def insertPenalizacao():
    clearFrame()
    
    text = Label(frame ,text = "Código Estafeta")
    textInsert = Entry(frame,textvariable=codEstafeta)
    
    text2 = Label(frame ,text = "Data de inicio")
    textInsert2 = Entry(frame,textvariable=dataInicial)
    
    text3 = Label(frame ,text = "Data de fim")
    textInsert3 = Entry(frame,textvariable=dataFinal)
     
    btn = ttk.Button(frame,text="Adicionar",command=insertPenalizacaoEntregueAux)
    
    text.pack()
    textInsert.pack()
    text2.pack()
    textInsert2.pack()
    text3.pack()
    textInsert3.pack()
    btn.pack()

def insertPenalizacaoEntregueAux():

    res = bool(list(prolog.query("evolucao(penalizado("+codEstafeta.get()+","+dataInicial.get()+","+dataFinal.get()+"))")))
    
    if res == True:
        prolog.assertz("penalizado("+codEstafeta.get()+","+dataInicial.get()+","+dataFinal.get()+")")
        messagebox.showinfo("Sucesso","Inserção realizada")
    if res == False:
        messagebox.showwarning("Erro", "Invariante não verificado")

def consult():
    filename = filedialog.askopenfilename()
    with open(filename, "r") as ins:
        i = 0
        for line in ins:
            l = line[:-1] 
            l2 = l[:-1]
            if (l2[-1] != ')'):
                    l2 = l2 + ')'
            res = bool(list(prolog.query("evolucao("+l2+")")))
            if res == True:
                prolog.assertz(l2)
                print("Deu: " + l2)
            if res == False:
                print(str(i) + " - Não Deu: " + l2)
                i = i + 1


# ----------------------------------------------------------------------- DONE

def query1():
    clearFrame()

    textString = StringVar()
    textString.set("Estafetas que mais usaram o transporte mais ecológico")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame,height=30)
    table['columns'] = ('Código','Estafeta')

    table.column("#0",width=0,stretch=NO)
    table.column("Código",width=400,anchor=CENTER)
    table.column("Estafeta",width=400,anchor=CENTER)
   
    table.heading("Código",text="Código",anchor=CENTER)
    table.heading("Estafeta",text="Estafeta",anchor=CENTER)
   
    i = 0
    res = list(prolog.query("query1(X)"))
    print(res[0]['X'][0].args[1])
    for answer in (res[0]['X']):
        table.insert(parent='',index='end',iid=i,values=(answer.args[0],answer.args[1]))
        i = i + 1
    
    table.pack()

menuBar = Menu(app)
menuQueries = Menu(menuBar)
menuFiles = Menu(menuBar)
menuInsert = Menu(menuBar)
menuCircuitos = Menu(menuBar)

menuQueries.add_command(label="Estafeta que utilizou mais vezes um meio de transporte mais ecológico",command=query1)
menuQueries.add_command(label="Estafetas que entregaram determinadas encomendas a um cliente",command=query2)
menuQueries.add_command(label="Clientes servidos por um estafeta",command=query3)
menuQueries.add_command(label="Valor faturado pela Green Distribution num dia",command=query4)
menuQueries.add_command(label="Zonas com maior volume de entregas",command=query5)
menuQueries.add_command(label="Classificação média de statisfação de cliente para um estafeta",command=query6)
menuQueries.add_command(label="Número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo",command=query7)
menuQueries.add_command(label="Número de entregas pelos estafetas, num determinado intervalo de tempo",command=query8)
menuQueries.add_command(label="Número de entregas entregues e não entregues, num determinado intervalo de tempo",command=query9)
menuQueries.add_command(label="Peso total transportado por estafeta num determinado dia",command=query10)
menuQueries.add_separator()

menuQueries.add_command(label="Clientes (EXTRA)",command=query11)
menuQueries.add_command(label="Estafetas (EXTRA)",command=query12)
menuQueries.add_command(label="Encomendas Criadas (EXTRA)",command=query14)
menuQueries.add_command(label="Encomendas Entregues (EXTRA)",command=query13)

menuFiles.add_command(label="Carregar ficheiro",command=consult)

menuCircuitos.add_command(label="Gerar circuitos usando DFS",command=lambda: graph.gerarDFS(frame))
menuCircuitos.add_command(label="Gerar circuitos usando BFS",command=lambda: graph.gerarBFS(frame))
menuCircuitos.add_command(label="Gerar circuitos usando DFS Iterativa",command=lambda: graph.gerarDFSI(frame))
menuCircuitos.add_command(label="Gerar circuitos usando Gulosa - distância",command=lambda: graph.gerarGulosaD(frame))
menuCircuitos.add_command(label="Gerar circuitos usando Gulosa - trânsito",command=lambda: graph.gerarGulosaT(frame))
menuCircuitos.add_command(label="Gerar circuitos usando AEstrela - distância",command=lambda: graph.gerarAEstrelaD(frame))
menuCircuitos.add_command(label="Gerar circuitos usando AEstrela - trânsito",command=lambda: graph.gerarAEstrelaT(frame))

menuInsert.add_command(label="Cliente (EXTRA)",command=insertCliente)
menuInsert.add_command(label="Registar nova encomenda (EXTRA) ",command=insertEncomenda)
menuInsert.add_command(label="Registar encomenda como entregue (EXTRA) ",command=insertEncomendaEntregue)
menuInsert.add_command(label="Estafeta (EXTRA) ",command=insertEstafeta)
menuInsert.add_command(label="Penalização (EXTRA) ",command=insertPenalizacao)

menuBar.add_cascade(label="Ficheiro",menu = menuFiles)
menuBar.add_cascade(label="Consultar",menu = menuQueries)
menuBar.add_cascade(label="Inserir",menu = menuInsert)
menuBar.add_cascade(label="Cicuitos",menu = menuCircuitos)

app.config(menu=menuBar)

app.mainloop()

