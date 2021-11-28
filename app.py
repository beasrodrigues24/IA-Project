from tkinter import *
import os
from pyswip import Prolog
from tkinter import ttk
from pyswip import Variable,Query

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


# GLOBAL VARIABLES
codClient = StringVar()
codEstafeta = StringVar()
nameClient = StringVar()

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
    table = ttk.Treeview(frame)
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
    table = ttk.Treeview(frame)
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

# ------------------------------------------------------ TODO

def query4():
    print("TODO")

def query5():
    print("TODO")

def query6():
    print("TODO")

def query7():
    print("TODO")

def query8():
    print("TODO")

def query9():
    print("TODO")

def query10():
    print("TODO")

# ---------------------------------------------------------- DONE

def query11():
    clearFrame()
    
    textString = StringVar()
    textString.set("Clientes Registados")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame)
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
    table = ttk.Treeview(frame)
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
    print(res)

def insertEstafeta():
    print("TODO")

def insertEncomenda():
    print("TODO")

def insertPenalizacao():
    print("TODO")

def consult():
    print("TODO")


# ----------------------------------------------------------------------- DONE

def query1():
    clearFrame()

    textString = StringVar()
    textString.set("Estafetas que mais usaram o transporte mais ecológico")
    text = Label(frame,textvariable=textString)
    text.pack()
    table = ttk.Treeview(frame)
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

menuFiles.add_command(label="Carregar ficheiro",command=consult)

menuInsert.add_command(label="Cliente",command=insertCliente)
menuInsert.add_command(label="Encomenda",command=insertEncomenda)
menuInsert.add_command(label="Estafeta",command=insertEstafeta)
menuInsert.add_command(label="Penalização",command=insertPenalizacao)

menuBar.add_cascade(label="Ficheiro",menu = menuFiles)
menuBar.add_cascade(label="Consultar",menu = menuQueries)
menuBar.add_cascade(label="Inserir",menu = menuInsert)

app.config(menu=menuBar)

app.mainloop()

