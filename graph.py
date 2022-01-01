from pyswip import Prolog
from tkinter import ttk
from tkinter import *
import os

prolog = Prolog()
prolog.consult("graph_queries.pl")

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
