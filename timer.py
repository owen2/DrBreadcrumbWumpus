#!/usr/bin/env python

#timer.py

from Tkinter import *
from sys import argv, stdin, exit
import select, math

FORMAT = "Seconds Left: %%%dd"


class Timer(Frame):
    
    def __init__(self, master, seconds):
        Frame.__init__(self, master=master)
        self.pack()
        self.amount = seconds
        self.format = FORMAT % len(str(seconds))
        self.display = Label(text=(self.format % seconds),
                             font=('courier',32,'bold'))
        self.display.pack()
        self.seconds = seconds
        self.running = False
        self.run()

    def run(self):
        while True:
            ready = select.select([0],[],[],1)
            if ready[0]:
                mess = stdin.readline()
                if mess == 'start\n':
                    self.running = True
                elif mess == 'stop\n':
                    self.running = False
                elif mess == 'exit\n':
                    exit(0)
                else:
                    try:
                        elapsed = float(mess)
                        self.seconds = self.amount - elapsed
                    except:
                        pass
            else:
                if self.running:
                    self.seconds = self.seconds - 1
            self.display.configure(text=self.format % int(math.ceil(self.seconds)))
            self.update()

app = Tk()
try:
    app.title(argv[2])
except:
    app.title("Timer")
timer = Timer(app,int(argv[1]))
app.mainloop()

        
