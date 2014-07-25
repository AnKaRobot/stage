import pylab as plb
import Tkinter as tk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from collections import deque

mainWin = tk.Tk()
mainWin.wm_title("Realtime Plot in Tkinter")

class Values :
    def __init__ (self) :
        self.xVals = deque([0])
        self.yVals = deque([0])
        self.n = 0

v = Values()

fig1 = plb.figure(1)
ax = fig1.add_subplot(1, 1, 1)
ax.grid(True)
ax.set_title("title")
ax.set_xlabel("x label")
ax.set_ylabel("y label")
ax.axis([0, 100,
         0, 120] )
line1 = ax.plot(v.xVals, v.yVals)

canvas = FigureCanvasTkAgg(fig1, master = mainWin)
canvas.show()
canvas.get_tk_widget().pack(side = tk.TOP, fill = tk.BOTH, expand = 1)
canvas._tkcanvas.pack(side = tk.TOP, fill = tk.BOTH, expand = 1)

def draw (v) :
    line1[0].set_data(v.xVals, v.yVals)
    canvas.draw()
    mainWin.after(25, draw, v)

def evolve (v) :
    v.yVals.appendleft(plb.random() * 100)
    v.xVals.appendleft(v.n)
    v.n += 1
    if  v.n < 100 :
        mainWin.after(25, evolve, v)

def _quit () :
    mainWin.quit()
    mainWin.destroy()

buttonQuit = tk.Button(master = mainWin, text = "Quit", command = _quit)
buttonQuit.pack(side = tk.BOTTOM)

mainWin.protocol("WM_DELETE_WINDOW", _quit)

mainWin.after(100, evolve, v)
mainWin.after(100, draw, v)
tk.mainloop()
