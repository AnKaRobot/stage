import sys
import camlpynet as cpn
import threading as th
import pylab as plb
import Tkinter   as tk
from time        import sleep
from collections import deque
from socket      import timeout
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg


# ------------------------------ #

def pidControlLoop (looping, getData, v) :
    while looping() :
        try :
            data = getData()
            try :
                control = float(data)
                v.lastControl = control
                #print >> sys.stderr, "control get : %f" % control
            except ValueError : 
                print >> sys.stderr, "control strange data : %s" % data
        except timeout :
            print >> sys.stderr, "*** Control TimeOut ***"

def pidMesureLoop (looping, sendData, v) :
    while looping() :
        data = str(v.lastMesure)
        try : 
            sendData(data)
        except timeout : 
            print sys.stderr, "*** Mesure TimeOut ***" 
        sleep(0.1)
            
def pidReferenceLoop (looping, sendData, v) :
    while looping() :
        data = str(v.lastReference)
        try :
            sendData(data)
        except timeout :
            print >> sys.stderr, "*** Reference TimeOut ***"
        sleep(0.1)

def update (v, scales) :
    v.lastReference = scales.reference.get()
    v.minAction = scales.minAction.get()
    v.maxAction = scales.maxAction.get()

def _quit (mainWin) :
    mainWin.quit()
    mainWin.destroy()

def draw (v, g) :
    g.figure.canvas.restore_region(g.background)
    g.mesure[0].set_data(v.time, v.mesure)
    g.control[0].set_data(v.time, v.control)
    g.reference[0].set_data(v.time, v.reference)
    g.axe.set_xlim([v.n - 30, v.n -1])
    g.axe.autoscale_view()
    g.axe.draw_artist(g.mesure[0])
    g.axe.draw_artist(g.control[0])
    g.axe.draw_artist(g.reference[0])
    g.figure.canvas.blit(g.axe.bbox)

def evolve (v, mainWin, draw, g) :
        
    if v.n > 30 :
        v.mesure.popleft()
        v.reference.popleft()
        v.control.popleft()
        v.time.popleft()

    control = v.lastControl
    if control > v.maxAction : control = v.maxAction;
    elif control < v.minAction : control = v.minAction;

    mesure = v.lastMesure + control
    if mesure < 0. : mesure = 0. ;

    v.reference.append(v.lastReference)
    
    v.mesure.append(mesure)
    v.lastMesure = mesure

    v.control.append(v.lastControl)
    #v.lastControl = 0.

    v.time.append(v.n)
    v.n += 1

    draw (v, g)
    mainWin.after(100, evolve, v, mainWin, draw, g)

class Values :
    def __init__ (self) :
        self.time = deque([0.])
        self.control = deque([0.])
        self.mesure  = deque([0.])
        self.reference  = deque([100.])
        self.n    = 0
        self.lastControl = 0.
        self.lastMesure = 0.
        self.lastReference = 100.
        self.maxAction = 30.
        self.minAction = -30.

class Scales :
    def __init__ (self) :
        self.reference = None
        self.minAction = None
        self.maxAction = None

class Graph :
    def __init__ (self) :
        self.figure = None
        self.axe = None
        self.mesure = None
        self.reference = None
        self.control = None
        self.background = None

# ----------------------------------- #

def pidscine () :

    # Graph
    v = Values()
    g = Graph()

    g.figure = plb.figure(1)
    g.axe = g.figure.add_subplot(1, 1, 1)
    g.axe.grid(True)
    g.axe.set_title("title")
    g.axe.set_xlabel("time")
    g.axe.set_ylabel("mesure reference control")
    g.axe.axis([-29, 0,
                     -100., 200.] )
    g.axe.set_autoscalex_on(True)
    
    g.mesure = g.axe.plot(v.mesure)
    g.control = g.axe.plot(v.control)
    g.reference = g.axe.plot(v.reference)

    g.figure.canvas.draw()

    # Tk canvas
    mainWin = tk.Tk()
    scales = Scales()

    canvas = FigureCanvasTkAgg(g.figure, master = mainWin)
    canvas.show()
    canvas.get_tk_widget().pack(side = tk.TOP, fill = tk.BOTH, expand = 1)
    canvas._tkcanvas.pack(side = tk.TOP, fill = tk.BOTH, expand = 1)

    g.background = g.figure.canvas.copy_from_bbox(g.axe.bbox)

    # Tk Toolbar
    scales.reference = tk.Scale(master = mainWin, label = "Reference", 
                                from_ = 0., to = 100., 
                                orient = tk.HORIZONTAL)
    scales.reference.set(100.)
    scales.reference.pack(side = tk.LEFT)

    scales.minAction = tk.Scale(master = mainWin, label = "min Action", 
                                from_ = 0., to = -100., 
                                orient = tk.HORIZONTAL)
    scales.minAction.set(v.minAction)
    scales.minAction.pack(side = tk.LEFT)

    scales.maxAction = tk.Scale(master = mainWin, label = "max Action", 
                                from_ = 0., to = 100., 
                                orient = tk.HORIZONTAL)
    scales.maxAction.set(v.maxAction)
    scales.maxAction.pack(side = tk.LEFT)

    buttonUpdate = tk.Button(master = mainWin, text = "Apply", 
                             command = lambda:(update(v, scales)) )
    buttonUpdate.pack(side = tk.LEFT)

    buttonQuit = tk.Button(master = mainWin, text = "Quit", 
                           command =lambda:(_quit(mainWin)))
    buttonQuit.pack(side = tk.LEFT)

    mainWin.protocol("WM_DELETE_WINDOW", lambda:(_quit(mainWin)))

    
    # PID control Server
    try : pidControlServer = cpn.Caml2pyServer("/tmp/pidscine/control", False)
    except timeout as err : 
        raise Exception("Impossible creation of server"), err
    
    try : pidControlServer.waitClient()
    except timeout as err : 
        raise Exception("No client"), err

    # PID mesure client
    try : pidMesureClient = cpn.Py2camlClient("/tmp/pidscine/mesure", False)
    except timeout as err :
        raise Exception("Impossible creation of client"), err

    # PID reference client
    try : pidReferenceClient = cpn.Py2camlClient("/tmp/pidscine/reference", False)
    except timeout3 as err :
        raise Exception("Impossible creation of client"), err

    # Launch
    mainThread = None
    for t in th.enumerate() :
        if t.name == "MainThread" : mainThread = t;    

    pidControlThread = th.Thread(
        target = pidControlLoop,
        args = (mainThread.isAlive, pidControlServer.getData, v) )

    pidMesureThread = th.Thread(
        target = pidMesureLoop,
        args = (mainThread.isAlive, pidMesureClient.sendData, v) )
    
    pidReferenceThread = th.Thread(
        target = pidReferenceLoop,
        args = (mainThread.isAlive, pidReferenceClient.sendData, v) )

    mainWin.after(10, evolve, v, mainWin, draw, g)
    pidControlThread.start()
    pidMesureThread.start()
    pidReferenceThread.start()
    
    mainWin.mainloop()

    try :
        pidControlServer.closeClient()
        pidControlServer.closeServer()
        pidMesureClient.close()
        pidReferenceClient.close()
    except : raise # TODO
    finally : 
        print >> sys.stderr, "End Pidscine" 
        exit()

# end testTurtle


if __name__ == "__main__" :
    pidscine()
