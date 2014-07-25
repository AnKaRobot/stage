import camlpynet as cpn
import threading as th
import sys
from socket import timeout
import turtle as tu
import Tkinter as tk
import random as rd
import time

def testTurtle () :

    # PID order Server
    try : pidServer = cpn.Caml2pyServer("/tmp/turtle/caml2py/pidOrder")
    except timeout as err : 
        raise Exception("Impossible creation of server"), err
    
    try : pidServer.waitClient()
    except timeout as err : 
        raise Exception("No client"), err

    # PID mesure client
    try : pidClient = cpn.Py2camlClient("/tmp/turtle/py2caml/pidMesure")
    except timeout as err :
        raise Exception("Impossible creation of client"), err

    # TK canvas
    mainWindow = tk.Tk()
    canvas = tk.Canvas(mainWindow, width = 500, height = 300)
    canvas.pack()
    mainWindowThread = th.Thread(target = tk.mainloop)

    # Turtle
    turtleScreen = tu.TurtleScreen(canvas)
    turtleScreen.setworldcoordinates(0, -150,
                                     500, 150)
    turtle1 = tu.RawTurtle(turtleScreen)
    turtle2 = tu.RawTurtle(turtleScreen)

    # Turtle1 moves
    pentes = [ rd.randint(-15, 15) for i in range(49) ]
    altitudes = [0]
    sumPentes = 0
    for i in range(49) :
        sumPentes = (sumPentes + pentes[i])
        altitudes.append(sumPentes)

    for i in range(50) :
        turtle1.goto(i * 10, altitudes[i])

    # Turtle2 moves
    for i in range(500) :
        try :
            nextY = altitudes[i / 10]
            pidClient.sendData(nextY)
            data = pidServer.getData()
            turtle2.goto(i, int(data))
        except timeout as err :
            print >> sys.stderr, "No data"
            pass
        except ValueError :
            print >> sys.stderr, "value error : \"%s\"" % data
            turtle2.goto(i, 0)
        #time.sleep(1.0)

    try :
        pidServer.closeClient()
        pidServer.closeServer()
    except : raise # TODO

    mainWindowThread.join()

# end testTurtle


if __name__ == "__main__" :
    testTurtle()
