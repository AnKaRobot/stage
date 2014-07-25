#!/usr/bin/env python

import socket
import sys
import os
import time
import matplotlib.pyplot as plt
import turtle as tur
import Tkinter as tk
import threading as thr
import random

class SharedSpeedOrder : 
    def __init__ (self, initVal = 0.) :
        self.v = initVal / 20. # from pixels by seconds to px/0.05s
    def setSpeed (self, speed) :
        self.v = float(speed) / 20.
        
def speedExecutor () :

    speedOrder = SharedSpeedOrder()

    speedServer = thr.Thread(target = speedServerThread, 
                                   args = (speedOrder,) )

    master = tk.Tk()
    canv = tk.Canvas(master, width = 500, height = 300)
    canv.pack()
    
    turtleScreen = tur.TurtleScreen(canv)
    turtle = tur.RawTurtle(turtleScreen)
    turtleMove = thr.Thread(target = turtleMoveThread,
                                  args = (speedOrder, turtle) )

    
    turtleTrace = tur.RawTurtle(turtleScreen)


    pentes = [random.randint(-10,10) for i in range(25)]
    altitudes = [sum(pentes[:i]) for i in range(len(pentes))]

    for i,j in enumerate(altitudes):
        turtleTrace.goto(i*10,j)

    
    while

    speedServer.start()
    turtleMove.start()

    tk.mainloop()
    

def turtleMoveThread (angle, turtle) :
    
    main = None
    for t in thr.enumerate() :
        if t.name == "MainThread" : main = t;
    
    while main.isAlive() :
        turtle.right(angle.v)
        turtle.fd(10)
        time.sleep(1.0) # 20 times by second

def validate (value):

  try:
    return int(value), True
  except ValueError:
    return 0, False



def speedServerThread (speedOrder) :

    main = None
    for t in thr.enumerate() :
        if t.name == "MainThread" : main = t;

    serverAddress = "/tmp/speedOrderer"
    
    try :
        os.unlink(serverAddress)
    except OSError :
        if os.path.exists(serverAddress) : raise

    socketServer = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    socketServer.bind(serverAddress)
    socketServer.listen(1)
    socketServer.settimeout(3.0)
    print >> sys.stderr, "starting up on %s" % serverAddress

    while main.isAlive() :
    
        socketClient = None
        
        try :
            print >> sys.stderr, "waiting for a connection.."
            socketClient, clientAddress = socketServer.accept() # waiting
            socketClient.settimeout(3.0)            
            print >> sys.stderr, "connection from %s" % clientAddress
            
            while main.isAlive() :
                try :
                    data = socketClient.recv(16)
                    print >> sys.stderr, "received %s" % data

                    if not data :
                        print >> sys.stderr, "no more data from %s" % clientAddress
                        break

                    speed, isValid = validate(data)
                    if isValid : 
                        speedOrder.setSpeed(speed)
                        print >> sys.stderr, "speed <- %s" % speed

                except socket.timeout : pass;
                
        except socket.timeout : pass;
        finally :
            try : socketClient.close();
            except Exception : pass;

    socketServer.close()

if __name__ == '__main__' :
    speedExecutor()
