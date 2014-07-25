#!/usr/bin/env python

import socket
import sys
import random
import time

socketClient = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
serverAddress = "/tmp/guessServer"
print >> sys.stderr, "connecting to %s" % serverAddress

try :
    socketClient.connect(serverAddress)
except socket.error, msg :
    print >> sys.stderr, "%s" % msg
    sys.exit(1)
        
socketClient.sendall("hi\n")

i = 10
while i :
    try :
        message = str(random.randint(0, 100))  # TODO : speed calcul
        print >> sys.stderr, "sending %s" %  message
        socketClient.sendall(message + "\n")
    finally :
        time.sleep(1.)    
        i = i + 1

socketClient.close()    
