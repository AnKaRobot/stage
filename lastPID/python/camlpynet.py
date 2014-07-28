import sys
import os
import socket

class Py2camlClient :
    def __init__ (self, socketfile) :
        self.socketfile = socketfile
        self.socketClient = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socketClient.connect(self.socketfile)
        self.socketClient.settimeout(1.0)
        print >> sys.stderr, "[Py2camlClient] : connected to \"%s\"" % self.socketfile
                
    def sendData (self, message) :
        messageFmt = str(message) + "\n"
        self.socketClient.sendall(messageFmt)
        print >> sys.stderr, "[Py2camlClient] (%s) : sent \"%s\n\"" % (self.socketfile, messageFmt.replace("\n", "\\n"))
        
    def close (self) :
        self.socketClient.shutdown(socket.SHUT_RDWR)
        self.socketClient.close()
        print >> sys.stderr, "[Py2camlClient] (%s) : closed client connection" % self.socketfile

class Caml2pyServer :
    def __init__ (self, socketfile) :
        self.socketfile = socketfile
        
        try : os.unlink(self.socketfile);
        except OSError :
            if os.path.exists(self.socketfile) : raise
        
        self.socketServer = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socketServer.bind(self.socketfile)
        self.socketServer.listen(1)
        print >> sys.stderr, "[Caml2pyServer] : listen on %s" % self.socketfile
                
    def waitClient (self) :
        self.socketClient, self.clientAddress = None, None
        print >> sys.stderr, "[Caml2pyServer] (%s) : waiting for a connection.." % self.socketfile
        self.socketClient, self.clientAddress = self.socketServer.accept()
        self.socketClient.settimeout(1.0)
        print >> sys.stderr, "[Caml2pyServer] (%s) : connection from \"%s\"" % (self.socketfile, self.clientAddress)

    def getData (self) :
        data = self.socketClient.recv(16)
        print >> sys.stderr, "[Caml2pyServer] (%s) : received \"%s\"" % (self.socketfile, data.replace("\n", "\\n"))
        return data

    def closeServer (self) :
        self.socketServer.shutdown(socket.SHUT_RDWR)
        self.socketServer.close()
        print >> sys.stderr, "[Caml2pyServer] (%s) : closed server" % self.socketfile
        
    def closeClient (self) :
        self.socketClient.shutdown(socket.SHUT_RDWR)
        self.socketClient.close()
        print >> sys.stderr, "[Caml2pyServer] (%s) : closed connection from \"%s\"" %( self.socketfile, self.clientAddress)
