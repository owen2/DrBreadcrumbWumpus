# pylogic.py

""" Module for communicating with a Prolog interpreter.
by: John Zelle.
"""

import shlex

PROLOG_COMMAND='/usr/bin/env swipl -G128m -f pylogic.pl -t pylogic:server -g pylogic:server 2> /dev/null'

#import os
from subprocess import Popen, PIPE

class KB:
    """ A KB object provides an interface to a Prolog logic engine. It can be passed
    the name of Prolog file to load on startup."""

    def __init__(self, ruleFile=None):
        p = Popen(shlex.split(PROLOG_COMMAND), shell=False,
                  stdin=PIPE, stdout=PIPE, close_fds=True)
        self.write, self.read = p.stdin, p.stdout
        self.childPID = p.pid
        #write,read = os.popen2(PROLOG_COMMAND)
        self._expect("ready")
        if ruleFile:
            self._send("[%s]." % (ruleFile))
            status, data = self._getReply()
            if status == 'error':
                raise Exception, data
            self._send("ok.")
            self._expect("ready")

    def tell(self, rule):
        """ Asserts rule in the Prolog database """
        
        rule = rule.strip()
        self.reset()
        self._send("assert(%s)." %(rule))
        status, data = self._getReply()
        if status != "yes":
            raise Exception, "Error telling %s: %s" %(rule,data)
        self._send("ok.")
        self._getReply()
        
    def ask(self, query):
        """Sends query to Prolog. Returns a (possibly empty) dictionary of bindings if
        the query succeeds. Returns None if the query fails."""
        
        self.reset()
        query=query.strip()
        if query[-1] != ".":
            query = query+"."
        self._send(query)
        status, bindings = self._getReply()
        if status == 'error':
            raise Exception, "KB Error: %s" % bindings
        if self.querymode:
            return None
        else:
            return self._toDictionary(bindings)

    def redo(self):
        """Asks for another solution to the last query."""
        
        if self.querymode:
            return None
        else:
            self._send('next.')
            status, bindings = self._getReply()
            if status == 'no':
                return None
            else:
                return self._toDictionary(bindings)

    def solutions(self, query):
        """Returns an iterator that will generate successive solutions to query"""
        
        ans = self.ask(query)
        while ans != None:
            yield ans
            ans = self.redo()
        
    def reset(self):
        """Put the Prolog process back into query accepting mode."""
        
        if not self.querymode:
            self._send("ok.")
            self._expect("ready")
        self.bindings=None

    def close(self):
        """Shutdown the Prolog process."""        
        self.reset()
        self._send("halt.")

    def _getReply(self):
        response = self.read.readline()
        answer = response.split(":")
        head, data = answer[0], ":".join(answer[1:])
        self.querymode = (head != "yes")
        return head.strip(), data.strip()

    def _expect(self, string):
        status,data = self._getReply()
        if status != string:
            raise Exception, "KB ERROR, expecting: "+string
        return status,data

    def _send(self, msg):
        self.write.write(msg+"\n")
        self.write.flush()

    def _toDictionary(self, bindings):
        blist = eval(bindings)
        ans = {}
        for pair in blist:
            var, value = pair.split("=")
            ans[var] = value
        return ans
        
    def __del__(self):
        try:
            self.close()
        except:
            pass
