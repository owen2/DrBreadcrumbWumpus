# wumpusenv.py
#    Simple graphical wumpus env.

# By: John Zelle
#    See testing code at bottom for how you can test your own agents.

# Color Code:
#    Wumpus: Red if alive, disappears when shot
#    Gold:   Gold, disappears when grabbed.
#    Pit:    Black
#    Agent:  Green if alive, turns red or black when "finished"
#    Note: If wumpus and gold are in the same location, it will be orange

from graphics import *
from random import random, randrange, choice
from subprocess import Popen, PIPE
import time, signal, os, traceback

# percepts: stench, breeze, glitter, bump, scream
# actions: turn(left), turn(right), forward, grab, shoot, climb
# sim status: exploring, out, chomped, falling, exhausted

DIR_XY = { 'N':(0,1),
           'E' :(1,0),
           'S':(0,-1),
           'W' :(-1,0)
         }

DIR_LEFT = { 'N':'W', 'E':'N', "S":"E", "W":"S"}
DIR_RIGHT = { 'W':'N', 'N':'E', 'E':'S', 'S':'W'}

TIME_LIMIT = 120
PAUSE_LENGTH = 0.15

class TimeOutException(Exception): pass

def raiseTimeout(s, e):
    raise TimeOutException, "Time limit exceeded."

class WumpusEnv:

    def __init__(self, x, y, gold, wumpus, pits, time_limit=TIME_LIMIT):
        #write,read = os.popen2("python timer.py %s 'Wumpus Time'" % (str(TIME_LIMIT)))
        p = Popen("python timer.py %s  Wumpus_Time" % str(time_limit),
                  shell=True, stdin=PIPE, stdout=PIPE, close_fds=True)
        (write, read) = (p.stdin, p.stdout)
        self.save = read
        self.timer = write
        self.xsize = x
        self.ysize = y
        self.pits = pits
        self.gold = gold
        self.wumpus = wumpus
        self.caveWin = CaveWin(self)
        self.reset()
#        self.statusWin = WumpusStatusWin(self)
        self.time_limit = time_limit

    def __repr__(self):
        return "WumpusEnv(%d,%d,%s,%s,%s,%d)" % (self.xsize, self.ysize, str(self.gold),
                                                 str(self.wumpus), str(self.pits), self.time_limit)



    def reset(self):
        self.agtLoc = (1,1)
        self.agtDir = 'N'
        self.haveArrow = True
        self.haveGold = False
        self.wumpusAlive = True
        self.status = 'exploring'
        self.bumped = False
        self.killedWumpus = False
        self.timer.write("0\n")
        self.timer.write("stop\n")
        self.timer.flush()

        self.caveWin.update()

    def pause(self):
        self.caveWin.pause()

    def run(self, agent):
        signal.signal(signal.SIGALRM, raiseTimeout)
        #signal.signal(signal.SIGINT, raiseTimeout)
        elapsed = 0
        steps = 0
        act = "None"
        try:
            while elapsed < self.time_limit and self.status=='exploring':
                if self.xsize * self.ysize < 15*15:time.sleep(PAUSE_LENGTH)
                remaining_time = self.time_limit - elapsed
                t1 = time.time()
                self.timer.write("start\n")
                self.timer.flush()
                signal.alarm(int(round(remaining_time)))
                try:
                    act = agent.action(self.percept())
                except:
                    traceback.print_exc()
                    act = None
                signal.alarm(0)
                self.timer.write("stop\n")
                self.timer.flush()
                t2 = time.time()
                elapsed = elapsed + (t2-t1)
                self.timer.write(str(elapsed)+"\n")
                self.timer.flush()
                steps += 1
                print "Action %d" % steps, act
                self.update(act)
        except TimeOutException:
            elapsed = self.time_limit
            self.status = 'exhausted'
        except:
            elapsed = self.time_limit
            self.status = 'exhausted'
        self.steps = steps
        self.elapsed = elapsed
        self.timer.write("stop\n")
        self.timer.write(str(elapsed)+"\n")
        self.timer.flush()

    def computeScore(self):
        elapsed = self.elapsed
        steps = self.steps
        score = 0
        if self.status == 'out':
            score += 500 
            if self.haveGold:
                score += 1000
        elif self.status == 'exhausted':
            score -= 500
        else:
            score -= 1000
        score -= steps
        if not self.haveArrow:
            score -= 10
        score -= elapsed
        return score

    def percept(self):
        p = []
        if self.agtLoc == self.gold and not self.haveGold:
            p.append('glitter')
        if self.adjacent(self.wumpus, self.agtLoc) or self.agtLoc == self.wumpus:
            p.append('stench')
        for pit in self.pits:
            if self.adjacent(self.agtLoc, pit):
                p.append('breeze')
                break
        if self.bumped:
            p.append('bump')
            self.bumped = False
        if self.killedWumpus:
            p.append('scream')
            self.killedWumpus = False
        return p

    def adjacent(self, (x1,y1), (x2,y2)):
        dx = abs(x1-x2)
        dy = abs(y1-y2)
        return (dx==0 and dy==1) or (dx==1 and dy==0)

    def update(self, act):
        if act == 'turn(left)':
            self.agtDir = DIR_LEFT[self.agtDir]
        elif act == 'turn(right)':
            self.agtDir = DIR_RIGHT[self.agtDir]
        elif act == 'grab':
            if self.gold == self.agtLoc:
                self.haveGold = True
        elif act == 'forward':
            x,y = self.agtLoc
            dx,dy = DIR_XY[self.agtDir]
            x1, y1 = x+dx, y+dy
            if 1 <= x1 <= self.xsize and 1 <= y1 <= self.ysize:
                self.agtLoc = x1,y1
            else:
                self.bumped = True

            if self.agtLoc in self.pits:
                self.status='falling'
            elif self.agtLoc == self.wumpus and self.wumpusAlive:
                self.status = 'chomped'
        elif act == 'shoot':
            if self.haveArrow:
                self.haveArrow = False
                dx,dy = DIR_XY[self.agtDir]
                x,y = self.agtLoc
                while 0<x<=self.xsize and 0<y<=self.ysize:
                    x += dx
                    y += dy
                    self.caveWin.shoot(dx,dy)
                    if (x,y) == self.wumpus:
                        self.killedWumpus = True
                        self.wumpusAlive = False
                        break
        elif act == 'climb':
            if self.agtLoc == (1,1):
                self.status='out'
        self.caveWin.update()
            
    def close(self):
        self.caveWin.win.close()
        self.timer.write("exit\n")
        self.timer.flush()

    def size(self):
        return self.xsize, self.ysize

    def finalReport(self):
        print "\n\nSimulation Results"
        print "-------------------"
        print "Status:", self.status
        print "Thinking time: %0.1f" % self.elapsed
        print "Steps:", self.steps
        print "Possessions:",
        if self.haveArrow: print "Arrow",
        if self.haveGold: print "Gold",
        print
        print "Final Score: %0.1f" % self.computeScore()



class CaveWin:

    def __init__(self, env, size=800, title="Wumpus Cave"):
        xSize = env.xsize
        ySize = env.ysize
        win = self.win = GraphWin(title, size, size*float(ySize)/xSize, autoflush=False)
        win.setCoords(0.49,0.5, xSize+.51, ySize+.51)
        cells = self.cells = {}
        for x in range(1,xSize+1):
            for y in range(1,ySize+1):
                cells[(x,y)] = Rectangle(Point(x-.5,y-.5),Point(x+.5,y+.5))
                cells[(x,y)].draw(win)
        self.agt = None
        self.arrowLoc = None
        self.env = env
        self.arrow = None
        self.agtPoints = {'N':(0,-.2,0,.2), 'E':(-.2,0,.2,0),
                          'S':(0,.2,0,-.2), 'W':(.2,0,-.2,0)}

    def update(self):
        # Allows you to view a new state in an existing window.
        for loc,cell in self.cells.items():
            if loc in self.env.pits:
                cell.setFill("black")
            elif loc == self.env.wumpus and self.env.wumpusAlive:
                if loc == self.env.gold:
                    cell.setFill("orange")
                else:
                    cell.setFill("red")
            elif loc == self.env.gold and not self.env.haveGold:
                cell.setFill("gold")
            else:
                cell.setFill("white")

        if self.agt:
            self.agt.undraw()
        if self.arrow:
            self.arrow.undraw()

        x,y = self.env.agtLoc
        dx0, dy0, dx1, dy1 = self.agtPoints[self.env.agtDir]
        p1 = Point(x+dx0,y+dy0)
        p2 = Point(x+dx1,y+dy1)
        self.agt = agt = Line(p1,p2)
        agt.setWidth(7)
        agt.setArrow('last')
        if self.env.status=='exploring':
            if self.env.haveGold:
                agt.setFill("gold")
            else:
                agt.setFill("green3")
        else:
            if self.env.status=='chomped':
                agt.setFill("black")
            else:
                agt.setFill("red4")
        if self.env.status != 'out':
            agt.draw(self.win)
            if self.env.haveArrow:
                arrow = agt.clone()
                if self.env.status == "exploring":
                    if self.env.haveGold:
                        arrow.setFill("yellow")
                    else:
                        arrow.setFill("cyan")
                elif self.env.status == "falling":
                    arrow.setFill("red1")
                elif self.env.status == "chomped":
                    arrow.setFill("darkgray")
                arrow.setWidth(2)
                arrow.draw(self.win)
                self.arrow = arrow
        self.win.flush()

    def shoot(self, dx, dy):
        # move arrow from current location to x,y
        for i in range(5):
            self.arrow.move(.2*dx,.2*dy)
            time.sleep(0.05)
            self.win.update()

    def pause(self):
        self.win.getMouse()


# Use this function to generate random environments of a given size.
def randomEnv(xsize,ysize, prob=0.2):
    locations = [(x,y) for x in range(1,xsize+1) for y in range(1,ysize+1)]
    pits = [loc for loc in locations if loc != (1,1) and random() < prob]
    while True:
        gold = choice(locations)
        if gold not in pits: break
    while True:
        wumpus = choice(locations)
        if wumpus != (1,1) and wumpus not in pits: break
    return WumpusEnv(xsize, ysize, gold, wumpus, pits)
        

# Test code -- runs the manual agent on the example env from the book
def testEnv():
    return WumpusEnv(4, 4, (2,3), (1,3), [(3,1),(3,3),(4,4)])

class WumpusAgent:

    """ An agent that simply prompts the user for what to do. """

    def __init__(self, xsize, ysize):
        # ignore the size information
        pass

    def action(self, percept):
        action = raw_input("TestAgent -- Percept:%s --> " % str(percept))
        if action == "fail":
            act = 5/0
        return action


def genEnvs(x,y):
    file = "env%d%d.dat" %(x,y)
    count = 0
    while True:
        if x*y > 25:
            p = .1
        else:
            p = .2
        env = randomEnv(x,y,p)
        save = raw_input("Save this one?")
        if save[0] in "yY":
            print "Saving"
            of = open(file,'a')
            of.write(str(env)+'\n')
            of.close()
            count += 1
            print count, "saved."
        env.close()
        

if __name__ == '__main__':
    testEnv = testEnv() #randomEnv(4,4) #
    for i in range(2):
        x,y = testEnv.size()
        agent=WumpusAgent(x,y)
        raw_input("Press <Enter> to Start")
        testEnv.run(agent)
        print
        testEnv.finalReport()
        testEnv.pause()
        testEnv.reset()
    testEnv.close()
    
