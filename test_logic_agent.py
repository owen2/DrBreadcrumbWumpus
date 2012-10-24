# test_logic_agent.py
from wumpusenv import *
from drBreadcrumb import WumpusAgent

#env = testEnv()
x,y = 8, 8
env = randomEnv(x,y, .1)
agent = WumpusAgent(x,y)
print "Click Window to Run"
env.pause()
env.run(agent)
env.finalReport()
print "Click to close"
env.pause()
env.close()
