# Dr, Breadcrumb
# Adam, Andrew, James, Kern, Owen


# logic_agent.py

# pylogic is the module that allows Python to interact with Prolog
from pylogic import KB


class WumpusAgent:

    def __init__(self,x,y):
        # load the KB rules
        self.kb = KB('fluent_agent')

        # set the size of the environment
        self.kb.tell("x_size(%d)" % x)
        self.kb.tell("y_size(%d)" % y)

        # initialize the agent
        self.kb.ask("agt_initialize")

    def action(self, percept):
        # Note: this version is slightly different from the one in the
        #  book. It is left up to the KB code to keep track of the
        #  situation and update its knowledge with the new percept.
        
        # make a prolog term from the percept list
        #   strings become symbols (no quotes around them).
        percept_str = "[" + ",".join(percept)+"]"
        #print percept
        # form a simple 
        query = "agt_act(%s,A)" % percept_str

        # perform the query and extract the answer
        #print query
        #safes = self.kb.ask("setof(Loc1, (location(Loc1), safe_unvisited(locDir(Loc1, _))), SafeCells)")
        #if safes is not None:
        #    print safes['SafeCells']
        ans = self.kb.ask(query)
        #print ans
        return ans['A']

        
