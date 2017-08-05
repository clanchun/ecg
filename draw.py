#!/usr/bin/python

from graphviz import Digraph
import sys

CALL = 'call'
SPAWN = 'spawn'
RETURN = 'returned from'

class Node:
    def __init__(self, pid=None, tag=None, mod=None,
                 fun=None, arity=None, msg=None, seq=None):
        self.pid = pid
        self.tag = tag
        self.mod = mod
        self.fun = fun
        self.arity = arity
        self.msg = msg
        self.seq = seq

    def toStr(self):
        return self.seq + ': ' + self.mod + ':' + self.fun + '/' + self.arity

def draw(f, dot):
    fd = open(f, 'r')
    seq = 1
    callStack = []
    spawns = {}
    subGraphs = []
    line = fd.readline()
    
    while line:
        els = line.strip(' \n').split(';')
        if len(els) == 6:
            [pid, tag, mod, fun, arity, msg] = els
        elif len(els) == 7:
            [pid, tag, childPid, mod, fun, arity, msg] = els

        updateGraph(pid, subGraphs)
        
        if tag == CALL:
            sg = findGraph(pid, subGraphs)
            callee = Node(pid, tag, mod, fun, arity, msg, str(seq))
            caller = findCaller(callee, callStack)
            sg.node(callee.seq, callee.toStr())

            if caller:
                sg.edge(caller.seq, callee.seq)

            if callee.pid in spawns:
                dot.edge(spawns[callee.pid][0],
                         callee.seq,
                         color = 'red',
                         lhead ='cluster' + callee.pid,
                         label = spawns[callee.pid][1])
                del spawns[callee.pid]
            
            seq += 1
            callStack.append(callee)

        elif tag == SPAWN:
            callee = Node(pid = pid, tag = tag, seq = str(seq))
            caller = findCaller(callee, callStack)
            callStack.append(callee)

            seq += 1
            if caller:
                spawns[childPid] = [caller.seq, callee.seq]

        elif tag == RETURN:
            removeCaller(pid, callStack)

        line = fd.readline()

    for sg in subGraphs:
        dot.subgraph(sg)

    dot.render(f + '.graph', view = True)

def updateGraph(pid, graphs):
    egs = [g for g in graphs if g.name == 'cluster' + pid]
    if egs == []:
        sg = Digraph('cluster' + pid)
        sg.body.append('label = ' + pid)
        sg.body.append('color=blue')
        sg.node_attr.update(style = 'filled')
        graphs.append(sg)
        return True
    else:
        return False
    
def findGraph(pid, graphs):
    for g in graphs:
        if g.name == 'cluster' + pid:
            return g

    return None
    
def findCaller(callee, stack):
    for node in reversed(stack):
        if node.pid == callee.pid and node.tag != 'spawn':
            return node

    return None

def removeCaller(pid, stack):
    for node in reversed(stack):
        if pid == node.pid and node.tag != 'spawn':
            stack.remove(node)
            return

    return

def main():
    dot = Digraph("ecg")
    dot.body.append('compound=true')
    draw(sys.argv[1], dot)

if __name__ == '__main__':
    main()
