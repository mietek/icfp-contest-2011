#!/usr/bin/env python
import random
import math

def toCards(n):
    result=[]
    while n>0:
        if n%2==1: result.append("succ")
        n = n/2
        if n>0: result.append("dbl")
        result
    return result

def card(t, t2):
    print 1
    print t
    print t2

def slot(t, t2):
    print 2
    print t
    print t2    
   
def help(cart, params): 
    slotRev = random.randint(50, 140)
    slot(slotRev, cart)
    
    for p in params:
        for t in toCards(p):
            card("K", slotRev)
            card("S", slotRev)
            slot(slotRev, t)
        slot(slotRev, "zero")
        
def setInt(slotNr, value):
        slot(slotNr, "zero")
        list = toCards(value)
        list.reverse()
        for t in list:
            card(t, slotNr)
    
def myCeil(x, y):
    return int(math.ceil(float(x)/float(y)))

def newHelp(cart, params, dest):
        slot(dest, cart)
        i=0
        for p in params:
            card("K", dest)
            card("S", dest)
            slot(dest, "get")
            for t in toCards(p):
                card("K", dest)
                card("S", dest)
                slot(dest, t)
            if i<2: 
                card("K", dest)
                card("S", dest)
                card("K", dest)
                card("S", dest)
                slot(dest, "K")
                slot(dest, "zero")
                card("S", dest)
                i=i+1
                slot(dest, "I")

slotW = 5
slotL1 = 6
slotL2 = 7
slotL3 = 8
slotA = 9

slotH1=10
slotH2=11
setInt(slotH1, 0)
setInt(slotH2, 1)
slotA1=12
slotA2=13
setInt(slotA1, 3)
setInt(slotA2, 0)


vL1 = 10000-1
vL2 = 10000+11*vL1/10-1
vL3 = 10000+11*vL2/10-1

setInt(slotL1, vL1)
setInt(slotL2, vL2)
setInt(slotL3, vL3)

setInt(slotA, 11112)

newHelp("help", [slotH1, slotH2, slotL1], 2)
newHelp("help", [slotH1, slotH2, slotL2], 3)
newHelp("help", [slotH1, slotH2, slotL3], 4)
newHelp("attack", [slotA1, slotA2, slotA], 1)

def execute(slotnr):
    setInt(35, slotnr)
    card("get", 35)
    slot(35, "zero")
    
for z in range(64):
    execute(2)
    card("succ", slotH1)
    card("succ", slotH2)
    execute(3)
    card("succ", slotH1)
    card("succ", slotH2)
    execute(4)
    card("succ", slotH1)
    card("succ", slotH2)
    card("succ", slotH1)
    card("succ", slotH2)
    execute(1)
    card("succ", slotA2)
    execute(1)
    card("succ", slotA2)
    execute(1)
    card("succ", slotA2)
    execute(1)
    card("succ", slotA2)
    card("succ", slotA1)
    card("succ", slotA1)
    card("succ", slotA1)
    card("succ", slotA1)

while True:
    for k in range(256):
        help("revive", [k])
        


