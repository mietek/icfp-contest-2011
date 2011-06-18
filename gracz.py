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
    slotRev = random.randint(32, 140)
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

def newHelp(cart, params):
        slot(slotW, cart)

        for p in params:        
            card("K", slotW)
            card("S", slotW)
            slot(slotW, "get")
            for t in toCards(p):
                card("K", slotW)
                card("S", slotW)
                slot(slotW, t)
            slot(slotW, "zero")

slotW = 9
slotL1 = 12
slotL2 = 14
slotL3 = 16
slotA = 17

slotH1=32
slotH2=33
setInt(slotH1, 0)
setInt(slotH2, 1)
slotA1=34
slotA2=35
setInt(slotA1, 3)
setInt(slotA2, 0)


vL1 = 10000-1
vL2 = 10000+11*vL1/10-1
vL3 = 10000+11*vL2/10-1

setInt(slotL1, vL1)
setInt(slotL2, vL2)
setInt(slotL3, vL3)

setInt(slotA, 11112)

for z in range(64):
    newHelp("help", [slotH1, slotH2, slotL1])
    card("succ", slotH1)
    card("succ", slotH2)
    newHelp("help", [slotH1, slotH2, slotL2])
    card("succ", slotH1)
    card("succ", slotH2)
    newHelp("help", [slotH1, slotH2, slotL3])
    card("succ", slotH1)
    card("succ", slotH2)
    card("succ", slotH1)
    card("succ", slotH2)
    newHelp("attack", [slotA1, slotA2,  slotA])
    card("succ", slotA2)
    newHelp("attack", [slotA1, slotA2,  slotA])
    card("succ", slotA2)
    newHelp("attack", [slotA1, slotA2,  slotA])
    card("succ", slotA2)
    newHelp("attack", [slotA1, slotA2,  slotA])
    card("succ", slotA2)
    card("succ", slotA1)
    card("succ", slotA1)
    card("succ", slotA1)
    card("succ", slotA1)
    # help("help", [0+2*z, 1+2*z, 9999])
    # help("help", [1+2*z, 0+2*z, 20997])
    # help("attack", [0+2*z, 0+2*z, 11112])
    # help("attack", [0+2*z, 1+2*z, 11112])

# while True:
    # for k in range(256):
        # help("revive", [k])
        


