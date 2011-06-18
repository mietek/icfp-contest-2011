#!/usr/bin/env python
import random

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
    slotRev = random.randint(32, 150)
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
    


def newHelp(cart, params, v3):
        slot(slotW, cart)

        for p in params:
            for t in toCards(p):
                card("K", slotW)
                card("S", slotW)
                slot(slotW, t)
            slot(slotW, "zero")
        
        card("K", slotW)
        card("S", slotW)
        slot(slotW, "get")
        for t in toCards(v3):
            card("K", slotW)
            card("S", slotW)
            slot(slotW, t)
        slot(slotW, "zero")

maxVal = 24        

slotW = 9
slotL1 = 12
slotL2 = 14
slotL3 = 16
slotA = 17

setInt(slotL1, 9999)
setInt(slotL2, 20997)
setInt(slotL3, 33095)

setInt(slotA, 11112)
newHelp("help", [0, 1], slotL1)

for z in range(64):
    newHelp("help", [0+4*z, 1+4*z], slotL1)
    newHelp("help", [1+4*z, 2+4*z], slotL2)
    newHelp("help", [2+4*z, 3+4*z], slotL3)
    newHelp("attack", [3+4*z, 0+4*z], slotA)
    newHelp("attack", [3+4*z, 1+4*z], slotA)
    newHelp("attack", [3+4*z, 2+4*z], slotA)
    newHelp("attack", [3+4*z, 3+4*z], slotA)
    # help("help", [0+2*z, 1+2*z, 9999])
    # help("help", [1+2*z, 0+2*z, 20997])
    # help("attack", [0+2*z, 0+2*z, 11112])
    # help("attack", [0+2*z, 1+2*z, 11112])

while True:
        for k in range(256):
            help("revive", [k])
        


