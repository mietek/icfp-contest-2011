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

def newHelp(cart, params, dest, zw1, zw2):
        slot(dest, cart)
        i=0
        for p in params:
            if i==0:
                for k in range(zw1):
                    card("K", dest)
                    card("S", dest)
                    slot(dest, "succ")
            if i==1:
                for k in range(zw2):
                    card("K", dest)
                    card("S", dest)
                    slot(dest, "succ")
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
                slot(dest, "I")
            i=i+1

def zloz(params, dest):
        slot(dest, "S")
        i=0
        for p in params:
            card("K", dest)
            card("S", dest)
            slot(dest, "get")
            for t in toCards(p):
                card("K", dest)
                card("S", dest)
                slot(dest, t)
            slot(dest, "zero")
            i=i+1            
            
slotW = 5
slotL1 = 16
slotL2 = 17
slotL3 = 18
slotA = 19
slotA1=15

vL1 = 10000-1
vL2 = 10000+11*vL1/10-1
vL3 = 10000+11*vL2/10-1


setInt(slotA1, 0)
setInt(slotL1, vL1)
setInt(slotL2, vL2)
setInt(slotL3, vL3)
setInt(slotA, 11112)

newHelp("help", [slotA1, slotA1, slotL1], 21, 0, 1)
newHelp("help", [slotA1, slotA1, slotL2], 22, 1, 2)
newHelp("help", [slotA1, slotA1, slotL3], 23, 2, 3)
newHelp("attack", [slotA1, slotA1, slotA], 24, 3, 0)
newHelp("attack", [slotA1, slotA1, slotA], 25, 3, 1)
newHelp("attack", [slotA1, slotA1, slotA], 26, 3, 2)
newHelp("attack", [slotA1, slotA1, slotA], 27, 3, 3)

def execute(slotnr):
    setInt(35, slotnr)
    card("get", 35)
    slot(35, "zero")
    
def mega(slotlst, dest):
    for slotnr in slotlst:
            slot(dest, "get")
            for t in toCards(slotnr):
                card("K", dest)
                card("S", dest)
                slot(dest, t)
            slot(dest, "zero")
            card("S", dest)

zloz([21,22], 10)            
zloz([10,23], 1)
zloz([24,25], 11)
zloz([26,27], 12)
zloz([11,12], 0)
            
for z in range(64):
    execute(1)
    execute(0)
    card("succ", slotA1)
    card("succ", slotA1)
    card("succ", slotA1)
    card("succ", slotA1)

# while True:
    # for k in range(256):
        # help("revive", [k])
       