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
            
slotL1 = 1
slotL2 = 2
slotA =  3
slotA1=  4

vL1 = 10000-1
vL2 = 10000+11*vL1/10-1
vL3 = 10000+11*vL2/10-1


setInt(slotL1, vL1)
setInt(slotL2, vL2)
setInt(slotA, 11112)

newHelp("help", [slotA1, slotA1, slotL1], 5, 0, 1)
newHelp("help", [slotA1, slotA1, slotL2], 6, 1, 0)
newHelp("attack", [slotA1, slotA1, slotA], 16, 0, 0)
newHelp("attack", [slotA1, slotA1, slotA], 17, 0, 1)

def execute(slotnr):
    setInt(35, slotnr)
    card("get", 35)
    slot(35, "zero")

zloz([5,6], 9)  
zloz([16,17], 8)
zloz([8,9], 0)


# setInt(slotA1, 254)
# execute(0)
# card("put", slotA1)
slot(slotA1, "zero")
          
for z in range(128):
    # execute(1)
    execute(0)
    card("succ", slotA1)
    card("succ", slotA1)

while True:
    for k in range(256):
        help("revive", [k])

       