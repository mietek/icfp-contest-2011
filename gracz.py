#!/bin/python


slotW=13
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
    
def help(cart, i, j, n): 
    slot(slotW, cart)
        
    for t in toCards(i):
        card("K", slotW)
        card("S", slotW)
        slot(slotW, t)
    slot(slotW, "zero")
    

    for t in toCards(j):
        card("K", slotW)
        card("S", slotW)
        slot(slotW, t)
    slot(slotW, "zero")
    
    for t in toCards(n):
        card("K", slotW)
        card("S", slotW)
        slot(slotW, t)
    slot(slotW, "zero")
    
for z in range(128):
    help("help", 0+2*z, 1+2*z, 9999)
    help("help", 1+2*z, 0+2*z, 20997)
    help("attack", 0+2*z, 0+2*z, 11112)
    help("attack", 0+2*z, 1+2*z, 11112)
# print toCards(19)