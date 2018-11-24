import pygame as pg
from random import randint, random
from random import choice as SAMP
from collections import Counter
pg.font.init()

def initialise(wait = False):
    width = 500 ; height = 500
    EXIT = False

    screen = pg.display.set_mode((width, height))
    state = 'MENU'

    while(not EXIT):
        for event in pg.event.get():
            if event.type==pg.QUIT: EXIT=True

        if state=='MENU':
            choice = Menu(screen)
            if (choice==False): EXIT = True
            state='GAME'
        if state=='GAME':
            result = Game(screen, choice[0], choice[1], choice[2], wait)
            if not result: EXIT = True

        pg.display.flip()

    pg.quit()

def Menu(screen):
    myfont = pg.font.SysFont('Times New Roman', 30)
    myfont20 = pg.font.SysFont('Times New Roman', 20)
    grey = (100,100,100)
    white = (255,255,255)
    GRIDcols = [white, white, white]
    STARTcols = [white, white]
    NEIGHBcols = [white, white, white, white]

    GRID = False ; START = False ; NEIGHB = False ; EXIT = False
    while(not (GRID and START and NEIGHB) and not EXIT):
        # Buttons for automata grid density selection
        screen.blit(myfont.render('Select Grid Density', False, white),(130,40))
        screen.blit(myfont20.render('Low', False, white),(140,100))
        screen.blit(myfont20.render('Medium', False, white),(200,100))
        screen.blit(myfont20.render('High', False, white),(300,100))
        for i in range(0,3): pg.draw.rect(screen,GRIDcols[i],(150+80*i,80,10,10))

        # Buttons for automata starting position pattern:
        screen.blit(myfont.render('Select Starting Pattern', False, (255,255,255)),(130,140))
        screen.blit(myfont20.render('Clustered', False, (255,255,255)),(140,200))
        screen.blit(myfont20.render('Random', False, (255,255,255)),(280,200))
        for i in range(0,2): pg.draw.rect(screen,STARTcols[i],(150+160*i,180,10,10))

        # Buttons for neighbourhood selection:
        screen.blit(myfont.render('Select Neighbourhood', False, (255,255,255)),(130,230))
        screen.blit(myfont20.render('Moore', False, (255,255,255)),(140,290))
        screen.blit(myfont20.render('Von Neumann', False, (255,255,255)),(280,290))
        screen.blit(myfont20.render('Large Moore', False, (255,255,255)), (140, 350))
        screen.blit(myfont20.render('Random VN', False, (255,255,255)), (310, 350))
        for i in range(0,2): pg.draw.rect(screen,NEIGHBcols[2+i],(150+160*i,330,10,10))
        for i in range(0,2): pg.draw.rect(screen,NEIGHBcols[i],(150+160*i,270,10,10))

        for event in pg.event.get():
            if event.type==pg.QUIT: return(False)
            if event.type==pg.MOUSEBUTTONDOWN:
                mouse = pg.mouse.get_pos()
                x = mouse[0] ; y = mouse[1]

                # For the first row of buttons:
                if 80<=y<=90:
                    if 150<=x<=160:
                        GRIDcols = [grey,white,white]
                        GRID = 'Low'
                    elif (230<=x<=240):
                        GRIDcols = [white, grey, white]
                        GRID = 'Medium'
                    elif (310<=x<=320):
                        GRIDcols = [white, white, grey]
                        GRID = 'High'

                # For the second row of buttons:
                elif 180<=y<=190:
                    if 150<=x<=160:
                        STARTcols = [grey,white]
                        START = 'Clus'
                    elif (310<=x<=320):
                        STARTcols = [white, grey]
                        START = 'Rand'

                # For the third row of buttons:
                elif 270<=y<=280:
                    if 150<=x<=160:
                        NEIGHBcols = [grey,white,white,white]
                        NEIGHB = 'Moore'
                    elif (310<=x<=320):
                        NEIGHBcols = [white,grey,white,white]
                        NEIGHB = 'Von'
                elif 330<=y<=340:
                    if 150<=x<160:
                        NEIGHBcols = [white,white,grey,white]
                        NEIGHB = 'LM'
                    elif 310<=x<320:
                        NEIGHBcols = [white,white,white,grey]
                        NEIGHB = 'Ran'

        pg.display.flip()

    return([GRID, START, NEIGHB])


def Game(screen, D, S, Neighb, wait):
    EXIT = False

    # We must initialise the grid matrix
    Mat = CreateGrid(D, S)

    # DrawGrid
    while True:
        screen.fill((100,100,100))
        for event in pg.event.get():
            if event.type==pg.QUIT: return(False)
            if event.type==pg.KEYDOWN:
                # If an equilibrium is hit, pressing the R
                # key will create a new start:
                if event.key == pg.K_r: Mat = CreateGrid(D,S) 
            if wait:
                if event.type==pg.KEYDOWN:
                    Mat = iterateGrid(Mat, Neighb)

        if not wait:
            Mat = iterateGrid(Mat, Neighb)
            #pg.time.wait(10)

        DrawGrid(screen, Mat)
        pg.display.flip()
        

def iterateGrid(Mat, Neighb):
    d = len(Mat)
    newMat = [['.' for i in range(d)] for i in range(d)]
    for x in range(d):
        for y in range(d):
            point = Mat[x][y]
            # Get neighbours:
            N = getNeighb(Mat, [x,y], Neighb)
            # with the 'RHS' neighbourhood rule, some points
            # have no neighbours, and so we set their neighbour 
            # equal to themself, to avoid problems:
            if len(N)==0: M = Mat[x][y]
            else: M = Mode(N)

            # simple rule set:
            #newMat[x][y] = M
          
            if point==0:
                raise ValueError('Non-value point')

            elif point==1:
                # the point is rock
                if M==0 or M==1 or M==3: newMat[x][y]=1
                elif M==2: newMat[x][y]=2
                else: raise ValueError('unexpected outcome')

            elif point==2:
                if M==0 or M==1 or M==2: newMat[x][y]=2
                elif M==3: newMat[x][y]=3
                else: raise ValueError('unexpected outcome')

            elif point==3:
                if M==0 or M==2 or M==3: newMat[x][y]=3
                elif M==1: newMat[x][y]=1
                else: raise ValueError('unexpected outcome')

            else: raise ValueError('unexpected outcome')

    return(newMat)

def getNeighb(Mat, C, Neighb):
    # purpose: returns a list of coordinates of the neighbours 
    #          of the given cell in a matrix, given the 
    #          neighbourhood rule (Moore or Von Neumann)
    # inputs : Mat    - The current matrix of states
    #          C      - The coordinates of the current cell
    #          Neighb - The rule to be used to find neighbours

    d = len(Mat)

    # Begin by producing a list of all possible neighbours:
    N = []
    if Neighb=='Moore':
        for x in [C[0]-1, C[0], C[0]+1]:
            for y in [C[1]-1, C[1], C[1]+1]: N.append([x,y])
        # Then we remove the point itself:
        N.remove([C[0],C[1]])

    elif Neighb=='Von':
        N = [[C[0],C[1]-1], [C[0],C[1]+1], [C[0]-1,C[1]], [C[0]+1,C[1]]]

    elif Neighb=='LM':
        for x in [C[0]-1, C[0], C[0]+1, C[0]-2, C[0]+2]:
            for y in [C[1]-1, C[1], C[1]+1, C[1]-2, C[1]+2]: N.append([x,y])
        # Then we remove the point itself:
        N.remove([C[0],C[1]])

    elif Neighb=="Ran":
        N.append(SAMP([[C[0],C[1]-1], [C[0],C[1]+1], [C[0]-1,C[1]], [C[0]+1,C[1]]]))

    vals = []
    for coord in N:
        x = coord[0]
        y = coord[1]
        if 0<=x<=d-1 and 0<=y<=d-1:vals.append(Mat[x][y])

    return(vals)

def Mode(lst):
    # purpose: returns the mode of a list, and randomly
    #          makes a choice in the presence of ties
    counts = Counter(lst)
    M = max(counts.values())
    choices = []
    for element in counts.items():
        if element[1]==M: choices.append(element[0])
    return(SAMP(choices))

def CreateGrid(Density, Start):
    NumCells = {'Low':10,'Medium':50,'High':55}[Density]

    Mat = [[0 for i in range(NumCells)] for i in range(NumCells)]

    if Start=='Rand':
        for x in range(NumCells):
            for y in range(NumCells): Mat[x][y] = randint(1,3)

    return(Mat)

def DrawGrid(Screen, Matrix):
    Cols = {0:(100,100,100),1:(255,255,255),2:(0,0,0),3:(125,125,125)}
    dimension = len(Matrix)
    pixelWidth = 500/dimension

    for x in range(dimension):
        for y in range(dimension):
            pg.draw.rect(Screen, Cols[Matrix[x][y]], (x*pixelWidth, y*pixelWidth, pixelWidth, pixelWidth))



initialise(wait = False)