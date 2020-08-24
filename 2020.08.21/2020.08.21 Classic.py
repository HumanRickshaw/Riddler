#Visual of Hamster Pen.
#
#Rohan Lewis
#2020.08.24

import math
import simplegui

#Globals
HEIGHT = 300
WIDTH = 550
CENTER = (150, 150)
n_sides = 3
post_weight = 0.3333

def reset() :
    global n_sides, post_weight
    n_sides = 3
    post_weight = 0.3333

def draw(canvas):
    global HEIGHT, WIDTH, CENTER, n_sides, post_weight
    
    increment_n()
    
    side = p_side(n_sides, post_weight)
    angle = p_angle(n_sides)
    apothem = p_apothem(side, angle)
    radius = p_radius(side, angle)
    
    area = p_area(apothem, n_sides, side)
    vertices = p_vertices(angle, n_sides, radius)
    
    #Color background white.
    canvas.draw_polygon([(0, 0), (0, HEIGHT), (WIDTH, HEIGHT), (WIDTH, 0)], 2, "Red", "White")
    
    #Draw top line.
    canvas.draw_polygon(vertices, 2, "Blue", "Blue")
    
    #Number of Sides.
    canvas.draw_text('Number of Sides : ' + str(n_sides), [270, 30], 20, 'Black')
    
    #Post Weight.
    if post_weight == 0 :
        canvas.draw_text('Post Weight : ~0 kilograms', [270, 50], 20, 'Black')
    else :
        canvas.draw_text('Post Weight : ' + str(post_weight) + ' kilograms', [270, 50], 20, 'Black') 
        
    #Side Length.
    canvas.draw_text('Side Length : ' + str(round(side, 5)) + ' meters', [270, 70], 20, 'Black')
    
    #Area of Polygon.
    canvas.draw_text('Pen Area : ' + str(round(area, 5)) + ' square meters', [270, 90], 20, 'Black')

    if post_weight > 0 :
        if n_sides == 3 :
            post_weight -= 0.001
        elif n_sides < 7 :
            post_weight -= 0.0001
        elif n_sides < 12 :
            post_weight -= 0.00001
        else : 
            post_weight -= 0.000001
    else :
        post_weight = 0
'''Calculates the minimum weight of a post for side n before
incrementing n.'''
def get_min_weight(n) :
    t_1 = math.tan(math.pi / n)
    t_2 = math.tan(math.pi / (n+1))
    a = n * (n + 1) * (n * t_2 - (n+1) * t_1)
    b = -2 * n * (n + 1) * (t_2 - t_1)
    c = ((n+1) * t_2 - n * t_1)
    d = b ** 2 - 4 * a * c
    min_weight = (-1 * b + math.sqrt(d)) / (2 * a)
    return(min_weight)

'''Increases n by 1 if weight drops below minimum weight.'''
def increment_n() :
    global n_sides
    if post_weight <= get_min_weight(n_sides) :
        n_sides += 1        
        
'''Takes the number of sides and post weight, returns polygon
side length so that total weight is 1 kilogram)'''
def p_side(n, weight) :
    side = (1.0 - n * weight) / n
    return(side)

'''Takes the number of sides and returns the angle between
the apothem and radius.'''            
def p_angle(n) :
    angle = math.pi / n
    return(angle)

'''Takes the side length and angle and returns the apothem
length.''' 
def p_apothem(side, angle) :
    apothem = side / (2 * math.tan(angle))
    return(apothem)

'''Takes the side length and angle and returns the radius
length.''' 
def p_radius(side, angle) :
    radius = side / (2 * math.sin(angle))
    return(radius)

'''Takes the apothem, number of sides, and side length and
returns the area.'''           
def p_area(apothem, n, side) :
    area = apothem * n * side / 2
    return(area)

'''Takes the angle, number of sides, and radius, and returns
the coordinates for the polygon.''' 
def p_vertices(angle, n, radius) :
    global CENTER
    vertices = []
    for v in range(n) :
        x = CENTER[0] + 750 * radius * math.cos(2 * angle * v)
        y = CENTER[1] + 750 * radius * math.sin(2 * angle * v) 
        vertices.append((x,y))

    return(vertices)    

# Create frame.
frame = simplegui.create_frame("Hamster Pen", WIDTH, HEIGHT)
frame.set_draw_handler(draw)
#frame.set_keydown_handler(keydown)
#frame.set_keyup_handler(keyup)

# Start frame.
reset()
frame.start()
