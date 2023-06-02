option base 0

def Scale!(n%, res%, min!, max!) as (csng(n) / csng(res)) * (max - min) + min

def ScaleX!(n%, res%) as Scale(n, res, -2.5, 1)
def ScaleY!(n%, res%) as Scale(n, res, -1, 1)

const resx% = 80
const resy% = 30

dim plot%(resx, resy)

for px = 0 to resx - 1
    for py = 0 to resy - 1
        x0! = ScaleX(px, resx)
        y0! = ScaleY(py, resy)
        
        x! = 0.0
        y! = 0.0
        
        iteration% = 0
        max_iteration% = 1000
        
        while ( x*x + y*y < 2*2  AND  iteration < max_iteration )
            xtemp! = x*x - y*y + x0
            y = 2*x*y + y0
            x = xtemp
            iteration = iteration + 1
        wend
        color_n% = iteration > 500 '' monochrome!
        
        plot(px, py) = color_n
    next
next

'' 'Render' to the console -
for r = 0 to resy - 1
    for c = 0 to resx - 1
        if plot(c, r) then print "*"; else print " ";
    next
    print
next
'xfree
