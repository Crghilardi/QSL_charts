
#Written by Casey Ghilardi
#Github: crghilardi
##Reineke SDI graph

using Gadfly

sdi=230
diarng=collect(1:20)
tparng=[1,1200]
maxline=sdi./(((diarng)./10.0).^1.605)
comp_mortline=maxline*0.55
crown_closeline=maxline*0.35

myxticks=log([1,5,10,50,100,500,1000])
myyticks=log([1,2,5,10,20])

myplot=plot(
    layer(x=[25], y=[4],Geom.point,Theme(default_color=colorant"red")),
    layer(x=maxline,y=diarng,Geom.line,Theme(default_color=colorant"black")),
    layer(x=comp_mortline,y=diarng,Geom.line,Theme(default_color=colorant"red")),
    layer(x=crown_closeline,y=diarng,Geom.line,Theme(default_color=colorant"blue")),
    Scale.x_log(labels=d-> @sprintf("%d",e^d)),
    Scale.y_log(labels=d-> @sprintf("%d",e^d)),
    Guide.xticks(ticks=myxticks),
    Guide.yticks(ticks=myyticks),
    Guide.XLabel("Trees per acre"),
     Guide.YLabel("Quadratic Mean Diameter"),
    Theme(grid_color=colorant"black")
    )

using Cairo
using Fontconfig

draw(PDF("myplot.pdf", 6.5inch, 9inch), myplot)
draw(PNG("myplot.png", 6.5inch, 9inch), myplot)
