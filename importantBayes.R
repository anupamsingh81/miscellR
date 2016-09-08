library(BEST)
x = rnorm(100,60,10)
y = rnorm(50,30,20)
 r =27
 s =32
 t = "mean"
u =24
v =0.95

z= c(input$r,input$s)
a= c(input$t)
d = c(input$u)
e = c(input$v)

z= c(r,s)
a= c(t)
d = c(u)
e = c(v)


BESTout = BESTmcmc(x,y,numSavedSteps = 20000)
plot(BESTout,which = a ,credMass = e, ROPE =z, compVal = d)
plotAll(BESTout, credMass= e, ROPEm= z ,
        ROPEeff=c(-0.2,0.2), compValm= d )
plotAll(BESTout, credMass=0.8, ROPEm= z,
        ROPEeff=c(-0.2,0.2), compValm= d, showCurve=TRUE)
summary(BESTout, credMass= e, ROPEm= z , ROPEsd=c(-0.15,0.15),
        ROPEeff=c(-0.2,0.2))
