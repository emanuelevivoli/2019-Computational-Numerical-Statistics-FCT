#================================================================
#                    Exercise 2
#================================================================
rm(list=ls())

#========================Functions===============================

#Exercise 1
sim.norm<-function(n, mu, std){
  
  "Box-Muller Algorithm. This function generates samples
  from two independent random variables X,Y ~ N(mu,std)."
  
  x<-vector()
  y<-vector()
  
  for (i in 1:n){
    
    theta=2*pi*runif(1,0,1)
    R=sqrt(-2*log(runif(1,0,1)))
    
    # add N(0,1) observations to the samples
    x=c(x,R*cos(theta))
    y=c(y,R*sin(theta))
  }
  
  # convert all observations from N(0,1) to N(mu,std)
  x=x*std+mu
  y=y*std+mu
  return(list("x"=x,"y"=y))
}


sim.quisquared<-function(n,df,lower=0,upper=1){
  #forms a quisquared distribution from random uniform values x=(0 to 1)
  s=vector()
  for (i in 1:n){
    x=sim.norm(df,lower,upper)$x
    s=c(s,sum(x^2))
  }
  return(s)
}

f<-function(x){(x*exp(-x))}
g<-function(x){1/(sqrt(2)*sqrt(pi))*exp(-x/2)*x^(1/2)}

h<-function(x){
  #function h(x)=f(x)/g(x)
  sqrt(2)*sqrt(pi)*exp(-x/2)*x^(1/2)
}

#============================(A)===============================

set.seed(123)
# plotting the histogram
hist(sim.quisquared(10000,3),freq=F,main="Quisquared(3)",ylim=c(0,0.3),xlab="x",col="grey",
     cex.main=1.5,cex.lab=1.15)

# adding the p.d.f. on top of the histogram
curve(dchisq(x,3),add=T,lwd=3,lty=1)
# drawing a box around the plot
box(lwd=2)


#the max of h(x)=M
M<- h(1)
optimise(h,interval = c(0,5),maximum = T)$objective


sim<-function(n){
  x<-vector()
  yx <- vector()
  rej_x<- vector() #rejected candidates
  yrejx<- vector() # y pos of rejected candidates
  for (i in 1:n){
    u<-1
    alpha<-0
    k<-0
    while(u>alpha){
      if(k!=0){
        rej_x <- c(rej_x,xc);
        yrejx <- c(yrejx,u*M*g(xc) )
      }
      xc<-sim.quisquared(1,3)
      alpha=(1/M)*h(xc)
      u<-runif(1,0,1)
      k <- k+1
    }
    x=c(x,xc)
    yx <- c(yx,u*M*g(xc))
  }
return(list(x=x, rej_x=rej_x, yx=yx, yrejx=yrejx))
}

#============================(B)===============================

set.seed(123)
fl=sim(10000)
ff=fl$x
rejection_rate= length(fl$rej_x)/(length(fl$rej_x)+length(fl$x)) 
rejection_rate

#============================(C)===============================

pdf("Exercicio2_c.pdf",width=7,height=5)
# plotting the histogram
hist(ff,freq=F,main="f(x)",ylim=c(0,0.4),xlab="x",col="grey",
     cex.main=1.5,cex.lab=1.15, breaks = 50)
# adding the p.d.f. on top of the histogram
curve(f(x),add=T,lwd=3,lty=1,col=c('blue'))
# drawing a box around the plot
box(lwd=2)

# Add a legend
legend(10,0.4, legend=("f(x)"), col=c("blue"), lty=1:2, cex=0.8)
dev.off()

#============================(D)===============================

#hit-and-miss plot
pdf("Exercicio2_d.pdf",width=7,height=5)
plot(f,lwd=3,lty=1,ylab = "u*M*g(x)",main="Hit-and-miss Plot",cex.axis=1.15,col="black",cex.main=1.5,
     cex.lab=1.15,ylim=c(0,0.4), xlim=c(0,8),pch=16,xlab="x",cex=1.5)
curve(M*g(x),lwd=3,col="blue",cex=2,add=T)
set.seed(123)
simul <- sim(10)
points(simul$x, simul$yx, pch=4,cex=1,lwd=1.5)
points(simul$rej_x, simul$yrejx,col=2,pch=4,cex=1,lwd=1.5)
box(lwd=2)
legend(6, 0.36, legend=c("f(x)", "Mg(x)"),col=c("black", "blue"), lty=c(1,1),cex=0.8)
dev.off()

