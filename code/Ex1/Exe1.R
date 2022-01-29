library(rstudioapi) # to automatically set the working directory to this file's path.

#set the working directory to this file's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Exercicio A
sim.norm<-function(n, mu, std){
  
  "Box-Muller Algorithm. This function generates samples
  from two independent random variables X,Y ~ N(mu,std)."
  
  x<-vector()
  y<-vector()
  
  # Box-Muller algorithm
  theta=2*pi*runif(n,0,1)
  R=sqrt(-2*log(runif(n,0,1)))
  
  # add N(0,1) observations to the samples
  x=c(x,R*cos(theta))
  y=c(y,R*sin(theta))
  
  # convert all observations from N(0,1) to N(mu,std)
  x=x*std+mu
  y=y*std+mu
  
  # return results in a table format
  samples = matrix(c(x,y),ncol=2)
  colnames(samples) = c("X","Y")
  rownames(samples) = seq(from=1,to=nrow(samples),by=1)
  samples = as.table(samples)
  return(samples)
}

#Exercicio B - generate 10k samples from X~(0,4)
set.seed(123)
mu = 0
std = 4
n = 10 * 1000
samples = sim.norm(n/2,0,4)  # n/2 because we can merge X and Y since their both are independent.
samples = c(samples[,"X"], samples[,"Y"]) # merge the 5k samples from X with the 5k samples from Y


#Exercicio C
pdf("Exercicio1_c.pdf",width=7,height=5)
hist(samples, freq=F, main="Box-Muller 10k samples from N(0,4)", ylim=c(0,0.1), xlab="x", col="grey", cex.main=1.5, cex.axis=1.15,
     breaks=50);
curve(dnorm(x ,0,4), add=T, lwd=3, lty=1, from=-14, to=14,col=c('blue'))
# drawing a box around the plot
box(lwd=2)
# Add a legend
legend(-17.5,0.10, legend=("True N(0,4) superimposed"), col=c("blue"), lty=1:2, cex=0.8)
dev.off()