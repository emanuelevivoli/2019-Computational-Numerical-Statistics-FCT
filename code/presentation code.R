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
  return (list("samples"=samples,"R^2"=R^2,"theta"=theta))
}
#generating the samples
set.seed(123)
res = sim.norm(10000,0,1)
samples=res$samples
#calculating the mean and cov
mean=colMeans(samples);mean
S=cov(samples);S


#Hexbin plot
library(ggplot2)
library(hexbin)
library(ggExtra)
# Bin size control + color palette
df=as.data.frame.matrix(samples)
ggplot(df, aes(x=X, y=Y) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# scatter-plot with marginal distribution :
n=10000
p <- ggplot(df[1:n,], aes(x=X, y=Y)) +
  geom_point(color="darkblue",pch=20) +
  theme(legend.position="none")

# with marginal histogram
p1 <- ggMarginal(p, type="histogram",color="blue");p1



#bivariate normal plot
library(MASS)
bivn=samples
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 100)

# now plot your results
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)

# fancy contour with image
image(bivn.kde); contour(bivn.kde, add = T)

# fancy perspective
persp(bivn.kde, phi = 30, theta =20, shade = .1, border = NA)
