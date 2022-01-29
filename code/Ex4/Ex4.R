library(rstudioapi) # to automatically set the working directory to this file's path.

#set the working directory to this file's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


m=1000; n=25; alpha=0.05; mu0=1; sigma=sqrt(3) 

xi <- vector()
set.seed(789)

for(i in 1:m){ 
  sam <- rnorm(n,mu0,sigma)
  xi <- c(xi, (n-1) * var(sam) / 2)
  xi
} 

##
#  the histogram of the simulated ??1,...,??1000 with the theoretical density superimposed; 
pdf("Exercicio4_a_hist.pdf",width=7,height=5)
hist(xi, freq = F , main = expression(paste("Histogram of 1000 simulated samples from  ", 
                                            frac((n-1)*S^2, 2))),
     col ="grey", xlab = "x",  xlim = c(0,80), ylim = c(0, 0.06))
curve(dchisq(x, n-1), col = "red", lwd = "2", add = T)
legend(55,0.06, legend=(expression(paste("True ", chi[df=24]^2, " superimposed"))),
       col="red", lty=1:2, cex=0.75)
box(lwd=2)
dev.off()

###


# plot of the empirical cumulative distribution (ecdf) of ??1,...,??1000 with the theoretical 
# cumulative probability function (pdf) superimposed 

empi.cdf <- ecdf(xi)

pdf("Exercicio4_a_ecdf_pdf.pdf",width=7,height=5)
plot(empi.cdf,xlim=c(0,70),main="e.c.d.f. and p.d.f. of the simulated X1,...,X1000",
     ylab="probability",cex=2,lwd=1.5) 
lines(seq(0, 70, by=.1), pchisq(seq(0, 70, by=.1),n-1), col=2) 
legend(-2.8,0.96, legend=c("ecdf","theoretical pdf of chi^2 (n-1)"), col=c("black","red"),
       lty=1,lwd=c(1.5,1), cex=0.8)
dev.off()

# Kolmogorov-Smirnov two-sided test
ks.test(xi, pchisq, n-1, alternative = "two.sided")
# p-value < 2.2e-16


# Compute the theoretical quantiles 0.90, 0.95,0.975 with the empirical quantiles 

theo.quantiles <- qchisq(c(.9,.95,.975), n-1); theo.quantiles #           33.19624 36.41503 39.36408
empi.quantiles <- quantile(xi, probs = c(.9,.95,.975)); empi.quantiles #       90%      95%    97.5% 
#                                                                         49.00081 54.54538 58.28360 




# (b) Assume one wants to test the hypothesis H0 : ??^2 ??? 3 H1 : ??^2 > 3 at the signi???cance level ?? = 0.05. 
# Compute the empirical p-values for this test using the previous Monte Carlo simulation. 
# Compute the empirical signi???cance level ??_hat and perform the binomial test to assess if ??_hat departs 
# signi???cantly from ??.

# Monte Carlo simulation
p <- numeric(m)
set.seed(789)
for(i in 1:m){ 
  x=rnorm(n,mu0,sigma)
  X2 = (n-1)*sd(x)^2/sigma^2
  p[i] = 1 - pchisq(X2, n-1)
  p     # empirical p-values
  
  # Step 2. for each dataset determine if each p ???value is lower than the pre-specified ?? 
  # and Step 3. estimate the true significance level 
  phat=mean(p<alpha)
  phat  # empirical signi???cance level ??_hat
}
phat # 0.042

##

##
bin.test <- binom.test(phat*m,m,p=0.05); bin.test
#data:  phat * m and m
# number of successes = 42, number of trials = 1000, p-value = 0.2761
# alternative hypothesis: true probability of success is not equal to 0.05
# 95 percent confidence interval:
#  0.03043471 0.05635029
# sample estimates:
#  probability of success 
# 0.042 

p.depart = bin.test$p.value; p.depart # 0.2760517


# c) For the hypothesis test above and the m simulations, construct a power plot for the 2 alternative
# values ?? = 4, . . . , 50. In face of these results, how far from H0 does one need  to be so that the 
# power of the test gets higher than 90%?

v1=4:50; sd1 = sqrt(v1); nv1=length(v1)
power=vector() 
set.seed(789) 
for(sds in sd1){
  set.seed(789) 
  p=vector() 
  for(i in 1:m){ 
    x=rnorm(n,mu0,sds) 
    X2 = (n-1)*sd(x)^2/sigma^2
    p[i] = 1 - pchisq(X2, n-1)
  } 
  power=c(power,mean(p<alpha)) 
} 

pdf("Exercicio4_c_power.pdf",width=7,height=5)
plot(v1,power,pch=20,cex=1.5,xlab=expression(variance), 
     main = "power plot for alternative variance values (4,...,50).", ylab="Power",cex.lab=1.5) 
lines(v1,power,type="l") 
abline(h = 0.9, v = 7.2, lty=c(2,2), col="blue")
box(lwd=2)
dev.off()
