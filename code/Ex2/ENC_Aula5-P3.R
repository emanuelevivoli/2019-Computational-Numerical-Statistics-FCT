# First Exercise slide 29/60
library("bootstrap")

data(law)
GPA = law$GPA
LSAT = law$LSAT

boxplot(GPA)
boxplot(LSAT)

# estimator of p (pearson's correlation)
r = cor(GPA,LSAT); r

# now bootstrap to estimate p
n = length(GPA)
B = 10000

# indices to resample data from
idx = 1:length(GPA); idx
correlations_bootstrapped = numeric(B)
set.seed(123)
for (i in 1:B){
  idx_sample = sample(idx,n,replace=TRUE)
  GPA_bootsample = GPA[idx_sample]
  LSAT_bootsample = LSAT[idx_sample]
  corr_bootsample = cor(GPA_bootsample,LSAT_bootsample)
  correlations_bootstrapped[i] = corr_bootsample
}

corr.boot = mean(correlations_bootstrapped); corr.boot
sd.boot = sd(correlations_bootstrapped); sd.boot
bias.boot = corr.boot - r; bias.boot
corr.BC = r - bias.boot; corr.BC

a = quantile(correlations_bootstrapped, c(0.025,0.975))
ci.npb = c(a[1], a[2]); ci.npb