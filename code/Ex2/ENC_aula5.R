# Exemplo 1 slide 18/60
X = c(30,37,36,43,42,43,43,46,41,42)
xbar = mean(X)

n = length(X)
B = 10000

M.npb = numeric(B)
sd.boot = numeric(B)

for (i in 1:B){
  x.npb = sample(X,size=n,replace=TRUE)
  M.npb[i] = mean(x.npb)
  sd.boot[i] = sd(x.npb)
}
# Computing the percentile bootstrap 95% CI
d = quantile(M.npb, c(0.025,0.975)); d


# Computin the pivotal (or basic) 95% CI
delta_star = M.npb - xbar
d = quantile(delta_star, c(0.025,0.975))
ci = xbar - c(d[2], d[1]); ci

# Computng the studentized bootstrap 95% CI
delta_star = (M.npb - xbar) / sd.boot
d = quantile(delta_star, c(0.025, 0.975))
ci = xbar - c(d[2], d[1]) * sd(M.npb); ci