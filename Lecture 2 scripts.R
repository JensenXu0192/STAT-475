########################################################
# Binomial Probabilities

# Yahtzee example
# in dbinom(), x=#successes, size=#trials, prob=P(Success).
dbinom(x=5, size=5, prob=1/6)
dbinom(x=0:5, size=5, prob=1/6)
cbind(c(0:5), dbinom(x=0:5, size=5, prob=1/6)) # Nicer print

# Examples from discussion of variance of binomial 
dbinom(x=0:5, size=100, prob=0.01) #Probabilities of 0-5 successes in 100 trials
# pbinom does cumulative distribution: sum of probabilities for up to q successes
# pbinom() gives back P(W<=q), so below computes 1-P(W<=5) = P(W>5) when n=100 and pi=0.01
1-pbinom(q=5, size=100, prob=0.01)

#Showing P(40<=W<=60 for n=100, pi=0.5)
pbinom(q=60, size=100, prob=0.5) - pbinom(q=39, size=100, prob=0.5)

###################################################
# Likelihood plots
# 
sum.y <- 20
n <- 100
pi<-c(0.01, 0.05, 0.1, 0.15, 0.2, 0.3, 0.35, 0.39, 0.4, 0.41, 0.5)
Lik<-pi^sum.y*(1-pi)^(n-sum.y)
data.frame(pi, Lik)

x11(width = 7, height = 6, pointsize = 12)
# Likelihood function plot
# curve() lets you put in a formula (as "expr=") and have it plotted
curve(expr = x^sum.y*(1-x)^(n-sum.y), from = 0, to = 1,
      xlab = expression(pi), ylab = "Likelihood function")
abline(h = 0)

# Log-likelihood function plot
x11(width = 7, height = 6, pointsize = 12)
curve(expr = sum.y*log(x) + (n-sum.y)*log(1-x), from = 0, to = 1,
      xlab = expression(pi), ylab = "Log-likelihood function")

# Similar plot but using plot() function
x11(width = 7, height = 6, pointsize = 12)
pi<-seq(from = 0, to = 1, by = 0.01)
Lik<-pi^sum.y*(1-pi)^(n-sum.y)
save<-data.frame(pi, Lik)
head(save)
plot(x = pi, y = Lik, xlab = expression(pi), xlim = c(0,1), ylab = "Likelihood function", type = "l")  

# Y-axis and title uses expression() as well
curve(expr = x^sum.y*(1-x)^(n-sum.y), from = 0, to = 1, col = "red",
      main = expression(paste("Likelihood function for ", pi)),
      xlab = expression(pi), ylab = expression(paste(L, "(", pi, "|", y[1],...,y[n],")" ) ))
abline(h=0)      




