# How to Fit an ARMA+GARCH model to a time series

library(rugarch)
data(bmw, package="evir")

# Fit an AR(1) + GARCH(1,1) model to the BMW time series:
arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)),
							variance.model=list(garchOrder=c(1,1)))
bmw.garch.norm = ugarchfit(data=bmw, spec=arma.garch.norm)
show(bmw.garch.norm)

# Ljung-Box tests output show that the standardized residuals:
	# have no serial auto-correlation in epsilonHat   (p=0.13)
	# Also no serial auto-correlation in epsilonHat^2 (p=0.88)
		# You can compute these independently of the "show" command, using:
Box.test((residuals(bmw.garch.norm,standardize=TRUE)^1),lag=10,type="Ljung-Box")
Box.test((residuals(bmw.garch.norm,standardize=TRUE)^2),lag=10,type="Ljung-Box")

# But the Pearson goodness of fit test shows that:
	# Gaussian model for epsilon is a poor fit: tails are too heavy

# So maybe a T-distribution would work better for epsilon? :
library(MASS)
e = residuals(bmw.garch.norm, standardize=TRUE)
fitdistr(e,"t")
	# df fit by MLE = 4.11

# Ruppert does a qq-plot of the t residuals using df=4
	# which actually looks pretty good, except for a few extrema

# Go back and re-fit AR(1)+GARCH(1,1) model, with t-dist epsilon:
arma.garch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)),
							variance.model=list(garchOrder=c(1,1)),
							distribution.model = "std")
bmw.garch.t = ugarchfit(data=bmw,spec=arma.garch.t)
	# Notice: rugarch will choose t DOF for you
show(bmw.garch.t)

# estimated t DOF = 4.07
# Now there IS serial auto-corr in the standardized residuals epsHat
# (but not in epsHat^2)!

# it is "due to small autocorr that should not be of practical importance"

# goodness of fit is better but still highly statistically significant

# Overall, t-distribution fit is much better than gaussian
	# AIC/BIC decreased substantially with t fit


# How do the residuals look?
	# uGARCHfit object methods can be found with "??residuals"
			# then click on "rugarch::uGARCHfit-class"
# We want to inspect the ordinary (a_t) and standardized (epsilon_t) residuals
		# as well as their squares (a_t)^2   and   (epsilon_t)^2
acf((residuals(bmw.garch.t,standardize=FALSE)^1))   # a_t
acf((residuals(bmw.garch.t,standardize=TRUE)^1))    # eps_t
acf((residuals(bmw.garch.t,standardize=FALSE)^2))   # (a_t)^2
acf((residuals(bmw.garch.t,standardize=TRUE)^2))    # (eps_t)^2

# Ljung-Box test on standardized residuals can be used for model checking:
Box.test((residuals(bmw.garch.t,standardize=TRUE)^1),lag=10,type="Ljung-Box")
Box.test((residuals(bmw.garch.t,standardize=TRUE)^2),lag=10,type="Ljung-Box")
	# result: p=0.013 for eps_t, but p=0.83 for (eps_t)^2


