# notes ----

# author: Sarah Power
# email: Sarah.Power@alaska.gov
# April 29, 2019

# This calculates the variance of the mark recapture (MR) estimate for Chinook in the Nushigak River in 2015
# when using only large fish (>660mm) for the MR and then using the proporiton of large to non-large fish to calculate
# the rest. This technique using bootstrapping. 
# Note this variance may be an underestimate since the estimate of large and non-large fish are not indipendant. 
# There are other estimates for the 2015 MR, in which the MR is stratified by sex. 
# The choice of which estimator to use is at this date undecided.

# load ----
# No specific libraries

# data ----
N_greater660 <- 47162
var_N_greater660 <- 65695634
percent_greater660 <- 0.55907
samp_size_percent <- 1300

#See operational plan for equation:
N_less660 <- N_greater660*(1/percent_greater660 -1)

# analysis ----

#This function returns an bootstrapped variance of 1 over "percent_greater660",
#where "percent_greater660" is a binomial
#estimated from a sample size of samp_size_percent (The number of fish measured for length from the weirs)
# var(1/p)
Tinv=function(sim=10000,samp=samp_size_percent,theta= percent_greater660){
x=rbinom(sim,samp,theta)
thetastar=x/samp
vtheta=var(thetastar)
thetainv=1/thetastar
vthetainv=var(thetainv)
cat("Sim Theta=",mean(thetastar),fill=T)
#cat("Simulated Var Theta=",vtheta,fill=T)
#cat("Theoretical var theta=",theta*(1-theta)/(samp-1),fill=T)
cat("Simulated Var Inverse theta=",vthetainv,fill=T)
#cat("Simulated SE Inverse Theta=",round(sqrt(vthetainv),4),fill=T)
#cat("Simulated G(Thetainv)=",vthetainv/(1/theta)^2,fill=T)
return(vthetainv)
}


var_1over_percent_greater660 <- Tinv()

#The following equation calculates the variance of the number of Chinook less than 660mm.
#Using Goodman's 1960 variance of products equation. 
var_N_less660 <- var_N_greater660*(1/percent_greater660)^2 + var_1over_percent_greater660*(N_greater660^2) - var_N_greater660*var_1over_percent_greater660
se_var_N_less660 <- sqrt(var_N_less660)

# So the estimated number of Chinook less than 660 mm is
N_less660 
# with Se
se_var_N_less660
#and CV
se_var_N_less660/N_less660 

#The total estimate Chinook in river is
(total <- N_greater660 + N_less660)

#Since the estimate the number of fish less than 660 and the number of fish greater than 660 is not independant
# then there is some uncounted for covariance which is assummed to be minimal. 
# With that said the total variance is greater than 
(var_total <- var_N_greater660 + var_N_less660)
# SE of total
(se_total <- sqrt(var_total))
#CV of total
se_total/total

