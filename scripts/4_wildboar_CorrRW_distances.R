###
# 
# Script to parametrise the RangeShiftR transfer sub-modue CorrRW (correlated random walk)
#
###


# 1.) define function to calculate distribution of settlement and death events per each step

pSteps <- function(pStepMort, pSettle, MaxSteps = 10){

    step <- seq(MaxSteps)
    sett <- rep(0, MaxSteps)
    mort <- rep(0, MaxSteps)

    pNotSettle <- (1-pStepMort) * (1-pSettle)

    sett[step] <- pNotSettle^(step-1) * (1-pStepMort) * pSettle
    mort[step] <- pNotSettle^(step-1) * pStepMort

    normconst <- sum(sett+mort) # normalisation constant
    #sett <- sett / normconst   # normalise

    return(list(dist = cbind(step,  # within/after n-th step:
                             sett,  # proportion of individuals settled 
                             mort), # proportion of individuals dead from step mortality
                mortMaxStep = 1-normconst))
}


# 2.) use function on an example correlated random walk (CorrRW) and plot results

# get step-wise stats
stepstats <- pSteps(pStepMort = 0.01, pSettle = 0.8, MaxSteps = 6)

# plot step-wise stats
plot( stepstats$dist[,'step'], stepstats$dist[,'sett'], type = 'b', col = "blue", ylim = c(0,0.5), xlab = "step", ylab = "per-step proportion" )
lines(stepstats$dist[,'step'], stepstats$dist[,'mort'], type = 'b', col = "red")
legend("topright", legend = c("settled","died"), col = c("blue","red"), lty = 1)

# cumulative proportions:
cum_stepstats <- cbind(step = stepstats$dist[,'step'],
                       cumsum(as.data.frame(stepstats$dist[,2:3])))

# plot cumulatives
plot( cum_stepstats[,'step'], cum_stepstats[,'sett'], type = 'b', col = "blue", ylim = c(0,1), xlab = "step", ylab = "cumulative proportion" )
lines(cum_stepstats[,'step'], cum_stepstats[,'mort'], type = 'b', col = "red")
legend("topleft", legend = c("settled","died"), col = c("blue","red"), lty = 1)


# 3.) get an estimate of expected dispersal distance (assuming constant settlement probability and step mortality)

# CorrRW parameters:
StepLength <- 4000 # given in meters
Rho <- 0.5

# calculate expected number of steps
(E_nsteps <- as.numeric(stepstats$dist[,'step']%*%stepstats$dist[,'sett']))

# estimate expected dispersal distance
(E_dispdist <- sqrt(E_nsteps*(1+Rho*(E_nsteps-1))*StepLength^2))

