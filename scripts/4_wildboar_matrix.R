remotes::install_github("jonesor/Rcompadre", build_opts = NULL)

library(Rcompadre)
comadre <- cdb_fetch("comadre")

comadre_wildboar <- comadre %>%
  filter(SpeciesAccepted == "Sus scrofa") %>%
  select(mat, MatrixID, DOI_ISBN)


comadre_wildboar <- filter(comadre, )

comadre_wildboar$mat[[1]]









matrixes <- readRDS('data/TransitionMatrices.rds')
(initial_trans_mat <- matrixes$wildboar$RS_TransitionMatrix)
#trans_mat[2:4,2] <- trans_mat[3:5,2]

trans_mat[3,2] <- sum(initial_trans_mat[3:4,2])
trans_mat[4,2] <- 0
trans_mat[4,3] <- initial_trans_mat[4,2] + initial_trans_mat[4,3]


state_xt <- c(40, 20, 15, 20, 10)


(state_xtplus1 <- trans_mat %*% state_xt)



state_ts <- as.matrix(state_xt)


nr_years <- 50
for(t in 1:nr_years){
  state_ts <- cbind(state_ts, trans_mat %*% state_ts[,t])
}

# Now plot:
nr_stages <- dim(trans_mat)[1]
{
  plot(0:nr_years, state_ts[1,], type = "b", col = 1, 
       xlab = "time step",
       ylab = "Population count", ylim = c(0, max(state_ts)),
       main = "Time series of matrix model")
  for(s in 2:nr_stages){
    lines(0:nr_years, state_ts[s,], type = "b", col = s)
  }
  legend("topleft", 
         legend = paste0("stage-",1:nr_stages),
         col = 1:nr_stages, lty = 1)
}


## 4.) Eigen analysis of transition matrix

# Use the built-in R function for Eigen analysis:
trans_mat_Eigen <- eigen(trans_mat)

# The resulting object contains the eigenvectors and their corresponding eigenvalues.
# In this step we will use the eigenvalues only and look at the eigenvectors in the next step:

(trans_mat_EValues <- trans_mat_Eigen$values)

# Determine the asymptotic growth rate (leading eigenvalue):
(growth_rate <- Re(trans_mat_EValues[1]))


## 5.) Stable age/stage distribution

# Get first eigenvector
stable_dist <- trans_mat_Eigen$vectors[,1]

# Get rid of imaginary part
stable_dist <- Re(stable_dist)

# Normalise
(stable_dist <- stable_dist/sum(stable_dist))


## 6.) Sensitivity & Elasticity

# There is an R package for the analysis of transition matrices in R: 
# 'popbio: Construction and Analysis of Matrix Population Models'

# We will calculate sensitivity and elasticity by hand:

# Get the leading left eigenvector as the eigenvector of the transposed transition matrix:
lev <- t(eigen(t(trans_mat))$vectors[,1])

# Calculate sensitivity as the outer product of the transposed conjugate left eigenvector 
# and the right eigenvector (both corresponding to the dominant eigenvalue), normalised
# by their inner product:
sensity <- (t(Re(lev)) %*% stable_dist) / as.numeric(Re(lev) %*% stable_dist)

# Then, the elasticity is the inverse asymptotic growth rate times the matrix product
# of sensitivity and transition matrix:
(elasty <- (trans_mat/ growth_rate) * sensity )
