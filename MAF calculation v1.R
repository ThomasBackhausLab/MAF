###########
# Calculates the MAF(exact) value for a given mixture. 
# 
# See https://doi.org/10.21203/rs.3.rs-1986611/v1
###########

########
# Arbitrary example with a 10-compound mixture
# RQ: risk quotients for each mixture component
#
# No missings, no zeroes.
# 
########
RQ<-c(0.8,0.3,0.3,0.1,0.01,0.01,0.01,0.01,0.01,0.01)


# Target for the optimisation, i.e. the sum of
# risk quotients that is deemed acceptable
Acceptable_RQ_Sum <- 1

# Function that is to be minimized in the call to optimize
f <- function (MAF, RQ, Acceptable_RQ_Sum) 
{
  RQ_temp <- RQ
  RQ_temp[RQ>1/MAF] <- 1/MAF
  
  # Calculate resulting STU
  RQ_Sum<-sum(RQ_temp)
  
  # returnvalue (minimization problem)
  # i.e. difference to sum of RQs=Acceptable_RQ_Sum is minimized
  # (down to zero)
  abs(RQ_Sum-Acceptable_RQ_Sum)
}

# calculate MAF(exact)
MAF <- optimize(f, c(1, length(RQ)),RQ, Acceptable_RQ_Sum,maximum=FALSE)$minimum
