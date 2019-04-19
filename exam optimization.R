library(lpSolveAPI)

## Set the coefficients of the decision variables
objective.in  <- c(25,  20)

## Create constraint martix
const.mat <- matrix(c(20,  12,  4,  4),  nrow=2, byrow=TRUE)

## define constraints
time_constraint <- (8*60)
resource_constraint <- 1800

## RHS for the constraints
const.rhs <- c(resource_constraint, time_constraint)

## Constraints direction
const.dir  <- c("<=",  "<=")

## Find the optimal solution
optimum <-  lp(direction="max",  objective.in, const.mat, const.dir,  const.rhs)

optimum$solution