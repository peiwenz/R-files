library(lpSolve)

# ## Set the coefficients of the decision variables
# objective.in  <- c(1,  1, 1)
# 
# ## Create constraint martix
# const.mat <- matrix(c(2, 3, 4, 
#                       160, 200, 197, 
#                       1,0,0,
#                       0,1,0,
#                       0,0,1),  nrow=5, byrow=TRUE)
# 
# ## define constraints
# resource_constraint <- (540*10)
# time_constraint <- (540*365*0.75)
# type1 <- 0
# type2 <- 0
# type3 <- 500
# 
# ## RHS for the constraints
# const.rhs <- c(resource_constraint,time_constraint, type1, type2, type3)
# 
# ## Constraints direction
# const.dir  <- c("<=",  "<=", ">=", ">=", ">=")
# 
# ## Find the optimal solution
# optimum <-  lp(direction="max",  objective.in, const.mat, const.dir,  const.rhs)
# 
# round(optimum$solution)
# 
# #summary(optimum)
# 
# 
# 







##############################
##############################
## Set the coefficients of the decision variables
objective.in  <- c(1, 1, 1,1,1)

## Create constraint martix
const.mat <- matrix(c(3, 3, 6,5,3, 
                      107,135,181,153,129,
                      1,0,0,0,-1,
                      0,1,0,0,0,
                      0,-1,1,0,0,
                      -1,0,0,1,0,
                      0,0,-1,0,1,
                      1,0,0,0,0,
                      0,1,0,0,0,
                      0,0,1,0,0,
                      0,0,0,1,0,
                      0,0,0,0,1,
                      1,0,0,0,0,
                      0,1,0,0,0,
                      0,0,1,0,0,
                      0,0,0,1,0,
                      0,0,0,0,1),  nrow=17, byrow=TRUE)

## define constraints
resource_constraint <- (545*8)
time_constraint <- (545*365*0.75)
cm <- 0
cc <- 0
dv <- 0
rt <- 0
te <- 0
type1_low <- 0
type2_low  <- 40
type3_low  <- 80
type4_low  <- 400
type5_low  <- 0
type1_up <- 300
type2_up <- 1000
type3_up <- 1000
type4_up <- 800
type5_up <- 100

## RHS for the constraints
const.rhs <- c(resource_constraint,
               time_constraint,
               cm,cc,dv,rt,te,
               type1_low ,type2_low ,type3_low ,type4_low ,type5_low,
               type1_up,type2_up,type3_up,type4_up,type5_up)

## Constraints direction
const.dir  <- c("<=", "<=", 
                ">", ">", ">", ">", ">", 
                ">", ">", ">", ">", ">", 
                "<", "<", "<", "<", "<")

## Find the optimal solution
optimum <-  lp(direction="max",  objective.in, const.mat, const.dir,  const.rhs)
round(optimum$solution)
sum(optimum$solution)
op<- round(optimum$solution)
s<- sum(optimum$solution)
op <- as.data.frame(op)
t <- rbind(op, s)
t<- as.data.frame(t)
t <- round(t)
n <- c("Capital Market","Clearing/Carrying","Diversified","Retail","Trading/Execution","Total")
row.names(t) 
#summary(optimum)
