# Generate data ------------------------------------------------
#To compute the test sensitivity, we need to use the number of test positive (138) among true positive (139). 
#For the specificity, we would need the number of test negative (113) among true negative (123).
#We could provide two datasets (one for each) and ran two models. But we could also do everything at once...
datalist <- list(T_pos=138, True_pos=139, T_neg=113, True_neg=123)


# Information for prior distributions -------------------------------------
#We could use a beta distribution. To use a vague prior, beta(1.0, 1.0) would work
Se.shapea <- 1         #a shape parameter for Se    
Se.shapeb <- 1         #b shape parameter for Se 
Sp.shapea <- 1         #a shape parameter for Sp    
Sp.shapeb <- 1         #b shape parameter for Sp 

# Create the JAGS text file -----------------------------------------------
model_single_test_accuracy <- paste0("model{
#=== LIKELIHOOD	===#

########################################
########################################
######## TO COMPLETE  ##################
########################################
## WRITE THE LIKELIHOOD FUNCTIONs (2) ##
## 1.LINKING T_pos and True_pos to Se ##
## 1.LINKING T_neg and True_neg to Sp ##
########################################
########################################

#=== PRIOR	===#
  Se ~ dbeta(",Se.shapea,", ",Se.shapeb,") 	## Prior for Se
  Sp ~ dbeta(",Sp.shapea,", ",Sp.shapeb,") 	## Prior for Sp
}")

#write as a text (.txt) file
write.table(model_single_test_accuracy, 
            file="model_single_test_accuracy.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)

# Generate initial values -------------------------------------------------
inits <- list(
  list(Se=0.10, Sp=0.10),
  list(Se=0.50, Sp=0.50),
  list(Se=0.90, Sp=0.90)
)

# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)


###################################
###################################
######## TO COMPLETE  #############
###################################
## USE jags() TO RUN THE MODEL   ##
##       ASK FOR: 3 CHAINS       ##
##       11,000 ITERATIONS       ##
##    ND A BURN-IN of 1,000      ##
###################################
###################################


# Check diagnostic plots --------------------------------------------------  
library(mcmcplots)

###################################
###################################
######## TO COMPLETE  #############
###################################
##   ASK TO SEE THE DIAGNOSTIC   ##
##          PLOTS                ##
###################################
###################################

# Compute Effective sample size -------------------------------------------
effectiveSize(bug.mcmc)   

# Check results -----------------------------------------------------------
print(bug.out,                  
      digits.summary=3) 