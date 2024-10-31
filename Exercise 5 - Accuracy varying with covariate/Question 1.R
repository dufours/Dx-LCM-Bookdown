
# Generate data and labels ------------------------------------------------
#n is of the form : (TestA pos and TestB pos), (TestA pos and TestB neg), (TestA neg and TestB pos), then (TestA neg and TestB neg)

##############################################
##############################################
##  BELOW PROVIDE THE DATASETS FOR THE LCM  ##
##############################################
##############################################

datalist <- list(Z0=c(###TO COMPLETE##),
                 Z1=c(###TO COMPLETE##)
                )

#We could first create labels for TestA and TestB
TestA <- "US"
TestB <- "PAG"

#We could create labels for Z=0 and Z=1
Z0 <- "28_35days"
Z1 <- "36_45days"

library(epiR) 

# US sensitivity ----------------------------------------------------------
# Sensitivity of US:  Mode=0.90, and we are 97.5% sure >0.85 
Se.US <- epi.betabuster(mode=0.90, conf=0.975, imsure="greater than", x=0.85)  

# US specificity ----------------------------------------------------------
# Specificity of US:  Mode=0.95, and we are 97.5% sure >0.90 
Sp.US <- epi.betabuster(mode=0.95, conf=0.975, imsure="greater than", x=0.90)  

#Provide information for the prior distributions (all beta distributions) for the unknown parameters 
Prev0.shapea <- 1         #a shape parameter for Prev in Z=0    
Prev0.shapeb <- 1         #b shape parameter for Prev in Z=0

Prev1.shapea <- 1         #a shape parameter for Prev in Z=1    
Prev1.shapeb <- 1         #b shape parameter for Prev in Z=1

Se.TestA.shapea <- Se.US$shape1     #a shape parameter for Se of TestA
Se.TestA.shapeb <- Se.US$shape2     #b shape parameter for Se of TestA
Sp.TestA.shapea <- Sp.US$shape1     #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- Sp.US$shape2     #b shape parameter for Sp of TestA

Se.TestB.Z0.shapea <- 1     #a shape parameter for Se of TestB in Z=0
Se.TestB.Z0.shapeb <- 1     #b shape parameter for Se of TestB in Z=0
Sp.TestB.Z0.shapea <- 1     #a shape parameter for Sp of TestB in Z=0
Sp.TestB.Z0.shapeb <- 1     #b shape parameter for Sp of TestB in Z=0

Se.TestB.Z1.shapea <- 1     #a shape parameter for Se of TestB in Z=1
Se.TestB.Z1.shapeb <- 1     #b shape parameter for Se of TestB in Z=1
Sp.TestB.Z1.shapea <- 1     #a shape parameter for Sp of TestB in Z=1
Sp.TestB.Z1.shapeb <- 1     #b shape parameter for Sp of TestB in Z=1

#We will also need the number of individuals tested in each populations (n0 and n1)
n <- sapply(datalist, sum)
n0 <- n[1]
n1 <- n[2]


# Create the JAGS text file -----------------------------------------------
#Create the JAGS text file

################################################################
################################################################
##          BELOW ADD THE LIKELIHOOD FUNCTION FOR Z=1         ##
################################################################
################################################################

model_2tests_covariate <- paste0("model{

#=== LIKELIHOOD	===#
  #For Z=0
  Z0[1:4] ~ dmulti(p0[1:4], ",n0,")
  p0[1] <- Prev_", Z0,"*Se_", TestA, "*Se_", TestB, "_", Z0," + (1-Prev_", Z0,")*(1-Sp_", TestA, ")*(1-Sp_", TestB, "_", Z0,")
  p0[2] <- Prev_", Z0,"*Se_", TestA, "*(1-Se_", TestB, "_", Z0,") + (1-Prev_", Z0,")*(1-Sp_", TestA, ")*Sp_", TestB, "_", Z0,"
  p0[3] <- Prev_", Z0,"*(1-Se_", TestA, ")*Se_", TestB, "_", Z0," + (1-Prev_", Z0,")*Sp_", TestA, "*(1-Sp_", TestB, "_", Z0,")
  p0[4] <- Prev_", Z0,"*(1-Se_", TestA, ")*(1-Se_", TestB, "_", Z0,") + (1-Prev_", Z0,")*Sp_", TestA, "*Sp_", TestB, "_", Z0,"
  
  #For Z=1
  ################TO COMPLETE##############
   

#=== PRIOR	===#

  Prev_", Z0," ~ dbeta(",Prev0.shapea,", ",Prev0.shapeb,") 	## Prior for Prev in Z=0
  Prev_", Z1," ~ dbeta(",Prev1.shapea,", ",Prev1.shapeb,") 	## Prior for Prev in Z=1
  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,") 	## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,") 	## Prior for Sp of Test A
  Se_", TestB, "_", Z0, " ~ dbeta(",Se.TestB.Z0.shapea,", ",Se.TestB.Z0.shapeb,") 	## Prior for Se of Test B in Z=0
  Sp_", TestB, "_", Z0, " ~ dbeta(",Sp.TestB.Z0.shapea,", ",Sp.TestB.Z0.shapeb,") 	## Prior for Sp of Test B in Z=0
  Se_", TestB, "_", Z1, " ~ dbeta(",Se.TestB.Z1.shapea,", ",Se.TestB.Z1.shapeb,") 	## Prior for Se of Test B in Z=1
  Sp_", TestB, "_", Z1, " ~ dbeta(",Sp.TestB.Z1.shapea,", ",Sp.TestB.Z1.shapeb,") 	## Prior for Sp of Test B in Z=1
  
}")

#write as a text (.txt) file
write.table(model_2tests_covariate, 
            file="model_2tests_covariate.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)


# Generate initial values -------------------------------------------------
inits <- list(list(Prev_28_35days=0.50,
                   Prev_36_45days=0.50,
                   Se_US=0.90,
                   Sp_US=0.90,
                   Se_PAG_28_35days=0.90,
                   Sp_PAG_28_35days=0.90,
                   Se_PAG_36_45days=0.90,
                   Sp_PAG_36_45days=0.90),
              
              list(Prev_28_35days=0.60,
                   Prev_36_45days=0.60,
                   Se_US=0.80,
                   Sp_US=0.80,
                   Se_PAG_28_35days=0.80,
                   Sp_PAG_28_35days=0.80,
                   Se_PAG_36_45days=0.80,
                   Sp_PAG_36_45days=0.80),
              
              list(Prev_28_35days=0.40,
                   Prev_36_45days=0.40,
                   Se_US=0.70,
                   Sp_US=0.70,
                   Se_PAG_28_35days=0.70,
                   Sp_PAG_28_35days=0.70,
                   Se_PAG_36_45days=0.70,
                   Sp_PAG_36_45days=0.70)
)


# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)

#Run the Bayesian model
#######################################################
#######################################################
## COMPLETE THE INFO NEEDED FOR THE JAGS() FUNCTION  ##
#######################################################
#######################################################

bug.out <- jags(###########TO COMPLETE#############)


# Check diagnostic plots --------------------------------------------------
library(mcmcplots)
bug.mcmc <- as.mcmc(bug.out)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 


# Compute Effective sample size -------------------------------------------
effectiveSize(bug.mcmc)


# Check results -----------------------------------------------------------
print(bug.out, digits.summary=3) 