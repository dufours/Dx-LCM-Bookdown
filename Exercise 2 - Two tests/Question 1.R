
# Generate data and labels ------------------------------------------------

#First we will need to create a dataset
#n is of the form : (TestA pos and TestB pos), (TestA pos and TestB neg), (TestA neg and TestB pos), then (TestA neg and TestB neg)

###########################################################
###########################################################
##     PROVIDE THE DATASET BASED ON THE 2X2 TABLE        ##
###########################################################
###########################################################

#We will also need the total number of individuals tested (n)
n <- sapply(datalist, sum)

#We could create labels for TestA and TestB
TestA <- "US"
TestB <- "PAG"


# Information for prior distributions -------------------------------------

#Below, We will generate informative priors for Prev, and Se and Sp of US. 
library(epiR) 

# US sensitivity ----------------------------------------------------------
# Sensitivity of US:  Mode=0.90, and we are 97.5% sure >0.85 
Se.US <- epi.betabuster(mode=0.90, conf=0.975, imsure="greater than", x=0.85)  
# Check values for Se
Se.US$shape1                #View the a shape parameter 
Se.US$shape2                #View the b shape parameter   
#plot the Se prior distribution
curve(dbeta(x, shape1=Se.US$shape1, shape2=Se.US$shape2), from=0, to=1, 
      main="Prior for ultrasound exam sensitivity", xlab = "Proportion", ylab = "Density")


# US specificity ----------------------------------------------------------
# Specificity of US:  Mode=0.95, and we are 97.5% sure >0.90 
Sp.US <- epi.betabuster(mode=0.95, conf=0.975, imsure="greater than", x=0.90)  
# Check values for Sp
Sp.US$shape1                #View the a shape parameter 
Sp.US$shape2                #View the b shape parameter   
#plot the Sp prior distribution
curve(dbeta(x, shape1=Sp.US$shape1, shape2=Sp.US$shape2), from=0, to=1, 
      main="Prior for ultrasound exam specificity", xlab = "Proportion", ylab = "Density")


# Prevalence --------------------------------------------------------------

###########################################################################
###########################################################################
##  TRY TO GENERATE THE PRIOR DISTRIBUTION FOR PREVALENCE OF PREGNANCY   ##
##     USE THE SAME TECHNIQUE AS FOR US SENSITIVITY AND SPECIFICITY      ##
###########################################################################
###########################################################################

# Assign values generated -------------------------------------------------
######################################################################################
Prev.shapea <- ######### ADD THE NAME OF THE PREVALENCE a SHAPE PARAMETER HERE #######   
Prev.shapeb <- ######### ADD THE NAME OF THE PREVALENCE b SHAPE PARAMETER HERE #######
######################################################################################

Se.TestA.shapea <- Se.US$shape1     #a shape parameter for Se of TestA
Se.TestA.shapeb <- Se.US$shape2     #b shape parameter for Se of TestA
Sp.TestA.shapea <- Sp.US$shape1     #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- Sp.US$shape2     #b shape parameter for Sp of TestA

#We could use vague priors for Se and Sp of PAG
Se.TestB.shapea <- 1     #a shape parameter for Se of TestB
Se.TestB.shapeb <- 1     #b shape parameter for Se of TestB
Sp.TestB.shapea <- 1     #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 1     #b shape parameter for Sp of TestB


# Create the JAGS text file -----------------------------------------------
model_2tests_1pop_indep <- paste0("model{

#=== LIKELIHOOD	===#

  n[1:4] ~ dmulti(p[1:4], ",n,")
  p[1] <- Prev*Se_", TestA, "*Se_", TestB, " + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")
  p[2] <- Prev*Se_", TestA, "*(1-Se_", TestB, ") + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "
  p[3] <- Prev*(1-Se_", TestA, ")*Se_", TestB, " + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")
  p[4] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ") + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "

#=== PRIOR	===#

  Prev ~ dbeta(",Prev.shapea,", ",Prev.shapeb,") 	## Prior for Prev
  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,") 	## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,") 	## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(",Se.TestB.shapea,", ",Se.TestB.shapeb,") 	## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(",Sp.TestB.shapea,", ",Sp.TestB.shapeb,") 	## Prior for Sp of Test B
  
}")

#write as a text (.txt) file
write.table(model_2tests_1pop_indep, 
            file="model_2tests_1pop_indep.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)


# Generate initial values -------------------------------------------------
inits <- list(list(Prev=0.50,
                   Se_US=0.90,
                   Sp_US=0.90,
                   Se_PAG=0.90,
                   Sp_PAG=0.90),
              
              list(Prev=0.70,
                   Se_US=0.10,
                   Sp_US=0.10,
                   Se_PAG=0.10,
                   Sp_PAG=0.10),
              
              list(Prev=0.50,
                   Se_US=0.50,
                   Sp_US=0.50,
                   Se_PAG=0.50,
                   Sp_PAG=0.50)
)


# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)
bug.out <- jags(
                #########################################################################
                #########################################################################
                ##        COMPLETE ALL THE INFORMATION NEEDED TO RUN THE LCM           ##
                ##      USE 3 CHAINS, A BURN-IN OF 1000, AND 11,000 ITERATIONS         ##
                #########################################################################
                #########################################################################  
                )  


# Check diagnostic plots --------------------------------------------------
library(mcmcplots)
bug.mcmc <- as.mcmc(bug.out)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 


# Compute Effective sample size -------------------------------------------
effectiveSize(bug.mcmc)


# Check results -----------------------------------------------------------
print(bug.out, digits.summary=3) 