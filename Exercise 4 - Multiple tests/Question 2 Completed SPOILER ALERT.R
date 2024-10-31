
# Generate data and labels ------------------------------------------------

#We could first create labels for TestA, TestB, and TestC
TestA <- "US"
TestB <- "PAG"
TestC <- "TRE"

#Then we will need to create a dataset
#n is of the form : (TestA+ TestB+ Test C+), (TestA+ TestB- TestC+), (TestA- TestB+ TestC+), (TestA- TestB- TestC+), (TestA+ TestB+ TestC-), (TestA+ TestB- TestC-), (TestA- TestB+ TestC-), (TestA- Test- TestC-)
datalist <- list(n=c(126, 0, 3, 0, 12, 1, 7, 112))

#We will also need the total number of individuals tested (n)
n <- sapply(datalist, sum)

# Information for prior distributions -------------------------------------

#Below, we will generate informative priors for Prev, and Se and Sp of US. 
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
# Prevalence:  Mode=0.42, and we are 97.5% sure >0.74 
Prev <- epi.betabuster(mode=0.42, conf=0.975, imsure="less than", x=0.74)  
# Check values for Prevalence
Prev$shape1                #View the a shape parameter 
Prev$shape2                #View the b shape parameter   
#plot the Sp prior distribution
curve(dbeta(x, shape1=Prev$shape1, shape2=Prev$shape2), from=0, to=1, 
      main="Prior for pregnancy prevalence", xlab = "Proportion", ylab = "Density")


# Assign values generated -------------------------------------------------
Prev.shapea <- Prev$shape1         #a shape parameter for Prev    
Prev.shapeb <- Prev$shape2         #b shape parameter for Prev

Se.TestA.shapea <- Se.US$shape1     #a shape parameter for Se of TestA
Se.TestA.shapeb <- Se.US$shape2     #b shape parameter for Se of TestA
Sp.TestA.shapea <- Sp.US$shape1     #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- Sp.US$shape2     #b shape parameter for Sp of TestA

#We could use vague priors for Se and Sp of PAG
Se.TestB.shapea <- 1     #a shape parameter for Se of TestB
Se.TestB.shapeb <- 1     #b shape parameter for Se of TestB
Sp.TestB.shapea <- 1     #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 1     #b shape parameter for Sp of TestB

#We could use vague priors for Se and Sp of TRE
Se.TestC.shapea <- 1     #a shape parameter for Se of TestC
Se.TestC.shapeb <- 1     #b shape parameter for Se of TestC
Sp.TestC.shapea <- 1     #a shape parameter for Sp of TestC
Sp.TestC.shapeb <- 1     #b shape parameter for Sp of TestC

# Create the JAGS text file -----------------------------------------------
#Create the JAGS text file
model_3tests_1pop_indep <- paste0("model{

#=== LIKELIHOOD	===#

  n[1:8] ~ dmulti(p[1:8], ",n,")
  p[1] <- Prev*Se_", TestA, "*Se_", TestB, "*Se_", TestC, " + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")*(1-Sp_", TestC,")
  p[2] <- Prev*Se_", TestA, "*(1-Se_", TestB, ")*Se_", TestC, " + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "*(1-Sp_", TestC,")
  p[3] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, " + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")*(1-Sp_", TestC,")
  p[4] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ")*Se_", TestC, " + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "*(1-Sp_", TestC,")
  
  p[5] <- Prev*Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC,") + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")*Sp_", TestC,"
  p[6] <- Prev*Se_", TestA, "*(1-Se_", TestB, ")*(1-Se_", TestC,") + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "*Sp_", TestC,"
  p[7] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC,") + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")*Sp_", TestC,"
  p[8] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ")*(1-Se_", TestC,") + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "*Sp_", TestC,"
  
  
#=== PRIOR	===#

  Prev ~ dbeta(",Prev.shapea,", ",Prev.shapeb,") 	## Prior for Prevalence
  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,") 	## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,") 	## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(",Se.TestB.shapea,", ",Se.TestB.shapeb,") 	## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(",Sp.TestB.shapea,", ",Sp.TestB.shapeb,") 	## Prior for Sp of Test B
  Se_", TestC, " ~ dbeta(",Se.TestC.shapea,", ",Se.TestC.shapeb,") 	## Prior for Se of Test C
  Sp_", TestC, " ~ dbeta(",Sp.TestC.shapea,", ",Sp.TestC.shapeb,") 	## Prior for Sp of Test C
  
}")

#write as a text (.txt) file
write.table(model_3tests_1pop_indep, 
            file="model_3tests_1pop_indep.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)

# Generate initial values -------------------------------------------------
inits <- list(list(Prev=0.50,
                   Se_US=0.90,
                   Sp_US=0.90,
                   Se_PAG=0.90,
                   Sp_PAG=0.90,
                   Se_TRE=0.90,
                   Sp_TRE=0.90),
              
              list(Prev=0.60,
                   Se_US=0.80,
                   Sp_US=0.80,
                   Se_PAG=0.80,
                   Sp_PAG=0.80,
                   Se_TRE=0.80,
                   Sp_TRE=0.80),
              
              list(Prev=0.40,
                   Se_US=0.70,
                   Sp_US=0.70,
                   Se_PAG=0.70,
                   Sp_PAG=0.70,
                   Se_TRE=0.70,
                   Sp_TRE=0.70)
)


# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)

#Run the Bayesian model
bug.out <- jags(data=datalist,                             
                model.file="model_3tests_1pop_indep.txt",     
                parameters.to.save=c("Prev", "Se_US", "Sp_US", "Se_PAG", "Sp_PAG", "Se_TRE", "Sp_TRE"),               
                n.chains=3,                                 
                inits=inits,                                
                n.iter=11000,                                
                n.burnin=1000,                              
                n.thin=1,                                   
                DIC=FALSE)   


# Check diagnostic plots --------------------------------------------------
library(mcmcplots)
bug.mcmc <- as.mcmc(bug.out)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 


# Compute Effective sample size -------------------------------------------
effectiveSize(bug.mcmc)


# Check results -----------------------------------------------------------
print(bug.out, digits.summary=3) 