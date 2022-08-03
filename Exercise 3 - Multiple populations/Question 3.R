
# Generate data and labels ------------------------------------------------

#First we will need to create a dataset
#n is of the form : (TestA pos and TestB pos), (TestA pos and TestB neg), (TestA neg and TestB pos), then (TestA neg and TestB neg)

##################################################
##################################################
##       ADD THE DATA FOR THE 3RD HERD          ##
##################################################
##################################################
datalist <- list(Pop1=c(138,1,10,113),
                 Pop2=c(71, 0, 8, 69)
                 )

#We will also need the total number of individuals tested (n)
#I will also need the total number of individuals tested in each population (nPop1 and nPop2)
n <- sapply(datalist, sum)
nPop1 <- n[1]
nPop2 <- n[2]
nPop3 <- n[3]

#We could create labels for TestA and TestB
TestA <- "US"
TestB <- "PAG"


# Information for prior distributions -------------------------------------

#Below, we will generate informative priors for Prev, and Se and Sp of US. 
library(epiR) 

# US sensitivity ----------------------------------------------------------
# Sensitivity of US:  Mode=0.90, and we are 97.5% sure >0.85 
Se.US <- epi.betabuster(mode=0.90, conf=0.975, greaterthan=T, x=0.85)  
# Check values for Se
Se.US$shape1                #View the a shape parameter 
Se.US$shape2                #View the b shape parameter   
#plot the Se prior distribution
curve(dbeta(x, shape1=Se.US$shape1, shape2=Se.US$shape2), from=0, to=1, 
      main="Prior for ultrasound exam sensitivity", xlab = "Proportion", ylab = "Density")


# US specificity ----------------------------------------------------------
# Specificity of US:  Mode=0.95, and we are 97.5% sure >0.90 
Sp.US <- epi.betabuster(mode=0.95, conf=0.975, greaterthan=T, x=0.90)  
# Check values for Sp
Sp.US$shape1                #View the a shape parameter 
Sp.US$shape2                #View the b shape parameter   
#plot the Sp prior distribution
curve(dbeta(x, shape1=Sp.US$shape1, shape2=Sp.US$shape2), from=0, to=1, 
      main="Prior for ultrasound exam specificity", xlab = "Proportion", ylab = "Density")


# Prevalence --------------------------------------------------------------
# Prevalence:  Mode=0.42, and we are 97.5% sure >0.74 
Prev <- epi.betabuster(mode=0.42, conf=0.975, greaterthan=F, x=0.74)  
# Check values for Prevalence
Prev$shape1                #View the a shape parameter 
Prev$shape2                #View the b shape parameter   
#plot the Sp prior distribution
curve(dbeta(x, shape1=Prev$shape1, shape2=Prev$shape2), from=0, to=1, 
      main="Prior for pregnancy prevalence", xlab = "Proportion", ylab = "Density")


# Assign values generated -------------------------------------------------
# I will use the same priors for prevalence of pregnancy in the three herds

Prev1.shapea <- Prev$shape1         #a shape parameter for Prev in population 1    
Prev1.shapeb <- Prev$shape2         #b shape parameter for Prev in population 1

Prev2.shapea <- Prev$shape1         #a shape parameter for Prev in population 2    
Prev2.shapeb <- Prev$shape2         #b shape parameter for Prev in population 2

##################################################
##################################################
##    ASSIGN THE SHAPE PARAMETERS FOR THE       ##
##   3RD HERD PREVALENCE PRIOR DISTRIBUTION     ##
##################################################
##################################################

Se.TestA.shapea <- Se.US$shape1     #a shape parameter for Se of TestA
Se.TestA.shapeb <- Se.US$shape2     #b shape parameter for Se of TestA
Sp.TestA.shapea <- Sp.US$shape1     #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- Sp.US$shape2     #b shape parameter for Sp of TestA

#I could use vague priors for Se and Sp of PAG
Se.TestB.shapea <- 1     #a shape parameter for Se of TestB
Se.TestB.shapeb <- 1     #b shape parameter for Se of TestB
Sp.TestB.shapea <- 1     #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 1     #b shape parameter for Sp of TestB


# Create the JAGS text file -----------------------------------------------
#Create the JAGS text file
model_2tests_3pop_indep <- paste0("model{

#=== LIKELIHOOD	===#

  #=== POPULATION 1 ===#
  Pop1[1:4] ~ dmulti(p1[1:4], ",nPop1,")
  p1[1] <- Prev1*Se_", TestA, "*Se_", TestB, " + (1-Prev1)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")
  p1[2] <- Prev1*Se_", TestA, "*(1-Se_", TestB, ") + (1-Prev1)*(1-Sp_", TestA, ")*Sp_", TestB, "
  p1[3] <- Prev1*(1-Se_", TestA, ")*Se_", TestB, " + (1-Prev1)*Sp_", TestA, "*(1-Sp_", TestB, ")
  p1[4] <- Prev1*(1-Se_", TestA, ")*(1-Se_", TestB, ") + (1-Prev1)*Sp_", TestA, "*Sp_", TestB, "

  #=== POPULATION 2 ===#  
  Pop2[1:4] ~ dmulti(p2[1:4], ",nPop2,")
  p2[1] <- Prev2*Se_", TestA, "*Se_", TestB, " + (1-Prev2)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")
  p2[2] <- Prev2*Se_", TestA, "*(1-Se_", TestB, ") + (1-Prev2)*(1-Sp_", TestA, ")*Sp_", TestB, "
  p2[3] <- Prev2*(1-Se_", TestA, ")*Se_", TestB, " + (1-Prev2)*Sp_", TestA, "*(1-Sp_", TestB, ")
  p2[4] <- Prev2*(1-Se_", TestA, ")*(1-Se_", TestB, ") + (1-Prev2)*Sp_", TestA, "*Sp_", TestB, "

###################################################
###################################################
##  ADD THE LIKELIHOOD FUNCTION FOR THE 3RD HERD ##  
###################################################
###################################################
  
#=== PRIOR	===#

  Prev1 ~ dbeta(",Prev1.shapea,", ",Prev1.shapeb,") 	## Prior for Prevalence in population 1
  Prev2 ~ dbeta(",Prev2.shapea,", ",Prev2.shapeb,") 	## Prior for Prevalence in population 2

##################################################
##################################################
##       ADD THE PRIOR FOR THE 3RD HERD         ##
##################################################
##################################################

  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,") 	## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,") 	## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(",Se.TestB.shapea,", ",Se.TestB.shapeb,") 	## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(",Sp.TestB.shapea,", ",Sp.TestB.shapeb,") 	## Prior for Sp of Test B
  
}")

#write as a text (.txt) file
write.table(model_2tests_3pop_indep, 
            file="model_2tests_3pop_indep.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)


# Generate initial values -------------------------------------------------
inits <- list(list(Prev1=0.50,
                   Prev2=0.50,
                   Prev3=0.50,
                   Se_US=0.90,
                   Sp_US=0.90,
                   Se_PAG=0.90,
                   Sp_PAG=0.90),
              
              list(Prev1=0.60,
                   Prev2=0.60,
                   Prev3=0.60,
                   Se_US=0.80,
                   Sp_US=0.80,
                   Se_PAG=0.80,
                   Sp_PAG=0.80),
              
              list(Prev1=0.40,
                   Prev2=0.40,
                   Prev3=0.40,
                   Se_US=0.70,
                   Sp_US=0.70,
                   Se_PAG=0.70,
                   Sp_PAG=0.70)
              )

# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)

#Run the Bayesian model
bug.out <- jags(data=datalist,                             
                model.file="model_2tests_3pop_indep.txt",     
                parameters.to.save=c("Prev1", "Prev2", "Prev3", "Se_US", "Sp_US", "Se_PAG", "Sp_PAG"),               
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