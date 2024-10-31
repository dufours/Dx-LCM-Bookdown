
# Generate data and labels ------------------------------------------------

#First we will need to create a dataset
#n is of the form : (TestA pos and TestB pos), (TestA pos and TestB neg), (TestA neg and TestB pos), then (TestA neg and TestB neg)
datalist <- list(n=c(138,1,10,113))

#We will also need the total number of individuals tested (n)
n <- sapply(datalist, sum)

#We could create labels for TestA and TestB
TestA <- "US"
TestB <- "PAG"


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

#I could use vague priors for Se and Sp of PAG
Se.TestB.shapea <- 1     #a shape parameter for Se of TestB
Se.TestB.shapeb <- 1     #b shape parameter for Se of TestB
Sp.TestB.shapea <- 1     #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 1     #b shape parameter for Sp of TestB


# Create the JAGS text file -----------------------------------------------

###########################################################################
###########################################################################
##   IN THE LIKELIHOOD FUNCTION BELOW WE DID NOT ADD THE covp AND covn   ##
##    PARAMETERS IN THE 4 EQUATIONS. ADD THEM TO ALLOW FOR CONDITIONAL   ##
##      DEPENDENCE BETWEEN TESTS (I.E., REPLACE THE ???). THE REST OF    ##
##                         THE MODEL SHOULD BE OK                        ##
###########################################################################
###########################################################################

#Create the JAGS text file
model_2tests_1pop_dep <- paste0("model{

#=== LIKELIHOOD	===#

  n[1:4] ~ dmulti(p[1:4], ",n,")
  p[1] <- Prev*(Se_", TestA, "*Se_", TestB, " ???) + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB, ") ???)
  p[2] <- Prev*(Se_", TestA, "*(1-Se_", TestB, ") ???) + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB, " ???)
  p[3] <- Prev*((1-Se_", TestA, ")*Se_", TestB, " ???) + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB, ") ???)
  p[4] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB, ") ???) + (1-Prev)*(Sp_", TestA, "*Sp_", TestB, " ???)

#=== PRIOR	===#

  Prev ~ dbeta(",Prev.shapea,", ",Prev.shapeb,") 	## Prior for Prev
  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,") 	## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,") 	## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(",Se.TestB.shapea,", ",Se.TestB.shapeb,") 	## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(",Sp.TestB.shapea,", ",Sp.TestB.shapeb,") 	## Prior for Sp of Test B
  
#=== CONDITIONAL DEPENDENCE STRUCTURE ===#

  covp ~ dunif(minp,maxp)
  covn ~ dunif(minn,maxn)
  minp <- (1-Se_", TestA, ")*(Se_", TestB, "-1)
  minn <- (Sp_", TestA, "-1)*(1-Sp_", TestB, ")
  maxp <- min(Se_", TestA, ",Se_", TestB, ") - Se_", TestA, "*Se_", TestB, "	
  maxn <- min(Sp_", TestA, ",Sp_", TestB, ") - Sp_", TestA, "*Sp_", TestB, "  
  
}")

#write as a text (.txt) file
write.table(model_2tests_1pop_dep, 
            file="model_2tests_1pop_dep.txt", 
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
                   covp=0,
                   covn=0),
              
              list(Prev=0.70,
                   Se_US=0.10,
                   Sp_US=0.10,
                   Se_PAG=0.10,
                   Sp_PAG=0.10,
                   covp=0.01,
                   covn=0.01),
              
              list(Prev=0.50,
                   Se_US=0.50,
                   Sp_US=0.50,
                   Se_PAG=0.50,
                   Sp_PAG=0.50,
                   covp=0.05,
                   covn=0.05)
)


# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)
bug.out <- jags(data=datalist,                             
                model.file="model_2tests_1pop_dep.txt",     
                parameters.to.save=c("Prev", "Se_US", "Sp_US", "Se_PAG", "Sp_PAG", "covp", "covn"),               
                n.chains=3,                                 
                inits=inits,                                
                n.iter=201000,                                
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