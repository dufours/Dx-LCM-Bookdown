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
  T_pos ~ dbin(Se, True_pos)
  T_neg ~ dbin(Sp, True_neg)

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
bug.out <- jags(data=datalist,                             
                model.file="model_single_test_accuracy.txt",         
                parameters.to.save=c("Se", "Sp"),               
                n.chains=3,                           #3 chains
                inits=inits,                                  
                n.iter=11000,                         #Ran for 10,000 iterations               
                n.burnin=1000,                        #First 1,000 iterations will be "burned"            
                n.thin=1,                                   
                DIC=FALSE)                                   

# Check diagnostic plots -------------------------------------------------- 
library(mcmcplots)
bug.mcmc <- as.mcmc(bug.out)          
mcmcplot(bug.mcmc, title="Diagnostic plots")

# Compute Effective sample size -------------------------------------------
effectiveSize(bug.mcmc)   

# Check results -----------------------------------------------------------
print(bug.out,                  
      digits.summary=3) 