
# Generate data ------------------------------------------------
datalist <- list(T=139, n=262)

# Information for prior distributions -------------------------------------
#We could use a beta distribution. To use a vague prior, beta(1.0, 1.0) would work
Prev.shapea <- 1         #a shape parameter for Prev    
Prev.shapeb <- 1         #b shape parameter for Prev 

# Create the JAGS text file -----------------------------------------------
model_single_prop <- paste0("model{
#=== LIKELIHOOD	===#
  T ~ dbin(Prev, n)

#=== PRIOR	===#
  Prev ~ dbeta(",Prev.shapea,", ",Prev.shapeb,") 	## Prior for Prev
}")

#write as a text (.txt) file
write.table(model_single_prop, 
            file="model_single_prop.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)

# Generate initial values -------------------------------------------------
inits <- list(
  list(Prev=0.10),
  list(Prev=0.50),
  list(Prev=0.90)
)

# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)
bug.out <- jags(data=datalist,                             
                model.file="model_single_prop.txt",         
                parameters.to.save=c("Prev"),               
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