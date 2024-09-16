# Generate data ------------------------------------------------
datalist <- list(T=139, n=262)

# Information for prior distributions -------------------------------------
#We will need a prior distribution for the prevalence of disease corresponding to mode of 0.42 and 97.5th percentile of 0.74
#We could use a beta distribution. To find the corresponding a and b parameters of the beta distribution we can use:
library(epiR) 
rval <- epi.betabuster(mode=0.42, conf=0.975, greaterthan=F, x=0.74)  
rval$shape1                
rval$shape2                  

#To "eyeball this prior distribution:
curve(dbeta(x, 
            shape1=rval$shape1, 
            shape2=rval$shape2), 
      from=0, to=1, 
      main="Prior for prevalence of pregnancy", 
      xlab = "Proportion", 
      ylab = "Density")

#Now we need to update the JAGS model with this informative prior distribution
#Let assign correct values to Prev.shapea and Prev.shapeb
Prev.shapea <- rval$shape1
Prev.shapeb <- rval$shape2

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