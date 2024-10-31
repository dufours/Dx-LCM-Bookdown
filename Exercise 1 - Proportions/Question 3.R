
# Generate data ------------------------------------------------
datalist <- list(T=139, n=262)

# Information for prevalence prior distributions -------------------------------------
#We could use a beta distribution for the prevalence. To use a vague prior, beta(1.0, 1.0) would work
Prev.shapea <- 1         #a shape parameter for Prev    
Prev.shapeb <- 1         #b shape parameter for Prev 

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


# Create the JAGS text file -----------------------------------------------
model_single_prop_true_prev <- paste0("model{
#=== LIKELIHOOD	===#
  T ~ dbin(AppPrev, n)

##################################################################
##################################################################
####################### TO COMPLETE  #############################
##################################################################
############## Complete THE LIKELIHOOD FUNCTION ##################
##   to link apparent (AppPRev) and true prevalence (Prev)     ###
##################################################################
##################################################################

#=== PRIOR	===#

################################################################
################################################################
####################### TO COMPLETE  ###########################
## Below we added priors for Se and Sp. Do you need to add    ##
##                     other priors?                          ##
################################################################
################################################################

  Se ~ dbeta(",Se.US$shape1,", ",Se.US$shape2,") 	## Prior for ultrasound sensitivity
  Sp ~ dbeta(",Sp.US$shape1,", ",Sp.US$shape2,") 	## Prior for ultrasound specificity
}")

#write as a text (.txt) file
write.table(model_single_prop_true_prev, 
            file="model_single_prop_true_prev.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)

# Generate initial values -------------------------------------------------
inits <- list(
  list(Prev=0.10,
       Se=0.90,
       Sp=0.90),
  list(Prev=0.50,
       Se=0.70,
       Sp=0.70),
  list(Prev=0.90,
       Se=0.95,
       Sp=0.95)
)

# Run the model -----------------------------------------------------------
library(R2jags)
library(coda)
bug.out <- jags(data=datalist,                             
                model.file="model_single_prop_true_prev.txt",         
                parameters.to.save=c("Prev", "AppPrev"),               
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
