##============= PRIORS ==============##
library(epiR) 
##=============  mu    ==============##
# Prevalence as Mode=0.42, and we are 95% sure <0.74 
rval <- epi.betabuster(mode=0.42, 
                       conf=0.95, 
                       imsure="less than", 
                       x=0.74)  

rval$shape1                #View the a shape parameter in rval
rval$shape2                #View the b shape parameter in rval

#Assign the generated values for later usage. 
#mu.shapea <- rval$shape1
#mu.shapeb <- rval$shape2  

mu.shapea <- 1.0
mu.shapeb <- 1.0 

#plot the prior distribution
curve(dbeta(x, 
            shape1=mu.shapea, 
            shape2=mu.shapeb), 
      from=0, to=1, 
      main="Hyperprior for mu (the mean within-herd prevalence)", 
      xlab = "Proportion", 
      ylab = "Density")


##=============  psi  ==============##
fifty.psi <- epi.betabuster(mode=0.42, 
                            conf = 0.90, 
                            imsure="less than", 
                            x=0.65)  

# Computing the value for the 50th percentile for the gamma distribution for psi 
fifty <- fifty.psi$shape1 + fifty.psi$shape2 

ninety.psi <- epi.betabuster(mode=0.42, 
                             conf = 0.90, 
                             imsure="less than", 
                             x=0.75)  

# Computing the value for the 50th percentile for the gamma distribution for psi 
ninety <- ninety.psi$shape1 + fifty.psi$shape2 

source('Functions/GammaParmsFromQuantiles.R')
psi.gamma <- gamma.parms.from.quantiles(q=c(fifty, ninety), p=c(0.50, 0.90), plot=T)

#psi.shapea <- psi.gamma$shape   # a shape parameter for gamma
#psi.shapeb <- psi.gamma$rate    # b shape parameter for gamma

psi.shapea <- 0.001   # a shape parameter for gamma
psi.shapeb <- 0.001    # b shape parameter for gamma

##=============  Se and Sp  ==============##
Se.TestA.shapea <- 1       #a shape parameter for Se of TestA
Se.TestA.shapeb <- 1       #b shape parameter for Se of TestA
Sp.TestA.shapea <- 1       #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- 1       #b shape parameter for Sp of TestA

Se.TestB.shapea <- 1       #a shape parameter for Se of TestB
Se.TestB.shapeb <- 1       #b shape parameter for Se of TestB
Sp.TestB.shapea <- 1       #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 1       #b shape parameter for Sp of TestB

##=============  Data ====================##
library(readxl)
obs <- read_excel("Data/Herd level results.xlsx")
#Check the data
obs


#Compute total number of animals by herd
totals <- rowSums(obs)
#Check the total number of animals per herd
totals

datalist <- list(obs=obs, totals=totals)
#Check the datalist
datalist

#Compute number of population
numherd <- dim(obs)[1]
#Check number of herd
numherd

##================ TESTS labels =============##
TestA <- "US"
TestB <- "PAG"

##========  WRITE MODEL  ===================##
#Create the JAGS text file
model_2tests_hierarchical <- paste0("model{

#=== LIKELIHOOD	===#
  for (i in 1:", numherd,"){
    obs[i, 1:4] ~ dmulti(p[i, 1:4], totals[i])
    
    p[i, 1] <- prev[i]*Se_", TestA, "*Se_", TestB, " + (1-prev[i])*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")
    p[i, 2] <- prev[i]*Se_", TestA, "*(1-Se_", TestB, ") + (1-prev[i])*(1-Sp_", TestA, ")*Sp_", TestB, "
    p[i, 3] <- prev[i]*(1-Se_", TestA, ")*Se_", TestB, " + (1-prev[i])*Sp_", TestA, "*(1-Sp_", TestB, ")
    p[i, 4] <- prev[i]*(1-Se_", TestA, ")*(1-Se_", TestB, ") + (1-prev[i])*Sp_", TestA, "*Sp_", TestB, "
    
    prev[i] ~ dbeta(alpha, beta)
  }
  
#=== PRIOR	===#
  alpha <- mu*psi      ## a parameter for the hierarchical beta distribution
  beta <- psi*(1-mu)   ## b parameter for the hierarchical beta distribution
  
  mu ~ dbeta(", mu.shapea, ", ", mu.shapeb, ") 	     ## Mean pregnancy prevalence distribution (mean of 42% and 95th percentile of 74%)
  
  psi ~ dgamma(", psi.shapea, ", ", psi.shapeb, ") 	 ## Gamma distribution for psi obtained (mean of 42%, and 50th and 90th percentiles of the 90th percentile of 65 and 75%)  

  Se_", TestA, " ~ dbeta(", Se.TestA.shapea, ", ", Se.TestA.shapeb, ") 	## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(", Sp.TestA.shapea, ", ", Sp.TestA.shapeb, ") 	## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(", Se.TestB.shapea, ", ", Se.TestB.shapeb, ") 	## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(", Sp.TestB.shapea, ", ", Sp.TestB.shapeb, ") 	## Prior for Sp of Test B
 
}")


#write as a text (.txt) file
write.table(model_2tests_hierarchical, 
            file="model_2tests_hierarchical.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)


##================= INITS ==================##
#Initializing values for the unknown parameters for the 3 chains.
inits <- list(list(mu=0.50,
                   psi=1,
                   Se_US=0.90,
                   Sp_US=0.90,
                   Se_PAG=0.90,
                   Sp_PAG=0.90),
              
              list(mu=0.40,
                   psi=2,
                   Se_US=0.80,
                   Sp_US=0.80,
                   Se_PAG=0.80,
                   Sp_PAG=0.80),
              
              list(mu=0.60,
                   psi=0.5,
                   Se_US=0.95,
                   Sp_US=0.95,
                   Se_PAG=0.95,
                   Sp_PAG=0.95)
)

##================= RUN MODEL ==================##
library(R2jags)
library(coda)

#Run the Bayesian model
bug.out <- jags(data=datalist,                             
                model.file="model_2tests_hierarchical.txt",     
                parameters.to.save=c("Se_US", "Sp_US", "Se_PAG", "Sp_PAG",
                                     "mu", "psi", "alpha", "beta"),               
                n.chains=3,                                 
                inits=inits,                                
                n.iter=25000,                                
                n.burnin=5000,                              
                n.thin=1,                                   
                DIC=FALSE)  

##================== Check model ================##
library(mcmcplots)
bug.mcmc <- as.mcmc(bug.out)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)
print(bug.out, digits.summary=3) 

##=========== VISUALIZE the Superpopulation distribution ========##
alpha <- 92
beta <- 78
#plot the posterior distribution
curve(dbeta(x, 
            shape1=alpha, 
            shape2=beta), 
      from=0, to=1, 
      main="Estimated distribution of within-herd prevalence in the target population", 
      xlab = "Proportion", 
      ylab = "Density")
