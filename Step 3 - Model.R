#running the model with the data from our recovered tags 

library(rjags); library(jagsUI); library(ggplot2) #load packages 

ewpw1 <- read.csv("./RecoveredEWPWptsw20randsandCOVS.csv",header=T) #read in data

#defining data 

T <- nrow(ewpw1) 
chsets <- ewpw1$CID 
alts <- ewpw1$Alts 
use <- ewpw1$Use 
nchsets <- max(ewpw1$CID) 
nalts <- max(ewpw1$Alts) 
ninds <- max(ewpw1$SID)

#defining covariates
iqr_100m <- (ewpw1$iqr_100) 
p75_100m <- (ewpw1$p75_100) 
p90_100m <- (ewpw1$p90_100)  
perc5to1_100m <- (ewpw1$perc5to1_100) 
percfirst5to1_100m <- (ewpw1$percfirst5to1_100)
TopRug30m_p95_100m <- (ewpw1$rugosity_100) 
cor(X1) #running a correlation matrix to see which variables are the least correlated

# delineate the matrix of variables
X1 <- cbind(p90_100m, percfirst5to1_100m, TopRug30m_p95_100m)
X1 <- scale(X1) 
npred <- ncol(X1) #number of predictor variables 

sub_id <- subset(ewpw1, Alts==20)$SID #assigning choice set to each bird

#creating dataframe for the responses
y <- data.frame("CID" = ewpw1$CID, "Use" = ewpw1$Use, "Alts" = ewpw1$Alts)
ytest <- reshape(y, direction = "wide", idvar = "CID", timevar = "Alts")

Z <- array(NA, dim=c(npred, nchsets, nalts)) 
Z[,-182,]
for(i in 1:T){               
  for(j in 1:npred){        
    Z[j, chsets[i], alts[i]] <- X1[i, j] 
  }
} 

jags.data <- list(npred = npred, 
                  sub_id = sub_id, 
                  ninds = ninds, 
                  chsets = as.integer(chsets),
                  nchsets = as.integer(nchsets),
                  alts = as.integer(alts),
                  nalts = as.integer(nalts),
                  T = as.integer(T),
                  y = as.matrix(ytest[,2:21]),
                  X1 = X1,
                  Z = Z)
params <- c("mu", "beta")
sink("m1.txt")


cat("
model{
  
  for(a in 1:ninds){                    
    for(j in 1:npred){                
      beta[a,j] ~ dnorm(mu[j], tau[j])  
      }
    } 
      
  for(j in 1:npred){        
    mu[j] ~ dnorm(0, 0.01)  
    sig[j] ~ dunif(0, 100) 
    tau[j] <- 1/(sig[j] * sig[j]) 
    }

  for(i in 1:nchsets){                          
    y[i, 1:nalts] ~ dmulti(p[i, 1:nalts], 1)

    for(k in 1:nalts){ 
      log(phi[i, k]) <- inprod(beta[sub_id[i],],Z[,i,k]) 
      p[i,k] <- phi[i,k] / sum(phi[i,1:nalts]) 
      }
    }
}
", fill=T)
sink()
inits <- function(){list(mu = rnorm(npred, 0, 1), sig = rlnorm(npred, 0, 1))}	

#starting MCM
mod1 <- autojags("m1.txt", 
                 data = jags.data,
                 inits = inits,
                 parameters.to.save = params,
                 max.iter = 50000, 
                 n.chains = 3, 
                 n.thin = 3, 
                 iter.increment = 5000,  
                 n.burnin = 1000, 
                 n.adapt = 1000, 
                 parallel = T) 

mod1
