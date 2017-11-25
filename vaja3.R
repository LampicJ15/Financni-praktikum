library(combinat)
library(Rlab)
#========================
#1.) 
#a.)
S0 <- 50
u <-1.05
d<-0.95
U <- 5
R <- 0.03
t <- 3

X <- c() #vektor izplačil nakupne opcije
Y <- c() #vektor izplačil prodajne opcije

#b.)

izplacilo <- function(vrsta, t, type){
  n <- length(vrsta)
  if (type == "call"){
    c <- max(vrsta[c((t+1):n)]) - max(vrsta[c(1:t)])
    return( max(0,c))
  }
  else{
    p <- min(vrsta[c((t+1):n)]) - min(vrsta[c(1:t)])
    return( max(0,p))
  }
}

#====================================================
#2.naloga

#a.)

binomski <- function(S0,u,d,U,R,t,type){
  p <- (1+R-d)/(u-d)
  E <- 0 #upanje
  
  drevo <- hcube(rep(2,U), translation = -1)
  for(i in c(1:nrow(drevo))){
    vrsta <- c(S0)
    P <- p^(sum(drevo[i,]))*(1-p)^(U - sum(drevo[i,]))
    for (j in c(1:U)){
      vrsta[j+1] <- vrsta[j]* (u*drevo[i,j] + (1-drevo[i,j])*d)
    }
  
    E <- E + P*izplacilo(vrsta,t,type)
    
  }
    return(E/(1+R)^U)
}

#b.)
monte <- function(S0,u,d,U,R,T,type,N){
  p <- (1+R-d)/(u-d)
  drevo <- matrix(rbinom(U*N,1,p),N,U) #stanja
  
  
  
  
  
  
  
}
