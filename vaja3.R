library(combinat)
library(Rlab)
library(dplyr)
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
  drevo <- hcube(rep(2,U), translation = -1) #drevo stanj 1 pomeni up 0 down
  
  
  #izračun verjetnosti končnih stanj
  k <- rowSums(drevo) #vektor, ki za vsako vrstico pove kolikorat je up
  P.S <- p^k*(1-p)^(U-k) #vektor verjetnosti končnih stanj
  
  drevo[drevo == 1]<- u #1 v matriki zamenjamo z u
  drevo[drevo == 0]<- d #0 v matriki zamenjamo z vrednostjo d
  
  drevo <- t( apply(drevo,1, cumprod)) #kumulativni produkt u in d po vrsticah 
  vrednosti <- cbind(S0, S0*drevo) #dobimo matriko, ki ima za vrstice vrednosti oz. vrsto za S0
  
  izplacila <- apply(vrednosti,1, function(x) izplacilo(x,t,type)) #po vrsticah matrikah izvedemo funkcijo izplacila
  E <- sum( izplacila * P.S) #upanje (skalarni produkt)
  
  return(E/(1+R)^U)
}

#b.)
#definiram funkcijo, ki iz zaporedja u in d (up in down), vrne vrsto vrednosti s. 


monte <- function(S0,u,d,U,R,t,type,N){
  p <- (1+R-d)/(u-d)
  drevo <- matrix(rbinom(U*N,1,p),N,U) #stanja 
  drevo[drevo == 1]<- u #1 v matriki zamenjamo z u
  drevo[drevo == 0]<- d #0 v matriki zamenjamo z vrednostjo d
  
  drevo <- t( apply(drevo,1, cumprod)) #kumulativni produkt u in d po vrsticah 
  vrednosti <- cbind(S0, S0*drevo) #dobimo matriko, ki ima za vrstice vrednosti oz. vrsto za S0
  izplacila <- apply(vrednosti,1, function(x) izplacilo(x,t,type)) #po vrsticah matrikah izvedemo funkcijo izplacila
  
  premija <- ( sum(izplacila)/N )/(1+R)^U #sum(izplacila)/N je vzorčno povprečje
  
  return(premija)
}
