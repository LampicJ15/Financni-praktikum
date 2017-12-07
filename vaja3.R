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

X <- c(0, 8.69, 0, 2.24, 5.24) #vektor izplačil nakupne opcije
Y <- c(0, 7.88, 0, 0, 0) #vektor izplačil prodajne opcije

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

#simuliranje vrednosti
monte(60,1.05,0.95,15,0.01,8,"put",10) 
monte(60,1.05,0.95,15,0.01,8,"put",100) 
monte(60,1.05,0.95,15,0.01,8,"put",1000) 

#========================================================
#3.naloga

#a.)
N1 <- c()
N2 <- c()
N3 <- c()

M <- 100 #število simulacij
for (i in c(1:M)){
  N1 <- c(N1, monte(60,1.05,0.95,15,0.01,8,"put",10))
  N2 <- c(N2, monte(60,1.05,0.95,15,0.01,8,"put",100))
  N3 <- c(N3, monte(60,1.05,0.95,15,0.01,8,"put",1000))
}

prem.opcije <- binomski(60,1.05,0.95,15,0.01,8,"put") #cena premije dobljena z binomksim modelom

min <- floor(min(c(N1,N2,N3))) 
max <- ceiling(max(c(N1,N2,N3))) 

  
#histogram N1
pov.N1 <- mean(N1) #povprečje vrednosti N1
odklon.N1 <- sqrt(var(N1)) #standardni odklon vrednosti N1

histogram1 <-hist(N1,breaks = 20,
                 main = "Monte Carlo: N=10",
                 xlab = "Premija",
                 xlim = c(min, max),
                 col ="yellow")
abline(v=pov.N1,col="green")
abline(v=prem.opcije, col="red",lty=2)
arrows(pov.N1,0,x1=pov.N1 - odklon.N1, col = "green",length = 0.1)
arrows(pov.N1,0,x1=pov.N1 + odklon.N1, col = "green",length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

#histogram N2

pov.N2 <- mean(N2) #povprečje vrednosti N2
odklon.N2 <- sqrt(var(N2)) #standardni odklon vrednosti N2

histogram2 <-hist(N2,
                  main = "Monte Carlo: N=100",
                  xlim = c(min,max),
                  xlab = "Premija",
                  col ="yellow")
abline(v=pov.N2,col="green")
abline(v=prem.opcije, col="red",lty=2)
arrows(pov.N2,0,x1=pov.N2 - odklon.N2, col = "green",length = 0.1)
arrows(pov.N2,0,x1=pov.N2 + odklon.N2, col = "green",length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

#histogram N3
pov.N3 <- mean(N3) #povprečje vrednosti N3
odklon.N3 <- sqrt(var(N3)) #standardni odklon vrednosti N3

histogram3 <-hist(N3,
                  main = "Monte Carlo: N=1000",
                  xlim = c(min,max),
                  xlab = "Premija",
                  col ="yellow")
abline(v=pov.N3,col="green")
abline(v=prem.opcije, col="red",lty=2)
arrows(pov.N3,0,x1=pov.N3 - odklon.N3, col = "green",length = 0.1)
arrows(pov.N3,0,x1=pov.N3 + odklon.N3, col = "green",length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))




