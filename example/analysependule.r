setwd('~/Documents/Implementation/intermediaire/decomposition/')
library(pracma)
source('donnees_pendule.R')

Nt=100+1
t=seq(0,10,len=Nt)
l=2
# définition des fonctions d'étude
f <- function(x){
#y=matrix(0,length(t), dim(x)[1])
y = x[,1]
for(i in 1:dim(x)[1]){
sys = SysPendulePlaque(M=5*x[i,2]+3,k=0.1+x[i,3]*2,theta0=x[i,5]*pi/4+pi/4,Dtheta0=x[i,4]/6,Temps=t,y0=x[i,1])
#y[,i]=sys[,1]
y[i] = mean(sys[,1])
}
return(RES=c(y))}

library(sensitivity)   # Methodes d'analyse de sensibilité
library(lhs)
library(DiceDesign)

#dimension du probl?me
dimprob = 5
Ndata = 2e2

xD = matrix(runif(Ndata*dimprob,0,1),ncol=dimprob)
xD1 = xD[1:(Ndata/2),]
xD2 = xD[(Ndata/2+1):Ndata,]

n=1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))


sa = soboljansen(model = f, X1 = X1, X2 = X2, nboot=100)

print(sa)
 
plot(sa)

names = c("y0","M","k","Dtheta0","theta0")
binf=c(0,0,0,0,0)
bsup=c(1,1,1,1,1)

voir = morris(model=f,factors=names,design=list(type="oat",levels=6,grid.jump=2),r=15,
        binf=binf,bsup=bsup,scale = TRUE)

## tentative d'indices de Morris dépendant du temps ##

voir = matrix(0,nrow = Nt, ncol = dimprob)
for (tetude in 1:Nt){
f <- function(x){
#y=matrix(0,length(t), dim(x)[1])
y = x[,1]
for(i in 1:dim(x)[1]){
sys = SysPendulePlaque(M=10*x[i,2]+1,k=0.1+x[i,3]*2,theta0=x[i,5]*pi/2-pi/4,Dtheta0=x[i,4]/6,Temps=t,y0=x[i,1])
#y[,i]=sys[,1]
y[i] = sys[tetude,1]
}
return(RES=y)}

voir[tetude,] = morris(model=f,factors=names,design=list(type="oat",levels=6,grid.jump=2),r=15,
	binf=binf,bsup=bsup,scale = TRUE)$ee[15,]
}
fapp<-function(x){return(x[,1]+x[,2]+x[,3]+x[,4]+x[,5])}

