#setwd('~/Thèse/Travail/codage/MultifideliteTemporel/optim/')
#setwd('~/Documents/Implementation/tempsMultifi/optim/')
library(pracma)
source('example/donnees_pendule.R')

Nt=100+1
t=seq(0,10,len=Nt)
l=2
# définition des fonctions d'étude
fappro <- function(x){
y=matrix(0,length(t), dim(x)[1])
for(i in 1:dim(x)[1]){
sys = SysPendulePlaqueLin(M=5*x[i,2]+3,k=0.1+x[i,3]*2,theta0=x[i,5]*pi/4+pi/4,Dtheta0=x[i,4]/6,Temps=t,y0=x[i,1])
y[,i]=sys[1,]
}
return(y)}
f <- function(x){
y=matrix(0,length(t), dim(x)[1])
for(i in 1:dim(x)[1]){
sys = SysPendulePlaque(M=5*x[i,2]+3,k=0.1+x[i,3]*2,theta0=x[i,5]*pi/4+pi/4,Dtheta0=x[i,4]/6,Temps=t,y0=x[i,1])
y[,i]=sys[,1]
}
return(y)}


library(MuFiCokriging)
library(MASS)
source('Fonctions/error.R', local=TRUE)
source('Fonctions/svdfunction.r', local=TRUE)
library(optimx)
library(cmaes)
require(DiceDesign)
source('Fonctions/fonctionClaire.R', local=TRUE)
source('Fonctions/optimFunction.R', local=TRUE)
