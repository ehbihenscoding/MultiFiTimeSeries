setwd('~/These/Travail/codage/MultifideliteTemporel/optim/')
#setwd('~/Documents/Implementation/tempsMultifi/optim/')
library(pracma)
source('donnees_pendule.R')

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
source('error.R')
source('svdfunction.r')
library(optimx)
library(cmaes)
require(DiceDesign)
source('fonctionClaire.R')

#####################################################
############ Le probl?me en lui m?me ################
#####################################################

#dimension du probl?me
dimprob = 5
Ndata = 1000

# fonction ? estimerse
xD = matrix(runif(Ndata*dimprob,0,1),ncol=dimprob)

a = f(xD)

# dÃ©finition de l'information disponible
level = 2

# design d'expÃ©rience
N1 <- 100
N2 <- 30
X1 <- lhsDesign(N1, dimprob) ###matrix(runif(N1*dimprob,0,1),ncol=dimprob) #fappro(DNest$PX)
X2 <- lhsDesign(N2, dimprob) ###matrix(runif(N2*dimprob,0,1),ncol=dimprob) #f(ExtractNestDesign(DNest,2))
#Dsg <- NestedDesignBuild(design = list(matrix(X1,ncol=dimprob),matrix(X2,ncol=dimprob)))
Dsg <- NestedDesignBuild(design = list(X1$design,X2$design))
X1 <- Dsg$PX
X2 <- ExtractNestDesign(Dsg,2)
# realisation de la fonction
Z1 <- fappro(X1)
Z2 <- f(X2)

######################################################
############## Visualisation des simulations #########
######################################################
x11()
plot(t,Z2[,1],type='l',col=1,ylim=c(min(min(Z1,Z2)),max(max(Z1,Z2))))
claire(t,Z1,val=N1,col=rgb(1,0,0,0.1),lwd=2)
claire(t,Z2,val=N2,col=rgb(0,0,1,0.5),lwd=2)

######################################################
################ Recherche plage #####################
######################################################
N <- 50
X <- maximinSA_LHS(randomLHS(N,dimprob))$design
Z1 <- fappro(X)
Z2 <- f(X)

DD = matrix(0,ncol=N,nrow=Nt)
for(i in 1:N){
	DD[,i] = abs(Z1[,i]-Z2[,i])
}
DDmax = apply(DD,2,max)
sel = which(DDmax>=0.05)
summary(X[sel,])
summary(X)


x11()
plot(t,Z2[,1],type='l',col=1,ylim=c(min(Z2),max(Z2)))
claire(t,Z2,val=N,col=rgb(0,0,1,0.5),lwd=2)
claire(t,Z1,val=N,col=rgb(1,0,0,0.4),lwd=2)

######################################################
####           Début de la résolution du Pb     ######
######################################################
### Construction de la matrice de Q2
Nexp = N2
Q2full = matrix(0,Nt,Nexp)


for (nb_iteration in 2:Nexp){
#######   Décomposition    ###########
coeffs = fSVDfullmethode(Z1, Z2, nb_iteration)
base = coefbase(Z1, coeffs$legere)
#basefull = coefbase(Z1, coeffs$basecoeff)
nb_parametres = dim(coeffs$legere)[1]
#coeffinter = 0*coeffs$fulllourd
#coeffinter[(dim(coeffs$lourd)[1]+1):dim(coeffs$fulllourd)[1],] = coeffs$fulllourd[(dim(coeffs$lourd)[1]+1):dim(coeffs$fulllourd)[1],]
Z2ortho = Z2 - fpred(coeffs$lourd, base) #fpred(coeffinter,basefull)

#
#####################################################
############### Multifi- krigeage ###################
#####################################################
cov.type<- "matern5_2"
presult = matrix(list(),nrow=nb_parametres,ncol=4,byrow = FALSE) # 4 c'est la taille de la liste predict

for( i in 1:nb_parametres){

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffs$legere[i,],coeffs$lourd[i,]),nlevel = level, covtype=cov.type)
indices <- c(2)
presult[i,] <- predict(object = modelmulti, newdata= xD,type = 'UK')
}

####################################################
################## Reconstruction ###################
#####################################################

coeffsapp = matrix(0,nb_parametres,length(presult[,1][[1]]))
coeffsapperr = matrix(0,nb_parametres,length(presult[,1][[1]]))
for (i in 1:nb_parametres){
coeffsapp[i,] = presult[,1][[i]]
coeffsapperr[i,] = presult[,2][[i]]}
dataapp = fpred(coeffsapp,base)
dataerr = fpred(coeffsapperr,base)

###################    Tensorisation   ##############

source('optimFunction.R')
# on utilise juste la partie orthogonale
X = X2
Y = Z2ortho
# ceci est la partie d'optilisation des hyperparamètres
lc=c(rep(0.2,dimprob))
T1<-Sys.time()
tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
T2<-Sys.time()
Tdiff= difftime(T2, T1)
lc=tempOpt$par

# prédiction
dataapportho = matrix(0,length(t),Ndata)
errapportho = dataapportho
for (ind in 1:Ndata){
        p <- predKmFonc( X, Y, xD[ind,], lc)
        dataapportho[,ind] = p$mu[,1]
        errapportho[,ind] = p$sd
}

zD = dataapp + dataapportho
Q2full[,nb_iteration] = errorQ2temp(zD, a)

print(nb_iteration)
}

######################################################
############ Affiche #################################
######################################################
Q2mean = apply(Q2full[,2:Nexp],2,mean)

x11(); plot(Q2full[,2],type='l',col=2,ylim=c(0.5,1))
for( i in 3:Nexp){
lines(Q2full[,i],col=i)}

x11()
plot(Q2fulltechnique,type='l',ylim=c(min(c(Q2svd,Q2fulltechnique)),1))
lines(Q2svd,col=2)

#############################################
########  Indépendant: affichage utile ######
#############################################
require(rgl)
open3d()
rgl.surface( 1:N2 , 1:Nt , Z2ortho)

###############################################
############ Calcule de Z2ortho   #############
###############################################

x11();plot(t,Z2ortho[,1],type='l',ylim=c(min(Z2ortho),max(Z2ortho)));claire(t=t,Z2ortho,N2)

###################################################
#############" Détermination cout de calcule ######
###################################################

T1<-Sys.time()
toto = fappro(xD)
T2<-Sys.time()
Tdiff2= difftime(T2, T1)
T1<-Sys.time()
toto = f(xD)
T2<-Sys.time()
Tdiff1= difftime(T2, T1)
### le code lourd est 23 fois plus couteux


################################################
############  affichage ########################
################################################
library(ggplot2)
library(reshape2)

melted <- melt(Q2full>Q2svdd)
ggplot(melted, aes(x = Var2, y = Var1, fill = value)) + geom_tile() +
    scale_fill_manual(values = c("white", "red"))
