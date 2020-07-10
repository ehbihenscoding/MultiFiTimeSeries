library(nonlinearTseries)
### Resolution simpliste du system de Lorenz
lorenzsimpli <- function( sigma, beta, rho, start, time, Nt, deltat){
	# creation des matrices
	x = seq( 1, Nt)
	y = seq( 1, Nt)
	z = seq( 1, Nt)
	# initialisation
	dx = 0
	dy = 0
	dz = 0
	x[1] = start[1]
	y[1] = start[2]
	z[1] = start[3]
	# iterations
	for ( i in 2:Nt){
		x[i] = x[i-1] + deltat * sigma*(y[i-1]-x[i-1])
		y[i] = y[i-1] + deltat * (rho*x[i-1]-y[i-1]-x[i-1]*z[i-1])
		z[i] = z[i-1] + deltat * (x[i-1]*y[i-1] - beta*z[i-1])
	}
	return(list( x=x, y=y, z=z))
}

deltat	=	0.2
t	=	seq(0,10,by = deltat)
Nt	=	length(t)
l=2
# définition des fonctions d'étude
fappro <- function(x){
	y=matrix(0,Nt, dim(x)[1])
	for(i in 1:dim(x)[1]){
		sys = lorenzsimpli( sigma = x[i,1], beta = x[i,2], rho = x[i,3], start = c(x[i,4],x[i,5],x[i,6]), time = t, Nt, deltat)
		y[,i]=sys$x
	}
	return(y)}

f <- function(x){
y=matrix(0,Nt, dim(x)[1])
for(i in 1:dim(x)[1]){
sys = lorenz( sigma = x[i,1], beta = x[i,2], rho = x[i,3], start = c(x[i,4],x[i,5],x[i,6]), time = t, do.plot=FALSE)
y[,i]=sys$x
}
return(y)}


library(MuFiCokriging)
library(MASS)
source('Fonctions/error.R', local=TRUE)
source('Fonctions/svdfunction.r', local=TRUE)
require(DiceDesign)
library(pracma)
source('Fonctions/fonctionClaire.R', local=TRUE)
source('Fonctions/optimFunction.R', local=TRUE)
source('Fonctions/meanvarbase.R')