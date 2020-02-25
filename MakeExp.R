#setwd('~/Documents/These/Code/Signaux/Decomposition/Mean/optim/')
#setwd('~/Rcode/optim/')
setwd('~/Th�se/Travail/codage/MultifideliteTemporel/optim/')
source('example/Import.R')
library(bigstatsr)

# définition des paramètres de l'expérience
Nb_exper = 12
Nt = 101
# Initialisation parallelisation
lock <- tempfile()
# creations des matrices de Q2
Q2SVD2FLFstat = FBM( Nt, Nb_exper)	#matrix( 0, Nt, Nb_exper)
Q2SVD2FHFstat = FBM( Nt, Nb_exper)	#matrix( 0, Nt, Nb_exper)
Q2TENCOV2Fstat = FBM( Nt, Nb_exper)	#matrix( 0, Nt, Nb_exper)
Q2SVD1Fstat = FBM( Nt, Nb_exper)	#matrix( 0, Nt, Nb_exper)

##### Parallelisation #######
registerDoSEQ()
c1 <- parallel::makeCluster(12)
doParallel::registerDoParallel(c1)

foreach( experi = 1:Nb_exper, .combine = 'c', .packages=c('DiceDesign','MuFiCokriging')) %dopar%{
	source('example/Import.R')
#	print(experi)
	source('example/data.R')
	source('SVD2FLF/SVD2FLF.R')
	locked <- flock::lock(lock)
	Q2SVD2FLFstat[,experi] = Q2SVD2FLFoptim
	flock::unlock(locked)
	source('SVD2FHF/SVD2FHF.R')
	locked <- flock::lock(lock)
	Q2SVD2FHFstat[,experi] = Q2SVD2FHFoptim
	flock::unlock(locked)
	source('TENSVD2F/TENSVD2F.R')
	locked <- flock::lock(lock)
	Q2TENCOV2Fstat[,experi] = Q2TENSVD2Foptim
	flock::unlock(locked)
	source('SVD1F/SVD1F.r')
	locked <- flock::lock(lock)
	Q2SVD1Fstat[,experi] = Q2iteration
	flock::unlock(locked)
}
###### Fin Parallelisation  #######
parallel::stopCluster(c1)

Q2SVD2FLFmean <- apply( Q2SVD2FLFstat[], 1, mean)
Q2SVD2FHFmean <- apply( Q2SVD2FHFstat[], 1, mean)
Q2TENCOV2Fmean	<- apply( Q2TENCOV2Fstat[], 1, mean)
Q2SVD1Fmean	<- apply( Q2SVD1Fstat[], 1, mean)

#### Affichage
x11();plot(t,Q2SVD2FHFmean,type='l',ylim=c(0.8,1))
lines(t,Q2TENCOV2Fmean, col=2)
lines(t,Q2SVD2FLFmean, col=3)
lines( t, Q2SVD1Fmean, col=4)
