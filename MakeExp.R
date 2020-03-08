#setwd('~/Documents/These/Code/Signaux/Decomposition/Mean/optim/')
#setwd('~/Rcode/optim/')
setwd('~/Th�se/Travail/codage/MultifideliteTemporel/optim/')
source('example/Import.R')
library(bigstatsr)
library(foreach)

# définition des paramètres de l'expérience
Nb_exper = 10

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
c1 <- parallel::makeCluster(10)
doParallel::registerDoParallel(c1)

#dimension du probl?me
dimprob = 5

# définition de l'information disponible
level = 2

# design d'expérience
N1 <- 100
N2 <- 10

#construction de la base de validation
Ndata = 1000
xD = matrix( runif(Ndata*dimprob,0,1), ncol=dimprob)
a=f(xD)

# Parametres pour les envoyer dans chaque thread
parall =  FBM( 5, 1)
parall[1] = N1
parall[2] = N2
parall[3] = Nt
parall[4] = level
parall[5] = dimprob

foreach( experi = 1:Nb_exper, .combine = 'c',
	.packages=c('DiceDesign','MuFiCokriging')) %dopar%{
	N1 = parall[1]
	N2 = parall[2]
	Nt = parall[3]
	level = parall[4]
	dimprob = parall[5]
	source('example/Import.R')
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


#### Export
data = data.frame( t=t, Q2SVD2FLFstat=Q2SVD2FLFstat[], Q2SVD2FHFstat=Q2SVD2FHFstat[], Q2SVD1Fstat=Q2SVD1Fstat[], Q2TENCOV2Fstat=Q2TENCOV2Fstat[])
write.csv( data, "~/outputs/exportQ2.csv")
