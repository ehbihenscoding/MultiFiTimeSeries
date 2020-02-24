#setwd('~/Documents/These/Code/Signaux/Decomposition/Mean/optim/')
setwd('~/Th�se/Travail/codage/MultifideliteTemporel/optim/')
source('example/Import.R')

# définition des paramètres de l'expérience
Nb_exper = 20
Nt = 101
# creations des matrices de Q2
Q2SVD2FLFstat = matrix( 0, Nt, Nb_exper)
Q2SVD2FHFstat = matrix( 0, Nt, Nb_exper)
Q2TENCOV2Fstat = matrix( 0, Nt, Nb_exper)


for( experi in 1:Nb_exper){
	print(experi)
	source('example/data.R')
	source('SVD2FLF/SVD2FLF.R')
	Q2SVD2FLFstat[,experi] = Q2SVD2FLFoptim
	source('SVD2FHF/SVD2FHF.R')
	Q2SVD2FHFstat[,experi] = Q2SVD2FHFoptim
	source('TENSVD2F/TENSVD2F.R')
	Q2TENCOV2Fstat[,experi] = Q2TENSVD2Foptim
}

Q2SVD2FLFmean <- apply( Q2SVD2FLFstat, 1, mean)
Q2SVD2FHFmean <- apply( Q2SVD2FHFstat, 1, mean)
Q2TENCOV2Fmean	<- apply( Q2TENCOV2Fstat, 1, mean)

#### Affichage
x11();plot(t,Q2SVD2FHFmean,type='l',ylim=c(0.8,1))
lines(t,Q2TENCOV2Fmean,col=2)
lines(t,Q2SVD2FLFmean,col=3)
