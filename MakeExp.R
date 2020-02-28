setwd('~/Rcode/optim/')
source('example/Import.R')

# définition des paramètres de l'expérience
Nb_exper = 100
Nt = 101
# creations des matrices de Q2
Q2SVD2FLFstat = matrix( 0, Nt, Nb_exper)
Q2SVD2FHFstat = matrix( 0, Nt, Nb_exper)
Q2TENCOV2Fstat = matrix( 0, Nt, Nb_exper)
Q2SVD1Fstat = matrix( 0, Nt, Nb_exper)
dimBaseTENCOV = rep( 0, Nb_exper)
dimBaseSVD2LF = rep( 0, Nb_exper)
dimBaseSVD2HF = rep( 0, Nb_exper)

for( experi in 1:Nb_exper){
	source('example/Import.R')
	source('example/data.R')
	source('SVD2FLF/SVD2FLF.R')
	Q2SVD2FLFstat[,experi] = Q2valSVD2FLF
	dimBaseSVD2LF[experi] = nb_optim
	source('SVD2FHF/SVD2FHF.R')
	Q2SVD2FHFstat[,experi] = Q2valSVD2FHF
	dimBaseSVD2HF[experi] = nb_optim
	source('TENSVD2F/TENSVD2F.R')
	Q2TENCOV2Fstat[,experi] = Q2valTENSVD2F
	dimBaseTENCOV[experi] = nb_optimTENCOV 
	source('SVD1F/SVD1F.r')
	Q2SVD1Fstat[,experi] = Q2valSVD1F
}

Q2SVD2FLFmean <- apply( Q2SVD2FLFstat, 1, mean)
Q2SVD2FHFmean <- apply( Q2SVD2FHFstat, 1, mean)
Q2TENCOV2Fmean	<- apply( Q2TENCOV2Fstat, 1, mean)
Q2SVD1Fmean	<- apply( Q2SVD1Fstat, 1, mean)

#### Export
data = data.frame( t=t, Q2SVD2FLFmean=Q2SVD2FLFstat, Q2SVD2FHFmean=Q2SVD2FHFstat, Q2SVD1Fmean=Q2SVD1Fstat, Q2TENCOV2Fmean=Q2TENCOV2Fstat)
write.csv( data, "~/outputs/exportQ2.csv")
data = data.frame( SVD2LF = dimBaseSVD2LF, SVD2HF = dimBaseSVD2HF, TENCOV = dimBaseTENCOV)
write.csv( data, "~/outputs/dimBase.csv")
