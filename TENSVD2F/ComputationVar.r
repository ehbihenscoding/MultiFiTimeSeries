### calcule de la moyenne de prédiction
## pour cette partie on utilise sumCovVE

# creation des matrices variance et moyenne
VEortho = apply( predortho, c(1,2), var)
EVortho = apply( pvarinter, c(1,2), var)
#initialisation pour les matrices necessitant des bases
sumVEparinter <- array( data = 0, dim = c( Nt, Ndata, N1-N2))
sumEVparinter <- array( data = 0, dim = c( Nt, Ndata, N1-N2))
sumCovEparinter <- array( data = 0, dim = c( Nt, Ndata))
sumCovorthointer <- array( data = 0, dim = c( Nt, Ndata))
# boucle pour ces réaliser la moyenne et la variance
for (tirage in 1:(N1-N2)){
	sumVEparinter[,,tirage] <- fpred( mean[,,tirage], gamma[,,tirage])
	sumEVparinter[,,tirage] <- fpred( var[,,tirage], abs(gamma[,,tirage]))
    sumCovorthointer <- sumCovorthointer
        + abs(cov( fpred( mean[,,tirage], gamma[,,tirage]), reechtot$predortho))# predortho[,,tirage]))
}
for ( iindice in 1:(N1-N2)){
    for ( jindice in 1:(N1-N2)){
        sumCovEparinter <-  sumCovEparinter
            + abs(cov( fpred( mean[,,iindice], gamma[,,iindice]), fpred( mean[,,jindice], gamma[,,jindice]))) 
    }
}
pmeaninter <- apply( sumVEparinter, c(1,2), mean)
sumVEpar <- apply( sumVEparinter, c(1,2), var)
sumEVpar <- apply( sumEVparinter, c(1,2), mean)

## calcule de la variance et de la moyenne de prédiction
pmean <- pmeaninter + apply( predortho, c(1,2), mean)
pvar <- sumVEpar + sumEVpar + sumCovEparinter + sumCovorthointer
        + EVortho + VEortho