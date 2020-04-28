#On dispose de sortie de code ainsi que la base et les coefficients SVD
# Ici l'objectif est de calculer la moyenne et la variance
meanvargamma <- function( Z1, Z2, Nt, N1, N2){
	fullbase = array( data = 0, dim = c( Nt, N2, N1))
	for ( indiceZ1 in 1:N1){
		coeffs = fSVDfullmethode(Z1[,-indiceZ1], Z2, N2)
		fullbase[,,indiceZ1] = coefbase(Z1[,-indiceZ1], coeffs$legere)
	}
	base = apply(fullbase, c(1,2), mean) 
	varbase = apply(fullbase, c(1,2), var) 
	return( list( base = base, varbase = varbase))

}
