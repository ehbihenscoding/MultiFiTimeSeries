# If we presume that N = 3
# the idea is to know the variations of orthogonal space
Nideal = 3

# number of iterations 
Setdifferents = 20

# initialisation of the diference set
orthosetmean = matrix(0, Nt, Setdifferents)

for (i in 1: Setdifferents){
    # first creation of sets 
    source('example/data.R', chdir = TRUE)
    # we compute the bases 
    coeffs = fSVDfullmethode(Z1, Z2, N2)
    base = coefbase(Z1, coeffs$legere)
    # realisation of the orthogonal set
    Z2ortho = Z2 - fpred(coeffs$lourd[1:Nideal,], base[,1:Nideal])
    # computation of the mean
    orthosetmean[,i] = apply( Z2ortho, 1, mean)
}
Zorthomean = apply( orthosetmean, 1, mean)
Zorthovar = apply( orthosetmean, 1, var)