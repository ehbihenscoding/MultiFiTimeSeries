#######################################################
# On cherche a présenter le bases autes et basse Fi ###
#######################################################

######### Haute fidélité  #############################
nb_iteration = N2
coeffs = fSVDfullmethode(Z2, Z1, nb_iteration)
base = coefbase(Z2, coeffs$legere)

############ Basse fidélité  ##########################
nb_iteration = N1
coeffs = fSVDfullmethode(Z1, Z1, nb_iteration)
base = coefbase(Z1, coeffs$legere)

############ Affichage ################################
x11()
plot( t, base[,1], type='l')
for (i in 1:N1){
	lines( t,  base[,i], col=i)
}
claire( t, base, N1)
