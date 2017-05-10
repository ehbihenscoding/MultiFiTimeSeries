#############################################
########### SVD multifi #####################
#############################################

#approxi = 10^-16	#0.001
#coeffsvd = fSVDfull(Z1, Z2, approxi)
#basesvd = coefbase(Z1, coeffsvd$legere)
#nb_parametressvd = dim(coeffsvd$legere)[1]
coeffsvd = fSVDfullmethode(Z1, Z2, N2)
basesvd = coefbase(Z1, coeffsvd$legere)

#### type de noyau
cov.type<- "matern5_2"
### création des matrices de résultat!
presult = matrix(0,N2,N2)
pvarrsl = matrix(0,N2,N2)

#### initialisation du paramètre d'itérarion
i <- 1
#### initialisation de paramètre d'erreur
err <- 1

##### tant que le Q2 est important et donc qu'on apprend bien la paramètres
while ( err > 0.8 & i<=N2) {

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffsvd$legere[i,],coeffsvd$lourd[i,]),nlevel = level, covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
# moyenne de prédication 
presult[i,] <- coeffsvd$lourd[i,] + apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall)
pvarrsl[i,]	<-	apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVvarall)


err <- errorQ2(presult[i,],coeffsvd$lourd[i,])
#print(err)
i <- i+1
}

# le nombre de paramètre à utiliser est égale a l'avant dernier calculé
nb_optim <- max(i-2,1)

Q2SVD2FLFoptim	<-	errorQ2temp( fpred(presult[1:nb_optim,],basesvd[,1:nb_optim]), Z2)

##################################################
#################   Q2 ###########################
##################################################

#Q2iteration = matrix(0,Nt,N2)
#for (j in 2:N2){
#dataapp = fpred(presult[1:j,],basesvd[,1:j])
#Q2iteration[,j] = errorQ2temp(dataapp, Z2)
#}

##################################################
######### Prédiction moyenne et variance #########
##################################################

## matrice résultat de prédiction et variance
pmean <- fpred(presult[1:nb_optim,],basesvd[,1:nb_optim])
pvar	<-	fpred( pvarrsl[1:nb_optim,], basesvd[,1:nb_optim]^2)
pvarortho	<- apply(fpred( coeffsvd$lourd[1:nb_optim,], basesvd[,1:nb_optim]) - Z2, 1, var)

##################################################
###################  Affichage ###################
##################################################
#x11();
#plot(apply(Q2iteration,2,mean),type='l')
#x11()
#plot(t,Q2iteration[,1],type='l',ylim=c(min(Q2iteration),max(Q2iteration)))
#claire(t,Q2iteration,N2)
#
# Pour le cas des variances
indice = 1
x11()
plot( t, pmean[,indice], type='l', col=2)
lines( t, pmean[,indice] + 1.96*(pvar[,indice]+pvarortho), col=3)
lines( t, pmean[,indice] - 1.96*(pvar[,indice]+pvarortho), col=3)
lines( t, Z2[,indice])
