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
### cr�ation des matrices de r�sultat!
presult = matrix( 0, N2, N2)
pcoeff	= matrix( 0, N2, Ndata)
pvar	= matrix( 0, N2, Ndata)

#### initialisation du param�tre d'it�rarion
i <- 1
#### initialisation de param�tre d'erreur
err <- 1

##### tant que le Q2 est important et donc qu'on apprend bien la param�tres
while ( err > 0.8 & i<=N2) {

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffsvd$legere[i,],coeffsvd$lourd[i,]),nlevel = level, covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
# moyenne de pr�dication 
presult[i,] <- coeffsvd$lourd[i,] + apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall)
inter	<-	predict(object = modelmulti, xD, type = 'UK')#, cov.compute=FALSE, se.compute=FALSE, light.return=TRUE)
pcoeff[i,]	<- inter$mean
pvar[i,]	<- inter$sig2

err <- errorQ2(presult[i,],coeffsvd$lourd[i,])
i <- i+1
}

# le nombre de param�tre � utiliser est �gale a l'avant dernier calcul�
nb_optim <- max(i-2,1)

Q2valSVD2FLF	<-	errorQ2temp( fpred( pcoeff[1:nb_optim,], basesvd[,1:nb_optim]), a)

##################################################
#################   Q2 ###########################
##################################################

#Q2iteration = matrix(0,Nt,N2)
#for (j in 2:N2){
#dataapp = fpred(presult[1:j,],basesvd[,1:j])
#Q2iteration[,j] = errorQ2temp(dataapp, Z2)
#}

##################################################
######### Pr�diction moyenne et variance #########
##################################################

## matrice r�sultat de pr�diction et variance
pmean <- fpred(pcoeff[1:nb_optim,],basesvd[,1:nb_optim])

# calcul de la variance de la base
gamma = meanvargamma( Z1, Z2, Nt, N1, N2)
# on r�alise plusieurs tirage de gamma
Ntirage <- 10
varpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
for (tirage in 1:Ntirage){
	basetemp <- randn( Nt, nb_optim) * gamma$varbase[,1:nb_optim] + basesvd[,1:nb_optim]
	coefftemp <- randn( nb_optim, Ndata) * pvar[1:nb_optim,] + pcoeff[1:nb_optim,]
	varpredtot[,,tirage] <- fpred( coefftemp, basetemp)
}
varpred <- apply( varpredtot, c(1,2), var)
#varpred <- fpred(pvar[1:nb_optim,],basesvd[,1:nb_optim]^2)
#pvarortho       <- apply(fpred( coeffsvd$lourd[1:nb_optim,], basesvd[,1:nb_optim]) - Z2, 1, var)

##################################################
##### Affichage de la prediction ainsi 95 % ######
##################################################
#indice = 5
#x11();
#plot( t, pmean[,indice], type='l')
#lines( t, a[,indice], col=2)
#lines( t, pmean[,indice]+1.96*sqrt(varpred[,indice]+pvarortho), col=3)
#lines( t, pmean[,indice]-1.96*sqrt(varpred[,indice]+pvarortho), col=3)
##################################################
###################  Affichage ###################
##################################################
#x11();
#plot(apply(Q2iteration,2,mean),type='l')
#x11()
#plot(t,Q2iteration[,1],type='l',ylim=c(min(Q2iteration),max(Q2iteration)))
#claire(t,Q2iteration,N2)

