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
presult = matrix( 0, N2, N2)
pcoeff	= matrix( 0, N2, Ndata)
pvarLFcoeff	= matrix( 0, N2, Ndata)

#### initialisation du paramétre d'itérarion
i <- 1
#### initialisation de paramétre d'erreur
err <- 1

##### tant que le Q2 est important et donc qu'on apprend bien la paramètres
while ( err > 0.8 & i<=N2) {
#while ( i<=N2) { # in order to explor all dimension remove to earn time

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffsvd$legere[i,],coeffsvd$lourd[i,]),nlevel = level, covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
# moyenne de prédication 
presult[i,] <- coeffsvd$lourd[i,] + apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall)
inter	<-	predict(object = modelmulti, xD, type = 'UK')#, cov.compute=FALSE, se.compute=FALSE, light.return=TRUE)
pcoeff[i,]	<- inter$mean
pvarLFcoeff[i,]	<- inter$sig2

err <- errorQ2(presult[i,],coeffsvd$lourd[i,])
i <- i+1
}

# le nombre de paramètre à utiliser est égale a l'avant dernier calculé
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
######### Prédiction moyenne et variance ########
##################################################

## matrice résultat de prédiction et variance
pmeanLF <- fpred(pcoeff[1:nb_optim,],basesvd[,1:nb_optim])
pvarLF <- fpred(pvarLFcoeff[1:nb_optim,],basesvd[,1:nb_optim]^2)

## calcul de la variance de la base
#gamma = meanvargamma( Z1, Z2, Nt, N1, N2)
## on réalise plusieurs tirage de gamma
#Ntirage <- 10
#varpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
#for (tirage in 1:Ntirage){
#	basetemp <- randn( Nt, nb_optim) * gamma$varbase[,1:nb_optim] + basesvd[,1:nb_optim]
#	coefftemp <- randn( nb_optim, Ndata) * pvar[1:nb_optim,] + pcoeff[1:nb_optim,]
#	varpredtot[,,tirage] <- fpred( coefftemp, basetemp)
#}
#varpredLF <- apply( varpredtot, c(1,2), var)
#varpredalter <- fpred(pvar[1:nb_optim,],basesvd[,1:nb_optim]^2)
##pvarortho       <- apply(fpred( coeffsvd$lourd[1:nb_optim,], basesvd[,1:nb_optim]) - Z2, 1, var)

##################################################
###################  Affichage ###################
##################################################
#x11();
#plot(apply(Q2iteration,2,mean),type='l')
#x11()
#plot(t,Q2iteration[,1],type='l',ylim=c(min(Q2iteration),max(Q2iteration)))
#claire(t,Q2iteration,N2)

##################################################
############ Error depending on dimension ########
##################################################
#
#errvector = matrix( 0, N2, 1)
#varvector = matrix( 0, N2, 1)
#for (i in 1:N2) {
#   errvector[i]= mean(errorQ2temp( fpred( pcoeff[1:i,], basesvd[,1:i]), a))
#   varvector[i]= mean(varerrorL2temp( fpred( pcoeff[1:i,], basesvd[,1:i]), a))
#}
#### Affichage
#x11();par(mar=c(4,4,3,5));plot(errvector,pch=16,xlab="dimension",ylab="",type="o")
#par(new=T)
#plot(varvector,col=2,xlab="",ylab="",axe=F,pch=15,type="o")
#mtext("Variance", side=4,col=2,line=2.5)
#axis(4,col=2,col.axis=2)
#mtext("Q2", side=2, col=1,line=2.5)