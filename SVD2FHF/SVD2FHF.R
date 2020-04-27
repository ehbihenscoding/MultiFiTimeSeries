#############################################
########### SVD multifi #####################
#############################################

approxi = 10^-16	#0.001
coeffsvd = fSVDfull(Z1, Z2, approxi)
basesvd = coefbase(Z2, coeffsvd$lourd)
nb_parametressvd = dim(coeffsvd$lourd)[1]

cov.type<- "matern5_2"
#presult = matrix(list(),nrow=nb_parametressvd,ncol=4,byrow = FALSE) # 4 c'est la taille de la liste predict
presult	= matrix(0,N2,N2)
pcoeff	= matrix(0,N2,Ndata)
pvar	= matrix(0,N2,Ndata)
#### initialisation du paramètre d'itérarion
i <- 1
#### initialisation de paramètre d'erreur
err <- 1

##### tant que le Q2 est important et donc qu'on apprend bien la paramètres
while ( err > 0.8 & i<=N2) {

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffsvd$legere[i,],coeffsvd$lourd[i,]),nlevel = level, covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
presult[i,] <- predict( modelmulti, X2,  'UK')$mean + apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall)
inter	 <- predict(object = modelmulti, xD, type = 'UK')
pcoeff[i,]	<- inter$mean
pvar[i,]	<- inter$sig2

err <- errorQ2(presult[i,],coeffsvd$lourd[i,])
#print(err)
i <- i+1
}

# le nombre de paramètre à utiliser est égale a l'avant dernier calculé
nb_optim <- max(i-2,1)

Q2valSVD2FHF	<-	errorQ2temp( fpred( pcoeff[1:nb_optim,], basesvd[,1:nb_optim]), a)

##################################################
################  Estimation  ####################
##################################################
pmean <- fpred(pcoeff[1:nb_optim,],basesvd[,1:nb_optim])
# variance de la partie pr�dite + variance de la partie orthogonale
varortho = apply(fpred( coeffsvd$lourd[1:nb_optim,], basesvd[,1:nb_optim])-Z2,1,var)
varpred <- fpred(pvar[1:nb_optim,],basesvd[,1:nb_optim]^2) + matrix( varortho, Nt, Ndata)
##################################################
##### Affichage de la prediction ainsi 95 % ######
##################################################
#lines( t, pmean[,indice], type='l', col=4)
#lines( t, pmean[,indice]+1.96*sqrt(varpred[,indice]), col=5)
#lines( t, pmean[,indice]-1.96*sqrt(varpred[,indice]), col=5)

##################################################
#################   Q2 ###########################
##################################################

#Q2iteration = matrix(0,Nt,N2)
#for (j in 2:N2){
#dataapp = fpred(presult[1:j,],basesvd[,1:j])
#Q2iteration[,j] = errorQ2temp(dataapp, Z2)
#}

##################################################
###################  Affichage ###################
##################################################
#x11();
#plot(apply(Q2iteration,2,mean),type='l')
#x11()
#plot(t,Q2iteration[,1],type='l',ylim=c(min(Q2iteration),max(Q2iteration)))
#claire(t,Q2iteration,N2)

