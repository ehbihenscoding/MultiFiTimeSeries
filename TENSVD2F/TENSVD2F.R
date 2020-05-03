### Construction de la matrice de Q2
Nexp = N2
Q2full = matrix(0,Nt,Nexp)

#### initialisation du paramètre d'itérarion
nb_iteration <- 1
#### initialisation de paramètre d'erreur
errnew <- 0
errold <- -1
### Pour la première erreur on prend le cas simple sans la multifidélité
X = X2
Y = Z2
# ceci est la partie d'optilisation des hyperparamètres
lc=c(rep(0.2,dimprob))
tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
lc=tempOpt$par

# prédiction
dataapp = matrix(0,length(t),N2)
for (ind in 1:N2){
        p <- predKmFonc( X[-ind,], Y[,-ind], X2[ind,], lc)
        dataapp[,ind] = p$mu[,1]
}

errnew = mean(errorQ2temp(dataapp, Z2)) - 0.2

presult = matrix(0,N2,N2)
pcoeff	=	matrix( 0, N2, Ndata)
pcvari	=	matrix( 0, N2, Ndata)
#######   DÃ©composition    ###########
coeffs = fSVDfullmethode(Z1, Z2, N2)
base = coefbase(Z1, coeffs$legere)

while ((errnew - errold) > 10^-4 & nb_iteration<=N2){
	#nb_parametres = dim(coeffs$legere)[1]
	Z2ortho = Z2 - fpred(coeffs$lourd[1:nb_iteration,], base[,1:nb_iteration]) #fpred(coeffinter,basefull)
	
	#####################################################
	############### Multifi- krigeage ###################
	#####################################################
	cov.type<- "matern5_2"
	
	modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg,
				response = list(coeffs$legere[nb_iteration,],
				coeffs$lourd[nb_iteration,]),nlevel = level,
				covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
	presult[nb_iteration,] <- predict( modelmulti, X2,  'UK')$mean + apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall) 
	interp	<- predict( modelmulti, xD, 'UK')
	pcoeff[nb_iteration,]	<-	interp$mean
	pcvari[nb_iteration,]	<-	interp$sig2
	####################################################
	################## Reconstruction ###################
	##################################################### 
	dataapp = fpred(presult[1:nb_iteration,],base[,1:nb_iteration])
	
	###################    Tensorisation   ##############
	
	# on utilise juste la partie orthogonale
	X = X2
	Y = Z2ortho
	# ceci est la partie d'optilisation des hyperparamÃ¨tres
	lc=c(rep(0.2,dimprob))
	tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
	lc=tempOpt$par
	
	# prÃ©diction
	dataapportho = matrix(0,length(t),N2)
	for (ind in 1:N2){
	        p <- predKmFonc( X[-ind,], Y[,-ind], X2[ind,], lc)
	        dataapportho[,ind] = p$mu[,1]
	}
	
	zD = dataapp + dataapportho
	### Q2 pour le set d'apprentissage
	Q2full[,nb_iteration] = errorQ2temp(zD, Z2)
	# actualisation de l'erreur
	errold	=	errnew
	errnew = mean(Q2full[,nb_iteration])
	
	nb_iteration	=	nb_iteration +1
}
# dépouillement de la sortie de boucle
nb_optimTENCOV <- nb_iteration-2
#### Dans le cas ou la multifidélité n'apporte rien
#### on a nb_optimTENCOV ==0 sinon on est dans les autres cas
if(nb_optimTENCOV == 0){
	X = X2
	Y = Z2
	# ceci est la partie d'optilisation des hyperparamètres
	lc=c(rep(0.2,dimprob))
	tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
	lc=tempOpt$par
	pred = matrix( 0, length(t), Ndata)
	pvar = matrix( 0, length(t), Ndata)
	for (ind in 1:Ndata){
		p <- predKmFonc( X, Y, xD[ind,], lc)
		pred[,ind] = p$mu[,1]
		pvar[,ind] = p$sd
	}

} else{
	X = X2
	Y = Z2 - fpred(coeffs$lourd[1:nb_optimTENCOV,], base[,1:nb_optimTENCOV])
	# ceci est la partie d'optilisation des hyperparamètres
	lc=c(rep(0.2,dimprob))
	tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
	lc=tempOpt$par
	predortho = matrix(0,length(t),Ndata)
	pvarinter = matrix( 0, length(t), Ndata)
	for (ind in 1:Ndata){
		p <- predKmFonc( X, Y, xD[ind,], lc)
		predortho[,ind] = p$mu[,1]
		pvarinter[,ind] = p$sd
	}
#	# Calcule de la moyenne prediction formule
#	Ntirage <- 50
#	meanpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
#	for (tirage in 1:Ntirage){
#		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]
#		meanpredtot[,,tirage] <- fpred( pcoeff[1:nb_optimTENCOV,], basetemp)
#	}
#	predformule <- predortho + apply( meanpredtot, c(1,2), mean)
	# Equation dans le cas de la base parfaite moyenne prediction
	pred = predortho + fpred( pcoeff[1:nb_optimTENCOV,], base[,1:nb_optimTENCOV])
	## Calcule de la variance full random
	Ntirage <- 50
	varpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
	for (tirage in 1:Ntirage){
		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]
		coefftemp <- randn( nb_optimTENCOV, Ndata) * pvar[1:nb_optimTENCOV,] + pcoeff[1:nb_optimTENCOV,]
		varpredtot[,,tirage] <- fpred( coefftemp, basetemp)
	}
	pvarSVDinter <- apply( varpredtot, c(1,2), var)	
	## Calcule de la variance best formula
	Ntirage <- 20
	varpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
	for (tirage in 1:Ntirage){
		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]
		varpredtot[,,tirage] <- fpred( pvar[1:nb_optimTENCOV,], basetemp)
	}
	pvarSVDform <- apply( varpredtot, c(1,2), var)

	#### Compute of full formula variance
	Ntirage <- 20
	meanpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
	meanpredsqrt <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
	for (tirage in 1:Ntirage){
		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + gamma$base[,1:nb_optimTENCOV]
		meanpredtot[,,tirage] <- fpred( pcoeff[1:nb_optimTENCOV,], basetemp)
		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + gamma$base[,1:nb_optimTENCOV]
		meanpredsqrt[,,tirage] <- fpred( pcoeff[1:nb_optimTENCOV,]^2, basetemp^2)
	}
	pvarformulefull <- pvarinter + apply( meanpredsqrt, c(1,2), mean) - apply( meanpredtot, c(1,2), mean)^2

	#Compilation des variances
	pvar = pvarinter + pvarSVDinter
	pvaralter = pvarinter + fpred(pvar[1:nb_optimTENCOV,],basesvd[,1:nb_optimTENCOV]^2)
	pvarformula = pvarinter + pvarSVDform
}
Q2valTENSVD2F = errorQ2temp( pred, a)