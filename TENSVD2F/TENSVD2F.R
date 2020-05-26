# On crée une fonction pour générer les moyennes de prédiction:
# et de variance de prédiction
tenscov2f <- function( X1inter, Z1inter, N1inter){
	### On utilise un nombre limité de sortie de basse fidélité
	### pour cela on fixe X1, Z1 et N1
	X1 = X1inter
	Z1 = Z1inter
	N1 = N1inter
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
	#######   Décomposition    ###########
	coeffs = fSVDfullmethode(Z1, Z2, N2)
	base = coefbase(Z1, coeffs$legere)
	Dsg=NestedDesignBuild(design = list(X1,X2))

	# Boucle de calcule de la taille de la base
	# Tant que l'errer ne diminue pas on augmente la taille de la base
	# la boucle peut aussi être arretée par un grand nombre d'itérations
	while (abs(errnew - errold) > 10^-4 & nb_iteration<=N2){
		#nb_parametres = dim(coeffs$legere)[1]
		Z2ortho = Z2 - fpred(coeffs$lourd[1:nb_iteration,], base[,1:nb_iteration]) #fpred(coeffinter,basefull)

		#####################################################
		############### Multifi- krigeage ###################
		#####################################################
		# choix du noyaux
		cov.type<- "matern5_2"

		# utilisation du la métamodélisation par Loic le Gratier
		modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg,
					response = list(coeffs$legere[nb_iteration,],
					coeffs$lourd[nb_iteration,]),nlevel = level,
					covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
		# prediction en LOO
		presult[nb_iteration,] <- predict( modelmulti, X2,  'UK')$mean
			+ apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall) 
		interp	<- predict( modelmulti, xD, 'UK')
		# extraction de la moyenne et la variance
		pcoeff[nb_iteration,]	<-	interp$mean
		pcvari[nb_iteration,]	<-	interp$sig2

		# Reconstruction grace à la base basse fidélité
		dataapp = fpred(presult[1:nb_iteration,],base[,1:nb_iteration])

		#####################################################
		###################    Tensorisation   ##############
		#####################################################

		# on utilise juste la partie orthogonale
		X = X2
		Y = Z2ortho
		# ceci est la partie d'optilisation des hyperparamÃ¨tres
		lc=c(rep(0.2,dimprob))
		tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
		lc=tempOpt$par

		# prédiction
		dataapportho = matrix(0,length(t),N2)
		# pour N2 elements (soit la taille du set haute fidélité)
		for (ind in 1:N2){
		        p <- predKmFonc( X[-ind,], Y[,-ind], X2[ind,], lc)
		        dataapportho[,ind] = p$mu[,1]
		}

		# on somme les deux parties pour le set
		# d'apprentissage
		zD = dataapp + dataapportho
		### Q2 pour le set d'apprentissage
		Q2full[,nb_iteration] = errorQ2temp(zD, Z2)
		# actualisation de l'erreur
		errold	=	errnew
		errnew = mean(Q2full[,nb_iteration])

		# ajout d'une itétation
		nb_iteration	=	nb_iteration +1
	}
	# dépouillement de la sortie de boucle
	nb_optimTENCOV <- nb_iteration-2
	#### Dans le cas ou la multifidélité n'apporte rien
	#### on a nb_optimTENCOV ==0 sinon on est dans les autres cas
	if(nb_optimTENCOV == 0){
		X = X2
		Y = Z2
		# ceci est la partie d'optimisation des hyperparamètres
		lc=c(rep(0.2,dimprob))
		tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
		lc=tempOpt$par
		# creation des matrices de résultat
		pred = matrix( 0, length(t), Ndata)
		pvar = matrix( 0, length(t), Ndata)

		# On prédit pour la base de test
		for (ind in 1:Ndata){
			p <- predKmFonc( X, Y, xD[ind,], lc)
			pred[,ind] = p$mu[,1]
			pvar[,ind] = p$sd
		}

	} else{	# dans le cas ou la base optimale a une taille suffisante:
		# on doit à nouveau réaliser l'optimisation pour la partie orthogonale
		X = X2
		# on soustrait à Z2 la partie apprise avec
		# la base basse fidélité
		Y = Z2 - fpred(coeffs$lourd[1:nb_optimTENCOV,], base[,1:nb_optimTENCOV])
		# ceci est la partie d'optilisation des hyperparamètres
		lc=c(rep(0.2,dimprob))
		tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
		lc=tempOpt$par
		# initialisation des sorties
		predortho = matrix(0,length(t),Ndata)
		pvarinter = matrix( 0, length(t), Ndata)
		# prediction sur la base
		for (ind in 1:Ndata){
			p <- predKmFonc( X, Y, xD[ind,], lc)
			predortho[,ind] = p$mu[,1]
			pvarinter[,ind] = p$sd
		}
		# pour la partie parallèle on a déjà les informations dans
		# base, pcoeff et pcvari
	}
	# les sorties de la fonction sont les suivantes
	return( list( predortho=predortho, pvarinter=pvarinter, gamma=base[,1:nb_optimTENCOV],
		mean=pcoeff[1:nb_optimTENCOV,], var=pcvari[1:nb_optimTENCOV,], nb_dim = nb_optimTENCOV))
}

### Initialisation nécessaire à cause d'une erreur de programmation
X = X2
Y = Z2
# initialisation des realisations de variables aléatoires
predortho = array( data = 0, dim = c( Nt, Ndata, N1-N2))
pvarinter = array( data = 0, dim = c( Nt, Ndata, N1-N2))
gamma = array( data = 0, dim = c( Nt, Ndata, N1-N2))
mean = array( data = 0, dim = c( Ndata, Ndata, N1-N2))
var = array( data = 0, dim = c( Ndata, Ndata, N1-N2))
### On realise plusieurs fois l'algorithm pour TENSCOV
# ainsi on dipose de plusieurs gamma et coeffs
for ( echanti in 1:(N1-N2)){
	reech = tenscov2f( X1[-echanti,], Z1[,-echanti], N1-1)
	nb_dim = reech$nb_dim
	predortho[,,echanti] = reech$predortho
	pvarinter[,,echanti] = reech$pvarinter
	gamma[,1:nb_dim,echanti] = reech$gamma
	mean[1:nb_dim,,echanti] = reech$mean
	var[1:nb_dim,,echanti] = reech$var
	print(echanti)
} 
source('TENSVD2F/ComputationVar.r', chdir = TRUE)

#	# Calcule de la moyenne prediction formule
#	Ntirage <- 50
#	meanpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
#	for (tirage in 1:Ntirage){
#		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]
#		meanpredtot[,,tirage] <- fpred( pcoeff[1:nb_optimTENCOV,], basetemp)
#	}
#	predformule <- predortho + apply( meanpredtot, c(1,2), mean)
	# Equation dans le cas de la base parfaite moyenne prediction


############### A calculer ########## !!!!!!!!!!!!!!!
#	pred = predortho + fpred( pcoeff[1:nb_optimTENCOV,], base[,1:nb_optimTENCOV])

## cette partie est complement fausse:
#	## Calcule de la variance full random
#	Ntirage <- 50
#	varpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
#	for (tirage in 1:Ntirage){
#		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]
#		coefftemp <- randn( nb_optimTENCOV, Ndata) * pvar[1:nb_optimTENCOV,] + pcoeff[1:nb_optimTENCOV,]
#		varpredtot[,,tirage] <- fpred( coefftemp, basetemp)
#	}
#	pvarSVDinter <- apply( varpredtot, c(1,2), var)	
#
#	#### Compute of full formula variance
#	Ntirage <- 100
#	meanpredtot <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
#	meanpredsqrt <- array( data = 0, dim = c( Nt, Ndata, Ntirage))
#	for (tirage in 1:Ntirage){
#		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]	# + gamma$base[,1:nb_optimTENCOV]
#		meanpredtot[,,tirage] <- fpred( pcoeff[1:nb_optimTENCOV,], basetemp)
#		basetemp <- randn( Nt, nb_optimTENCOV) * gamma$varbase[,1:nb_optimTENCOV] + basesvd[,1:nb_optimTENCOV]	# + gamma$base[,1:nb_optimTENCOV]
#		meanpredsqrt[,,tirage] <- fpred( pcoeff[1:nb_optimTENCOV,], basetemp)^2 + fpred( pcvari[1:nb_optimTENCOV,], basetemp^2)
#	}
#	pvarformulefull <- pvarinter + apply( meanpredsqrt, c(1,2), mean) - apply( meanpredtot, c(1,2), mean)^2

#### A calculer
#	#Compilation des variances
#	pvar = pvarinter + pvarSVDinter
#	pvaralter = pvarinter + fpred(pcvari[1:nb_optimTENCOV,],basesvd[,1:nb_optimTENCOV]^2)
## on calcule le Q2 pour la base de test
#Q2valTENSVD2F = errorQ2temp( pred, a)