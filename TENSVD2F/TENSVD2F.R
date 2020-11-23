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
					coef.rho = list(1), coef.trend = list(0,0),	# Prior 
					coef.var = list(0.2,0.2), coef.cov = list(c(1.5,1,2,2,0.5),c(1.5,1,2,2,0.5)), # Prior + hyperparameters
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

#### Nombre de sous set et taille set
Nset = 10
Ntailleset = 8

# initialisation des realisations de variables aléatoires
predortho = array( data = 0, dim = c( Nt, Ndata, Nset))
pvarinter = array( data = 0, dim = c( Nt, Ndata, Nset))
gamma = array( data = 0, dim = c( Nt, Ndata, Nset))
mean = array( data = 0, dim = c( Ndata, Ndata, Nset))
var = array( data = 0, dim = c( Ndata, Ndata, Nset))
### On realise plusieurs fois l'algorithm pour TENSCOV
# ainsi on dipose de plusieurs gamma et coeffs
for ( echanti in 1:(Nset)){
	remove = 1:Ntailleset + (echanti-1)*Ntailleset
	reech = tenscov2f( X1[-remove,], Z1[,-remove], N1-Ntailleset)
	nb_dim = reech$nb_dim
	predortho[,,echanti] = reech$predortho
	pvarinter[,,echanti] = reech$pvarinter
	gamma[,1:nb_dim,echanti] = reech$gamma
	mean[1:nb_dim,,echanti] = reech$mean
	var[1:nb_dim,,echanti] = reech$var
#	print(echanti)
} 
reechtot = tenscov2f( X1, Z1, N1)
source('TENSVD2F/ComputationVar.r', chdir = TRUE)
pred = fpred( reechtot$mean, reechtot$gamma) + reechtot$predortho
pvaralter = fpred( reechtot$var, reechtot$gamma^2) + reechtot$pvarinter
Q2valTENSVD2F = errorQ2temp( pred, a)
Q2valTENSVD2Fvar = errorQ2temp( pmean, a)