################################################
############ SVD simple fidélité ###############
################################################
nb_iteration = N2
coeffs = fSVDfullmethode(Z2, Z1, nb_iteration)
base = coefbase(Z2, coeffs$legere)

nb_parametres = dim(coeffs$legere)[1]

cov.type<- "matern5_2"
#presult = matrix(list(),nrow=nb_parametres,ncol=7,byrow = FALSE) # 4 c'est la taille de la liste predict

presult = matrix(0,N2,N2)
for( i in 1:nb_parametres){
	model <- km(~1,design=data.frame(x=X2),response=data.frame(y=coeffs$legere[i,]), covtype=cov.type, control=list( trace=FALSE))
	presult[i,] <- leaveOneOut.km(model, 'UK')$mean #predict(object = model, data.frame(x=X2),type = 'UK')
}

##################################################
#################   Q2 ###########################
##################################################

Q2iteration = matrix(0,Nt,N2)
for (j in 2:nb_iteration){
dataapp = fpred(presult[1:j,],base[,1:j])
Q2iteration[,j] = errorQ2temp(dataapp, Z2)
}

#####################################################
################## Reconstruction ###################
#####################################################

coeffsapp = matrix(0,nb_parametres,length(presult[,1][[1]]))
coeffsapperr = matrix(0,nb_parametres,length(presult[,1][[1]]))
for (i in 1:nb_parametres){
coeffsapp[i,] = presult[,1][[i]]
coeffsapperr[i,] = presult[,2][[i]]}
dataapp = fpred(coeffsapp,base)
dataerr = fpred(coeffsapperr,base)

Q2SVD1F = errorQ2temp(dataapp, a)

#############################################
########### Tensorisaton simple fi ##########
#############################################
X = X2
Y = Z2
# ceci est la partie d'optilisation des hyperparamètres
lc=c(rep(0.2,dimprob))
tempOpt=optim(lc,fct_cout,derfct_cout,method ="Nelder-Mead")
lc=tempOpt$par

# prédiction
dataapp = matrix(0,length(t),Ndata)
errapp = dataapp
for (ind in 1:Ndata){
        p <- predKmFonc( X, Y, xD[ind,], lc)
        dataapp[,ind] = p$mu[,1]
        errapp[,ind] = p$sd
}

Q2TEN1F = errorQ2temp(dataapp, a)


#############################################
########### SVD multifi #####################
#############################################

approxi = 10^-16	#0.001
coeffsvd = fSVDfull(Z1, Z2, approxi)
basesvd = coefbase(Z2, coeffsvd$lourd)
nb_parametressvd = dim(coeffsvd$lourd)[1]

cov.type<- "matern5_2"
#presult = matrix(list(),nrow=nb_parametressvd,ncol=4,byrow = FALSE) # 4 c'est la taille de la liste predict
presult = matrix(0,N2,N2)

for( i in 1:nb_parametressvd){

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffsvd$legere[i,],coeffsvd$lourd[i,]),nlevel = level, covtype=cov.type, estim.method="LOO", control=list( trace=FALSE))
presult[i,] <- predict( modelmulti, X2,  'UK')$mean + apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall)
#presult[i,] <- apply(matrix(1:N2),1,function(x) CrossValidationMuFicokmAll(modelmulti,x)$CVerrall)
#presult[i,] <- CrossValidationMuFicokmAll(modelmulti,c(1:N2))$CVerrall
#print(i)
print(errorQ2(presult[i,],coeffsvd$lourd[i,]))
##presult[i,] <- coeffsvd$lourd[i,]
}


##################################################
#################   Q2 ###########################
##################################################

Q2iteration = matrix(0,Nt,N2)
for (j in 2:N2){
#coeffsapp = matrix(0,j,length(presult[,1][[1]]))
#for (i in 1:j){
#coeffsapp[i,] = presult[i]
#}
dataapp = fpred(coeffsvd$lourd[1:j,],base[,1:j])
Q2iteration[,j] = errorQ2temp(dataapp, Z2)
}

##################################################
###################  Affichage ###################
##################################################
x11();
plot(apply(Q2iteration,2,mean),type='l')
x11()
plot(t,Q2iteration[,1],type='l',ylim=c(min(Q2iteration),max(Q2iteration)))
claire(t,Q2iteration,N2)


coeffsappsvd = matrix(0,nb_parametressvd,length(presult[,1][[1]]))
coeffsapperrsvd = matrix(0,nb_parametressvd,length(presult[,1][[1]]))
for (i in 1:nb_parametressvd){
coeffsappsvd[i,] = presult[,1][[i]]
coeffsapperrsvd[i,] = presult[,2][[i]]}
dataappsvd = fpred(coeffsappsvd,basesvd)
dataerrsvd = fpred(coeffsapperrsvd,basesvd)

Q2SVD2F = errorQ2temp(dataappsvd, a)

############################################
#############  SVD Multi basse FI ##########
############################################
nb_iteration=4
#######   Décomposition    ###########
coeffs = fSVDfullmethode(Z1, Z2, nb_iteration)
base = coefbase(Z1, coeffs$legere)
nb_parametres = dim(coeffs$legere)[1]
Z2ortho = Z2 - fpred(coeffs$lourd, base) #fpred(coeffinter,basefull)

cov.type<- "matern5_2"
presult = matrix(list(),nrow=nb_parametres,ncol=4,byrow = FALSE) # 4 c'est la taille de la liste predict

for( i in 1:nb_parametres){

modelmulti <- MuFicokm(formula = list(~1,~1),MuFidesign = Dsg, response = list(coeffs$legere[i,],coeffs$lourd[i,]),nlevel = level, covtype=cov.type)
presult[i,] <- predict(object = modelmulti, newdata= xD,type = 'UK')
}

coeffsapp = matrix(0,nb_parametres,length(presult[,1][[1]]))
coeffsapperr = matrix(0,nb_parametres,length(presult[,1][[1]]))
for (i in 1:nb_parametres){
coeffsapp[i,] = presult[,1][[i]]
coeffsapperr[i,] = presult[,2][[i]]}
dataapp = fpred(coeffsapp,base)
dataerr = fpred(coeffsapperr,base)

Q2SVD2FBF = errorQ2temp(dataapp, a)


#############################################
########  Affichage #########################
#############################################
x11();
plot(t,Q2full[,3],col=2,type='l',ylim=c(-0.2,1))
lines(t,Q2TEN1F,col=4)
lines(t,Q2SVD1F,col=3)
lines(t,Q2SVD2F,col=1)
lines(t,Q2SVD2FBF,col=6)
