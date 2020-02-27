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
# Initialisation err et dim base
i <- 1
err <- 1

while( err >0.8 & i<=N2){
	model <- km(~1,design=data.frame(x=X2),response=data.frame(y=coeffs$legere[i,]), covtype=cov.type, control=list( trace=FALSE))
	presult[i,] <- leaveOneOut.km(model, 'UK')$mean #predict(object = model, data.frame(x=X2),type = 'UK')
	err	<-	errorQ2(presult[i,],coeffs$legere[i,])
	i	<-	i+1
}

nb_opt	<-	max( i-2, 1)
##################################################
#################   Q2 ###########################
##################################################

Q2iteration = errorQ2temp( fpred(presult[1:nb_opt,],base[,1:nb_opt]), Z2)
