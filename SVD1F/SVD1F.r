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

Q2nbpara = apply(Q2iteration, 2, mean)
##################################################
##############   Affichage  ######################
##################################################
	
x11();
plot(apply(Q2iteration,2,mean),type='l')
x11()
plot(t,Q2iteration[,1],type='l',ylim=c(min(Q2iteration),max(Q2iteration)))
claire(t,Q2iteration,N2)


