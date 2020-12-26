Matern52<-function(distances){
  return = (1+sqrt(5)*distances+5/3*distances^2)*exp(-sqrt(5)*distances)  # OK
}

### Calcule de la dérivé pour amméliorrer l'optimisaton
Matern52der<-function(x){
	return = -5/3 * x * (1 + sqrt(5)*x) * exp(-sqrt(5)*x)
	#return = (sqrt(5)+5/3*x)*exp(-sqrt(5)*x) + sqrt(5)*(1+sqrt(5)*x+5/3*x^2)*exp(-sqrt(5)*x)  # OK
}

covMatern <- function(z,y=NULL){
	d <- dim(z)[2]
	if(is.null(y)){
		distances=as.matrix(dist(z,upper=TRUE))
	} else{
		y=matrix(y,ncol=d)
		Nz=dim(as.matrix(z))[1]
		Ny=dim(as.matrix(y))[1]
		distances=matrix(0,Nz,Ny)
		for(i in 1:Ny){
			if(d==1){
				distances[,i]=abs(z-y[i])
			} else{
				distances[,i]=sqrt(apply((z-t(as.numeric(y[i,])+matrix(0,d,Nz)))^2,1,sum))
			}
		}
	}
	return = Matern52(distances)
}

# dérivée de la covariance de matern
covMaternder <- function( z, l){
	d <- dim(z)[2]
	D <- length(l)
	distance = as.matrix(dist(z,upper=TRUE))
	G <- list()	# pour généraliser à toutes les échelles
	for (k in 1:D){ G[[k]] = distance}
	for( i in 1:d){
		for(j in 1:d){
			if( i == j) {
				for(k in 1:D){ G[[k]][i,j] <-0 }
			}else{
				for(k in 1:D){ G[[k]][i,j] <-(z[k,i]-z[k,j])^2*l[k]*Matern52der(distance[i,j])/distance[i,j] }
			}
		}
	}
	return(lapply(G,as.matrix))
}

covtemp <- function( y, iRz){
	Nz = dim(iRz)[1]
	Nt = dim(y)[1]
	mhat <- apply(y,1,mean)	# cas du krigeage simple
	mhat_mat <- matrix( 0, Nt, Nz) + as.numeric(mhat)
	Rtparfait=1/(Nz-1)*(y-mhat_mat) %*% iRz %*% t(y-mhat_mat)	# equation 43
	Rt = 0.5*(Rtparfait+t(Rtparfait))
	return(Rt)
}

# Fonction d'erreur de LOO
errLOO <- function(z,y, l){

	d <- dim(z)[2]
	Nz <- dim(z)[1]
	#Nt <- dim(y)[1]
	ht <- matrix(1,Nz,1)
	# covariance
	Rz <- covMatern(z %*% (1/l*diag(l*0+1)))
	iRz <- inv(Rz+10^-4*diag(Nz))
	Rt <- covtemp( y, iRz)

	# Erreur LOO en temps et en espace
	############################
	### nugget 
	tau2 = 10^-5
	K = inv(Rz + tau2 * diag(Nz))	# le calcule de cette approximation est dispo sur document Baptiste Kerleguer
	###
	tempsig = diag(K)^(-1)
	diff = diag(tempsig) %*% K %*% t(y)
	sig2 = matrix(tempsig,ncol=1)%*%t(diag(Rt))
	############################
	
	return=list(diff2=sum(diff^2),sig2=sig2)
	
}

# fonction de prédiction conditionné par les paramètres
predKmFonc <- function( z, y, data, l){

	Nz <- dim(z)[1]
	#Ndata <- dim(data)[1]
	ht <- matrix(1,Nz,1)
	# covariance
	Rz <- covMatern(z %*% (1/l*diag(l*0+1)))
	iRz <- inv(Rz)
	rz <- covMatern(z %*% (1/l*diag(l*0+1)), data %*% (1/l*diag(l*0+1)))
	Rt <- covtemp( y, iRz)
	
	# calcule de A*
	A_star = (y %*% iRz %*% ht)/sum(iRz)	# formule 2 equation 36
	# calcule de la moyenne du processus
	mu_starstar = A_star  + (y - A_star%*% t(ht)) %*% iRz %*% rz	# formule 3 et 4 equation 36 matrix(1,1,Ndata) permet d'avoir les bonnes tailles de matrice mais ne représente rien
	# calcule de la covariance du processus
	c <- covMatern(data %*% (1/l*diag(l*0+1))) - t(rz) %*% iRz %*% rz 	# equation 33 sous formule
	R_starstar <- Rt %x% as.numeric(c+(1-t(ht)%*%iRz%*%rz)^2/sum(iRz))

	return( list( mu = mu_starstar, C = R_starstar, sd = sqrt(diag(R_starstar))))
	
}

# minimisation de l'erreur LOO de prediction
fct_cout<-function(l){
	temp=errLOO(X,Y,abs(l))
	return = temp$diff2 #sum(temp$diff2)
}

derfct_cout <- function(l){
	# calcule des coefficients
	temp = covMaternder(X , abs(l))
	d = length(temp)
	Rz <- covMatern(X %*% diag(1/abs(l)))
	iRz <- inv(Rz)
	# calcules des matrices
	D = list()
	for( k in 1:d){D[[k]] = 2*Y %*% iRz %*% diag(diag(iRz)^(-2)) %*%diag(diag(iRz %*% temp[[k]] %*% iRz))%*%diag(diag(iRz)^(-1))%*% iRz %*% t(Y) - 2*Y %*% iRz %*% diag(diag(iRz)^(-2)) %*% iRz %*% temp[[k]] %*% iRz %*%t(Y)}
	return= lapply(D,sum)
}

