#####################################################
############ Generation donnes exp  #################
#####################################################

#dimension du probl?me
dimprob = 5
segm = 2
Ndata = segm^6
xD = matrix( 0, ncol = dimprob, nrow = Ndata) # matrix(runif(Ndata*dimprob,0,1),ncol=dimprob)

for (i in 1:Ndata){
	for (j in 1:dimprob){
		xD[i,j] = (as.integer(((i-1)/((segm+1)^(j-1)))%%4)+1) *1/(segm+1)
	}
}

# fonction ? estimerse

a = f(xD)

# définition de l'information disponible
level = 2

# design d'expérience
N1 <- 100
N2 <- 15
X1 <- lhsDesign(N1, dimprob) ###matrix(runif(N1*dimprob,0,1),ncol=dimprob) #fappro(DNest$PX)
X2 <- lhsDesign(N2, dimprob) ###matrix(runif(N2*dimprob,0,1),ncol=dimprob) #f(ExtractNestDesign(DNest,2))
#Dsg <- NestedDesignBuild(design = list(matrix(X1,ncol=dimprob),matrix(X2,ncol=dimprob)))
Dsg <- NestedDesignBuild(design = list(X1$design,X2$design))
X1 <- Dsg$PX
X2 <- ExtractNestDesign(Dsg,2)
# realisation de la fonction
Z1 <- fappro(X1)
Z2 <- f(X2)

