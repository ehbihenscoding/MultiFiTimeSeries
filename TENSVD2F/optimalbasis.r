# In this part we use the method presented in:
# The efficient cross-validation of principal components applied to PCR
SVDfull = svd(Z1)
Q = t(SVDfull$v)
n = length(SVDfull$d)
E = SVDfull$d * (n-1)
Ezeros = c(E, rep(0,N1-Nt))
nu = n/(n-1)
i = 1
rhoI = nu * sum(t(Q)%*%Z1[,i])^2
zIT = (t(Q)%*%Z1[,i])/sum(t(Q)%*%Z1[,i])
ei = eigen(diag(Ezeros) - rhoI*zIT%*%t(zIT))$values
V = matrix(data = 0, nrow = N1, ncol = N1)
for (j in 1:Nt){
    invinterij = diag(1/(Ezeros - ei[j]+10^-16))
    vectorinterij = invinterij %*% zIT
    V[j,] = vectorinterij/sum(vectorinterij)
}
Qi = Q%*%V
Qireal = svd(Z1[,-1])$v