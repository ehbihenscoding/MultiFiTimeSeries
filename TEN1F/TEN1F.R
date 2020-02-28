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
for (ind in 1:Ndata){
        p <- predKmFonc( X, Y, xD[ind,], lc)
        dataapp[,ind] = p$mu[,1]
}

Q2valTEN1F = errorQ2temp(dataapp, a)

