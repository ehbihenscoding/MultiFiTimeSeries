#setwd('~/Documents/These/Code/Signaux/Decomposition/Mean/optim/')
#setwd('~/Rcode/optim/')
setwd('~/Thèse/Travail/codage/MultifideliteTemporel/optim/')

# definition des parametres de l'experience
Nt = 101

source('example/Import.R')
source('example/data.R')
source('SVD2FHF/SVD2FHF.R')
source('TENSVD2F/TENSVD2F.R')

#### On s interesse a Z2ortho 
Z2ortho = Z2 - fpred(coeffs$lourd[1:nb_optimTENCOV,], base[,1:nb_optimTENCOV]) 

#### Affichage Z2ortho
x11();plot(t,Z2ortho[,1],type='l',ylim=c(min(Z2ortho),max(Z2ortho)))
claire(t,Z2ortho,N2)

#### Affichage base basse fi
x11();plot(t,base[,1],type='l',ylim=c(min(base),max(base)))
claire(t,base,N2)
