# definition des parametres de l'experience
Nt = 101

#setwd('~/Documents/These/Code/Signaux/Decomposition/Mean/optim/')
#setwd('~/Rcode/optim/')
#setwd('~/Thèse/Travail/codage/MultifideliteTemporel/optim/')

# definition des parametres de l'experience
Nt = 101

source('example/Import.R')
source('example/data.R')
source('SVD2FLF/SVD2FLF.R')
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


source('TENSVD2F/TENSVD2F.R')

#### On s interesse a Z2ortho 
Z2ortho = Z2 - fpred(coeffs$lourd[1:3,], base[,1:3]) 

#### Affichage Z2ortho
pdf("Z2ortho.pdf")
plot(t,Z2ortho[,1],type='l',ylim=c(min(Z2ortho),max(Z2ortho)),
		xlab = "t", ylab = "Z2ortho")
claire(t,Z2ortho,N2)
dev.off()

#### Affichage base basse fi
pdf("baseZ2.pdf")
plot(t,base[,1],type='l',ylim=c(min(base),max(base)),
		xlab = "t", ylab = "Z2base", col=2)
lines( t, base[,2], col=3)
lines( t, base[,3], col=4)
legend( 8, -5, legend=c("1","2","3"), col=c(2,3,4),
	lty=1, cex=0.8)
dev.off()
#claire(t,base,3)
