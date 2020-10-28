##### Affichage Q2 #####
x11();  #new figure
plot( t, Q2SVD2FHFmean, type='l', ylim=c(0.8,1))    #Q2SVD2FHF
lines( t, Q2TENCOV2Fmean, col=2)  #Q2TENCOV2F
lines( t, Q2SVD2FLFmean, col=3)   #Q2SVD2FLF
lines( t, Q2SVD1Fmean, col=4)   #Q2SVD1F

#pdf("ortho.pdf")
#par(mar = par("mar") + c( 0, 1, 0, 0))
#plot(t,reechtot$predortho[,1],type='l', ylab='Zortho', cex.lab=1.5, cex.axis=1.5,ylim=c(min(reechtot$predortho),max(reechtot$predortho)))
#claire(t,reechtot$predortho,10)
#dev.off()