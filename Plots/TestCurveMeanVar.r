##### Plot of the i-th elment of the test set
#i = 1

#x11();  #create a new window
pdf("curve12_100_1.pdf")
par(mar = par("mar") + c( 0, 1, 0, 0))
plot( t, a[,i], type='l', col=1, ylab="Zh(x,t)", cex.axis=2, cex.lab =2, lwd = 3,
	ylim = c(min(pred[,i] - 1.96*sqrt(pvaralter[,i]),pmean[,i] - 1.96*sqrt(pvar[,i]),pmeanLF[,i] - 1.96*sqrt(pvarLF[,i])),
	max(pred[,i]+1.96*sqrt(pvaralter[,i]),pmean[,i] + 1.96*sqrt(pvar[,i]),pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]))))    #true curve
#TENSVD2F model
#lines( t, pred[,i] + 1.96*sqrt(pvarformulefull[,i]), col=4, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvarformulefull[,i]), col=4, lty=2)
#polygon( c( t, t[length(t):1]), c( pred[,i] + 1.96*sqrt(pvarformulefull[,i]), (pred[,i] - 1.96*sqrt(pvarformulefull[,i]))[length(pred[,i]):1]),
	#col =  4, border = NA, density = 4, lty= "dashed")
#polygon( c( t, t[length(t):1]), c( pred[,i] + 1.96*sqrt(pvaralter[,i]), (pred[,i] - 1.96*sqrt(pvaralter[,i]))[length(pred[,i]):1]),
#	col =  rgb(1,0,0,1), border = NA, density = 6, lty= "dashed")
polygon( c( t, t[length(t):1]), c( pmean[,i] + 1.96*sqrt(pvar[,i]), (pmean[,i] - 1.96*sqrt(pvar[,i]))[length(pred[,i]):1]),
	col =  rgb( 0, 0, 1, 1), border = NA, density = 5, lty= "twodash")
#polygon( c( t, t[length(t):1]), c( pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]), (pmeanLF[,i] - 1.96*sqrt(pvarLF[,i]))[length(pred[,i]):1]),
#	col =  3, border = NA, density = 4, lty= "dotdash")
lines( t, a[,i], lwd = 3, col=1)
#lines( t, pred[,i], col=2) 
lines( t, pmean[,i], lwd = 3, col=4)
#lines( t, pmeanLF[,i], col=3)
#lines( t, pred[,i] + 1.96*sqrt(pvaralter[,i]), col=2, lty=2)
lines( t, pmean[,i] + 1.96*sqrt(pvar[,i]), lwd = 3, col=4, lty=6)
#lines( t, pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]), col=3, lty=4)
#lines( t, pred[,i] - 1.96*sqrt(pvaralter[,i]), col=2, lty=2)
lines( t, pmean[,i] - 1.96*sqrt(pvar[,i]), lwd = 3, col=4, lty=6)
#lines( t, pmeanLF[,i] - 1.96*sqrt(pvarLF[,i]), col=3, lty=4)
#legend( 3, 0.7, legend=c("Real output","Full Method Dirac","Only projection","Full Method Empirical"), col=c(rgb(0,0,0),rgb(1,0,0),rgb(0,1,0),rgb(0,0,1)),
#	lty=1, cex=1.3)

dev.off()
pdf("curve12_100_2.pdf")
par(mar = par("mar") + c( 0, 1, 0, 0))
plot( t, a[,i], type='l', col=1, ylab="Zh(x,t)", cex.axis=2, cex.lab =2, lwd = 3,
	ylim = c(min(pred[,i] - 1.96*sqrt(pvaralter[,i]),pmean[,i] - 1.96*sqrt(pvar[,i]),pmeanLF[,i] - 1.96*sqrt(pvarLF[,i])),
	max(pred[,i]+1.96*sqrt(pvaralter[,i]),pmean[,i] + 1.96*sqrt(pvar[,i]),pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]))))    #true curve
polygon( c( t, t[length(t):1]), c( pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]), (pmeanLF[,i] - 1.96*sqrt(pvarLF[,i]))[length(pred[,i]):1]),
	col =  3, border = NA, density = 4, lty= "dotdash")
lines( t, pmeanLF[,i], lwd = 3, col=3)
lines( t, pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]), lwd = 3, col=3, lty=4)
lines( t, pmeanLF[,i] - 1.96*sqrt(pvarLF[,i]), lwd = 3, col=3, lty=4)
dev.off()

##SVD2FHF model
#lines( t, pmeanHF[,i], type='l', col=4)
#lines( t, pmeanHF[,i]+1.96*sqrt(varpredHF[,i]), col=4, lty=2)
#lines( t, pmeanHF[,i]-1.96*sqrt(varpredHF[,i]), col=4, lty=2)
##SVD2FLF model
#lines( t, pmeanLF[,i], type='l', col=4)
#lines( t, pmeanLF[,i]+1.96*sqrt(varpredLF[,i]), col=4, lty=2)
#lines( t, pmeanLF[,i]-1.96*sqrt(varpredLF[,i]), col=4, lty=2)
