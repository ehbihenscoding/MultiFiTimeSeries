##### Plot of the i-th elment of the test set
#i = 1

#x11();  #create a new window
pdf("curve12_100.pdf")
plot( t, a[,i], type='l', col=2, ylab="Zh(x,t)",
	ylim = c(min(pred[,i] - 1.96*sqrt(pvaralter[,i]),pmean[,i] - 1.96*sqrt(pvar[,i]),pmeanLF[,i] - 1.96*sqrt(pvarLF[,i])),
	max(pred[,i]+1.96*sqrt(pvaralter[,i]),pmean[,i] + 1.96*sqrt(pvar[,i]),pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]))))    #true curve
#TENSVD2F model
#lines( t, pred[,i] + 1.96*sqrt(pvarformulefull[,i]), col=4, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvarformulefull[,i]), col=4, lty=2)
#polygon( c( t, t[length(t):1]), c( pred[,i] + 1.96*sqrt(pvarformulefull[,i]), (pred[,i] - 1.96*sqrt(pvarformulefull[,i]))[length(pred[,i]):1]),
	#col =  4, border = NA, density = 4, lty= "dashed")
polygon( c( t, t[length(t):1]), c( pred[,i] + 1.96*sqrt(pvaralter[,i]), (pred[,i] - 1.96*sqrt(pvaralter[,i]))[length(pred[,i]):1]),
	col =  rgb(0,1,0,0.2), border = NA)#, density = 4, lty= "dashed")
polygon( c( t, t[length(t):1]), c( pmean[,i] + 1.96*sqrt(pvar[,i]), (pmean[,i] - 1.96*sqrt(pvar[,i]))[length(pred[,i]):1]),
	col =  rgb( 0, 0, 1, 0.4), border = NA)#, density = 4, lty= "dashed")
polygon( c( t, t[length(t):1]), c( pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]), (pmeanLF[,i] - 1.96*sqrt(pvarLF[,i]))[length(pred[,i]):1]),
	col =  rgb( 0, 1, 1, 0.4), border = NA)#, density = 4, lty= "dashed")
lines( t, a[,i], col=2)
lines( t, pred[,i], col=3) 
lines( t, pmean[,i], col=4)
lines( t, pmeanLF[,i], col=5)
lines( t, pred[,i] + 1.96*sqrt(pvaralter[,i]), col=3, lty=2)
lines( t, pmean[,i] + 1.96*sqrt(pvar[,i]), col=4, lty=2)
lines( t, pmeanLF[,i] + 1.96*sqrt(pvarLF[,i]), col=5, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvaralter[,i]), col=3, lty=2)
lines( t, pmean[,i] - 1.96*sqrt(pvar[,i]), col=4, lty=2)
lines( t, pmeanLF[,i] - 1.96*sqrt(pvarLF[,i]), col=5, lty=2)
legend( 4, 0.7, legend=c("Real output","Only projection","Full Method Dirac","Full Method Empirical"), col=c(rgb(1,0,0),rgb(0,1,0),rgb(0,1,1),rgb(0,0,1)),
	lty=1, cex=0.8)

dev.off()
##SVD2FHF model
#lines( t, pmeanHF[,i], type='l', col=4)
#lines( t, pmeanHF[,i]+1.96*sqrt(varpredHF[,i]), col=4, lty=2)
#lines( t, pmeanHF[,i]-1.96*sqrt(varpredHF[,i]), col=4, lty=2)
##SVD2FLF model
#lines( t, pmeanLF[,i], type='l', col=4)
#lines( t, pmeanLF[,i]+1.96*sqrt(varpredLF[,i]), col=4, lty=2)
#lines( t, pmeanLF[,i]-1.96*sqrt(varpredLF[,i]), col=4, lty=2)