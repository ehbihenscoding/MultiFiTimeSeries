##### Plot of the i-th elment of the test set
#i = 1

x11();  #create a new window
plot( t, a[,i], type='l', col=2)    #true curve
#TENSVD2F model
lines( t, pred[,i], col=1) 
#lines( t, pred[,i] + 1.96*sqrt(pvarformulefull[,i]), col=4, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvarformulefull[,i]), col=4, lty=2)
#polygon( c( t, t[length(t):1]), c( pred[,i] + 1.96*sqrt(pvarformulefull[,i]), (pred[,i] - 1.96*sqrt(pvarformulefull[,i]))[length(pred[,i]):1]),
	#col =  4, border = NA, density = 4, lty= "dashed")
polygon( c( t, t[length(t):1]), c( pred[,i] + 1.96*sqrt(pvaralter[,i]), (pred[,i] - 1.96*sqrt(pvaralter[,i]))[length(pred[,i]):1]),
	col =  3, border = NA, density = 4, lty= "dashed")
lines( t, pmean[,i], col=5)
lines( t, pred[,i] + 1.96*sqrt(pvaralter[,i]), col=3, lty=2)
lines( t, pmean[,i] + 1.96*sqrt(pvar[,i]), col=5, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvaralter[,i]), col=3, lty=2)
lines( t, pmean[,i] - 1.96*sqrt(pvar[,i]), col=5, lty=2)
polygon( c( t, t[length(t):1]), c( pmean[,i] + 1.96*sqrt(pvar[,i]), (pmean[,i] - 1.96*sqrt(pvar[,i]))[length(pred[,i]):1]),
	col =  5, border = NA, density = 4, lty= "dashed")

##SVD2FHF model
#lines( t, pmeanHF[,i], type='l', col=4)
#lines( t, pmeanHF[,i]+1.96*sqrt(varpredHF[,i]), col=4, lty=2)
#lines( t, pmeanHF[,i]-1.96*sqrt(varpredHF[,i]), col=4, lty=2)
##SVD2FLF model
#lines( t, pmeanLF[,i], type='l', col=4)
#lines( t, pmeanLF[,i]+1.96*sqrt(varpredLF[,i]), col=4, lty=2)
#lines( t, pmeanLF[,i]-1.96*sqrt(varpredLF[,i]), col=4, lty=2)