# Comparing the computations of the variance

x11(); #new window
plot( t, a[,i], type='l', col=2)    #true curve
#TENSVD2F model
lines( t, pred[,i], col=1) 
#lines( t, predformule[,i], col=6)
lines( t, pred[,i] + 1.96*sqrt(pvar[,i]), col=3, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvar[,i]), col=3, lty=2)
lines( t, pred[,i] + 1.96*sqrt(pvaralter[,i]), col=4, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvaralter[,i]), col=4, lty=2)
lines( t, pred[,i] + 1.96*sqrt(pvarformula[,i]), col=5, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvarformula[,i]), col=5, lty=2)