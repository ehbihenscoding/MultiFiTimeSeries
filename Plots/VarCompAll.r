## Comparing the computations of the variance
#
#x11(); #new window
#plot( t, a[,i], type='l', col=2)    #true curve
##TENSVD2F model
#lines( t, pred[,i], col=1) 
##lines( t, predformule[,i], col=6)
#lines( t, pred[,i] + 1.96*sqrt(pvar[,i]), col=3, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvar[,i]), col=3, lty=2)
#lines( t, pred[,i] + 1.96*sqrt(pvaralter[,i]), col=4, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvaralter[,i]), col=4, lty=2)
#lines( t, pred[,i] + 1.96*sqrt(pvarformula[,i]), col=5, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvarformula[,i]), col=5, lty=2)
#lines( t, pred[,i] + 1.96*sqrt(pvarformulefull[,i]), col=6, lty=2)
#lines( t, pred[,i] - 1.96*sqrt(pvarformulefull[,i]), col=6, lty=2)

# Strict compareason of variances
x11();  #new window
plot( t, apply( pvar, 1, mean), col=2, type='l',ylim=c(0,0.02))
lines( t, apply( pvaralter, 1, mean), col=3)
lines( t, apply( pvarformula, 1, mean), col=4)
lines( t, apply( pvarformulefull, 1, mean), col=5)