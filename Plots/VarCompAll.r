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
plot( t, apply( pvaralter, 1, mean), col=3, type='l', ylim=c(0,0.07))
lines( t, apply( pvarinter, 1, mean), col=5)
polygon( c( t, t[length(t):1]), c( apply( pvarinter, 1, mean), rep(0, Nt)),
	col =  5, border = NA, density = 4, lty= "dashed")
lines( t, apply(pvarinter + apply( meanpredsqrt, c(1,2), mean), 1, mean), col=6)
polygon( c( t, t[length(t):1]), c( apply(pvarinter + apply( meanpredsqrt, c(1,2), mean), 1, mean), apply( pvarinter, 1, mean)[Nt:1]),
	col =  6, border = NA, density = 4, lty= "dashed")
lines( t, apply( pvarformulefull, 1, mean), col=4)
polygon( c( t, t[length(t):1]), c( apply( pvarformulefull, 1, mean), apply(pvarinter + apply( meanpredsqrt, c(1,2), mean), 1, mean)[Nt:1]),
	col =  4, border = NA, density = 3, lty= "dashed")