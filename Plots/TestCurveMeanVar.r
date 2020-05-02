##### Plot of the i-th elment of the test set
i = 1

x11();  #create a new window
plot( t, a[,i], type='l', col=2)    #true curve
lines( t, pred[,i], col=1)  #TENSVD2F model
lines( t, pred[,i] + 1.96*sqrt(pvar[,1]), col=1, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvar[,1]), col=1, lty=2)