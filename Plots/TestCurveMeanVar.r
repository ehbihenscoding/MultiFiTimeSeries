##### Plot of the i-th elment of the test set
i = 1

x11();  #create a new window
plot( t, a[,i], type='l', col=2)    #true curve
#TENSVD2F model
lines( t, pred[,i], col=1) 
lines( t, pred[,i] + 1.96*sqrt(pvar[,i]), col=1, lty=2)
lines( t, pred[,i] - 1.96*sqrt(pvar[,i]), col=1, lty=2)
#SVD2FHF model
lines( t, pmean[,i], type='l', col=3)
lines( t, pmean[,i]+1.96*sqrt(varpredHF[,i]), col=3, lty=2)
lines( t, pmean[,i]-1.96*sqrt(varpredHF[,i]), col=3, lty=2)
#SVD2FLF model
lines( t, pmeanLF[,i], type='l', col=4)
lines( t, pmeanLF[,i]+1.96*sqrt(varpred[,i]), col=4, lty=2)
lines( t, pmeanLF[,i]-1.96*sqrt(varpred[,i]), col=4, lty=2)