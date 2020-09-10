meanGamma = apply( gamma, c(1,2), mean)
varGamma = apply( gamma, c(1,2), var)
i=10

##x11();
#pdf("Basis01.pdf")
#plot( t, meanGamma[,i], type='l',col=3,ylab="",ylim=c(min(meanGamma[,1]),max(meanGamma[,1])))
#lines( t, meanGamma[,i]+1.96*sqrt(varGamma[,i]),col=3,lty=2)
#lines( t, meanGamma[,i]-1.96*sqrt(varGamma[,i]),col=3,lty=2)
#polygon( c( t, t[length(t):1]), c( meanGamma[,i] + 1.96*sqrt(varGamma[,i]), (meanGamma[,i] - 1.96*sqrt(varGamma[,i]))[Nt:1]),
#	col =  3, border = NA, density = 4, lty= "dashed")
#lines( t, meanGamma[,i], col=3)
#dev.off()
##claire( t, meanGamma, 3)
##claire( t, meanGamma+1.96*varGamma,3)
##claire( t, meanGamma-1.96*varGamma,3)


##x11();
#pdf("Basis03.pdf")
#plot( t, meanGamma[,i], type='l',col=3,ylab="")#,ylim=c(min(meanGamma[,1]),max(meanGamma[,1])))
#claire( t, gamma[,i,],10)
#lines( t, meanGamma[,i]+1.96*sqrt(varGamma[,i]),col=3,lty=2)
#lines( t, meanGamma[,i]-1.96*sqrt(varGamma[,i]),col=3,lty=2)
#polygon( c( t, t[length(t):1]), c( meanGamma[,i] + 1.96*sqrt(varGamma[,i]), (meanGamma[,i] - 1.96*sqrt(varGamma[,i]))[Nt:1]),
#	col =  3, border = NA, density = 4, lty= "dashed")
#dev.off()

#x11();
pdf("Basis06.pdf")
plot( t, gamma[,1,i],type='l',col=3,ylab="")
claire( t, gamma[,,i],10, col=3)
dev.off()