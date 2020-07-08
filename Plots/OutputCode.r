# Data 
setofData = Z1

#x11();

pdf("lowFiData.pdf")
plot( t, setofData[,N2], type='l', ylim=c(min(setofData),max(setofData)),
        ylab="Zh(x,t)")
claire( t, setofData[,(N1-N2):N1], N2)
dev.off()