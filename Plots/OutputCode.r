# Data 
setofData = Z1

#x11();

#pdf("Data.pdf")
plot( t, setofData[,N2], type='l', ylim=c(min(setofData),max(setofData)),
        ylab="Zh(x,t)",col=rgb(0,0,1,0.3))
claire( t, setofData, N1, col=rgb(0,0,1,0.3))
claire( t, Z2, N2, col=2)
#dev.off()