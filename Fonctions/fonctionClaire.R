claire <- function(t,Y,val,...){
	#x11()
	#plot( t, Y[,1],type='l',ylim=c(min(Y),max(Y)))
	for (i in 1:val){
		lines(t, Y[,i],...)
	}
}

clairebalistique <- function( Y, val){
	x11()
	plot( Y[[1]], type='l')
	for (i in 2:val){
		lines(Y[[i]])
	}
}
