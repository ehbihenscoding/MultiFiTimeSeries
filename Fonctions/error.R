errorQ2 <- function( estimation, reference){
	PRESS <- sum((estimation -  reference)^2)
	return(1-PRESS/(length(estimation)*var(reference)))
}

errorL2 <- function( estimation, reference){
	interErr <- (estimation -  mean(reference))^2/var(reference)
	return(sum(interErr)/length(interErr))
}

errorQ2temp <- function( estimation, reference){
	output <- 1:dim(estimation)[1]
	for( i in 1:dim(estimation)[1]){
		output[i] <- errorQ2(estimation[i,],reference[i,])
	}
	return(output)
}
