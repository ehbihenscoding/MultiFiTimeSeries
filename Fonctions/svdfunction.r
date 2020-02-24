fSVDfull = function(Y1,Y2,err){
# Y1 code légé
# Y2 code lourd
sY = svd(Y2) # on fait la svd code lourd pour avoir la base la plus précise dans ce cas
coeffs = t(sY$v) # calcule de la base pour le code lourd
Iner = sY$d^2/sum(sY$d^2) # calcule de l'erreur 
selec = 1:which(cumsum(Iner)>=(1-err))[1] # sélection de la grandeur d'intéret pour l'erreur
if (length(selec)==1){coeffs_red2 =matrix(coeffs[selec,],nrow=1) 
}else {coeffs_red2 = coeffs[selec,]}
big_diag_red_inv = diag(sY$d)*0
for(i in 1:dim(coeffs_red2)[1]){big_diag_red_inv[i,i]=1/sY$d[i]}
coeffs_red1 = ((big_diag_red_inv %*% t(sY$u)) %*% Y1)[1:dim(coeffs_red2)[1],]
return(list(lourd = coeffs_red2,legere = coeffs_red1))
}

fSVDfullmethode = function(Y1,Y2,n){
# Y1 code légé
# Y2 code lourd
sY = svd(Y1) # on fait la svd code lege pour avoir la base la plus précise dans ce cas
coeffs = t(sY$v) # calcule de la base pour le code lege
Iner = sY$d^2/sum(sY$d^2) # calcule de l'erreur 
#selec = 1:which(cumsum(Iner)>=(1-err))[1] # sélection de la grandeur d'intéret pour l'erreur
#if (length(selec)==1){coeffs_red1 =matrix(coeffs[selec,],nrow=1) 
#}else {coeffs_red1 = coeffs[selec,]}
big_diag_red_inv = diag(sY$d)*0
for(i in 1:length(sY$d)){big_diag_red_inv[i,i]=1/sY$d[i]}
coeffs_red2 = ((big_diag_red_inv %*% t(sY$u)) %*% Y2)#[1:dim(coeffs_red1)[1],]
return(list(lourd = coeffs_red2[1:n,],legere = coeffs[1:n,],fulllourd=coeffs_red2,basecoeff=coeffs,Iner=Iner))
}

coefbase = function(Y, coeffs){ return(Y%*%t(coeffs))}

fpred = function(coeffs,base){
	if (is.null(dim(base))){
		return(base%*%t(coeffs))}
	else{ return(base%*%coeffs)}
}

