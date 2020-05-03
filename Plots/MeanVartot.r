## Plot mean of Var comparing algo ##
x11(); #new window
plot( t, apply( pvar, 1, mean), type='l', col=2) #TENCOV2F
lines( t, apply( varpredHF, 1, mean), col=3)    #SVD2FHF
lines( t, apply( varpredLF, 1, mean), col=4)  #SVD2FLF