# Résolution linéaire du système plaque-pendule (sans amortissement)

Nt=100+1
t=seq(0,10,len=Nt)

# parametres a retrouver
d_beta=2
Mopt=2; kopt=1.2
beta_opt=c(Mopt,kopt)

# X <-> energie du systeme
d_x=1
l=2; g=9.81
X_MIN=g*l*sin(pi/9)
X_MAX=g*l*sin(pi/3)

# fonctions de mise a l echelle des entrees
mise_echelle<-function(x,X_MIN,X_MAX){
  d=length(X_MIN)
  x=matrix(x,ncol=d)
  Nx=dim(x)[1]
  if(d>1){
    return = x%*% diag((X_MAX-X_MIN))+t(matrix(0,d,Nx)+X_MIN)
  } else{
    return = x*(X_MAX-X_MIN)+X_MIN
  }
}

mise_echelle_inv<-function(x,X_MIN,X_MAX){
  d=length(X_MIN)
  x=matrix(x,ncol=d)
  Nx=dim(x)[1]
  if(d>1){
    return = (x-t(matrix(0,d,Nx)+X_MIN))%*% diag((X_MAX-X_MIN)^(-1))
  } else{
    return = (x-X_MIN)/(X_MAX-X_MIN)
  }
}

############################"
# modeles utilises

# modele lineaire (pas d'erreur de modèle)
Modele_Lin<-function(x,beta){
  M=beta[1]; k=beta[2]
  theta0=asin(x/g/l);  Dtheta0=1
  temp = SysPendulePlaqueLin(M=M,k=k,theta0=theta0,Dtheta0=Dtheta0,Temps=t)
  return = temp[1,]
}

# modele non lineaire (potentiellement de l'erreur de modele)
Modele_NL<-function(x,beta){
  M=beta[1]; k=beta[2]
  theta0=asin(x/g/l);  Dtheta0=1
  return = SysPendulePlaque(M=M,k=k,theta0=theta0,Dtheta0=Dtheta0,Temps=t)
}


SysPendulePlaqueLin<-function(M,       # masse de la plaque
                           m=0.5,      # masse du pendule 
                           l=2,        # longueur du pendule
                           k,          # rigidité de la plaque
                           g=9.81,     # gravité
                           y0=0,       # position initiale de la plaque
                           Dy0=0,      # vitesse initiale de la plaque
                           theta0,     # position initiale du pendule
                           Dtheta0,    # vitesse initiale du pendule
                           Temps       # temps de mesure
                           ){
  
  # definition des matrices
  Masse=as.matrix(rbind(c((M+m)/(m*l),1),c(1,l)))
  Rigidite=as.matrix(rbind(c(k/(m*l),0),c(0,g)))
  
  # identification des valeurs propres
  a=M*l; b=-((M+m)*g+k*l); c=g*k;
  omega2=-b/(2*a)+sqrt(b^2-4*a*c)/(2*a)*c(-1,1)
  
  # identification des vecteurs propres
  MAT=diag(omega2^(-1))%*%Rigidite-Masse  
  a1=null(Rigidite-omega2[1]*Masse)
  a2=null(Rigidite-omega2[2]*Masse)  
  V=cbind(a1,a2)

  omega=sqrt(omega2)
  
  # regroupement des conditions limites
  Y0=rbind(y0,theta0)
  DY0=rbind(Dy0,Dtheta0)
  
  # identification des constantes
  c1=inv(V) %*% Y0
  c2=diag(omega^(-1)) %*% inv(V) %*% DY0
  
  # solution
  Y=V %*% rbind(c1[1]*cos(omega[1]*Temps)+c2[1]*sin(omega[1]*Temps),
                c1[2]*cos(omega[2]*Temps)+c2[2]*sin(omega[2]*Temps))
  
  return= Y
  # sortie Y = matrice de dimensions 2 * NT, avec sur la premiere ligne le mouvement de la plaque et sur la seconde ligne le mouvement associé du pendule
  # et où NT est le nombre de points de discrétisation du vecteur Temps 
}

SysPendulePlaqueLin_matrice<-function(M,  # masse de la plaque
                              m=0.5,      # masse du pendule 
                              l=2,        # longueur du pendule
                              k,          # rigidité de la plaque
                              g=9.81,     # gravité
                              Temps       # temps de mesure
){
  
  # definition des matrices
  Masse=as.matrix(rbind(c((M+m)/(m*l),1),c(1,l)))
  Rigidite=as.matrix(rbind(c(k/(m*l),0),c(0,g)))
  
  # identification des valeurs propres
  a=M*l; b=-((M+m)*g+k*l); c=g*k;
  omega2=-b/(2*a)+sqrt(b^2-4*a*c)/(2*a)*c(-1,1)
  
  # identification des vecteurs propres
  MAT=diag(omega2^(-1))%*%Rigidite-Masse
  a1=null(Rigidite-omega2[1]*Masse)
  a2=null(Rigidite-omega2[2]*Masse)
  V=cbind(a1,a2)
  Vinv=solve(V)
  
  # reconstruction des fonctions solutions
  F1=Vinv[1,2]*V[1,1]*cos(sqrt(omega2[1])*Temps)+Vinv[2,2]*V[1,2]*cos(sqrt(omega2[2])*Temps)
  F2=Vinv[1,2]*V[1,1]*sin(sqrt(omega2[1])*Temps)/sqrt(omega2[1])+Vinv[2,2]*V[1,2]*sin(sqrt(omega2[2])*Temps)/sqrt(omega2[2])

  return= cbind(F1,F2)
}


SysPendulePlaque<-function(M,          # masse de la plaque
                           m=0.5,      # masse du pendule 
                           l=2,        # longueur du pendule
                           k,          # rigidité de la plaque
                           g=9.81,     # gravité
                           y0=0,       # position initiale de la plaque
                           Dy0=0,      # vitesse initiale de la plaque
                           theta0,     # position initiale du pendule
                           Dtheta0,   # vitesse initiale du pendule
                           Temps
){
  
  Tem=seq(0,max(Temps),len=length(Temps))
  dt=mean(diff(Tem))
  NT=length(Tem)
  C=0
  
  POSI_plaque=matrix(0,NT,1)
  
  M_inv=inv(as.matrix(rbind(c(M+m,m*l),c(1,l))))
  
  acc<-function(y,Dy,theta,Dtheta){return=inv(as.matrix(rbind(c(M+m,m*l*cos(theta)),c(cos(theta),l)))) %*% as.vector(c(m*l*Dtheta^2*sin(theta)-k*y-C*Dy,-g*sin(theta)))}
  accL<-function(y,Dy,theta,Dtheta){return=M_inv %*% as.vector(c(-k*y-C*Dy,-g*theta))}
  vit<-function(acct,vit0){return=acct*dt+vit0}
  pos<-function(acct,pos0,vit0){return=acct*dt^2+vit0*dt+pos0}
  
  POS=matrix(0,NT,2)
  VIT=matrix(0,NT,2)

  POS[1,]=c(y0,theta0)
  VIT[1,]=c(Dy0,Dtheta0)
  
  for(i in 2:NT){
    ACC=t(acc(POS[i-1,1],VIT[i-1,1],POS[i-1,2],VIT[i-1,2]))
    VIT[i,]=vit(ACC,VIT[i-1,])
    POS[i,]=pos(ACC,t(POS[i-1,]),VIT[i-1,])
  }
  
  POSI_plaque=POS[,1]
  
  pas=floor(seq(1,NT,len=length(Temps)))
  #return=POSI_plaque[pas]
  return(POS[pas,])

}
