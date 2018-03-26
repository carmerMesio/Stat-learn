## k-Fold Cross-Validation
k.fold.cv <- function(x,y,k=10,h=range(x)/10,p=1,type.kernel="normal"){
  n <- length(x)
  Ik <- rep(1:length(x)) ##el antes aqui dividia el espacio en funcion de agrupar los datos desde 1 hasta 10.
  ssr <- 0
  for (i in (1:k)){
    y.i <- y[Ik==i]
    aux <- locpolreg(x[Ik!=i],y[Ik!=i],h=h,p=p,tg=x[Ik==i],
                     type.kernel=type.kernel, doing.plot=FALSE)
    ssr <- ssr + sum((y.i-aux$mtgr)^2)
  }
  k.cv <- ssr/n
  return(k.cv)
}
##Para hacer leave one out le pasaremos a k la longitud de nuestro dataframe.
h.k.fold.cv <- function(x,y,h.v = exp(seq(log(diff(range(x))/20),
                                          log(diff(range(x))/4),l=10)), 
                        k=length(x),p=1,type.kernel="normal"){
  n <- length(x)
  perm <- sample(1:n)
  xperm <- x[perm]
  yperm <- y[perm]
  
  k.cv <- h.v*0
  for (i in (1:length(h.v))){
    h <- h.v[i]
    k.cv[i] <- k.fold.cv(x=xperm,y=yperm,k=k,h=h,p=p,
                         type.kernel=type.kernel)
  }
  return(list(k=k,h.v=h.v,k.cv=k.cv))
}

