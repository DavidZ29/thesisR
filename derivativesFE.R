derivative1<-function(f){
  D(f,"phi")
}
derivative2<-function(g){
  D(D(g,"phi"),"phi")
}


#Normal
fe<-expression(phi*(y*mu-((1/2)*(mu)^(2)))-(1/2)*((phi*y^(2))+log(2*pi*(phi)^(-1))))
fe1<-derivative1(fe)
fe1
c<-expression(-(1/2)*((phi*y^(2))+log(2*pi*(phi)^(-1))))
c2<-derivative2(c)
c2
#inverseNormal
fe<-expression(phi*((y*(-1*(2*mu*mu)^(-1)))-(-sqrt(1/(mu*mu))))+(-(1/2)*(log(2*y^(3)*pi*(phi)^(-1))+phi*y)))
fe1<-derivative1(fe)
fe1
c<-expression(-(1/2)*(log(2*y^(3)*pi*(phi)^(-1))+phi*y))
c2<-derivative2(c)
c2
#GAMA
fe<-expression(phi*((-y/mu)-log(mu))+(phi)*log(phi)+(phi-1)*log(y)-lgamma(phi))
fe1<-derivative1(fe)
fe1
c<-expression(phi*(log(phi)+log(y))-log(y)-lgamma(phi))
c2<-derivative2(c)
c2

#POISSON
fe<-expression(phi*(y*log(mu)-mu)-phi*log(factorial(y)))
fe1<-derivative1(fe)
fe1
c<-expression(-log(factorial(y)))
c2<-derivative2(c)
c2
