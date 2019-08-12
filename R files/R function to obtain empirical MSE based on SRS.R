library("gaussquad")
library("lme4")

#  SRS #

MseNormsrs=function(ji,kji,bji.sd,mu,a1,G,Gau.ord)
{
  delta.NP=rep(0,G)
  delta.MLE=rep(0,G)
  orthonormal.rules=hermite.h.quadrature.rules(Gau.ord, TRUE )
  g.h.q=as.vector(unlist(orthonormal.rules[Gau.ord]))
  t=g.h.q[1:Gau.ord]
  w=g.h.q[(Gau.ord+1):(2*Gau.ord)]
  
  i=1
  for(i in 1:G){
    
    #control group#
    
    p0=rep(0,ji)
    b0=rnorm(ji,mean=0,sd=bji.sd)
    p0=1/(1+exp(-mu-b0))
    j=1
    y0=matrix(rep(0,ji*kji), nrow=ji)
    for(j in 1:ji) {
      y0[j,]=rbinom(kji, 1, p0[j])
    }
    
    #treatment group#
    
    p1=rep(0,ji)
    b1=rnorm(ji,mean=0,sd=bji.sd)
    p1=1/(1+exp(-mu-a1-b1))
    
    j=1
    y1=matrix(rep(0,ji*kji), nrow=ji)
    for(j in 1:ji) {
      y1[j,]=rbinom(kji, 1, p1[j])
    }
    
    # NP estimator #
    
    y0.sumk=apply(y0,1,sum)
    y1.sumk=apply(y1,1,sum)
    delta.NP[i]=mean(log((y1.sumk+0.5)/(kji-y1.sumk+0.5)))-mean(log((y0.sumk+0.5)/(kji-y0.sumk+0.5)))
    
    # MLE estimator #
    
    y=c(as.vector(t(y0)),as.vector(t(y1)))
    clust=rep(1:(2*ji),rep(kji,(2*ji)))
    treat=c(rep(0,ji*kji),rep(1,ji*kji))
    mod=glmer(y ~ treat +(1| clust), family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ=10)
    delta.PL[i]=as.numeric(fixef(mod)[2])
    
  }
  
  MSE.MLE=sum((delta.MLE-a1)^2/G)
  MSE.NP=sum((delta.NP-a1)^2/G)
  
  return(c(MSE.MLE,MSE.NP))
  
}
