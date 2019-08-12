library("gaussquad")
library("lme4")

# rank individual level only #


MseNormid.e=function(ji,kji,hji,bji.sd,r.right,mu,a1,G,Gau.ord)
{
  delta.NP=rep(0,G)
  delta.PL=rep(0,G)
  delta.MLE=rep(0,G)
  orthonormal.rules=hermite.h.quadrature.rules(Gau.ord, TRUE )
  g.h.q=as.vector(unlist(orthonormal.rules[Gau.ord]))
  t=g.h.q[1:Gau.ord]
  w=g.h.q[(Gau.ord+1):(2*Gau.ord)]
  
  i=1
  for(i in 1:G){
    
    # Sampling control group#
    
    h.pr=1
    p0=rep(0,ji)
    b0=rnorm(ji,mean=0,sd=bji.sd)
    p0=1/(1+exp(-mu-b0))
    y0=matrix(rep(0,ji*kji), nrow=ji)
    j=1
    for(j in 1:ji) {
      y=matrix(rep(0,kji),nrow=hji)
      for(h.pr in 1:hji)
      {
        y.hpr=matrix(rbinom(kji, 1, p0[j]),nrow=hji)
        r=matrix(runif(kji),nrow=hji)
        x.hpr=abs((r>r.right)-y.hpr) # r.right is the prob of judging correctly #
        x.horder=apply(x.hpr, 2, order,decreasing=TRUE) # rank based on x #
        y[h.pr,]=diag(y.hpr[x.horder[h.pr,],1:(kji/hji)])
      }  
      y0[j,]=as.vector(t(y))
    }
    
    # Sampling treatment group#
    
    h.pr=1
    p1=rep(0,ji)
    b1=rnorm(ji,mean=0,sd=bji.sd)
    
    p1=1/(1+exp(-mu-a1-b1))
    y1=matrix(rep(0,ji*kji), nrow=ji)
    j=1
    for(j in 1:ji) {
      y=matrix(rep(0,kji),nrow=hji)
      for(h.pr in 1:hji)
      {
        y.hpr=matrix(rbinom(kji, 1, p1[j]),nrow=hji)
        r=matrix(runif(kji),nrow=hji)
        x.hpr=abs((r>r.right)-y.hpr) 
        x.horder=apply(x.hpr, 2, order,decreasing=TRUE) # rank based on x #
        y[h.pr,]=diag(y.hpr[x.horder[h.pr,],1:(kji/hji)])
      }  
      y1[j,]=as.vector(t(y))
    }
    
    # NP estimator #
    
    y0.sumk=apply(y0,1,sum)
    y1.sumk=apply(y1,1,sum)
    delta.NP[i]=mean(log((y1.sumk+0.5)/(kji-y1.sumk+0.5)))-mean(log((y0.sumk+0.5)/(kji-y0.sumk+0.5)))
    
    # PL estimator #
    
    y=c(as.vector(t(y0)),as.vector(t(y1)))
    clust=rep(1:(2*ji),rep(kji,(2*ji)))
    treat=c(rep(0,ji*kji),rep(1,ji*kji))
    mod=glmer(y ~ treat +(1| clust), family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ=10)
    delta.PL[i]=as.numeric(fixef(mod)[2])
    
    # MLE estimator #
    
    h.rank=1:hji
    min.fn=function(x)
    {ml.mu=x[1]
    ml.a1=x[2]
    ml.sd.b=x[3]
    
    j=1
    eqn0=rep(0,ji)
    eqn1=rep(0,ji)
    for(j in 1:ji){
      m=1
      gauss.qudr0=rep(0,Gau.ord)
      gauss.qudr1=rep(0,Gau.ord)
      h.ind=1
      y0.sumhk=rep(0,hji)
      y1.sumhk=rep(0,hji)
      for(h.ind in 1:hji) {
        y0.sumhk[h.ind]=sum(y0[j,(kji/hji*(h.ind-1)+1):(kji/hji*h.ind)])
      }
      h.ind=1
      for(h.ind in 1:hji) {
        y1.sumhk[h.ind]=sum(y1[j,(kji/hji*(h.ind-1)+1):(kji/hji*h.ind)])
      }
      for(m in 1:Gau.ord)  {
        gauss.qudr0[m]=w[m]*exp(t[m]^2/2+sum(y0.sumhk*log((1-pbinom(h.rank-1,hji,1/(1+exp(-ml.mu-ml.sd.b*t[m]))))/pbinom(h.rank-1,hji,1/(1+exp(-ml.mu-ml.sd.b*t[m]))))
                                             +kji/hji*log(pbinom(h.rank-1,hji,1/(1+exp(-ml.mu-ml.sd.b*t[m]))))))
        gauss.qudr1[m]=w[m]*exp(t[m]^2/2+sum(y1.sumhk*log((1-pbinom(h.rank-1,hji,1/(1+exp(-ml.mu-ml.a1-ml.sd.b*t[m]))))/pbinom(h.rank-1,hji,1/(1+exp(-ml.mu-ml.a1-ml.sd.b*t[m]))))
                                             +kji/hji*log(pbinom(h.rank-1,hji,1/(1+exp(-ml.mu-ml.a1-ml.sd.b*t[m]))))))
      }
      eqn0[j]=log(sum(gauss.qudr0))
      eqn1[j]=log(sum(gauss.qudr1))
    }
    -sum(eqn0)-sum(eqn1)
    }
    
    delta.MLE[i]=optim(c(mu,a1,bji.sd), min.fn)$par[2]
    
  }
  
  MSE.PL=sum((delta.PL-a1)^2/G)
  MSE.MLE=sum((delta.MLE-a1)^2/G)
  MSE.NP=sum((delta.NP-a1)^2/G)
  
  return(c(MSE.MLE,MSE.PL,MSE.NP))
  
}
