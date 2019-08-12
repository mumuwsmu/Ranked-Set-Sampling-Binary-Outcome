library(lme4)
library("gaussquad")

PowerNormid.e=function(ji,kji,hji,bji.sd,r.right,mu,a1,rept,G,Gau.ord)
{
  est.NP=rep(0,G)
  est.PL=rep(0,G)
  est.MLE=rep(0,G)
  sd.NP=rep(0,G)
  sd.PL=rep(0,G)
  sd.MLE=rep(0,G)
  orthonormal.rules=hermite.h.quadrature.rules(Gau.ord, TRUE )
  g.h.q=as.vector(unlist(orthonormal.rules[Gau.ord]))
  t=g.h.q[1:Gau.ord]
  w=g.h.q[(Gau.ord+1):(2*Gau.ord)]
  
  i=1
  for (i in 1:G){
    #control group#
    
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
        x.hpr=abs((r>r.right)-y.hpr) # r.right prob right #
        x.horder=apply(x.hpr, 2, order,decreasing=TRUE) # rank based on x #
        y[h.pr,]=diag(y.hpr[x.horder[h.pr,],1:(kji/hji)])
        
      }  
      y0[j,]=as.vector(t(y))
    }
    
    #treatment group#
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
        x.hpr=abs((r>r.right)-y.hpr) # r.right prob right #
        x.horder=apply(x.hpr, 2, order,decreasing=TRUE) # rank based on x #
        y[h.pr,]=diag(y.hpr[x.horder[h.pr,],1:(kji/hji)])
        
      }  
      y1[j,]=as.vector(t(y))
    }
    
    # PL #
    y=c(as.vector(t(y0)),as.vector(t(y1)))
    clust=rep(1:(2*ji),rep(kji,(2*ji)))
    treat=c(rep(0,ji*kji),rep(1,ji*kji))
    
    mod=glmer(y ~ treat +(1| clust), family=binomial, control=glmerControl(optimizer="bobyqa"), nAGQ=10)
    
    R=1
    resample.est.PL=rep(0,rept)
    newy0=matrix(rep(0,(ji-1)*kji), nrow=ji-1)
    newy1=matrix(rep(0,(ji-1)*kji), nrow=ji-1)
    newclust=rep(1:(2*(ji-1)),rep(kji,(2*(ji-1))))
    newtreat=c(rep(0,(ji-1)*kji),rep(1,(ji-1)*kji))
    for ( R in 1:rept)
    {
      resample=sample(1:ji,(ji-1),replace=T)
      newy0[1:(ji-1),]=y0[resample,]
      resample=sample(1:ji,(ji-1),replace=T)
      newy1[1:(ji-1),]=y1[resample,]
      
      newy=c(as.vector(t(newy0)),as.vector(t(newy1)))
      newmod=glmer(newy ~ newtreat +(1| newclust), family=binomial, control=glmerControl (optimizer="bobyqa"), nAGQ=10)
      resample.est.PL[R]=as.numeric(fixef(newmod)[2])
    }
    
    est.PL[i]=as.numeric(fixef(mod)[2])
    sd.PL[i]=sd(resample.est.PL)
    
    
    # NP #
    y0.sumk=apply(y0,1,sum)
    y1.sumk=apply(y1,1,sum)
    a=as.vector(t(rbind(y0,y1)))
    R=1
    delta.resample=rep(0,rept)
    for ( R in 1:rept)
    {
      sum.yj=c(y0.sumk,y1.sumk)
      newsum=rep(0,2*(ji-1))
      gr=1
      for(gr in 1:2) 
      {resample=sample(((gr-1)*ji+1):(gr*ji),(ji-1),replace=T)
      newsum[((gr-1)*(ji-1)+1):(gr*(ji-1))]=sum.yj[resample]
      }
      fit=log((newsum+0.5)/(kji-newsum+0.5))
      delta.resample[R]= mean(fit[ji:(2*(ji-1))])-mean(fit[1:(ji-1)])
    }
    
    est.NP[i]=mean(log((y1.sumk+0.5)/(kji-y1.sumk+0.5)))-mean(log((y0.sumk+0.5)/(kji-y0.sumk+0.5)))
    sd.NP[i]=sd(delta.resample)
    
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
    
    fit=optim(c(mu,a1,bji.sd), min.fn,hessian=TRUE)
    est.MLE[i]=fit$par[2]
    fisher_info=solve(fit$hessian)
    sd.MLE[i]=diag(sqrt(diag(fisher_info)))[2,2]
    
  }
  
  NP.inside=((est.NP-1.96*sd.NP)<0)*(0<(est.NP+1.96*sd.NP)) #NP.inside=1, if 0 in the interval#
  PL.inside=((est.PL-1.96*sd.PL)<0)*(0<(est.PL+1.96*sd.PL)) #PL.inside=1, if 0 in the interval#
  MLE.inside=((est.MLE-1.96*sd.MLE)<0)*(0<(est.MLE+1.96*sd.MLE)) #MLE.inside=1, if 0 in the interval#
  
  NP.power=(G-sum(NP.inside))/G
  PL.power=(G-sum(PL.inside))/G
  MLE.power=(G-sum(MLE.inside))/G
  
  return(list(c(NP.power,PL.power,MLE.power),est.NP,sd.NP,est.PL,sd.PL,est.MLE,sd.MLE))
}

