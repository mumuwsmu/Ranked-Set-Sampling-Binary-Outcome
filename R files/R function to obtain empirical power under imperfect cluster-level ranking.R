library(lme4)
library("gaussquad")

PowerNormcl.e=function(ji,hi,kji,bji.sd,e.sd,mu,a1,rept,G,Gau.ord)
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
    # Sampling control group #
    h=1
    h.pr=1
    p0=matrix(rep(0,ji),nrow=hi)
    for(h in 1:hi)
    {
      
      b0.h=matrix(rnorm(ji,mean=0,sd=bji.sd),nrow=hi) #i=0, cluster rank=h#
      x0.h=matrix(rnorm(ji,(1-e.sd^2/bji.sd^2)*b0.h, e.sd*sqrt(1-e.sd^2/bji.sd^2)),nrow=hi) # ranking with error #
      x0.horder=apply(x0.h, 2, order,decreasing=TRUE) # rank based on x #
      p0[h,]=1/(1+exp(-mu-diag(b0.h[x0.horder[h,],1:(ji/hi)]))) # diag is sorted b #
      
    }
    
    p0=as.vector(t(p0))
    j=1
    y0=matrix(rep(0,ji*kji), nrow=ji)
    for(j in 1:ji) {
      y0[j,]=rbinom(kji, 1, p0[j])
      
    }
    
    # Sampling treatment group#
    h=1
    h.pr=1
    p1=matrix(rep(0,ji),nrow=hi)
    for(h in 1:hi)
    {
      b1.h=matrix(rnorm(ji,mean=0,sd=bji.sd),nrow=hi) #i=0, cluster rank=h#
      x1.h=matrix(rnorm(ji,(1-e.sd^2/bji.sd^2)*b1.h, e.sd*sqrt(1-e.sd^2/bji.sd^2)),nrow=hi) # ranking with error #
      x1.horder=apply(x1.h, 2, order,decreasing=TRUE)
      p1[h,]=1/(1+exp(-mu-a1-diag(b1.h[x1.horder[h,],1:(ji/hi)]))) # diag is sorted b #
      
    }
    
    p1=as.vector(t(p1))
    j=1
    y1=matrix(rep(0,ji*kji), nrow=ji)
    for(j in 1:ji) {
      y1[j,]=rbinom(kji, 1, p1[j])
    }
    
    # PL #
    y=c(as.vector(t(y0)),as.vector(t(y1)))
    clust=rep(1:(2*ji),rep(kji,(2*ji)))
    treat=c(rep(0,ji*kji),rep(1,ji*kji))
    mod=glmer(y ~ treat +(1| clust), family=binomial, control=glmerControl(optimizer="bobyqa"), nAGQ=10)
    
    R=1 # Start resampling #
    resample.est.PL=rep(0,rept)
    newy0=matrix(rep(0,(ji-hi)*kji), nrow=ji-hi)
    newy1=matrix(rep(0,(ji-hi)*kji), nrow=ji-hi)
    newclust=rep(1:(2*(ji-hi)),rep(kji,(2*(ji-hi))))
    newtreat=c(rep(0,(ji-hi)*kji),rep(1,(ji-hi)*kji))
    for ( R in 1:rept)
    {
      gr=1
      for(gr in 1:hi) 
      {resample=sample(((gr-1)*ji/hi+1):(gr*ji/hi), ((ji/hi)-1),replace=T)
      newy0[((gr-1)*(ji/hi-1)+1):(gr*(ji/hi-1)),]=y0[resample,]
      }
      gr=1
      for(gr in 1:hi) 
      {resample=sample(((gr-1)*ji/hi+1):(gr*ji/hi),((ji/hi)-1),replace=T)
      newy1[((gr-1)*(ji/hi-1)+1):(gr*(ji/hi-1)),]=y1[resample,]
      }
      newy=c(as.vector(t(newy0)),as.vector(t(newy1)))
      newmod <- glmer(newy ~ newtreat +(1| newclust), family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ=10)
      resample.est.PL[R]=as.numeric(fixef(newmod)[2])
    }
    
    est.PL[i]=as.numeric(fixef(mod)[2])
    sd.PL[i]=sd(resample.est.PL)
    
    # NP #
    y0.sumk=apply(y0,1,sum)
    y1.sumk=apply(y1,1,sum)
    a=as.vector(t(rbind(y0,y1)))
    R=1 # Start resampling #
    delta.resample=rep(0,rept)
    for ( R in 1:rept)
    {
      sum.yj=rep(0,2*ji)
      r=1
      for( r in 1:(2*ji))
      {sum.yj[r]=sum(a[((r-1)*kji+1):(r*kji)])  }
      
      newsum=rep(0,2*(ji-hi))
      gr=1
      for(gr in 1:(2*hi)) 
      {resample=sample(((gr-1)*ji/hi+1):(gr*ji/hi),((ji/hi)-1),replace=T)
      newsum[((gr-1)*(ji/hi-1)+1):(gr*(ji/hi-1))]=sum.yj[resample]
      }
      fit=log((newsum+0.5)/(kji-newsum+0.5))
      delta.resample[R]= mean(fit[(ji-hi+1):(2*(ji-hi))])-mean(fit[1:(ji-hi)])
    }
    
    est.NP[i]=mean(log((y1.sumk+0.5)/(kji-y1.sumk+0.5)))-mean(log((y0.sumk+0.5)/(kji-y0.sumk+0.5)))
    sd.NP[i]=sd(delta.resample)
    
    # MLE estimator #
    
    h.rank=rep(1:hi, rep((ji/hi),hi))
    min.fn=function(x)
    {ml.mu=x[1]
    ml.a1=x[2]
    ml.sd.b=x[3]
    j=1
    eqn0=rep(0,ji)
    eqn1=rep(0,ji)
    for (j in 1:ji) {
      eqn0[j]=log(sum(w*exp(t^2/2+y0.sumk[j]*(ml.mu+ml.sd.b*t))*(1+exp(ml.mu+ml.sd.b*t))^(-kji)*
                        pnorm(t)^(hi-h.rank[j])*(1-pnorm(t))^(h.rank[j]-1)))
      eqn1[j]=log(sum(w*exp(t^2/2+y1.sumk[j]*(ml.mu+ml.a1+ml.sd.b*t))*(1+exp(ml.mu+ml.a1+ml.sd.b*t))^(-kji)*
                        pnorm(t)^(hi-h.rank[j])*(1-pnorm(t))^(h.rank[j]-1)))
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



