
library("gaussquad")
library("lme4")

# rank cluster level only #

MseNormcl.e=function(ji,hi,kji,bji.sd,e.sd,mu,a1,G,Gau.ord)
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

# Sampling Process #
# Sampling control group#
h=1
h.pr=1
p0=matrix(rep(0,ji),nrow=hi)
for(h in 1:hi)
 {

b0.h=matrix(rnorm(ji,mean=0,sd=bji.sd),nrow=hi) # i=0, cluster rank=h #
x0.h=matrix(rnorm(ji,(1-e.sd^2/bji.sd^2)*b0.h, e.sd*sqrt(1-e.sd^2/bji.sd^2)),nrow=hi) # ranking with error #
x0.horder=apply(x0.h, 2, order,decreasing=TRUE) # rank based on x #
p0[h,]=1/(1+exp(-mu-diag(b0.h[x0.horder[h,],1:(ji/hi)]))) # diag is sorted b #

 }

p0=as.vector(t(p0))
 j=1
 y0=matrix(rep(0,ji*kji), nrow=ji)
 for(j in 1:ji) {
 y0[j,]=rbinom(kji, 1, p0[j]) # y0 is the data for control group #

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

delta.MLE[i]=optim(c(mu,a1,bji.sd), min.fn)$par[2]

}

MSE.PL=sum((delta.PL-a1)^2/G)
MSE.MLE=sum((delta.MLE-a1)^2/G)
MSE.NP=sum((delta.NP-a1)^2/G)

return(c(MSE.MLE,MSE.PL,MSE.NP))

}
