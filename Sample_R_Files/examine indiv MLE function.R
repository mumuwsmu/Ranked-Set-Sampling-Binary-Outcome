# install.packages("numDeriv") #
library(numDeriv)
library("gaussquad")
ji=5
kji=8
hji=2
Gau.ord=30

y0=matrix(c(rep(0,26),1,rep(0,13)),nrow=5)
y1=matrix(c(rep(0,26),0,rep(0,13)),nrow=5)


  orthonormal.rules=hermite.h.quadrature.rules(Gau.ord, TRUE )
  g.h.q=as.vector(unlist(orthonormal.rules[Gau.ord]))
  t=g.h.q[1:Gau.ord]
  w=g.h.q[(Gau.ord+1):(2*Gau.ord)]

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

fit=optim(c(-3,0,0.6), min.fn,hessian=F)
est.MLE=fit$par[2]
fisher_info=solve(fit$hessian)
sd.MLE=diag(sqrt(diag(fisher_info)))[2,2]

hessian(func=min.fn, x=fit$par, method="Richardson")
