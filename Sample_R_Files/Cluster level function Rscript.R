
library(lme4)
library("gaussquad")

# edu.cl(12,2,5,response0.07,20,10,30) #

# edu_rank=read.csv("D:/Dropbox/Mumu/Research New/Data example/Edu Data set/Edu_rank.csv",header=T) #
# edu_school=read.csv("D:/Dropbox/Mumu/Research New/Data example/Edu Data set/Edu_school.csv",header=T) #

# edu_rank=read.csv("Edu_rank.csv",header=T)
# edu_school=read.csv("Edu_school.csv",header=T)

stu_start=edu_school[,3]
stu_end=edu_school[,4]

schoolnum=length(edu_school[,1])

response0.05=edu_rank[,3]
response0.07=edu_rank[,4]
response0.09=edu_rank[,5]
response0.11=edu_rank[,6]

edu.cl=function(ji,hi,kji,response,rept,I,Gau.ord)
{
est.NP=rep(0,I)
est.PL=rep(0,I)
est.MLE=rep(0,I)
sd.NP=rep(0,I)
sd.PL=rep(0,I)
sd.PLbt=rep(0,I)
sd.MLE=rep(0,I)
  orthonormal.rules=hermite.h.quadrature.rules(Gau.ord, TRUE )
  g.h.q=as.vector(unlist(orthonormal.rules[Gau.ord]))
  t=g.h.q[1:Gau.ord]
  w=g.h.q[(Gau.ord+1):(2*Gau.ord)]

i=1
for(i in 1:I){
 y0=matrix(rep(0,ji*kji), nrow=ji)
 y1=matrix(rep(0,ji*kji), nrow=ji)

#control group#
h=1
idsch0=matrix(rep(0,ji),nrow=hi)
for(h in 1:hi)
 {
idsch0.h=matrix(sample(1:schoolnum,ji),nrow=hi)
idsch0.hsort=apply(idsch0.h, 2, sort,decreasing=F)
idsch0[h,]=idsch0.hsort[h,]
}
idsch0=as.vector(t(idsch0))
 j=1
 idstu0=matrix(rep(0,ji*kji), nrow=ji)
 for(j in 1:ji) {
 idstu0[j,]=sample(stu_start[idsch0[j]]:stu_end[idsch0[j]],kji,replace=T)
 y0[j,]=response0.05[idstu0[j,]]
  }

#treatment group#
h=1
idsch1=matrix(rep(0,ji),nrow=hi)
for(h in 1:hi)
 {
idsch1.h=matrix(sample(1:schoolnum,ji),nrow=hi)
idsch1.hsort=apply(idsch1.h, 2, sort,decreasing=F)
idsch1[h,]=idsch1.hsort[h,]
}
idsch1=as.vector(t(idsch1))
 j=1
 idstu1=matrix(rep(0,ji*kji), nrow=ji)
 for(j in 1:ji) {
 idstu1[j,]=sample(stu_start[idsch1[j]]:stu_end[idsch1[j]],kji,replace=T)
 y1[j,]=response[idstu1[j,]]
  }

# PL #
  if ( all(y0==0)&all(y1==0))
  { est.PL[i]=0
    sd.PL[i]=0
    sd.PLbt[i]=0
  } else
  {
y=c(as.vector(t(y0)),as.vector(t(y1)))
clust=rep(1:(2*ji),rep(kji,(2*ji)))
treat=c(rep(0,ji*kji),rep(1,ji*kji))
mod <- glmer(y ~ treat +(1| clust), family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ=10)

 R=1
 resample.est.PL=rep(0,rept)
 newy0=matrix(rep(0,(ji-hi)*kji), nrow=ji-hi)
 newy1=matrix(rep(0,(ji-hi)*kji), nrow=ji-hi)
 newclust=rep(1:(2*(ji-hi)),rep(kji,(2*(ji-hi))))
 newtreat=c(rep(0,(ji-hi)*kji),rep(1,(ji-hi)*kji))
 for ( R in 1:rept)
 {
  gr=1
  for(gr in 1:hi) 
  {resample=sample(((gr-1)*ji/hi+1):(gr*ji/hi),((ji/hi)-1),replace=T)
   newy0[((gr-1)*(ji/hi-1)+1):(gr*(ji/hi-1)),]=y0[resample,]
  }
  gr=1
  for(gr in 1:hi) 
  {resample=sample(((gr-1)*ji/hi+1):(gr*ji/hi),((ji/hi)-1),replace=T)
   newy1[((gr-1)*(ji/hi-1)+1):(gr*(ji/hi-1)),]=y1[resample,]
  }
    if ( all(newy0==0)&all(newy1==0))
    {resample.est.PL[R]=0 } else
    {
 newy=c(as.vector(t(newy0)),as.vector(t(newy1)))
 newmod <- glmer(newy ~ newtreat +(1| newclust), family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ=10)
 resample.est.PL[R]=as.numeric(fixef(newmod)[2])
    }
 }

est.PL[i]=as.numeric(fixef(mod)[2])
sd.PL[i]=summary(mod)$coefficients[2,2]
sd.PLbt[i]=sd(resample.est.PL)
 }

# NP #
y0.sumk=apply(y0,1,sum)
y1.sumk=apply(y1,1,sum)
 a=as.vector(t(rbind(y0,y1)))
 R=1
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
fit=optim(c(-2.7,0,0.6), min.fn,hessian=TRUE)
est.MLE[i]=fit$par[2]
fisher_info=solve(fit$hessian)
sd.MLE[i]=diag(sqrt(diag(fisher_info)))[2,2]

}
# adjust for size=0.05 #
chisq.NP=(est.NP/sd.NP)^2
z.adj.NP=sqrt(quantile(chisq.NP, 0.95))

chisq.PL=(est.PL/sd.PL)^2
z.adj.PL=sqrt(quantile(chisq.PL, 0.95))

chisq.PLbt=(est.PL/sd.PLbt)^2
z.adj.PLbt=sqrt(quantile(chisq.PLbt, 0.95))

chisq.MLE=(est.MLE/sd.MLE)^2
z.adj.MLE=sqrt(quantile(chisq.MLE, 0.95))

NP.inside=((est.NP-1.96*sd.NP)<=0)*(0<=(est.NP+1.96*sd.NP)) #NP.inside=1, if 0 in the interval#
PL.inside=((est.PL-1.96*sd.PL)<=0)*(0<=(est.PL+1.96*sd.PL)) #PL.inside=1, if 0 in the interval#
PLbt.inside=((est.PL-1.96*sd.PLbt)<=0)*(0<=(est.PL+1.96*sd.PLbt)) #PLbt.inside=1, if 0 in the interval#
MLE.inside=((est.MLE-1.96*sd.MLE)<=0)*(0<=(est.MLE+1.96*sd.MLE)) #MLE.inside=1, if 0 in the interval#

NP.power=(I-sum(NP.inside))/I
PL.power=(I-sum(PL.inside))/I
PLbt.power=(I-sum(PLbt.inside))/I
MLE.power=(I-sum(MLE.inside))/I

return(list(c(NP.power,PL.power,PLbt.power,MLE.power),c(z.adj.NP,z.adj.PL,z.adj.PLbt,z.adj.MLE),est.NP,sd.NP,est.PL,sd.PL,sd.PLbt,est.MLE,sd.MLE))

}

# ob.designi1.d0.2=ob.cl(15,3,20,-0.2,200,5000,30) #


# save.image(file="ob.designi1.d0.2_v2.RData") #


