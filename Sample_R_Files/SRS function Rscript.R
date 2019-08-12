
library(lme4)

# edu.srs(5,5,response0.05,20) #

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

edu.srs=function(ji,kji,response,I)
{
est.SRS=rep(0,I)
sd.SRS=rep(0,I)

i=1
for(i in 1:I){

 y0=matrix(rep(0,ji*kji), nrow=ji)
 y1=matrix(rep(0,ji*kji), nrow=ji)

#control group#
idsch0=sample(1:schoolnum,ji)
 j=1
 idstu0=matrix(rep(0,ji*kji), nrow=ji)
 for(j in 1:ji) {
 idstu0[j,]=sample(stu_start[idsch0[j]]:stu_end[idsch0[j]],kji,replace=T)
 y0[j,]=response0.05[idstu0[j,]]
  }

#treatment group#
idsch1=sample(1:schoolnum,ji)
 j=1
 idstu1=matrix(rep(0,ji*kji), nrow=ji)
 for(j in 1:ji) {
 idstu1[j,]=sample(stu_start[idsch1[j]]:stu_end[idsch1[j]],kji,replace=T)
 y1[j,]=response[idstu1[j,]]
  }

  if ( all(y0==0)&all(y1==0))
  { est.SRS[i]=0
    sd.SRS[i]=0
  } else
  {
y=c(as.vector(t(y0)),as.vector(t(y1)))
clust=rep(1:(2*ji),rep(kji,(2*ji)))
treat=c(rep(0,ji*kji),rep(1,ji*kji))

mod=glmer(y ~ treat+(1|clust), family=binomial,control=glmerControl(optimizer = "bobyqa"), nAGQ = 10)

est.SRS[i]=as.numeric(fixef(mod)[2])
sd.SRS[i]=summary(mod)$coefficients[2,2]
  }
}

# adjust for size=0.05 #
chisq.SRS=(est.SRS/sd.SRS)^2
z.adj.SRS=sqrt(quantile(chisq.SRS, 0.95))

SRS.inside=((est.SRS-1.96*sd.SRS)<0)*(0<(est.SRS+1.96*sd.SRS)) #SRS.inside=1, if 0 in the interval#
SRS.power=(I-sum(SRS.inside))/I

return(list(SRS.power, z.adj.SRS, est.SRS, sd.SRS))
}

