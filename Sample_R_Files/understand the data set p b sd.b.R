

edu_rank=read.csv("D:/Dropbox/Mumu/Research New/Data example/Edu Data set/Edu_rank.csv",header=T) 
edu_school=read.csv("D:/Dropbox/Mumu/Research New/Data example/Edu Data set/Edu_school.csv",header=T) 

stu_start=edu_school[,3]
stu_end=edu_school[,4]
schoolnum=length(edu_school[,1])
response0.05=edu_rank[,3]

p=rep(0,schoolnum)
b=rep(0,schoolnum)
for (i in 1: schoolnum)
{
p[i]= (sum(response0.05[stu_start[i]:stu_end[i]])+0.5)/(stu_end[i]-stu_start[i]+2)
b[i]=log(p[i]/(1-p[i])) 

}
sd(b)
plot(density(b))


p= 0.5/21
b=log(p/(1-p))

 # find corr between p.bar and mean.x1 for cluster level #
p=rep(0,schoolnum)

for (i in 1: schoolnum)
{
p[i]= mean(response0.05[stu_start[i]:stu_end[i]])

}
mean.x1=edu_school[,6]
cor(p,mean.x1)