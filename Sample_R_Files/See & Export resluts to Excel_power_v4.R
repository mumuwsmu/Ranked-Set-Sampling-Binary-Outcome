# install.packages("xlsx") #
library('xlsx')

load("D:\\Dropbox\\Mumu\\Research New\\Data example\\HPC Mar7 Edu Rimage\\all.ed_v4.RData")

ed.clh3p.13=ed.clh3p.13_v1
ed.clh3p.15=ed.clh3p.15_v1
ed.clh6p.13=ed.clh6p.13_v1
ed.clh6p.15=ed.clh6p.15_v1

srs.p.05=rep(ed.srsp.05[[1]],8)
srs.p.07=rep(ed.srsp.07[[1]],8)
srs.p.09=rep(ed.srsp.09[[1]],8)
srs.p.11=rep(ed.srsp.11[[1]],8)
srs.p.13=rep(ed.srsp.11[[1]],8)
srs.p.15=rep(ed.srsp.15[[1]],8)

srsNP.p.05=rep(ed.srsNPp.05[[1]],8)
srsNP.p.07=rep(ed.srsNPp.07[[1]],8)
srsNP.p.09=rep(ed.srsNPp.09[[1]],8)
srsNP.p.11=rep(ed.srsNPp.11[[1]],8)
srsNP.p.13=rep(ed.srsNPp.13[[1]],8)
srsNP.p.15=rep(ed.srsNPp.15[[1]],8)

p.05_v1=c(ed.clh3p.05[[1]],ed.clh6p.05[[1]],ed.indh3p.05_v1[[1]],ed.indh5p.05_v1[[1]],ed.bothh3p.05_v1[[1]],ed.bothh3h5p.05_v1[[1]],ed.bothh6h3p.05_v1[[1]],ed.bothh6h5p.05_v1[[1]]) 
p.07_v1=c(ed.clh3p.07[[1]],ed.clh6p.07[[1]],ed.indh3p.07_v1[[1]],ed.indh5p.07_v1[[1]],ed.bothh3p.07_v1[[1]],ed.bothh3h5p.07_v1[[1]],ed.bothh6h3p.07_v1[[1]],ed.bothh6h5p.07_v1[[1]])
p.09_v1=c(ed.clh3p.09[[1]],ed.clh6p.09[[1]],ed.indh3p.09_v1[[1]],ed.indh5p.09_v1[[1]],ed.bothh3p.09_v1[[1]],ed.bothh3h5p.09_v1[[1]],ed.bothh6h3p.09_v1[[1]],ed.bothh6h5p.09_v1[[1]])
p.11_v1=c(ed.clh3p.11[[1]],ed.clh6p.11[[1]],ed.indh3p.11_v1[[1]],ed.indh5p.11_v1[[1]],ed.bothh3p.11_v1[[1]],ed.bothh3h5p.11_v1[[1]],ed.bothh6h3p.11_v1[[1]],ed.bothh6h5p.11_v1[[1]])
p.13_v1=c(ed.clh3p.13[[1]],ed.clh6p.13[[1]],ed.indh3p.13_v1[[1]],ed.indh5p.13_v1[[1]],ed.bothh3p.13_v1[[1]],ed.bothh3h5p.13_v1[[1]],ed.bothh6h3p.13_v1[[1]],ed.bothh6h5p.13_v1[[1]])
p.15_v1=c(ed.clh3p.15[[1]],ed.clh6p.15[[1]],ed.indh3p.15_v1[[1]],ed.indh5p.15_v1[[1]],ed.bothh3p.15_v1[[1]],ed.bothh3h5p.15_v1[[1]],ed.bothh6h3p.15_v1[[1]],ed.bothh6h5p.15_v1[[1]])

p.05_v2=c(ed.clh3p.05[[1]],ed.clh6p.05[[1]],ed.indh3p.05_v2[[1]],ed.indh5p.05_v2[[1]],ed.bothh3p.05_v2[[1]],ed.bothh3h5p.05_v2[[1]],ed.bothh6h3p.05_v2[[1]],ed.bothh6h5p.05_v2[[1]]) 
p.07_v2=c(ed.clh3p.07[[1]],ed.clh6p.07[[1]],ed.indh3p.07_v2[[1]],ed.indh5p.07_v2[[1]],ed.bothh3p.07_v2[[1]],ed.bothh3h5p.07_v2[[1]],ed.bothh6h3p.07_v2[[1]],ed.bothh6h5p.07_v2[[1]])
p.09_v2=c(ed.clh3p.09[[1]],ed.clh6p.09[[1]],ed.indh3p.09_v2[[1]],ed.indh5p.09_v2[[1]],ed.bothh3p.09_v2[[1]],ed.bothh3h5p.09_v2[[1]],ed.bothh6h3p.09_v2[[1]],ed.bothh6h5p.09_v2[[1]])
p.11_v2=c(ed.clh3p.11[[1]],ed.clh6p.11[[1]],ed.indh3p.11_v2[[1]],ed.indh5p.11_v2[[1]],ed.bothh3p.11_v2[[1]],ed.bothh3h5p.11_v2[[1]],ed.bothh6h3p.11_v2[[1]],ed.bothh6h5p.11_v2[[1]])
p.13_v2=c(ed.clh3p.13[[1]],ed.clh6p.13[[1]],ed.indh3p.13_v2[[1]],ed.indh5p.13_v2[[1]],ed.bothh3p.13_v2[[1]],ed.bothh3h5p.13_v2[[1]],ed.bothh6h3p.13_v2[[1]],ed.bothh6h5p.13_v2[[1]])
p.15_v2=c(ed.clh3p.15[[1]],ed.clh6p.15[[1]],ed.indh3p.15_v2[[1]],ed.indh5p.15_v2[[1]],ed.bothh3p.15_v2[[1]],ed.bothh3h5p.15_v2[[1]],ed.bothh6h3p.15_v2[[1]],ed.bothh6h5p.15_v2[[1]])

p.05_v3=c(ed.clh3p.05_v2[[1]],ed.clh6p.05_v2[[1]],ed.indh3p.05_v3[[1]],ed.indh5p.05_v3[[1]],ed.bothh3p.05_v3[[1]],ed.bothh3h5p.05_v3[[1]],ed.bothh6h3p.05_v3[[1]],ed.bothh6h5p.05_v3[[1]]) 
p.07_v3=c(ed.clh3p.07_v2[[1]],ed.clh6p.07_v2[[1]],ed.indh3p.07_v3[[1]],ed.indh5p.07_v3[[1]],ed.bothh3p.07_v3[[1]],ed.bothh3h5p.07_v3[[1]],ed.bothh6h3p.07_v3[[1]],ed.bothh6h5p.07_v3[[1]])
p.09_v3=c(ed.clh3p.09_v2[[1]],ed.clh6p.09_v2[[1]],ed.indh3p.09_v3[[1]],ed.indh5p.09_v3[[1]],ed.bothh3p.09_v3[[1]],ed.bothh3h5p.09_v3[[1]],ed.bothh6h3p.09_v3[[1]],ed.bothh6h5p.09_v3[[1]])
p.11_v3=c(ed.clh3p.11_v2[[1]],ed.clh6p.11_v2[[1]],ed.indh3p.11_v3[[1]],ed.indh5p.11_v3[[1]],ed.bothh3p.11_v3[[1]],ed.bothh3h5p.11_v3[[1]],ed.bothh6h3p.11_v3[[1]],ed.bothh6h5p.11_v3[[1]])
p.13_v3=c(ed.clh3p.13_v2[[1]],ed.clh6p.13_v2[[1]],ed.indh3p.13_v3[[1]],ed.indh5p.13_v3[[1]],ed.bothh3p.13_v3[[1]],ed.bothh3h5p.13_v3[[1]],ed.bothh6h3p.13_v3[[1]],ed.bothh6h5p.13_v3[[1]])
p.15_v3=c(ed.clh3p.15_v2[[1]],ed.clh6p.15_v2[[1]],ed.indh3p.15_v3[[1]],ed.indh5p.15_v3[[1]],ed.bothh3p.15_v3[[1]],ed.bothh3h5p.15_v3[[1]],ed.bothh6h3p.15_v3[[1]],ed.bothh6h5p.15_v3[[1]])

p.05_v4=c(ed.clh3p.05_v2[[1]],ed.clh6p.05_v2[[1]],ed.indh3p.05_v4[[1]],ed.indh5p.05_v4[[1]],ed.bothh3p.05_v4[[1]],ed.bothh3h5p.05_v4[[1]],ed.bothh6h3p.05_v4[[1]],ed.bothh6h5p.05_v4[[1]]) 
p.07_v4=c(ed.clh3p.07_v2[[1]],ed.clh6p.07_v2[[1]],ed.indh3p.07_v4[[1]],ed.indh5p.07_v4[[1]],ed.bothh3p.07_v4[[1]],ed.bothh3h5p.07_v4[[1]],ed.bothh6h3p.07_v4[[1]],ed.bothh6h5p.07_v4[[1]])
p.09_v4=c(ed.clh3p.09_v2[[1]],ed.clh6p.09_v2[[1]],ed.indh3p.09_v4[[1]],ed.indh5p.09_v4[[1]],ed.bothh3p.09_v4[[1]],ed.bothh3h5p.09_v4[[1]],ed.bothh6h3p.09_v4[[1]],ed.bothh6h5p.09_v4[[1]])
p.11_v4=c(ed.clh3p.11_v2[[1]],ed.clh6p.11_v2[[1]],ed.indh3p.11_v4[[1]],ed.indh5p.11_v4[[1]],ed.bothh3p.11_v4[[1]],ed.bothh3h5p.11_v4[[1]],ed.bothh6h3p.11_v4[[1]],ed.bothh6h5p.11_v4[[1]])
p.13_v4=c(ed.clh3p.13_v2[[1]],ed.clh6p.13_v2[[1]],ed.indh3p.13_v4[[1]],ed.indh5p.13_v4[[1]],ed.bothh3p.13_v4[[1]],ed.bothh3h5p.13_v4[[1]],ed.bothh6h3p.13_v4[[1]],ed.bothh6h5p.13_v4[[1]])
p.15_v4=c(ed.clh3p.15_v2[[1]],ed.clh6p.15_v2[[1]],ed.indh3p.15_v4[[1]],ed.indh5p.15_v4[[1]],ed.bothh3p.15_v4[[1]],ed.bothh3h5p.15_v4[[1]],ed.bothh6h3p.15_v4[[1]],ed.bothh6h5p.15_v4[[1]])

p.05=(p.05_v1+p.05_v2+p.05_v3+p.05_v4)/4
p.07=(p.07_v1+p.07_v2+p.07_v3+p.07_v4)/4
p.09=(p.09_v1+p.09_v2+p.09_v3+p.09_v4)/4
p.11=(p.11_v1+p.11_v2+p.11_v3+p.11_v4)/4
p.13=(p.13_v1+p.13_v2+p.13_v3+p.13_v4)/4
p.15=(p.15_v1+p.15_v2+p.15_v3+p.15_v4)/4

no.adjust=data.frame(srsNP.p.05,p.05,srsNP.p.07,p.07,srsNP.p.09,p.09,srsNP.p.11,p.11,srsNP.p.13,p.13,srsNP.p.15,p.15)
colnames(no.adjust)=c("srsNPp.05","NPp.05","srsNPp.07","NPp.07","srsNPp.09","NPp.09","srsNPp.11","NPp.11","srsNPp.13","NPp.13","srsNPp.15","NPp.15")

write.xlsx(no.adjust,"D:\\Dropbox\\Mumu\\Research New\\Data example\\Edu_results.xlsx",sheetName="New_power_v4_no.adjust",append=TRUE)

# write.xlsx(no.adjust, "/Users/Mumu/Dropbox/Mumu/Research New/Data example/Edu_results.xlsx",sheetName="Mar7_power_v4_no.adjust",append=TRUE ) 
#  ,append=TRUE  #

######################################################################
#######################################################################
#### adjust for alpha=0.05 ##

#### adjust for alpha=0.05 ##

adj.edu=function(a.d0,a,I)
{
z.adj.NP=a.d0[[2]]
est.NP=a[[4]]
sd.NP=a[[5]]
adj.NP.inside=((est.NP-z.adj.NP*sd.NP)<0)*(0<(est.NP+z.adj.NP*sd.NP)) 
adj.NP.power=(I-sum(adj.NP.inside))/I
return(adj.NP.power)
}

adj.edu_v2=function(a.d0_v1,a.d0_v2,a_v1,a_v2,I)
{
est.NPd0=c(a.d0_v1[[4]],a.d0_v2[[4]])
sd.NPd0=c(a.d0_v1[[5]],a.d0_v2[[5]])

chisq.NP=(est.NPd0/sd.NPd0)^2
z.adj.NP=sqrt(quantile(chisq.NP, 0.95))
est.NP=c(a_v1[[4]],a_v2[[4]])
sd.NP=c(a_v1[[5]],a_v2[[5]])

adj.NP.inside=((est.NP-z.adj.NP*sd.NP)<0)*(0<(est.NP+z.adj.NP*sd.NP)) 
adj.NP.power=(I-sum(adj.NP.inside))/I

return(adj.NP.power)
}

adj.edu_v4=function(a.d0_v1,a.d0_v2,a.d0_v3,a.d0_v4,a_v1,a_v2,a_v3,a_v4,I)
{
est.NPd0=c(a.d0_v1[[4]],a.d0_v2[[4]],a.d0_v3[[4]],a.d0_v4[[4]])
sd.NPd0=c(a.d0_v1[[5]],a.d0_v2[[5]],a.d0_v3[[5]],a.d0_v4[[5]])

chisq.NP=(est.NPd0/sd.NPd0)^2
z.adj.NP=sqrt(quantile(chisq.NP, 0.95))
est.NP=c(a_v1[[4]],a_v2[[4]],a_v3[[4]],a_v4[[4]])
sd.NP=c(a_v1[[5]],a_v2[[5]],a_v3[[5]],a_v4[[5]])

adj.NP.inside=((est.NP-z.adj.NP*sd.NP)<0)*(0<(est.NP+z.adj.NP*sd.NP)) 
adj.NP.power=(I-sum(adj.NP.inside))/I

return(adj.NP.power)
}

adjsrs.edu=function(a.d0,a,I)
{
z.adj.srs=a.d0[[2]]
est.srs=a[[4]]
sd.srs=a[[5]]
adj.srs.inside=((est.srs-z.adj.srs*sd.srs)<0)*(0<(est.srs+z.adj.srs*sd.srs)) 
adj.srs.power=(I-sum(adj.srs.inside))/I
return(adj.srs.power)
}

ad.srs.p.05=rep(adjsrs.edu(ed.srsp.05,ed.srsp.05,40000), 5)
ad.srs.p.07=rep(adjsrs.edu(ed.srsp.05,ed.srsp.07,40000), 5)
ad.srs.p.09=rep(adjsrs.edu(ed.srsp.05,ed.srsp.09,40000), 5)
ad.srs.p.11=rep(adjsrs.edu(ed.srsp.05,ed.srsp.11,40000), 5)

ad.srsNP.p.05=rep(adjsrs.edu(ed.srsNPp.05,ed.srsNPp.05,40000),8)
ad.srsNP.p.07=rep(adjsrs.edu(ed.srsNPp.05,ed.srsNPp.07,40000),8)
ad.srsNP.p.09=rep(adjsrs.edu(ed.srsNPp.05,ed.srsNPp.09,40000),8)
ad.srsNP.p.11=rep(adjsrs.edu(ed.srsNPp.05,ed.srsNPp.11,40000),8)
ad.srsNP.p.13=rep(adjsrs.edu(ed.srsNPp.05,ed.srsNPp.13,40000),8)
ad.srsNP.p.15=rep(adjsrs.edu(ed.srsNPp.05,ed.srsNPp.15,40000),8)

ad.p.05=c(adj.edu_v2(ed.clh3p.05,ed.clh3p.05_v2,ed.clh3p.05,ed.clh3p.05_v2,20000),
          adj.edu_v2(ed.clh6p.05,ed.clh6p.05_v2,ed.clh6p.05,ed.clh6p.05_v2,20000),
          adj.edu_v4(ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,
                     ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,20000),
          adj.edu_v4(ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,
                     ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,20000),
          adj.edu_v4(ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,
                     ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,20000),
          adj.edu_v4(ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,
                     ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,20000),
          adj.edu_v4(ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,
                     ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,20000),
          adj.edu_v4(ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,
                     ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,20000) )

ad.p.07=c(adj.edu_v2(ed.clh3p.05,ed.clh3p.05_v2,ed.clh3p.07,ed.clh3p.07_v2,20000),
          adj.edu_v2(ed.clh6p.05,ed.clh6p.05_v2,ed.clh6p.07,ed.clh6p.07_v2,20000),
          adj.edu_v4(ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,
                     ed.indh3p.07_v1,ed.indh3p.07_v2,ed.indh3p.07_v3,ed.indh3p.07_v4,20000),
          adj.edu_v4(ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,
                     ed.indh5p.07_v1,ed.indh5p.07_v2,ed.indh5p.07_v3,ed.indh5p.07_v4,20000),
          adj.edu_v4(ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,
                     ed.bothh3p.07_v1,ed.bothh3p.07_v2,ed.bothh3p.07_v3,ed.bothh3p.07_v4,20000),
          adj.edu_v4(ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,
                     ed.bothh3h5p.07_v1,ed.bothh3h5p.07_v2,ed.bothh3h5p.07_v3,ed.bothh3h5p.07_v4,20000),
          adj.edu_v4(ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,
                     ed.bothh6h3p.07_v1,ed.bothh6h3p.07_v2,ed.bothh6h3p.07_v3,ed.bothh6h3p.07_v4,20000),
          adj.edu_v4(ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,
                     ed.bothh6h5p.07_v1,ed.bothh6h5p.07_v2,ed.bothh6h5p.07_v3,ed.bothh6h5p.07_v4,20000) )

ad.p.09=c(adj.edu_v2(ed.clh3p.05,ed.clh3p.05_v2,ed.clh3p.09,ed.clh3p.09_v2,20000),
          adj.edu_v2(ed.clh6p.05,ed.clh6p.05_v2,ed.clh6p.09,ed.clh6p.09_v2,20000),
          adj.edu_v4(ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,
                     ed.indh3p.09_v1,ed.indh3p.09_v2,ed.indh3p.09_v3,ed.indh3p.09_v4,20000),
          adj.edu_v4(ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,
                     ed.indh5p.09_v1,ed.indh5p.09_v2,ed.indh5p.09_v3,ed.indh5p.09_v4,20000),
          adj.edu_v4(ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,
                     ed.bothh3p.09_v1,ed.bothh3p.09_v2,ed.bothh3p.09_v3,ed.bothh3p.09_v4,20000),
          adj.edu_v4(ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,
                     ed.bothh3h5p.09_v1,ed.bothh3h5p.09_v2,ed.bothh3h5p.09_v3,ed.bothh3h5p.09_v4,20000),
          adj.edu_v4(ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,
                     ed.bothh6h3p.09_v1,ed.bothh6h3p.09_v2,ed.bothh6h3p.09_v3,ed.bothh6h3p.09_v4,20000),
          adj.edu_v4(ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,
                     ed.bothh6h5p.09_v1,ed.bothh6h5p.09_v2,ed.bothh6h5p.09_v3,ed.bothh6h5p.09_v4,20000) )

ad.p.11=c(adj.edu_v2(ed.clh3p.05,ed.clh3p.05_v2,ed.clh3p.11,ed.clh3p.11_v2,20000),
          adj.edu_v2(ed.clh6p.05,ed.clh6p.05_v2,ed.clh6p.11,ed.clh6p.11_v2,20000),
          adj.edu_v4(ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,
                     ed.indh3p.11_v1,ed.indh3p.11_v2,ed.indh3p.11_v3,ed.indh3p.11_v4,20000),
          adj.edu_v4(ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,
                     ed.indh5p.11_v1,ed.indh5p.11_v2,ed.indh5p.11_v3,ed.indh5p.11_v4,20000),
          adj.edu_v4(ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,
                     ed.bothh3p.11_v1,ed.bothh3p.11_v2,ed.bothh3p.11_v3,ed.bothh3p.11_v4,20000),
          adj.edu_v4(ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,
                     ed.bothh3h5p.11_v1,ed.bothh3h5p.11_v2,ed.bothh3h5p.11_v3,ed.bothh3h5p.11_v4,20000),
          adj.edu_v4(ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,
                     ed.bothh6h3p.11_v1,ed.bothh6h3p.11_v2,ed.bothh6h3p.11_v3,ed.bothh6h3p.11_v4,20000),
          adj.edu_v4(ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,
                     ed.bothh6h5p.11_v1,ed.bothh6h5p.11_v2,ed.bothh6h5p.11_v3,ed.bothh6h5p.11_v4,20000) )

ad.p.13=c(adj.edu_v2(ed.clh3p.05,ed.clh3p.05_v2,ed.clh3p.13,ed.clh3p.13_v2,20000),
          adj.edu_v2(ed.clh6p.05,ed.clh6p.05_v2,ed.clh6p.13,ed.clh6p.13_v2,20000),
          adj.edu_v4(ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,
                     ed.indh3p.13_v1,ed.indh3p.13_v2,ed.indh3p.13_v3,ed.indh3p.13_v4,20000),
          adj.edu_v4(ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,
                     ed.indh5p.13_v1,ed.indh5p.13_v2,ed.indh5p.13_v3,ed.indh5p.13_v4,20000),
          adj.edu_v4(ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,
                     ed.bothh3p.13_v1,ed.bothh3p.13_v2,ed.bothh3p.13_v3,ed.bothh3p.13_v4,20000),
          adj.edu_v4(ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,
                     ed.bothh3h5p.13_v1,ed.bothh3h5p.13_v2,ed.bothh3h5p.13_v3,ed.bothh3h5p.13_v4,20000),
          adj.edu_v4(ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,
                     ed.bothh6h3p.13_v1,ed.bothh6h3p.13_v2,ed.bothh6h3p.13_v3,ed.bothh6h3p.13_v4,20000),
          adj.edu_v4(ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,
                     ed.bothh6h5p.13_v1,ed.bothh6h5p.13_v2,ed.bothh6h5p.13_v3,ed.bothh6h5p.13_v4,20000) )

ad.p.15=c(adj.edu_v2(ed.clh3p.05,ed.clh3p.05_v2,ed.clh3p.15,ed.clh3p.15_v2,20000),
          adj.edu_v2(ed.clh6p.05,ed.clh6p.05_v2,ed.clh6p.15,ed.clh6p.15_v2,20000),
          adj.edu_v4(ed.indh3p.05_v1,ed.indh3p.05_v2,ed.indh3p.05_v3,ed.indh3p.05_v4,
                     ed.indh3p.15_v1,ed.indh3p.15_v2,ed.indh3p.15_v3,ed.indh3p.15_v4,20000),
          adj.edu_v4(ed.indh5p.05_v1,ed.indh5p.05_v2,ed.indh5p.05_v3,ed.indh5p.05_v4,
                     ed.indh5p.15_v1,ed.indh5p.15_v2,ed.indh5p.15_v3,ed.indh5p.15_v4,20000),
          adj.edu_v4(ed.bothh3p.05_v1,ed.bothh3p.05_v2,ed.bothh3p.05_v3,ed.bothh3p.05_v4,
                     ed.bothh3p.15_v1,ed.bothh3p.15_v2,ed.bothh3p.15_v3,ed.bothh3p.15_v4,20000),
          adj.edu_v4(ed.bothh3h5p.05_v1,ed.bothh3h5p.05_v2,ed.bothh3h5p.05_v3,ed.bothh3h5p.05_v4,
                     ed.bothh3h5p.15_v1,ed.bothh3h5p.15_v2,ed.bothh3h5p.15_v3,ed.bothh3h5p.15_v4,20000),
          adj.edu_v4(ed.bothh6h3p.05_v1,ed.bothh6h3p.05_v2,ed.bothh6h3p.05_v3,ed.bothh6h3p.05_v4,
                     ed.bothh6h3p.15_v1,ed.bothh6h3p.15_v2,ed.bothh6h3p.15_v3,ed.bothh6h3p.15_v4,20000),
          adj.edu_v4(ed.bothh6h5p.05_v1,ed.bothh6h5p.05_v2,ed.bothh6h5p.05_v3,ed.bothh6h5p.05_v4,
                     ed.bothh6h5p.15_v1,ed.bothh6h5p.15_v2,ed.bothh6h5p.15_v3,ed.bothh6h5p.15_v4,20000) )


adjust=data.frame(ad.srsNP.p.05,ad.p.05,ad.srsNP.p.07,ad.p.07,ad.srsNP.p.09,ad.p.09,ad.srsNP.p.11,ad.p.11,ad.srsNP.p.13,ad.p.13,ad.srsNP.p.15,ad.p.15)
colnames(adjust)=c("srsNPp.05","NPp.05","srsNPp.07","NPp.07","srsNPp.09","NPp.09","srsNPp.11","NPp.11","srsNPp.13","NPp.13","srsNPp.15","NPp.15")

# write.xlsx(adjust,"/Users/Mumu/Dropbox/Mumu/Research New/Data example/Edu_results.xlsx",sheetName="Mar7_power_v4_adjust",append=TRUE)

write.xlsx(adjust,"D:\\Dropbox\\Mumu\\Research New\\Data example\\Edu_results.xlsx",sheetName="New_power_v4_adjust",append=TRUE)

 
