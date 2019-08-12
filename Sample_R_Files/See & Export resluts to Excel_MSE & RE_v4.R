# install.packages("xlsx") #
library('xlsx')

load("D:\\Dropbox\\Mumu\\Research New\\Data example\\HPC Mar7 Edu Rimage\\all.ed_v4.RData")

MSE.srsMLE=ed.srsp.05[[3]]
MSE.srsNP=ed.srsNPp.05[[3]]
MSE.NPcl= mean(c(ed.clh3p.05[[3]][1],ed.clh3p.05_v2[[3]][1]))
MSE.PLcl= mean(c(ed.clh3p.05[[3]][2],ed.clh3p.05_v2[[3]][2]))
MSE.MLEcl=mean(c(ed.clh3p.05[[3]][3],ed.clh3p.05_v2[[3]][3]))

MSE.NPindh3=mean(c(ed.indh3p.05_v1[[3]][1],ed.indh3p.05_v2[[3]][1],ed.indh3p.05_v3[[3]][1],ed.indh3p.05_v4[[3]][1]))
MSE.NPbothh3=mean(c(ed.bothh3p.05_v1[[3]][1],ed.bothh3p.05_v2[[3]][1],ed.bothh3p.05_v3[[3]][1],ed.bothh3p.05_v4[[3]][1]))
MSE.NPindh5=mean(c(ed.indh5p.05_v1[[3]][1],ed.indh5p.05_v2[[3]][1],ed.indh5p.05_v3[[3]][1],ed.indh5p.05_v4[[3]][1]))
MSE.NPbothh3h5=mean(c(ed.bothh3h5p.05_v1[[3]][1],ed.bothh3h5p.05_v2[[3]][1],ed.bothh3h5p.05_v3[[3]][1],ed.bothh3h5p.05_v4[[3]][1]))

MSE.PLindh3=mean(c(ed.indh3p.05_v1[[3]][2],ed.indh3p.05_v2[[3]][2],ed.indh3p.05_v3[[3]][2],ed.indh3p.05_v4[[3]][2]))
MSE.PLbothh3=mean(c(ed.bothh3p.05_v1[[3]][2],ed.bothh3p.05_v2[[3]][2],ed.bothh3p.05_v3[[3]][2],ed.bothh3p.05_v4[[3]][2]))
MSE.PLindh5=mean(c(ed.indh5p.05_v1[[3]][2],ed.indh5p.05_v2[[3]][2],ed.indh5p.05_v3[[3]][2],ed.indh5p.05_v4[[3]][2]))
MSE.PLbothh3h5=mean(c(ed.bothh3h5p.05_v1[[3]][2],ed.bothh3h5p.05_v2[[3]][2],ed.bothh3h5p.05_v3[[3]][2],ed.bothh3h5p.05_v4[[3]][2]))

MSE.MLEindh3=mean(c(ed.indh3p.05_v1[[3]][3],ed.indh3p.05_v2[[3]][3],ed.indh3p.05_v3[[3]][3],ed.indh3p.05_v4[[3]][3]))
MSE.MLEbothh3=mean(c(ed.bothh3p.05_v1[[3]][3],ed.bothh3p.05_v2[[3]][3],ed.bothh3p.05_v3[[3]][3],ed.bothh3p.05_v4[[3]][3]))
MSE.MLEindh5=mean(c(ed.indh5p.05_v1[[3]][3],ed.indh5p.05_v2[[3]][3],ed.indh5p.05_v3[[3]][3],ed.indh5p.05_v4[[3]][3]))
MSE.MLEbothh3h5=mean(c(ed.bothh3h5p.05_v1[[3]][3],ed.bothh3h5p.05_v2[[3]][3],ed.bothh3h5p.05_v3[[3]][3],ed.bothh3h5p.05_v4[[3]][3]))

MSE.NP=c(MSE.NPcl,MSE.NPindh3,MSE.NPindh5,MSE.NPbothh3,MSE.NPbothh3h5)
MSE.PL=c(MSE.PLcl,MSE.PLindh3,MSE.PLindh5,MSE.PLbothh3,MSE.PLbothh3h5)
MSE.MLE=c(MSE.MLEcl,MSE.MLEindh3,MSE.MLEindh5,MSE.MLEbothh3,MSE.MLEbothh3h5)
MSE.srs_MLE=rep(MSE.srsMLE, 5)
MSE.srs_NP=rep(MSE.srsNP, 5)

MSE=data.frame(MSE.srs_NP,MSE.NP,MSE.srs_MLE,MSE.PL,MSE.MLE)
colnames(MSE)=c("srs_NP","NP","srs_MLE","PL","MLE")


write.xlsx(MSE,"D:\\Dropbox\\Mumu\\Research New\\Data example\\Edu_results.xlsx",sheetName="Mar7_MSE_v4",append=TRUE)

###### NEW ONLY NP iteration=100,000 ######

REclh3=ed.srsNPp.05_NP_MSE/ed.clh3p.05_NP_MSE
REclh6=ed.srsNPp.05_NP_MSE/ed.clh6p.05_NP_MSE
REindh3=ed.srsNPp.05_NP_MSE/ed.indh3p.05_NP_MSE
REindh5=ed.srsNPp.05_NP_MSE/ed.indh5p.05_NP_MSE
REbothh3=ed.srsNPp.05_NP_MSE/ed.bothh3p.05_NP_MSE
REbothh3h5=ed.srsNPp.05_NP_MSE/ed.bothh3h5p.05_NP_MSE
REbothh6h3=ed.srsNPp.05_NP_MSE/ed.bothh6h3p.05_NP_MSE
REbothh6h5=ed.srsNPp.05_NP_MSE/ed.bothh6h5p.05_NP_MSE

RE=data.frame(REclh3,REclh6,REindh3,REindh5,REbothh3,REbothh3h5,REbothh6h3,REbothh6h5)

# write.xlsx(RE,"/Users/Mumu/Dropbox/Mumu/Research New/Data example/Edu_results.xlsx",sheetName="Mar7_RE_v4",append=TRUE) #

write.xlsx(RE,"D:\\Dropbox\\Mumu\\Research New\\Data example\\Edu_results.xlsx",sheetName="New_RE_i100000",append=TRUE)


###### ONLY NP  ######
MSE.NPindh3=mean(c(ed.indh3p.05_v1[[3]][1],ed.indh3p.05_v2[[3]][1],ed.indh3p.05_v3[[3]][1],ed.indh3p.05_v4[[3]][1]))
MSE.NPbothh3=mean(c(ed.bothh3p.05_v1[[3]][1],ed.bothh3p.05_v2[[3]][1],ed.bothh3p.05_v3[[3]][1],ed.bothh3p.05_v4[[3]][1]))

MSE.NPindh5=mean(c(ed.indh5p.05_v1[[3]][1],ed.indh5p.05_v2[[3]][1],ed.indh5p.05_v3[[3]][1],ed.indh5p.05_v4[[3]][1]))
MSE.NPbothh3h5=mean(c(ed.bothh3h5p.05_v1[[3]][1],ed.bothh3h5p.05_v2[[3]][1],ed.bothh3h5p.05_v3[[3]][1],ed.bothh3h5p.05_v4[[3]][1]))

MSE.NPcl=mean(c(ed.clh3p.05[[3]][1],ed.clh3p.05_v2[[3]][1]))
MSE.srsNP=ed.srsNPp.05[[3]]

RE.NPindh3=MSE.srsNP/MSE.NPindh3
RE.NPbothh3=MSE.srsNP/MSE.NPbothh3
RE.NPindh5=MSE.srsNP/MSE.NPindh5
RE.NPbothh3h5=MSE.srsNP/MSE.NPbothh3h5
RE.NPcl=MSE.srsNP/MSE.NPcl

RE=data.frame(RE.NPcl,RE.NPindh3,RE.NPindh5,RE.NPbothh3,RE.NPbothh3h5)

write.xlsx(RE,"/Users/Mumu/Dropbox/Mumu/Research New/Data example/Edu_results.xlsx",sheetName="Mar7_RE_v4",append=TRUE)

write.xlsx(RE,"D:\\Dropbox\\Mumu\\Research New\\Data example\\Edu_results.xlsx",sheetName="Mar7_RE_v4",append=TRUE)


 