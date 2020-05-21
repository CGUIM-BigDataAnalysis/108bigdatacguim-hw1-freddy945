
library(dplyr)
library(readr)
library(stringr)
#-----Q1
newcomer104=read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
newcomer107 <- read_csv("C:/Users/Freddy/Desktop/hi.csv")
newcomer104[[2]]=gsub("部門"," ",newcomer104$大職業別)
NEWcomer104=newcomer104[,c(1,2,11)]
NEWcomer107=newcomer107[,c(1,2,11)]
NEWcomer104[[3]]=gsub("—"," ",NEWcomer104[[3]])
NEWcomer107[[3]]=gsub("—|…"," ",NEWcomer107[[3]])
NEWcomer104[[3]]=as.numeric(NEWcomer104[[3]])
NEWcomer107[[3]]=as.numeric(NEWcomer107[[3]])
cNEWcomer104=NEWcomer104[complete.cases(NEWcomer104),]
cNEWcomer107=NEWcomer107[complete.cases(NEWcomer107),]
for(i in 1:119){
cNEWcomer104[i,2]=str_trim(cNEWcomer104[i,2])
cNEWcomer104[i,2]=gsub("、","_",cNEWcomer104[i,2])
cNEWcomer104[i,2]=gsub(" -","-",cNEWcomer104[i,2])
cNEWcomer104[i,2]=gsub("教育服務業","教育業",cNEWcomer104[i,2])
cNEWcomer104[i,2]=gsub("營造業","營建工程",cNEWcomer104[i,2])
cNEWcomer104[i,2]=gsub("醫療保健服務業","醫療保健業",cNEWcomer104[i,2])
cNEWcomer104[i,2]=gsub("資訊及通訊傳播業","出版、影音製作、傳播及資通訊服務業",cNEWcomer104[i,2])
}
for(j in 1:117)
cNEWcomer104[j,2]==str_trim(cNEWcomer107[j,2])
mergeDF=merge(cNEWcomer104,cNEWcomer107,by="大職業別")
for(i in 1:117){
mergeDF[i,6]=(mergeDF[i,5]/mergeDF[i,3])
}
names(mergeDF)[6]="107年度薪資較104年度薪資高的職業比例"
head(arrange(mergeDF, desc(mergeDF[[6]])),10)
mergeDF1=subset(mergeDF,mergeDF[[6]]>1.05)
for(i in 1:53){
  mergeDF1[i,7]=strsplit(mergeDF1[i,1],"-")
}
table(mergeDF1[,7])

#-----Q2

newcomer104[[2]]=gsub("部門"," ",newcomer104$大職業別)
gender_rate104=newcomer104[,c(1,2,12)]
gender_rate107=newcomer107[,c(1,2,12)]
gender_rate104[[3]]=gsub("—|…"," ",gender_rate104[[3]])
gender_rate107[[3]]=gsub("—|…"," ",gender_rate107[[3]])
newgender_rate104=gender_rate104[complete.cases(gender_rate104),]
newgender_rate107=gender_rate107[complete.cases(gender_rate107),]
newgender_rate104[[3]]=as.numeric(newgender_rate104[[3]])
newgender_rate107[[3]]=as.numeric(newgender_rate107[[3]])

for(i in 1:119){
  newgender_rate104[i,2]=str_trim(newgender_rate104[i,2])
  newgender_rate104[i,2]=gsub("、","_",newgender_rate104[i,2])
  newgender_rate104[i,2]=gsub(" -","-",newgender_rate104[i,2])
  newgender_rate104[i,2]=gsub("教育服務業","教育業",newgender_rate104[i,2])
  newgender_rate104[i,2]=gsub("營造業","營建工程",newgender_rate104[i,2])
  newgender_rate104[i,2]=gsub("醫療保健服務業","醫療保健業",newgender_rate104[i,2])
  newgender_rate104[i,2]=gsub("資訊及通訊傳播業","出版、影音製作、傳播及資通訊服務業",newgender_rate104[i,2])
}
for(j in 1:117)
newgender_rate104[j,2]==str_trim(newgender_rate107[j,2])

newgender_rateDF=merge(newgender_rate104,newgender_rate107,by="大職業別")

female_105=newgender_rateDF[,c(1:3)]
female_105=subset(female_105,female_105[[3]]>100)
male_105=newgender_rateDF[,c(1:3)]
male_105=subset(male_105,male_105[[3]]<100)
female_107=newgender_rateDF[,c(1,4,5)]
female_107=subset(female_107,female_107[[3]]>100)
male_107=newgender_rateDF[,c(1,4,5)]
male_107=subset(male_107,male_107[[3]]<100)
head(arrange(female_105, desc(female_105[[3]])),10)
head(arrange(female_107, desc(female_107[[3]])),10)
tail(arrange(male_105, desc(male_105[[3]])),10)
tail(arrange(male_107, desc(male_107[[3]])),10)

#-----Q3
graduate_institute=newcomer107[,c(1,2,11,13)]
names(graduate_institute)[5]="研究所薪資 / 大學薪資"
graduate_institute[[3]]=gsub("—|…"," ",graduate_institute[[3]])
graduate_institute[[4]]=gsub("—|…"," ",graduate_institute[[4]])
graduate_institute[[3]]=as.numeric(graduate_institute[[3]])
graduate_institute[[4]]=as.numeric(graduate_institute[[4]])
graduate_institute[[5]]=(graduate_institute[[4]]/graduate_institute[[3]])
head(arrange(graduate_institute, desc(graduate_institute[[5]])),10)

#-----Q4
graduate=graduate_institute[grepl("其他服務業",graduate_institute[[2]]),]
names(graduate)[6]="研究所薪資與大學薪資差"
graduate[[6]]=(graduate[[4]]-graduate[[3]])

