108-2 大數據分析方法 作業一
================
title: "108-2 大數據分析方法 作業一"
output: github_document
author: 江昱叡

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**-
（107年）<https://data.gov.tw/dataset/6647>
（104-105年）<http://ipgod.nchc.org.tw/dataset/a17000000j-020066>
，可初步了解台灣近幾年各行各業、各學歷的起薪。

## 比較104年度和107年度大學畢業者的薪資資料

### 資料匯入與處理

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### 107年度薪資較104年度薪資高的職業有哪些?

``` r
newcomer104=read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
newcomer107 <- read_csv("C:/Users/Freddy/Desktop/hi.csv")
newcomer104[[2]]=gsub("部門"," ",newcomer104[[2]])
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
```

### 提高超過5%的的職業有哪些?

``` r
newcomer104[[2]]=gsub("部門"," ",newcomer104[[2]])
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
```

### 主要的職業種別是哪些種類呢?

``` r
mergeDF1=subset(mergeDF,mergeDF[[6]]>1.05)
for(i in 1:53){
  a[i]=strsplit (mergeDF1[i,1],"-")
}

table(mergeDF1[[7]])
```

## 男女同工不同酬現況分析

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 104和107年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
female_105=newgender_rateDF[,c(1:3)]
female_105=subset(female_105,female_105[[3]]>100)
male_105=newgender_rateDF[,c(1:3)]
male_105=subset(male_105,male_105[[3]]<100)
female_107=newgender_rateDF[,c(1,4,5)]
female_107=subset(female_107,female_107[[3]]>100)
male_107=newgender_rateDF[,c(1,4,5)]
male_107=subset(male_107,male_107[[3]]<100)
tail(arrange(male_105, desc(male_105[[3]])),10)
tail(arrange(male_107, desc(male_107[[3]])),10)
```

### 哪些行業女生薪資比男生薪資多?

``` r
head(arrange(female_105, desc(female_105[[3]])),10)
head(arrange(female_107, desc(female_107[[3]])),10)
```

## 研究所薪資差異

以107年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
graduate_institute=newcomer107[,c(1,2,11,13)]
names(graduate_institute)[5]="研究所薪資 / 大學薪資"
graduate_institute[[3]]=gsub("—|…"," ",graduate_institute[[3]])
graduate_institute[[4]]=gsub("—|…"," ",graduate_institute[[4]])
graduate_institute[[3]]=as.numeric(graduate_institute[[3]])
graduate_institute[[4]]=as.numeric(graduate_institute[[4]])
graduate_institute[[5]]=(graduate_institute[[4]]/graduate_institute[[3]])
head(arrange(graduate_institute, desc(graduate_institute[[5]])),10)
```

## 我有興趣的職業別薪資狀況分析

### 有興趣的職業別篩選，呈現薪資

``` r
graduate=graduate_institute[grepl("其他服務業",graduate_institute[[2]]),]
names(graduate)[6]="研究所薪資與大學薪資差"
```

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
graduate=graduate_institute[grepl("其他服務業",graduate_institute[[2]]),]
names(graduate)[6]="研究所薪資與大學薪資差"
graduate[[6]]=(graduate[[4]]-graduate[[3]])
```
