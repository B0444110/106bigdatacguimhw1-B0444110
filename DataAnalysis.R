install.packages("jsonlite")

install.packages("httr")

install.packages("dplyr")
install.packages("knitr")

library(jsonlite)
library(httr)
library(dplyr)

e103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=ab4d900522783ba0dc18606eae55cfd8")

ne103<-rename(e103,usx=`大學-薪資`)
ne103$usx<-gsub("—","",ne103$usx)
ne103$usx<-as.numeric(ne103$usx)


e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")

ne105<-rename(e105,usy=`大學-薪資`)
ne105$usy<-gsub("—","",ne105$usy)
ne105$usy<-as.numeric(ne105$usy)
new<-inner_join(ne103,ne105,by="大職業別")%>%

mutate(rate=new$usy/new$usx) 

head(arrange(new,desc(rate))$"大職業別",10)

#1-1 前十名

library(jsonlite)
library(httr)
library(dplyr)

e103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=ab4d900522783ba0dc18606eae55cfd8")

ne103<-rename(e103,usx=`大學-薪資`)
ne103$usx<-gsub("—","",ne103$usx)
ne103$usx<-as.numeric(ne103$usx)


e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")

ne105<-rename(e105,usy=`大學-薪資`)
ne105$usy<-gsub("—","",ne105$usy)
ne105$usy<-as.numeric(ne105$usy)

new<-inner_join(ne103,ne105,by="大職業別")%>%
  
mutate(rate=new$usy/new$usx) 

#1-2

new[new$rate>1.05,2]

#1-3 
m=new[new$rate>1.05,2]
a<-strsplit (m,"-")
sapply(a, "[", 1)
table(sapply(a, "[", 1))

#2-1
#(男女比>100,女比男多)
library(jsonlite)
library(httr)
library(dplyr)

e103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=ab4d900522783ba0dc18606eae55cfd8")
ne103<-rename(e103,usbg=`大學-女/男`)
ne103$usbg<-gsub("—|…","",ne103$usbg)
ne103$usbg<-as.numeric(ne103$usbg)

a103<- filter(ne103,usbg<100)
head(arrange(a103,(usbg))$"大職業別",10)


e104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=d95458abf9ace893d7bd94422156be84")
ne104<-rename(e104,usbg=`大學-女/男`)
ne104$usbg<-gsub("—|…","",ne104$usbg)
ne104$usbg<-as.numeric(ne104$usbg)

a104<- filter(ne104,usbg<100)
head(arrange(a104,(usbg))$"大職業別",10)

e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")
ne105<-rename(e105,usbg=`大學-女/男`)
ne105$usbg<-gsub("—|…","",ne105$usbg)
ne105$usbg<-as.numeric(ne105$usbg)

a105<- filter(ne105,usbg<100)
head(arrange(a105,(usbg))$"大職業別",10)


#2-2
e103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=ab4d900522783ba0dc18606eae55cfd8")
ne103<-rename(e103,usbg=`大學-女/男`)
ne103$usbg<-gsub("—|…","",ne103$usbg)
ne103$usbg<-as.numeric(ne103$usbg)

a103<- filter(ne103,usbg>100)
head(arrange(a103,(usbg))$"大職業別",10)


e104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=d95458abf9ace893d7bd94422156be84")
ne104<-rename(e104,usbg=`大學-女/男`)
ne104$usbg<-gsub("—|…","",ne104$usbg)
ne104$usbg<-as.numeric(ne104$usbg)

a104<- filter(ne104,usbg>100)
head(arrange(a104,(usbg))$"大職業別",10)

e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")
ne105<-rename(e105,usbg=`大學-女/男`)
ne105$usbg<-gsub("—|…","",ne105$usbg)
ne105$usbg<-as.numeric(ne105$usbg)

a105<- filter(ne105,usbg>100)
head(arrange(a105,(usbg))$"大職業別",10)

#3
e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")

ne105<-rename(e105,usy=`大學-薪資`)
ne105$usy<-gsub("—","",ne105$usy)
ne105$usy<-as.numeric(ne105$usy)

ne105<-rename(ne105,usgu=`研究所及以上-薪資`)
ne105$usgu<-gsub("—","",ne105$usgu)
ne105$usgu<-as.numeric(ne105$usgu)

ne105<-mutate(ne105,sarate=ne105$usgu/ne105$usy) 

head(arrange(ne105,desc(sarate))$"大職業別",10)

str(new105)

#4

ne103<-rename(e103,usx=`大學-薪資`)
ne103$usx<-gsub("—","",ne103$usx)
ne103$usx<-as.numeric(ne103$usx)
ne103$大職業別<-c(ne103$大職業別)


ne103<-rename(ne103,usgu=`研究所及以上-薪資`)
ne103$usgu<-gsub("—","",ne103$usgu)
ne103$usgu<-as.numeric(ne103$usgu)
ne103$大職業別<-c(ne103$大職業別)

ne104<-rename(e104,usx=`大學-薪資`)
ne104$usx<-gsub("—","",ne104$usx)
ne104$usx<-as.numeric(ne104$usx)
ne104$大職業別<-c(ne104$大職業別)


ne104<-rename(ne104,usgu=`研究所及以上-薪資`)
ne104$usgu<-gsub("—","",ne104$usgu)
ne104$usgu<-as.numeric(ne104$usgu)
ne104$大職業別<-c(ne104$大職業別)

ne105<-rename(e105,usx=`大學-薪資`)
ne105$usx<-gsub("—","",ne105$usx)
ne105$usx<-as.numeric(ne105$usx)
ne105$大職業別<-c(ne105$大職業別)


ne105<-rename(ne105,usgu=`研究所及以上-薪資`)
ne105$usgu<-gsub("—","",ne105$usgu)
ne105$usgu<-as.numeric(ne105$usgu)
ne105$大職業別<-c(ne105$大職業別)

tech<-data.frame(job=ne103$大職業別[grep("資訊",ne103$大職業別)],
                 salary103us=ne103$usx[grep("資訊",ne103$大職業別)],
                 salary104us=ne104$usx[grep("資訊",ne104$大職業別)],
                 salary105us=ne105$usx[grep("資訊",ne105$大職業別)],
                 salary103gu=ne103$usgu[grep("資訊",ne103$大職業別)],
                 salary104gu=ne104$usgu[grep("資訊",ne104$大職業別)],
                 salary105gu=ne105$usgu[grep("資訊",ne105$大職業別)])

View(tech)

