106-2 大數據分析方法 作業一
================
徐宛萱

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和105年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
library(jsonlite)
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

``` r
e103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=ab4d900522783ba0dc18606eae55cfd8")

ne103<-rename(e103,usx=`大學-薪資`)
ne103$usx<-gsub("—","",ne103$usx)
ne103$usx<-as.numeric(ne103$usx)

e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")

ne105<-rename(e105,usy=`大學-薪資`)
ne105$usy<-gsub("—","",ne105$usy)
ne105$usy<-as.numeric(ne105$usy)
new<-inner_join(ne103,ne105,by="大職業別")
#fromjson()
#inner_join()
```

### 105年度薪資較103年度薪資高的職業有哪些?

``` r
#這是R Code Chunk
library(jsonlite)
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
```

    ##  [1] "礦業及土石採取業-服務及銷售工作人員"                
    ##  [2] "用水供應及污染整治業-技術員及助理專業人員"          
    ##  [3] "藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"
    ##  [4] "其他服務業-技術員及助理專業人員"                    
    ##  [5] "其他服務業-技藝、機械設備操作及組裝人員"            
    ##  [6] "住宿及餐飲業-技術員及助理專業人員"                  
    ##  [7] "住宿及餐飲業-服務及銷售工作人員"                    
    ##  [8] "用水供應及污染整治業-技藝、機械設備操作及組裝人員"  
    ##  [9] "運輸及倉儲業-技藝、機械設備操作及組裝人員"          
    ## [10] "不動產業-專業人員"

提高比率由大至小前十名為以下:

1."礦業及土石採取業-服務及銷售工作人員"

2."用水供應及污染整治業-技術員及助理專業人員"

3."藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"

4."其他服務業-技術員及助理專業人員"

5."其他服務業-技藝、機械設備操作及組裝人員"

6."住宿及餐飲業-技術員及助理專業人員"

7."住宿及餐飲業-服務及銷售工作人員"

8."用水供應及污染整治業-技藝、機械設備操作及組裝人員"

9."運輸及倉儲業-技藝、機械設備操作及組裝人員"

10."不動產業-專業人員"

文字說明:

"用水供應及污染整治業"、"其他服務業"、"住宿及餐飲業"皆在前十名當中各占了20% 值得注意的是，前十名產業的類別當中，僅有"專業人員"這個細項在第十名占了一個數目， "服務及銷售工作人員"(2),"技藝、機械設備操作及組裝人員"(4),"技術員及助理專業人員"(3)， 在這份資料中，在不同產業型別有專業能力的人薪水漲幅相較其他大。

### 提高超過5%的的職業有哪些?

``` r
#這是R Code Chunk
new[new$rate>1.05,2]
```

    ##  [1] NA                                                   
    ##  [2] NA                                                   
    ##  [3] "礦業及土石採取業-服務及銷售工作人員"                
    ##  [4] NA                                                   
    ##  [5] NA                                                   
    ##  [6] "電力及燃氣供應業-服務及銷售工作人員"                
    ##  [7] NA                                                   
    ##  [8] "用水供應及污染整治業"                               
    ##  [9] "用水供應及污染整治業-技術員及助理專業人員"          
    ## [10] "用水供應及污染整治業-事務支援人員"                  
    ## [11] "用水供應及污染整治業-服務及銷售工作人員"            
    ## [12] "用水供應及污染整治業-技藝、機械設備操作及組裝人員"  
    ## [13] NA                                                   
    ## [14] "營造業-專業人員"                                    
    ## [15] NA                                                   
    ## [16] "服務業部門-技藝、機械設備操作及組裝人員"            
    ## [17] NA                                                   
    ## [18] NA                                                   
    ## [19] "運輸及倉儲業-技藝、機械設備操作及組裝人員"          
    ## [20] NA                                                   
    ## [21] "住宿及餐飲業"                                       
    ## [22] "住宿及餐飲業-技術員及助理專業人員"                  
    ## [23] "住宿及餐飲業-服務及銷售工作人員"                    
    ## [24] "住宿及餐飲業-技藝、機械設備操作及組裝人員"          
    ## [25] NA                                                   
    ## [26] "資訊及通訊傳播業"                                   
    ## [27] "資訊及通訊傳播業-專業人員"                          
    ## [28] "資訊及通訊傳播業-事務支援人員"                      
    ## [29] "資訊及通訊傳播業-技藝、機械設備操作及組裝人員"      
    ## [30] NA                                                   
    ## [31] NA                                                   
    ## [32] "不動產業-專業人員"                                  
    ## [33] NA                                                   
    ## [34] "專業、科學及技術服務業"                             
    ## [35] "專業、科學及技術服務業-專業人員"                    
    ## [36] "專業、科學及技術服務業-服務及銷售工作人員"          
    ## [37] NA                                                   
    ## [38] "支援服務業-專業人員"                                
    ## [39] "支援服務業-技術員及助理專業人員"                    
    ## [40] "支援服務業-服務及銷售工作人員"                      
    ## [41] NA                                                   
    ## [42] "教育服務業-事務支援人員"                            
    ## [43] "教育服務業-技藝、機械設備操作及組裝人員"            
    ## [44] NA                                                   
    ## [45] "醫療保健服務業-技術員及助理專業人員"                
    ## [46] NA                                                   
    ## [47] "藝術、娛樂及休閒服務業-事務支援人員"                
    ## [48] "藝術、娛樂及休閒服務業-服務及銷售工作人員"          
    ## [49] "藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"
    ## [50] NA                                                   
    ## [51] "其他服務業-專業人員"                                
    ## [52] "其他服務業-技術員及助理專業人員"                    
    ## [53] "其他服務業-技藝、機械設備操作及組裝人員"            
    ## [54] NA

提高超過5%的為以下:

1."礦業及土石採取業-服務及銷售工作人員"

2."電力及燃氣供應業-服務及銷售工作人員"

3."用水供應及污染整治業"

4."用水供應及污染整治業-技術員及助理專業人員"

5."用水供應及污染整治業-事務支援人員"

6."用水供應及污染整治業-服務及銷售工作人員"

7."用水供應及污染整治業-技藝、機械設備操作及組裝人員"

8."營造業-專業人員"

9."服務業部門-技藝、機械設備操作及組裝人員"

10."運輸及倉儲業-技藝、機械設備操作及組裝人員"

11."住宿及餐飲業"

12."住宿及餐飲業-技術員及助理專業人員"

13."住宿及餐飲業-服務及銷售工作人員"

14."住宿及餐飲業-技藝、機械設備操作及組裝人員"

15."資訊及通訊傳播業"

16."資訊及通訊傳播業-專業人員"

17."資訊及通訊傳播業-事務支援人員"

18."資訊及通訊傳播業-技藝、機械設備操作及組裝人員"

19."不動產業-專業人員"

20."專業、科學及技術服務業"

21."專業、科學及技術服務業-專業人員"

22."專業、科學及技術服務業-服務及銷售工作人員"

23."支援服務業-專業人員"

24."支援服務業-技術員及助理專業人員"

25."支援服務業-服務及銷售工作人員"

26."教育服務業-事務支援人員"

27."教育服務業-技藝、機械設備操作及組裝人員"

28."醫療保健服務業-技術員及助理專業人員"

29."藝術、娛樂及休閒服務業-事務支援人員"

30."藝術、娛樂及休閒服務業-服務及銷售工作人員"

31."藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"

32."其他服務業-專業人員"

33."其他服務業-技術員及助理專業人員"

34."其他服務業-技藝、機械設備操作及組裝人員"

### 主要的職業種別是哪些種類呢?

``` r
m=new[new$rate>1.05,2]
a<-strsplit (m,"-")
sapply(a, "[", 1)
```

    ##  [1] NA                       NA                      
    ##  [3] "礦業及土石採取業"       NA                      
    ##  [5] NA                       "電力及燃氣供應業"      
    ##  [7] NA                       "用水供應及污染整治業"  
    ##  [9] "用水供應及污染整治業"   "用水供應及污染整治業"  
    ## [11] "用水供應及污染整治業"   "用水供應及污染整治業"  
    ## [13] NA                       "營造業"                
    ## [15] NA                       "服務業部門"            
    ## [17] NA                       NA                      
    ## [19] "運輸及倉儲業"           NA                      
    ## [21] "住宿及餐飲業"           "住宿及餐飲業"          
    ## [23] "住宿及餐飲業"           "住宿及餐飲業"          
    ## [25] NA                       "資訊及通訊傳播業"      
    ## [27] "資訊及通訊傳播業"       "資訊及通訊傳播業"      
    ## [29] "資訊及通訊傳播業"       NA                      
    ## [31] NA                       "不動產業"              
    ## [33] NA                       "專業、科學及技術服務業"
    ## [35] "專業、科學及技術服務業" "專業、科學及技術服務業"
    ## [37] NA                       "支援服務業"            
    ## [39] "支援服務業"             "支援服務業"            
    ## [41] NA                       "教育服務業"            
    ## [43] "教育服務業"             NA                      
    ## [45] "醫療保健服務業"         NA                      
    ## [47] "藝術、娛樂及休閒服務業" "藝術、娛樂及休閒服務業"
    ## [49] "藝術、娛樂及休閒服務業" NA                      
    ## [51] "其他服務業"             "其他服務業"            
    ## [53] "其他服務業"             NA

``` r
table(sapply(a, "[", 1))
```

    ## 
    ##               不動產業             支援服務業   用水供應及污染整治業 
    ##                      1                      3                      5 
    ##           住宿及餐飲業             其他服務業             服務業部門 
    ##                      4                      3                      1 
    ## 專業、科學及技術服務業             教育服務業       資訊及通訊傳播業 
    ##                      3                      2                      4 
    ##           運輸及倉儲業       電力及燃氣供應業                 營造業 
    ##                      1                      1                      1 
    ##         醫療保健服務業 藝術、娛樂及休閒服務業       礦業及土石採取業 
    ##                      1                      3                      1

``` r
#這是R Code Chunk
```

產業類別的計數:

1.不動產業 (1)

2.支援服務業(3)

3.用水供應及污染整治業(5)

4.住宿及餐飲業 (4)

5.其他服務業 (3)

6.服務業部門 (1)

7.專業、科學及技術服務業(3)

8.教育服務業(2)

9.資訊及通訊傳播業(4)

10.運輸及倉儲業(1)

11.電力及燃氣供應業(1)

12.營造業(1)

13.醫療保健服務業(1)

14.藝術、娛樂及休閒服務業(3)

15.礦業及土石採取業(1)

文字說明:

"用水供應及污染整治業"系列最多，相關產業成長超過5%就有五個，比率也很前面(前十名當中有兩個)，

"住宿及餐飲業"(在前十名當中有兩個)以及"資訊及通訊傳播業"則是四個，

相比之下，"資訊及通訊傳播業"並沒有在前十名當中，

"其他服務業"這個類別有三個，有相關專業技術的人員比率也相當前面。

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到105年度的大學畢業薪資。

### 103到105年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#這是R Code Chunk
#103男生比女生多(差異由大至小)
e103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=ab4d900522783ba0dc18606eae55cfd8")
ne103<-rename(e103,usbg=`大學-女/男`)
ne103$usbg<-gsub("—|…","",ne103$usbg)
ne103$usbg<-as.numeric(ne103$usbg)

a103<- filter(ne103,usbg<100)
head(arrange(a103,(usbg))$"大職業別",10)
```

    ##  [1] "礦業及土石採取業-技藝、機械設備操作及組裝人員"      
    ##  [2] "教育服務業-技藝、機械設備操作及組裝人員"            
    ##  [3] "其他服務業-技術員及助理專業人員"                    
    ##  [4] "電力及燃氣供應業-技藝、機械設備操作及組裝人員"      
    ##  [5] "礦業及土石採取業-服務及銷售工作人員"                
    ##  [6] "營造業"                                             
    ##  [7] "教育服務業-事務支援人員"                            
    ##  [8] "教育服務業"                                         
    ##  [9] "藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"
    ## [10] "其他服務業"

``` r
#104男生比女生多(差異由大至小)
e104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=d95458abf9ace893d7bd94422156be84")
ne104<-rename(e104,usbg=`大學-女/男`)
ne104$usbg<-gsub("—|…","",ne104$usbg)
ne104$usbg<-as.numeric(ne104$usbg)

a104<- filter(ne104,usbg<100)
head(arrange(a104,(usbg))$"大職業別",10)
```

    ##  [1] "電力及燃氣供應業-技藝、機械設備操作及組裝人員"    
    ##  [2] "教育服務業-服務及銷售工作人員"                    
    ##  [3] "礦業及土石採取業-技術員及助理專業人員"            
    ##  [4] "礦業及土石採取業-技藝、機械設備操作及組裝人員"    
    ##  [5] "礦業及土石採取業"                                 
    ##  [6] "其他服務業-事務支援人員"                          
    ##  [7] "營造業-技藝、機械設備操作及組裝人員"              
    ##  [8] "用水供應及污染整治業-技藝、機械設備操作及組裝人員"
    ##  [9] "營造業"                                           
    ## [10] "教育服務業"

``` r
#105男生比女生多(差異由大至小)
e105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6647&md5_url=835639294a2da9d9d1adb436b450e1ca")
ne105<-rename(e105,usbg=`大學-女/男`)
ne105$usbg<-gsub("—|…","",ne105$usbg)
ne105$usbg<-as.numeric(ne105$usbg)

a105<- filter(ne105,usbg<100)
head(arrange(a105,(usbg))$"大職業別",10)
```

    ##  [1] "不動產業-技藝、機械設備操作及組裝人員"        
    ##  [2] "醫療保健服務業-專業人員"                      
    ##  [3] "用水供應及污染整治業-事務支援人員"            
    ##  [4] "營造業-事務支援人員"                          
    ##  [5] "不動產業-事務支援人員"                        
    ##  [6] "營造業"                                       
    ##  [7] "營造業-專業人員"                              
    ##  [8] "資訊及通訊傳播業-技藝、機械設備操作及組裝人員"
    ##  [9] "不動產業-服務及銷售工作人員"                  
    ## [10] "其他服務業"

文字說明:

103-105年度男生薪水比女生高的各個前十名，以礦業及土石採取業以及營造業系列在三年之中幾乎是男女薪資比率當中相差很大的產業，另外，在職業類別當中，"技藝、機械設備操作及組裝人員"在前十名當中佔了較多數目。

### 哪些行業女生薪資比男生薪資多?

``` r
#這是R Code Chunk
#103女生比男生多(差異由大至小)
a103<- filter(ne103,usbg>100)
head(arrange(a103,(usbg))$"大職業別",10)
```

    ## character(0)

``` r
#104女生比男生多(差異由大至小)
a104<- filter(ne104,usbg>100)
head(arrange(a104,(usbg))$"大職業別",10)
```

    ## [1] "專業、科學及技術服務業-技藝、機械設備操作及組裝人員"

``` r
#105女生比男生多(差異由大至小)
a105<- filter(ne105,usbg>100)
head(arrange(a105,(usbg))$"大職業別",10)
```

    ## [1] "金融及保險業-專業人員"

文字說明:

103-105年度女生薪資比男生高的產業少了很多，只有在金融類跟專業類分別有在104以及105年度較高，大部分的工作僅僅持平或甚至更少。

研究所薪資差異
--------------

以105年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#這是R Code Chunk


ne105<-rename(e105,usy=`大學-薪資`)
ne105$usy<-gsub("—","",ne105$usy)
ne105$usy<-as.numeric(ne105$usy)

ne105<-rename(ne105,usgu=`研究所及以上-薪資`)
ne105$usgu<-gsub("—","",ne105$usgu)
ne105$usgu<-as.numeric(ne105$usgu)

ne105<-mutate(ne105,sarate=ne105$usgu/ne105$usy) 

head(arrange(ne105,desc(sarate))$"大職業別",10)
```

    ##  [1] "專業、科學及技術服務業-專業人員"    
    ##  [2] "專業、科學及技術服務業"             
    ##  [3] "教育服務業-技術員及助理專業人員"    
    ##  [4] "資訊及通訊傳播業-專業人員"          
    ##  [5] "其他服務業-專業人員"                
    ##  [6] "資訊及通訊傳播業"                   
    ##  [7] "專業、科學及技術服務業-事務支援人員"
    ##  [8] "製造業"                             
    ##  [9] "工業部門"                           
    ## [10] "工業及服務業部門"

文字說明:

105年度的資料當中，"專業、科學及技術服務業"(3)在前十名當中占了最多，"資訊及通訊傳播業"(2)的數量則居第二位，在前十名的資料中，類別是"專業人員"的排名較前面，也較多。

我有興趣的職業別薪資狀況分析
----------------------------

### 有興趣的職業別篩選，呈現薪資

``` r
#這是R Code Chunk
ne103<-rename(e103,usx=`大學-薪資`)
ne103$usx<-gsub("—","",ne103$usx)
ne103$usx<-as.numeric(ne103$usx)
ne103$"大職業別"<-c(ne103$"大職業別")


ne103<-rename(ne103,usgu=`研究所及以上-薪資`)
ne103$usgu<-gsub("—","",ne103$usgu)
ne103$usgu<-as.numeric(ne103$usgu)
ne103$"大職業別"<-c(ne103$"大職業別")

ne104<-rename(e104,usx=`大學-薪資`)
ne104$usx<-gsub("—","",ne104$usx)
ne104$usx<-as.numeric(ne104$usx)
ne104$"大職業別"<-c(ne104$"大職業別")


ne104<-rename(ne104,usgu=`研究所及以上-薪資`)
ne104$usgu<-gsub("—","",ne104$usgu)
ne104$usgu<-as.numeric(ne104$usgu)
ne104$"大職業別"<-c(ne104$"大職業別")

ne105<-rename(e105,usx=`大學-薪資`)
ne105$usx<-gsub("—","",ne105$usx)
ne105$usx<-as.numeric(ne105$usx)
ne105$"大職業別"<-c(ne105$"大職業別")


ne105<-rename(ne105,usgu=`研究所及以上-薪資`)
ne105$usgu<-gsub("—","",ne105$usgu)
ne105$usgu<-as.numeric(ne105$usgu)
ne105$"大職業別"<-c(ne105$"大職業別")
```

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
#這是R Code Chunk
tech<-data.frame(job=ne103$"大職業別"[grep("資訊",ne103$"大職業別")],
                 salary103us=ne103$usx[grep("資訊",ne103$"大職業別")],
                 salary104us=ne104$usx[grep("資訊",ne104$"大職業別")],
                 salary105us=ne105$usx[grep("資訊",ne105$"大職業別")],
                 salary103gu=ne103$usgu[grep("資訊",ne103$"大職業別")],
                 salary104gu=ne104$usgu[grep("資訊",ne104$"大職業別")],
                 salary105gu=ne105$usgu[grep("資訊",ne105$"大職業別")])

fin<-data.frame(job=ne103$"大職業別"[grep("金融",ne103$"大職業別")],
                 salary103us=ne103$usx[grep("金融",ne103$"大職業別")],
                 salary104us=ne104$usx[grep("金融",ne104$"大職業別")],
                 salary105us=ne105$usx[grep("金融",ne105$"大職業別")],
                 salary103gu=ne103$usgu[grep("金融",ne103$"大職業別")],
                 salary104gu=ne104$usgu[grep("金融",ne104$"大職業別")],
                 salary105gu=ne105$usgu[grep("金融",ne105$"大職業別")])


knitr::kable(tech)
```

| job                                           |  salary103us|  salary104us|  salary105us|  salary103gu|  salary104gu|  salary105gu|
|:----------------------------------------------|------------:|------------:|------------:|------------:|------------:|------------:|
| 資訊及通訊傳播業                              |        27055|        27478|        28634|        31762|        32135|        34286|
| 資訊及通訊傳播業-專業人員                     |        28839|        29352|        30815|        33519|        34274|        37180|
| 資訊及通訊傳播業-技術員及助理專業人員         |        27288|        27470|        28466|        31479|        31164|        32866|
| 資訊及通訊傳播業-事務支援人員                 |        25276|        25728|        26619|        29076|        29764|        30457|
| 資訊及通訊傳播業-服務及銷售工作人員           |        25995|        26210|        27173|           NA|           NA|           NA|
| 資訊及通訊傳播業-技藝、機械設備操作及組裝人員 |        26013|        26812|        27702|           NA|           NA|           NA|
| 資訊及通訊傳播業-基層技術工及勞力工           |           NA|           NA|           NA|           NA|           NA|           NA|

``` r
knitr::kable(fin)
```

| job                                       |  salary103us|  salary104us|  salary105us|  salary103gu|  salary104gu|  salary105gu|
|:------------------------------------------|------------:|------------:|------------:|------------:|------------:|------------:|
| 金融及保險業                              |        30577|        30787|        31059|        35717|        35934|        35921|
| 金融及保險業-專業人員                     |        33642|        33706|        33468|        38123|        38425|        37895|
| 金融及保險業-技術員及助理專業人員         |        30564|        30495|        30740|        35119|        35011|        35025|
| 金融及保險業-事務支援人員                 |        29070|        29447|        29945|        33743|        34185|        34646|
| 金融及保險業-服務及銷售工作人員           |        28074|        28213|        29236|           NA|           NA|           NA|
| 金融及保險業-技藝、機械設備操作及組裝人員 |        30664|        30074|        29369|           NA|           NA|           NA|
| 金融及保險業-基層技術工及勞力工           |           NA|           NA|           NA|           NA|           NA|           NA|
| 文字說明:                                 |             |             |             |             |             |             |

在103-105年度的資料中，以有興趣/相關科系的關鍵字進行搜尋，"資訊"相關的產業，從103年度至105年度，研究所與大學薪資薪資差距依序大約從4000→6000，大學畢業專業人員的薪水相較在"資訊"搜尋出的產業又多了大約1500至2000左右。

以"金融"做搜尋，103-105年度的資料中，相關的產業起薪幾乎相比資訊相關產業高一些("專業人員"的薪水排名在103-105年度皆在前排)，研究所與大學的薪資差異103-105年度幾乎持平，大約5000左右。

雖然無法推斷相關科系的的產業薪水會長或是跌，但基本上還是會念相關科系的研究所。
