library(dplyr)
library(readr)
salary104EC <- read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")

salary107EC <- read_csv("C:/Program Files/R/R-3.6.3/X107EC.csv")

salary104EC$大職業別<-NULL
salary104EC$大職業別<-salary107EC$大職業別

join104107<-inner_join(salary107EC,salary104EC,by=c("大職業別"))

join104107$`大學-薪資.x`<-gsub("—|…","",join104107$`大學-薪資.x`)
join104107$`大學-薪資.y`<-gsub("—|…","",join104107$`大學-薪資.y`)
join104107$`大學-薪資.x`<-as.numeric(join104107$`大學-薪資.x`)
join104107$`大學-薪資.y`<-as.numeric(join104107$`大學-薪資.y`)
##Q1-1
salary_increase<-filter(join104107,join104107$`大學-薪資.x`>join104107$`大學-薪資.y`)
View(salary_increase)
##Q1-2
join104107$salary_increase_rate<-join104107$`大學-薪資.x`/join104107$`大學-薪資.y`
salary_increase_top10<-head(join104107[order(join104107$salary_increase_rate,decreasing = T),],10)
View(salary_increase_top10)

#Q1-3
salary_increase_5percent<-filter(join104107,salary_increase_rate>1.05)
View(salary_increase_5percent)  
salary_increase_5percent_kind<-table(sapply(strsplit(salary_increase_5percent$大職業別,"-"),"[",1))
View(salary_increase_5percent_kind)
#Q2-1
is.numeric(salary104EC$`大學-女/男`)
salary104EC
salary104EC$`大學-女/男`<-gsub("—|…","",salary104EC$`大學-女/男`)
salary104EC$`大學-女/男`<-as.numeric(salary104EC$`大學-女/男`)
is.numeric(salary107EC$`大學-女/男`)
salary107EC$`大學-女/男`<-gsub("—|…","",salary107EC$`大學-女/男`)
salary107EC$`大學-女/男`<-as.numeric(salary107EC$`大學-女/男`)
boy_higher104<-filter(salary104EC,salary104EC$`大學-女/男`<100)
boy_higher107<-filter(salary107EC,salary107EC$`大學-女/男`<100)
View(boy_higher104)
View(boy_higher107)
#Q2-2
boy_higher104_difference<-boy_higher104[order(boy_higher104$`大學-女/男`),]
View(boy_higher104_difference)
boy_higher107_difference<-boy_higher107[order(boy_higher107$`大學-女/男`),]
View(boy_higher107_difference)
boy_higher104_difference_top10<-head(boy_higher104_difference,10)
boy_higher107_difference_top10<-head(boy_higher107_difference,10)
View(boy_higher104_difference_top10)
View(boy_higher107_difference_top10)
#Q2-3
girl_higher104<-filter(salary104EC,salary104EC$`大學-女/男`>100)
girl_higher107<-filter(salary107EC,salary107EC$`大學-女/男`>100)
View(girl_higher104)
View(girl_higher107)
#Q2-4
girl_higher104_difference<-girl_higher104[order(girl_higher104$`大學-女/男`,decreasing = T),]
View(girl_higher104_difference)
girl_higher107_difference<-girl_higher107[order(girl_higher107$`大學-女/男`,decreasing = T),]
View(girl_higher107_difference)
girl_higher104_difference_top10<-head(girl_higher104_difference,10)
girl_higher107_difference_top10<-head(girl_higher107_difference,10)
View(girl_higher104_difference_top10)
View(girl_higher107_difference_top10)
#Q3

salary107EC$`研究所-薪資`<-gsub("—|…","",salary107EC$`研究所-薪資`)
salary107EC$`大學-薪資`<-gsub("—|…","",salary107EC$`大學-薪資`)
salary107EC$`研究所-薪資`<-as.numeric(salary107EC$`研究所-薪資`)
salary107EC$`大學-薪資`<-as.numeric(salary107EC$`大學-薪資`)
salary107EC$`研究所-薪資/大學-薪資`<-salary107EC$`研究所-薪資`/salary107EC$`大學-薪資`
s107_gra_higher<-filter(salary107EC,`研究所-薪資/大學-薪資`>1)
View(s107_gra_higher)
s107_gra_higher_top10<-head(s107_gra_higher[order(s107_gra_higher$`研究所-薪資/大學-薪資`,decreasing = T),],10)
View(s107_gra_higher_top10)


like<-salary107EC[grep("藝術_娛樂及休閒服務業-專業人員|教育業-專業人員|專業_科學及技術服務業-專業人員|出版、影音製作、傳播及資通訊服務業-專業人員|工業-專業人員",salary107EC$大職業別),]
selectlike<-
  select(like,大職業別,`大學-薪資`,`研究所-薪資`)
View(selectlike)
selectlike$"研究所薪資與大學薪資差"<-selectlike$`研究所-薪資`-selectlike$`大學-薪資`
View(selectlike)
