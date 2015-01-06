#load required packages
require(ggplot2)
require(scales)
require(zoo)
require(lubridate)
require(plyr)
require(car)

#read in csv of forum posts
steve<-read.csv(file="/Users/k8lin/Dropbox/Dissertation/Forum posts/Steve/Steve_comments.csv",header=TRUE,sep=",")

#format date column as dates
steve$Date<-as.Date(steve$Date,"%d-%b-%y")

#create year/month column using zoo and populate with only the year-month data from dates column
steve$yeMo<-as.yearmon(steve$Date)

#change yemo to date class
steve$yeMo<-as.Date(steve$yeMo)

#create table for checking results of monthly counts in histogram
steveCount<-join(steve,ddply(steve,.(yeMo),'nrow'))

#recode forums - remember to find/replace all : in csv to - and all parens and quotes to blanks
steve$subForum<-recode(steve$Forum,"'Dialysis-Centers'='Dialysis'; 'Dialysis-General Discussion'='Dialysis'; 'Dialysis-Humor, Pictures, Stories and Poems'='Dialysis'; 'Dialysis-Medicare/Insurance'='Dialysis'; 'Dialysis-Spouses and Caregivers'='Dialysis'; 'Dialysis-Transplant Discussion'='Transplant'; 'Dialysis-Working while on Dialysis'='Dialysis'; 'IHD Chat Room'='Community'; 'Introduce Yourself'='Introductions'; 'Off-Topic-Talk about anything you want.'='Off-topic'; 'Other Severe Medical Conditions'='Other illnesses'; 'Dialysis-Diet and Recipes'='Dialysis'; 'Political Debates - Thick Skin Required for Entry'='Off-topic'; 'Premium Member Information'='Community'; 'Site Requests, Comments, Technical Help'='Community'; 'The IHD Family - Our Members'='Community'; 'Transplant Stories'='Transplant'; 'Dialysis-Home Dialysis'='Home dialysis'; 'Dialysis-Nocturnal'='Nocturnal dialysis'; 'Dialysis-Home Dialysis-NxStage Users'='Home dialysis'; 'Dialysis-Medical Breakthroughs'='News and breakthroughs'; 'Dialysis-News Articles'='News and breakthroughs'; 'Dialysis-Traveling Tips and Stories'='Dialysis'; 'Tribute for Patsy Aurora Bajanne Barker'='Tributes'; 'Dialysis-F.A.Q. Frequently Asked Questions'='Dialysis';'Dialysis-Pre-Dialysis'='Pre-dialysis';'Tribute For Bill Epoman Halcomb'='Tributes'; 'Tribute for Hanify'='Tributes'; 'Tribute for Karen547'='Tributes'; 'Tributes for Frank'='Tributes'; 'Tributes for Inara Michelle'='Tributes'; 'Tributes for Kevin Katonsdad'='Tributes'; 'Tribute for Ham OpBill'='Tributes'; 'The IHD Family-Our Members'='Community'; 'Dialysis-Easter Recipes'='Dialysis'; 'Dialysis-Workers'='Dialysis'; 'Dialysis-Home Dialysis-NXStage Users'='Home dialysis'; 'I Hate Dialysis GAMES'='Off-topic'; 'I Hate Dialysis ONLINE STORE'='Community'; 'Introduction-PLEASE READ THIS SECTION FIRST'='Introductions'; 'Las Vegas a Vision Come True.'='Community'; 'Political Debates-Thick Skin Required for Entry'='Off-topic'; 'Site Requests, Comments, Technical Help.'='Community'")

#creates histogram of monthly counts, with color bars for each sub-forum
stevePlot<-ggplot(data=steve,aes(yeMo,count,fill=steve$subForum))+stat_summary(fun.y=sum,geom="bar",position="stack")+scale_x_date(labels=date_format("%b-%Y"),breaks="3 months")+labs(x='Date',y='Number of posts',title="Steve")+scale_fill_discrete(name="Sub-forums")

#save stevePlot
ggsave(file="stevePlot.png",scale=2)

#creates density plot of sub-forum use over time
steveDensity<-qplot(steve$yeMo,..count..,data=steve,geom="density",fill=steve$subForum,position="fill")+scale_x_yearmon()

#adds titles for axes and legend in density plot
steveDensity<-steveDensity+labs(x='Date',y=NULL,title="Steve subforum densities")+scale_fill_discrete(name="Sub-forums")

