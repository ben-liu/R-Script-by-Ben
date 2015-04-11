
# setwd("~/SkyDrive/ACCA/ACCA UK guide")

# http://www.accaglobal.com/gb/en/technical-activities/technical-resources-search.r-geographicLocation--Europe__United_Kingdom.f-format--Text__Guide_to.pm-100.html


require(rvest)
require(qdap)

# function

txt=function(url,css){
  url %>%
    html() %>%
    html_nodes(css) %>%
    html_text()
}

href=function(url,css){
  url %>%
    html() %>%
    html_nodes(css) %>%
    html_attr("href")
}


# css selector
css_date="b"
css_title=".seeMore"
css_desc="p+ p"
css_dl1=".word a"
css_dl2=".pdf a"

# base url
base="http://www.accaglobal.com"

################
# first round
################

# date, title, description, url
Date=NULL
title=NULL
description=NULL
link=NULL
download=NULL

# should be 3 pages, 100 results per page
for (i in 1:3){
  url=paste0(
    "http://www.accaglobal.com/gb/en/technical-activities/technical-resources-search.r-geographicLocation--Europe__United_Kingdom.f-format--Text__Guide_to.pm-100.start-",
    (i-1)*100,
    ".html")
  
  Date=c(Date,txt(url,css_date))
  title=c(title,txt(url,css_title))
  description=c(description,txt(url,css_desc))
  link=c(link,href(url,css_title))
}

history=as.data.frame(matrix(c(Date,title,description,link),ncol=4))
names(history)=c("date","title","description","link")

for (i in 1:length(link)){
  dllink=href(paste0(base,link[i]),css_dl1)
  if(length(dllink)==0){dllink=href(paste0(base,link[i]),css_dl2)}
  if(length(dllink)==0){dllink=NA}
  history$download[i]=dllink
}

# clean titles
history$title=mgsub(c("ACCA guide to... ","ACCA Guide to... ","ACCA Guide to....","ACCA Guide to ",":","/"),c("","","",""," -","-"),history$title)

write.csv(history,"acca.guide.history.csv",row.names=F)
history$date=as.character(history$date)
# history=read.csv("acca.guide.history.csv",colClasses=c(rep("character",5)))


# download first round files
for (i in 1:nrow(history)){
  foo=history$download[i]
  if(!is.na(foo)){
    download.file(url=paste0(base,foo),
                  destfile = paste0(getwd(),"/",unlist(strsplit(history$date[i]," "))[3],"/",
                    history$title[i],".",
                    unlist(strsplit(foo,"[.]"))[2]
                  ))
    }
}


#####################
# subsequent rounds
#####################

# load record
history=read.csv("acca.guide.history.csv",colClasses=c(rep("character",5)))

# new listing
url = "http://www.accaglobal.com/gb/en/technical-activities/technical-resources-search.r-geographicLocation--Europe__United_Kingdom.f-format--Text__Text__Guide_to.pm-100.html"
new=as.data.frame(
  matrix(
    c(
      c(txt(url,css_date)),
      c(txt(url,css_title)),
      c(txt(url,css_desc)),
      c(href(url,css_title))),
    ncol=4))
names(new)=c("date","title","description","link")

for (i in 1:length(new)){
  dllink=href(paste0(base,new$link[i]),css_dl1)
  if(length(dllink)==0){dllink=href(paste0(base,new$link[i]),css_dl2)}
  if(length(dllink)==0){dllink=NA}
  new$download[i]=dllink
}


# count new factsheets
count=0
while (new$title[count+1]!=history$title[1]){count=count+1}

# download new articles
if (count>0){
for (i in 1:count){
  foo=new$download[i]
  if(!is.na(foo)){
    download.file(url=paste0(base,foo),
                  destfile = paste0(
                    new$title[i],".",
                    unlist(strsplit(foo,"[.]"))[2]
                  ))
  }
}}

