
# setwd("~/SkyDrive/ACCA/ACCA UK article")

# http://www.accaglobal.com/gb/en/technical-activities/technical-resources-search.r-geographicLocation--Europe__United_Kingdom.f-format--Text__Article.pm-100.html

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

page= function(url){
  url %>%
  html() %>%
  html_nodes("h1 , .articleContent") %>%
  html_text()
}

# css selector
css_date="b"
css_title=".seeMore"
css_desc="p+ p"

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

# should be 5 pages, 100 results per page
for (i in 1:5){
  url=paste0(
    "http://www.accaglobal.com/gb/en/technical-activities/technical-resources-search.r-geographicLocation--Europe__United_Kingdom.f-format--Text__Article.pm-100.start-",
  (i-1)*100,
  ".html")
  
  Date=c(Date,txt(url,css_date))
  title=c(title,txt(url,css_title))
  description=c(description,txt(url,css_desc))
  link=c(link,href(url,css_title))
}

history=as.data.frame(matrix(c(Date,title,description,link),ncol=4))
names(history)=c("date","title","description","link")

# clean titles
history$title=mgsub(c("/","?",":"),c(" or ",""," -"),history$title)

write.csv(history,"acca.articals.history.csv",row.names=F)

# download first round text files
for (i in 1:nrow(history)){
  content=page(paste0(base,history$link[i]))
  cat(content,history$link[i],file=paste0(history$title[i],"_",history$date[i],".txt"),sep="\n")
}

# c(353,432) buggy - no fix yet
for (i in 1:nrow(history)){
  content=page(paste0(base,history$link[i]))
  cat(content,history$link[i],file=paste0(history$title[i],"_",history$date[i],".txt"),sep="\n")
}

#####################
# subsequent rounds
#####################

# load record
history=read.csv("acca.articals.history.csv")

# new listing
url = "http://www.accaglobal.com/gb/en/technical-activities/technical-resources-search.r-geographicLocation--Europe__United_Kingdom.f-format--Text__Article.pm-100.html"
new=as.data.frame(
  matrix(
    c(
      c(txt(url,css_date)),
      c(txt(url,css_title)),
      c(txt(url,css_desc)),
      c(href(url,css_title))),
    ncol=4))
names(new)=c("date","title","description","link")

# count new articles
count=0
while (new$title[count+1]!=history$title[1]){count=count+1}

# download new articles
if (count>0){
  for (i in 1:count){
    content=page(paste0(base,new$link[i]))
    cat(content,new$link[i],file=paste0(new$title[i],"_",new$date[i],".txt"),sep="\n")
  }
}




