# this script is to parse and process passed affliates 
# in recent ACCA sitting on ACCA website


library(XML)
library(RCurl)


# fetch database

database=NULL

# from A to Z
for (i in 1:26){
  # from 0 to 2000
  for (n in 0:100){
    url = paste0("https://www.acca-business.org/pls/ecommerce/epfpl001.p_results?p_client_type=GRAD&p_letter="
                 ,LETTERS[i]
                 ,"&p_start="
                 ,20*n)
    # Download page using RCurl
    webpage <- getURL(url)
    # Process escape characters
    webpage <- readLines(tc <- textConnection(webpage)); close(tc)
    # extract relevant lines
    df=webpage[grepl("<TD width=\"20%\">",webpage)]
    # break if no content in that page
    if (length(df)==0) break
    
    # clean html tags
    df=gsub("<TD width=\"20%\">","",df)
    df=gsub("</TD>|</B>|<B>","",df)
    dat=as.data.frame(matrix(df,ncol=5,byrow=T,dimnames = list(NULL,df[1:5])))
    dat=dat[-1,]
    
    database=rbind(database,dat)
  }
}

names(database)= gsub(" |/",".",names(database))
# write.csv(database,"acca dec 2014 affiliate.csv",row.names=F)

database$Country=as.character(database$Country)

# passers by country

tb=sort(table(database$Country),decreasing=T)
tb=as.data.frame(tb)
tb$Country=row.names(tb)
names(tb)=c("No.of.Affiliates","Country")

# mark location on map (WIP)
library(rworldmap)
newmap <- getMap(resolution = "coarse")
plot(newmap)
