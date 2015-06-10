# setwd("~/Google Drive/R working/twitteR/")
# setwd("C:/Users/Ben/Google Drive/R working/twitteR/")
# setwd("C:/Users/Harry Samsung Laptop/Downloads/ben.liu's work/R working/twitteR/")

# devtools::install_github("jennybc/googlesheets")
require("googlesheets")
suppressMessages(library("dplyr"))
require("twitteR")
suppressMessages(library("RCurl"))
suppressMessages(library("plyr"))

# list all googlesheet (require authentication)
my_sheets <- list_sheets()
# re-authorise user
# authorize(new_user = TRUE)


# read survey sheet
survey=register_ss("Marvelous Europe - Language Survey (Responses)")

# extract certain worksheet
raw <- get_via_lf(survey, ws = "Form responses 1")

# twitter account info
raw=raw[,-c(2:3,11,14)]
names(raw)=c("timestamp","japan_game_fan","purchased_game","lang_choice","buy_if_primary_lang","dub_or_sub","dual_audio","twitter_id","country","language")

###### clean up

# get rid of @
raw$twitter_id=gsub("^@","",raw$twitter_id)

# if provided an url
raw$twitter_id=gsub("https://twitter.com/","",raw$twitter_id)
raw$twitter_id=gsub("/statuses/[0-9]*","",raw$twitter_id)
raw$twitter_id=gsub("/status/[0-9]*","",raw$twitter_id)

# if input anything before @
raw$twitter_id=gsub("[A-z0-9]* @","",raw$twitter_id)

# random comments
raw$twitter_id=gsub("Yes|Thanks","",raw$twitter_id)

# manually handle email inputs
email=raw[grep('\\S+@\\S+',raw$twitter_id),]
#write.csv(email,"email input extracts.csv",row.names=F)

#####

# extract lottery pool
pool=raw[!is.na(raw$twitter_id) & !(raw$twitter_id %in% ""),]

# select user login credentials
user="MEU"

# load twitter oauth
login=read.csv("twitterapi.csv",colClasses = rep("character",3))
consumer_key = login$value[login$user %in% user & login$item %in% 'consumer_key']
consumer_secret = login$value[login$user %in% user & login$item %in% 'consumer_secret']
access_token = login$value[login$user %in% user & login$item %in% 'access_token']
access_secret = login$value[login$user %in% user & login$item %in% 'access_secret']
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# get relation of voters
relation=NULL
for (i in 1:ceiling(nrow(pool)/100)){
  df=friendships(screen_names=pool$twitter_id[((i-1)*100+1):(i*100)])
  relation=rbind(relation,df)
}
# generate url link to their twitter
relation$url=paste0("https://twitter.com/",relation$screen_name)

relation=relation[!(relation$screen_name %in% "NA"),]

## put result back into googlesheet
# add new worksheet with sheet name
# survey=add_ws(survey,ws_title="email_input")
# survey=delete_ws(survey,ws="twitter_relation")

# copy in relation dataframe
survey <- survey %>% edit_cells(ws="twitter_relation",relation[,-1],trim=T,header=T)
# email inputs for lottery draw
survey <- survey %>% edit_cells(ws="email_input",email,trim=T,header=T)

