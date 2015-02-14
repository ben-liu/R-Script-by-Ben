# script written by @M_T_Patterson

#### initialize ####

## loading libraries:
library(tuneR)
library(RCurl)

morseref.url <- getURL("https://raw.githubusercontent.com/MarkTPatterson/Blog/master/Morse/morseref.csv",ssl.verifypeer = FALSE)
ref.df <- read.csv(text = morseref.url)

# helper function:
var_find = function(vec, t, s){
  var.out = var(vec[(t-s):(t+s)])
  return(var.out)}

## loading reference files (local)
sf.1 = readMP3(list.files(pattern="*.mp3"))

# defining the morse to text function:
m.to.text.func = function(sound.file){
  # read data into a dataframe
  df = data.frame(indx = 1:length(sound.file@left), vec = sound.file@left)
  # points to sample:
  sample.points = seq(from = 100, by = 100, to = length(df$vec))
  # applying the variance finder at the sampled points:
  tiny.df = data.frame(var = sapply(sample.points, 
                                    function(x){var_find(vec = df$vec,
                                                         t = x,
                                                         s = 50)}))
  # decide which points are 'on'
  tiny.df$on = as.numeric(tiny.df$var > 100000)
  tiny.df$indx = 1:nrow(tiny.df)
  
  # create a vector of changes in on:
  raw.vec = diff(tiny.df$on)
  
  # create indices for change instances -- these will be 1 and -1
  beep.start.vals = which(raw.vec == 1)
  beep.stop.vals = which(raw.vec == -1)
  
  # converting indices to durations:
  beep.durs = beep.stop.vals - beep.start.vals
  pause.durs = beep.start.vals[-1] - beep.stop.vals[-length(beep.stop.vals)]
  
  
  ## note: for some files, there seems to be a few 
  ## few beep durs that are only 1; for now, hard coding these out:
  
  
  beep.durs = beep.durs[beep.durs>1]
  pause.durs = pause.durs[pause.durs>1]
  
  
  
  ## recoding beep durs 
  
  ## note: this step needs to  take the beep.durs data and the pause.durs data
  ## and return duration barriers.  
  
  
  ## first, creating pause barriers:
  
  raw.tab = table(pause.durs)
  
  
  pause.centers.raw = kmeans(as.numeric(names(raw.tab[raw.tab > 5])),3)$centers[,1]
  
  pause.centers = pause.centers.raw[order(pause.centers.raw,decreasing = F)]
  pause.levels = as.vector(pause.centers)
  
  
  # determining separator values:
  pause.sep.1 = mean(pause.levels[1:2])
  pause.sep.2 = mean(pause.levels[2:3])
  
  
  ## similar exercise for beep.durs:
  raw.tab = table(beep.durs)
  beep.centers.raw = kmeans(as.numeric(names(raw.tab[raw.tab > 5])),2)$centers[,1]
  beep.centers = beep.centers.raw[order(beep.centers.raw,decreasing = F)]
  beep.levels = as.vector(beep.centers)
  
  beep.sep = mean(beep.levels[1:2])
  
  
  ## creating the letter and word end vectors:
  letter.ends = which(pause.durs > pause.sep.1)
  word.ends = which(as.numeric(pause.durs[pause.durs > pause.sep.1] > pause.sep.2) == 1)
  
  
  
  # recoding beep durations to long and short:
  beep.durs.let = beep.durs
  beep.durs.let[beep.durs.let > beep.sep] = "l"
  beep.durs.let[beep.durs.let < beep.sep] = "s"
  
  
  
  
  ## grouping the beep duration letters (l's and s's) into letters
  ## based on the letter ends vector
  empty.list = list()
  start.val = 1
  for(i in 1:length(letter.ends)){
    cur.points = beep.durs.let[start.val:letter.ends[i]]
    empty.list[[i]] = paste(cur.points,collapse = "")
    start.val = letter.ends[i] + 1  
  }
  
  letter.vec = unlist(lapply(empty.list, function(x){ref.df$letter[which(ref.df$code == x)]}))
  
  
  ## grouping letters into words based on word.ends vec:
  start.val = 1
  empty.list = list()
  for(i in 1:length(word.ends)){
    cur.points = letter.vec[start.val:word.ends[i]]
    empty.list[[i]] = paste(cur.points,collapse = "")
    start.val = word.ends[i] + 1  
  }
  
  
  ## saving as a new vector, with spacing:
  out = paste(unlist(empty.list),collapse = " ")
  
  
  return(out)
}

# examples:
m.to.text.func(sf.1)
