#a)
ShakespeareOriginal<-readLines("~/Shakespeare.txt")
#Now I need to clean up the
Shakespeare_tmp<-paste(ShakespeareOriginal, collapse = "\n")
#You need to unlist it here, or it will form a list, I just eliminated the redundant<<copyright...>> part.
#The following command helps to clean the data and eliminate some extra information that I don't
# need in my analysis such as "stage direction","<<Extra information about copyright>>".
Shakespeare_tmp1<-unlist(strsplit(gsub("<<.*?>>", "", Shakespeare_tmp), split = "\n"))
Shakespeare_tmp2<-gsub("^[[:blank:]]{2,}Enter","",Shakespeare_tmp1, ignore.case = 1)
Shakespeare_tmp3<-gsub("\\[.*?\\]", "", Shakespeare_tmp2, ignore.case = 1)
Shakespeare_tmp4<-gsub("^[[:blank:]]{2,}Re[-]*enter", "", Shakespeare_tmp3, ignore.case = 1)
Shakespeare_tmp5<-gsub("EPILOGUE.*","",Shakespeare_tmp4, ignore.case = 0)
Shakespeare<-gsub("Exeunt.*","",Shakespeare_tmp5, ignore.case = 0)
#I use the year as the start mark of the play
plays<-grep("^[[:digit:]]{4}$",Shakespeare)
#I use the 'THE END' as the end mark of the play
Ends<-grep("THE END",Shakespeare)
#We don't want to include the sonnet and the final sing.
infor<-data.frame("Starts" = plays[2:37],"Ends" = Ends[2:37])
#Here I save all of the play in a playlist, which is the list of lists,but I didn't use
#this format to deal with my following problems.
Playlist<-list("")
for(i in 1:36){
  playstarts<-infor[i,1]
  playends<-infor[i,2]
  Playlist[[i]]<-Shakespeare[playstarts:playends]
}

# b)
#These are the years of the play
years<-Shakespeare[infor[,1]]
#These are the names of the play, you can get the name of the play by assigning x
#as Playlist[[k]] where k is an integer from 1 to 36. We need to call the funcion Shakes
name<-function (x) {
  tmp<-grep("^([[:upper:]]+[[:punct:]]*[ \t]*[[:upper:]]*[ \t]*){1,}[[:upper:]]+$",x)
  tmp1<-grep("^by William Shakespeare",x)
  for (i in tmp){
    if(i < tmp1){ #Since I notice that name always come before "by William Shakespeare"
      return(x[i])
    }
  }
}
Name<-unlist(lapply(Playlist,name))
#Number of ACTS
Act<-function(x){
  act<-grep("^[ \t]*A[Cc][Tt][^[:alpha:]][ \t]*[[:punct:]]*[I|II|III|IV|V|VI]*[[:digit:]]*",x)
  Max<-max(act)
  if(substring(x[Max],5,7) == "III"){
    number<-substring(x[Max],5,7)
  }
  else{
    if(substring(x[Max],5,5) == 5){
      number<-substring(x[Max],5,5)}
    else{number<-substring(x[Max],5,6)}
  }
  return(number)
}
NumberAct<-unlist(lapply(Playlist,Act))

#Number of Scenes
Scene<-function(x){
  scene<-grep("scene[ _]+[a-z0-9]+[.]*$",x,ignore.case = TRUE)
  number<-length(scene)
  return(number)
}
NumberScene<-unlist(lapply(Playlist,Scene))

#The bodylist of all of the plays
Bl<-function(){
  Bodylist<-list("")
  for(i in 1:36){
  bodystarts<-grep("S[Cc][Ee][Nn][Ee][[:punct:]]",Playlist[[i]])
  bodyends<-grep("THE END",Playlist[[i]])
  Bodylist[[i]]<-Playlist[[i]][bodystarts:bodyends]
  }
  return(Bodylist)
}
Bodylist<-Bl()
#Metainfor:
Meta<-data.frame("Years"=years,Name,NumberAct,NumberScene)

# c)
#Dialogue Chunk: this is a function that is used to to split the play according to chunks
#x should be the number of the play, and I did not kick out the 4th play. Instead, I used 
#a if selection to deal with the 4th play.
Chunk<-function(x){
  if(x!=4){
  chunklist<-list("")
  chunk<-grep("(^[ \t]{1,2}[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)",Playlist[[x]])
  len<-length(grep("(^[ \t]{1,2}[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)",Playlist[[x]]))
  for(i in 1:as.numeric(len-1)){
    tmp1<-chunk[i]
    tmp2<-chunk[as.numeric(i+1)]
    chunklist[[i]]<-Playlist[[x]][tmp1:as.numeric(tmp2-1)]    
  }
  tmp3<-chunk[as.numeric(len)]
  tmp4<-grep("THE END",Playlist[[x]])
  chunklist[[as.numeric(len)]]<-Playlist[[x]][tmp3:tmp4]
  }
  else {
  chunklist<-list("")
  chunk<-grep("(^[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)",Playlist[[x]])
  len<-length(grep("(^[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)",Playlist[[x]]))
  for(i in 1:as.numeric(len-1)){
      tmp1<-chunk[i]
      tmp2<-chunk[as.numeric(i+1)]
      chunklist[[i]]<-Playlist[[x]][tmp1:as.numeric(tmp2-1)] 
  }
  tmp3<-chunk[as.numeric(len)]
  tmp4<-grep("THE END",Playlist[[x]])#I used the 'THE END' to set as the finish line
  chunklist[[as.numeric(len)]]<-Playlist[[x]][tmp3:tmp4]
  }
  return(chunklist)
}  
#Output the chunklist
Dialogue<-function(){
  Dialoguelist<-list("")
  for(i in 1:36){
    Dialoguelist[[i]]<-Chunk(i)
  }
  return(Dialoguelist)
}
Chunklist<-Dialogue()
# d)
#Unique Speakers
numberofspeakers<-function(){
  number<-vector("numeric",36)
  for(i in 1:36){
  if(i != 4){
    test<-grep("(^[ \t]{1,2}[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)",Playlist[[i]],value = 1)
    test1<-gsub("(^[ \t]{1,2}[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)","\\1",test)
    speakers<-length(unique(test1))#I have included something that we don't want
    number[i]<-speakers
  }
  else{
    test<-grep("(^[[:upper:]]+[ \t]*[[:upper:]]*[ \t]*[[:upper:]]+\\.)(.*)",Playlist[[i]],value = 1)
    test1<-gsub("(^[[:upper:]]+[ \t]*[[:upper:]]*[ \t]*[[:upper:]]+\\.)(.*)","\\1",test)
    speakers<-length(unique(test1))-1#I have included one redundant element that I noticed
    number[i]<-speakers
  }
}
  return(number)
}
Numberofspeakers<-numberofspeakers()

#The number of spoken chunks
numberofchunks<-function(){
  number<-vector("numeric",36)
  for(i in 1:36){
  if(i != 4){
    test<-grep("(^[ \t]{1,2}[[:upper:]]+[ \t]*[[:alpha:]]*[ \t]*[[:alpha:]]+\\.)(.*)",Playlist[[i]],value = 1)
    chunks<-length(unique(test))#I have included something that we don't want
    number[i]<-chunks
  }
  else{
    test<-grep("(^[[:upper:]]+[ \t]*[[:upper:]]*[ \t]*[[:upper:]]+\\.)(.*)",Playlist[[i]],value = 1)
    chunks<-length(test)-1#I have included one redundant element that I noticed
    number[i]<-chunks
  }
}
  return(number)
}

Numberofspeeches<-numberofchunks()
#Average sentences and words
Sentence<-function(x){
  s<-paste(x, collapse = "\n")
  tmp<-gregexpr("[[:lower:]][.?!]", s)
  Len<-length(unlist(tmp))
  return(Len)
}#The function is used to count the number of sentences

Word<-function(x){
  w<-gregexpr("([[:alpha:]]+[']*[[:alpha:]]*)",x)
  tmp<-as.numeric(length(unlist(w)))
  return(tmp)
}#The function is used to count the number of words
#TotalS generates a list of lists of the number of sentences for each chunk
TotalS<-function(){
  TSentences<-list("")
  for(j in 1:36){
  TSentences[j]<-list("")
  for(i in 1:length(Chunklist[[j]])){
    TSentences[[j]][[i]]<-Sentence(Chunklist[[j]][[i]])    
   }
  }
  return(TSentences)
}
TSentences<-TotalS()

#TotalW generates a list of lists of the number of words for each chunk
TotalW<-function(){
  TWords<-list("")
  for(j in 1:36){
    TWords[j]<-list("")
    for(i in 1:length(Chunklist[[j]])){
      TWords[[j]][[i]]<-Word(Chunklist[[j]][[i]])    
    }      
  }
  return(TWords)
}
TWords<-TotalW()

#The number of words per chunk is:
wperchunk<-function(){
  vec<-vector("numeric",36)
  vec1<-vector("numeric",36)
  for(i in 1:36){
    vec[i]<-mean(as.numeric(TWords[[i]]))
    vec1[i]<-sd(as.numeric(TWords[[i]]))
  }
  return(cbind(vec,vec1))
}
#The number of sentences per chunk is:
sperchunk<-function(){
  vec<-vector("numeric",36)
  vec1<-vector("numeric",36)
  for(i in 1:36){
    vec[i]<-mean(as.numeric(TSentences[[i]]))
    vec1[i]<-sd(as.numeric(TSentences[[i]]))
  }
  return(cbind(vec,vec1))
}
perchunk<-data.frame("Wordperspeech"=wperchunk()[,1],"Word(sd)"=wperchunk()[,2],"Sentenceperspeech"=sperchunk()[,1],"Sentence(sd)"=sperchunk()[,2])
summary<-cbind(Meta,Numberofspeakers,Numberofspeeches,perchunk)
summary
#d).5 The number of meaningful words
nonmeaning<-scan("http://www.textfixer.com/resources/common-english-words.txt","character",sep = ",")
Bodylistpaste<-paste(Bodylist)
CBodylist<-strsplit((unlist(unlist(CBodylist))),split = "[[:space:]]+")
#e)
plot(x = years, y = summary$Wordperspeech, type = "p", main = "Average Word")
plot(x = years, y = summary$Numberofspeeches, type = "p", main = "Average Number of Dialogue")
plot(x = years, y = summary$Sentenceperspeech, type = "p", main = "Average Sentence")
plot(x = years, y = summary$NumberScene, type = "", main = "Number of Scenes for Each Play")
plot(x = years, y = summary$Numberofspeakers, type = "p", main = "Number of speakers per play")