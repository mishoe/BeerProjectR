
library(Matrix)

termDocumentGenerator<-function(data_input,text_col,stop_list=NULL,word_freq=10,freq_inc=F){
  
tmp.list<-NULL
for(n in text_col){
txt_dat1<-data_input[,n]
#remove punctuation, numbers,extra spaces, capital letters


txt_dat1<-gsub("[[:punct:]]", "", txt_dat1)
txt_dat1<-gsub('[[:digit:]]+', '', txt_dat1)
txt_dat1<-gsub('  ',' ',txt_dat1)
txt_dat1<-tolower(txt_dat1)


#create the term by document matrix
#find the words first
words_temp<-NULL
for(i in 1:length(txt_dat1)){
  doc.tmp<-strsplit(txt_dat1[i],' ')
  for(j in 1:length(doc.tmp[[1]])){
    if(length(which(doc.tmp[[1]][j]==words_temp))==0){words_temp<-c(words_temp,doc.tmp[[1]][j])}
  }
}

#remove the non unique
words_temp<-unique(words_temp)

#remove words of length 1
ind1<-NULL
for(i in 1:length(words_temp)){
  if(is.na(nchar(words_temp[i]))){}
  else if(nchar(words_temp[i])>1){ind1<-c(ind1,i)}else{}
}
words_temp<-words_temp[ind1]

#remove the stop words
if(is.null(stop_list)==T){}else{
words_temp_inds<-NULL
for(i in 1:length(words_temp)){
  if(length(which(words_temp[i]==stoplist))==0){words_temp_inds<-c(words_temp_inds,i)}
}
words_temp<-words_temp[words_temp_inds]
}



#id the most common words
wordcount_tmp<-matrix(0,nrow=length(words_temp),ncol = 2)

wordcount_tmp[,1]<-words_temp

for(i in 1:length(txt_dat1)){
  doc.tmp<-strsplit(txt_dat1[i],' ')
  doc.tmp<-doc.tmp[[1]]
  for(j in 1:length(doc.tmp)){
    if(length(which(doc.tmp[j]==words_temp))>0){
      ind<-which(wordcount_tmp[,1] == as.character(doc.tmp[j]))
      wordcount_tmp[ind,2]<-as.numeric(wordcount_tmp[ind,2])+1
    }
  }
}

if(word_freq==0){}else{
wordcount_tmp<-wordcount_tmp[which(as.numeric(wordcount_tmp[,2])>word_freq),]
}
wordcount_tmp<-wordcount_tmp[order(as.numeric(wordcount_tmp[,2]),decreasing = T),]




tdm1<-Matrix(0,ncol=length(txt_dat1),nrow=length(wordcount_tmp[,1]),sparse = TRUE)
colnames(tdm1)<-paste(colnames(data_input)[text_col[1]],seq(1,length(txt_dat1)))
rownames(tdm1)<-wordcount_tmp[,1]

#make the full term by document matrix
for(i in 1:length(txt_dat1)){
  doc.tmp<-strsplit(txt_dat1[i],' ')
  doc.tmp<-doc.tmp[[1]]
  wrd_tmp<-unique(doc.tmp)
  for(j in 1:length(wrd_tmp)){
    if(length(which(wrd_tmp[j]==words_temp))>0){
    ind<-which(wordcount_tmp[,1] == as.character(wrd_tmp[j]))
    tdm1[ind,i]<-as.numeric(tdm1[ind,i])+1
    }
  }
}
tdm1<-t(tdm1)

if(freq_inc==T){
  tmp.list<-c(tmp.list,list(name1=tdm1,name2=wordcount_tmp))
  #assign(paste(colnames(data_input)[n],'_tdm'),tdm1)
  #assign(paste(colnames(data_input)[n],'_freq'),wordcount_tmp)
  names(tmp.list)[(length(tmp.list)-1):length(tmp.list)]<-c(paste(colnames(data_input)[n],'_tdm',sep = ''),paste(colnames(data_input)[n],'_freq',sep = ''))
  
}else{
  tmp.list<-c(tmp.list,list(name1=tdm1))
  names(tmp.list)[length(tmp.list)]<-c(paste(colnames(data_input)[n],'_tdm',sep = ''))
  
}
}
  return(tmp.list)
}

#A<-termDocumentGenerator(All_Recipes_clean,text_col = c(9,10),stop_list = stoplist)
