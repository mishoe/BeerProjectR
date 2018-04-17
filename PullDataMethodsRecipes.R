install.packages('XML')
install.packages('RCurl')
install.packages('XPath')
library(XML)
library(RCurl)

tab.rec.all<-NULL
for(j in 1:150){

txt.html<-readLines(paste('https://beerrecipes.org/Homebrew-Beer-Recipes/All-Recipes/Page-',j,sep = ''))

print(txt.html[1])
inds<-which(grepl('a href="/Re',txt.html)==T)


#html endings for websites
tab.rec.cur<-NULL
for(i in inds){
  #split string to find html
  split.link.1<-strsplit(txt.html[i],'"')
  link.h<-as.character(split.link.1[[1]][2])
  beer.n<-substr(as.character(split.link.1[[1]][5]),2,nchar(as.character(split.link.1[[1]][5]))-4)
  
  # find recipe type
  split.link.2<-strsplit(txt.html[i+1],'"')
  beer.t<-substr(as.character(split.link.2[[1]][3]),2,nchar(as.character(split.link.2[[1]][3]))-6)
  

  #recipe views
  split.link.3<-strsplit(txt.html[i+5],'"')
  beer.v<-substr(as.character(split.link.3[[1]][3]),2,nchar(as.character(split.link.3[[1]][3]))-5)
  
  #recipe rating
  split.link.4<-strsplit(txt.html[i+6],'"')
  beer.r<-substr(as.character(split.link.4[[1]][4]),15,nchar(as.character(split.link.4[[1]][4]))-4)
  
  
  tab.rec.cur<-rbind(tab.rec.cur,c(j,link.h,beer.n,beer.t,beer.v,beer.r))
  
  }
tab.rec.all<-rbind(tab.rec.all,tab.rec.cur)
}






#pull other information from each page







tab.rec.all.n<-NULL


for(n in 1:length(tab.rec.all[,1])){


#visit one page
txt.html<-readLines(paste('https://beerrecipes.org/',tab.rec.all[n,2],sep = ''))


#extract the beer style

ind<-which(grepl('<b>Beer Style:</b>',txt.html)==T)

split.st<-strsplit(txt.html[ind],';')
beer.st<-substr(as.character(split.st[[1]][1]),20,nchar(as.character(split.st[[1]][1]))-5)

#extract yield
split.y<-strsplit(txt.html[ind+2],'"')
beer.y<-substr(as.character(split.y[[1]][3]),2,nchar(as.character(split.y[[1]][3]))-11)


#extract the description
ind1<-which(grepl('<span itemprop="description">',txt.html)==T)
ind2<-which(grepl('<h4>Ingredients:</h4>',txt.html)==T)

if(length(ind1)>0 & length(ind2)>0){
beer.des<-NULL
for(h in seq(ind1,ind2)){
  beer.des<-paste(beer.des,txt.html[h])
}
beer.des<-gsub('<span itemprop=\"description\">','',beer.des)
beer.des<-gsub('</span> </p> <h4>Ingredients:</h4>','',beer.des)
beer.des<-gsub('<br/>','',beer.des)
}else{beer.des<-0}


inds.in<-which(grepl('<span itemprop="ingredients">',txt.html)==T)
if(length(inds.in)>0){
beer.ing<-NULL
for(k in 1:length(inds.in)){
  split.in<-strsplit(txt.html[inds.in[k]],'>')
  beer.ing<-paste(beer.ing,substr(as.character(split.in[[1]][2]),1,nchar(as.character(split.in[[1]][2]))-6))
}}else{beer.ing<-0}


ind1<-which(grepl('<div itemprop="recipeInstructions">',txt.html)==T)
ind2<-which(grepl('<b>Source:</b>',txt.html)==T)

if(length(ind1)>0 & length(ind2)>0){
beer.pro<-NULL
for(h in seq(ind1+1,ind2-1)){
  beer.pro<-paste(beer.pro,txt.html[h])
}

beer.pro<-gsub('</div>','',beer.pro)
beer.pro<-gsub('<p>','',beer.pro)
beer.pro<-gsub('</p>','',beer.pro)
}else{beer.pro<-0}


tab.rec.all.n<-rbind(tab.rec.all.n,c(tab.rec.all[n,],beer.y,beer.des,beer.ing,beer.pro,beer.st))

}
colnames(tab.rec.all.n)<-c('Page_#','URL_End','Name','Recipe_Type','Page_Views','Rating','Yield','Description','Ingredients','Procedure','Beer_Style')
write.csv(tab.rec.all.n,'All_Recipes.csv')
getwd(
)
