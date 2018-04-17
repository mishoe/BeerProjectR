#categorical encoder
library(Matrix)
categoryEncoder<-function(data_input,cat_col){
  lst.temp<-NULL
  for(n in cat_col){
data_temp<-data_input[,n]

cat_un_temp<-unique(data_temp)

cat_mat_temp<-Matrix(0,nrow=length(cat_un_temp),ncol = length(data_temp),sparse=TRUE)
row.names(cat_mat_temp)<-cat_un_temp
colnames(cat_mat_temp)<-paste(colnames(data_input)[n],seq(1:length(data_temp)))


for(i in 1:length(data_temp)){
  ind<-which(cat_un_temp==data_temp[i])
  cat_mat_temp[ind,i]<-1
}
cat_mat_temp<-t(cat_mat_temp)
lst.temp<-c(lst.temp,list(name1=cat_mat_temp))
names(lst.temp)[length(lst.temp)]<-paste(colnames(data_input)[n])

}
return(lst.temp)

}

#B<-categoryEncoder(All_Recipes_clean,cat_col = c(4,6))
