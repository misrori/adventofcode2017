library(data.table)
adat <- data.table(read.csv("tree.txt", sep = "\t", header = F, stringsAsFactors = F))
names(adat)<- "tree"
head(adat)

adat$node <- sapply(strsplit(sapply(strsplit(adat$tree, ' -> '), "[[", 1), ' (', fixed =T), '[[', 1) 

adat$suly <- gsub('\\)','',sapply(strsplit(sapply(strsplit(adat$tree, ' -> '), "[[", 1), ' (', fixed = T), "[[", 2))



my_claen <- function(sor){
  my_df_rows<-list()
  szamlalo<- 1
  if(grepl('->', sor)){
    tree_source<- strsplit(strsplit(sor, ' -> ')[[1]][1], ' (', fixed = T)[[1]][1]
    tree_targets<- strsplit(strsplit(sor, ' -> ')[[1]][2], ', ')[[1]]
    for (i in tree_targets){
      my_df_rows[[szamlalo]] <- list("my_source"= tree_source, "my_target" = i)
      szamlalo <- szamlalo+1
    }
    
    
  }else{
    my_df_rows[[szamlalo]] <- list("my_source"=strsplit(strsplit(sor, ' -> ')[[1]][1], ' (', fixed = T)[[1]][1] ,
                                   "my_target" = strsplit(strsplit(sor, ' -> ')[[1]][2], ', ')[[1]][1])
    
    
  }
  return(rbindlist(my_df_rows))
  
}
my_ddd <- list()
for(sor in 1:nrow(adat)){
  print(sor)
  my_ddd[[sor]]<- my_claen(adat$tree[sor])
}


ered <-data.table(rbindlist(my_ddd))
gyoker <- ered[my_source%in%my_target==F, ]$my_source[1]

#part_b
str(ered)
vegek<- unique(ered[is.na(my_target), ]$my_source)

get_fa<- function(level){
  faag<- level
  my_temp <- level
  while(TRUE){
    my_temp <-ered[my_target==my_temp]$my_source
    faag<- c(faag,my_temp)
    if(my_temp==gyoker){
      break()
    }
  }
  faag <- faag[length(faag): 1]
 
  a <- data.table(t(data.frame(faag)))
  return(a)
}


faagak <-sapply(vegek, get_fa) 

osszesfa <- rbindlist(faagak, fill = T)
setorder(osszesfa, V1, V2, V3, V4, V5, V6)

meg_hatra <- osszesfa[is.na(osszesfa$V5)==F,]$V5

my_list <- list()
hiba<- list()



while(TRUE){
  i<- meg_hatra[1]
  s <- ered[my_target==i]$my_source
  meg_hatra <- setdiff(meg_hatra, ered[my_source==s]$my_target)
  print((as.numeric(adat[node%in%ered[my_source==s]$my_target,]$suly)))
  source_szulyok <-(as.numeric(adat[node%in%ered[my_source==s]$my_target,]$suly))
  my_list[[s]] <- sum(as.numeric(adat[node%in%ered[my_source==s]$my_target,]$suly))
  if(length(unique(source_szulyok))>1){
    hiba[[i]]<- source_szulyok
  }
  if(length(meg_hatra)==0){
    break
  }
  
}


ered[my_target=="wlkatbq"]$my_source







for(i in meg_hatra){
  print(i)
  s <- ered[my_target==i]$my_source
  meg_hatra <- setdiff(meg_hatra, ered[my_source==s]$my_target)
  source_szulyok <-(as.numeric(adat[node%in%ered[my_source==s]$my_target,]$suly))
  my_list[[s]] <- sum(as.numeric(adat[node%in%ered[my_source==s]$my_target,]$suly))
  if(length(unique(source_szulyok))>1){
    hiba[[i]]<- source_szulyok
  }
  
}


ered[my_target=="texfyi"]$my_source
ered[my_source=="cxzuprb"]$my_target



while(TRUE){
  

    
}
#


#
























# 
# adat <- adat[,c(2,3)]
# i<- 3
# 
# 
# my_list <- list()
# 
# for(i in 1:nrow(osszesfa)){
# 
#   my_list[[i]]<-  data.frame(t(data.table(cumsum(merge(x=data.table(t(osszesfa[i,])), y = adat, by.x='V1', by.y='node', sort = F)$suly))))
# 
#   }
# 
# 
# fin <-rbindlist(my_list, fill = T)
# 
# 
# summary(factor(fin$X1))
# summary(factor(fin$X2))
# summary(factor(fin$X3))
# summary(factor(fin$X4))
# summary(factor(fin$X5))
# summary(factor(fin$X6))
# 

# setorder(fin, X1, X2, X3, X4, X5, X6)
# 
# fin[X2<X1,]

# 
# otos_szint <- osszesfa[,c('V5', 'V6'), with=F]
# elemek <- unique(otos_szint$V5)
# 
# get_sullyok <- function(e){
#   a<- list()
#   a[[e]]=sum(as.numeric(adat[node %in%c(otos_szint[V5==e]$V6, e),  ]$suly))
#   return(a)
#   
#   
# }
# 
# 
# sapply(elemek, get_sullyok)
# 
# 
# 
# 
# 
# 
# 
