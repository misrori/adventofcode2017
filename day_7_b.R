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
  my_ddd[[sor]]<- my_claen(adat$tree[sor])
}

ered <-data.table(rbindlist(my_ddd))
gyoker <- ered[my_source%in%my_target==F, ]$my_source[1]

i <- 'bltmlm'
van_e_lejjebb<- function(i){
 i %in%ered[is.na(my_target)==F , ]$my_source  
}


get_sub_graph <- function(i){
  
  kezdet <-i 
  my_down_list <- i
  while(TRUE){
    
    kezdet<-ered[my_source%in%kezdet]$my_target
    my_down_list<- c(my_down_list, kezdet)
    if(sum(van_e_lejjebb(kezdet))==0){
      break
    }
    
  }
  er <-sum(as.numeric(adat[node%in%unique(my_down_list[is.na(my_down_list)==F])]$suly))
  return(er)
}
get_sub_graph(gyoker)




