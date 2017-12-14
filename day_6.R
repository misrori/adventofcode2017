library(data.table)
#day_6
#####

my_load <- c(5,	1,	10,	0,	1,	7,	13,	14,	3,	12,	8,	10,	7,	12,	0,	6)


egyenlit <- function(my_load) {
  
  
  my_max_id<- which.max(my_load)
  my_max<- max(my_load)
  my_load[my_max_id]<-0
  my_max_id_t<- my_max_id+1
  for(novelni in 1:my_max){
    if(my_max_id_t>length(my_load)){
      my_max_id_t<- 1
    }
    my_load[my_max_id_t]<- my_load[my_max_id_t]+1
    my_max_id_t<- my_max_id_t +1
  }
  return(my_load)
}

my_list<- list()
szamlalo<- 1
while(TRUE){
  my_load <-egyenlit(my_load)
  my_list[[szamlalo]] <-  my_load 
  szamlalo<- szamlalo+1
  if(sum(duplicated(my_list))==1){
    print("kesz")
    break
  }
}
szamlalo-1

anyDuplicated(my_list, fromLast = T)


my_list[[5042]]
my_list[[3956]]
5042-3956