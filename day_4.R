adat<- fread('day_4.txt', sep='#', header = F)
names(adat) <- "sor"

# part a
######
szavak <- 0

for(i in 1:nrow(adat)){
  hosz<- length(strsplit(adat$sor[i], ' ')[[1]])
  uni <- length(unique(strsplit(adat$sor[i], ' ')[[1]]))
  if(hosz==uni){
    szavak <- szavak+1
  }
}
szavak


######


szoveg <- adat$sor[2]

my_test <- function(szoveg) {
  hosz<- length(strsplit(adat$sor[i], ' ')[[1]])
  uni <- length(unique(strsplit(adat$sor[i], ' ')[[1]]))
  if(hosz!=uni){
    return(FALSE)
  }else{
    
    elemek <- strsplit(szoveg, ' ')[[1]]
    betuk <- sapply(elemek, function(x){unname(unlist(paste(sort(strsplit(x,'')[[1]]), collapse = '')))})
    
    unname(betuk)
    hosz<- length(unname(betuk))
    uni <- length(unique(unname(betuk)))
    if(hosz!=uni){
      return(FALSE)
    }else{
      return(TRUE)
    }

  }
  
    
}

sum(sapply(adat$sor, my_test))


