get_x_y <- function(my_number){
  reteg <- ifelse(ceiling(sqrt(my_number))%%2!=1,ceiling(sqrt(my_number))+1, ceiling(sqrt(my_number))) 
  
  alja <- seq( (reteg*reteg) - (reteg-1), (reteg*reteg))
  ball<- seq((min(alja)-(reteg-1)), min(alja)) 
  teto <-  seq( (min(ball)-(reteg-1)), min(ball)) 
  jobb<- c(seq(min(teto),min(teto)-(reteg-2)) , max(alja))
  
  
  
  if(my_number %in% teto){
    y<-(reteg-1)/2
    
    names(teto)<- seq((reteg-1)/2,-(reteg-1)/2 )
    x<- as.numeric(names(which(teto==my_number)))
    
  }else if(my_number %in% jobb){
    x<-(reteg-1)/2
    names(jobb)<- seq((reteg-1)/2,-(reteg-1)/2 )
    y<- as.numeric(names(which(jobb==my_number)))
    
  }else if(my_number %in% alja){
    y<- -(reteg-1)/2
    names(alja)<- seq(-(reteg-1)/2,(reteg-1)/2 )
    x <- as.numeric(names(which(alja==my_number)))
  }else if(my_number %in% ball){
    x<- -(reteg-1)/2
    names(ball)<- seq((reteg-1)/2,-(reteg-1)/2 )
    y<- as.numeric(names(which(ball==my_number)))
  }
  
  ertek <- 0
  return(list('x'=x,'y'=y, 'ertek'=0))
}

poziciok <- lapply(1:100,get_x_y)
poziciok[[1]]$ertek<- 1


for(i in c(2:100)){
  
  ipozx<-poziciok[[i]]$x
  ipozy<-poziciok[[i]]$y
  xposible <- c(ipozx + 1,ipozx -1) 
  x_lehet <- seq(min(xposible), max(xposible))
  yposible <- c(ipozy + 1,ipozy -1)
  y_lehet <- seq(min(yposible), max(yposible))
  
  ezekajok <- poziciok[sapply(poziciok, "[[", 'x')%in%x_lehet & sapply(poziciok, "[[", 'y')%in%y_lehet ]
  
  
  poziciok[[i]]$ertek<- sum(sapply(ezekajok, "[[", 'ertek'))
  
  
}

sapply(poziciok, "[[", 'ertek')>289326



