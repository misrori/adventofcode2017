library(data.table)

#part a
adat <- read.table("day2_matrix.txt", sep = "\t", header = F)
osszeg <- 0
for (i in 1:nrow(adat)){
  osszeg <-osszeg + (max(adat[i,]) - min(adat[i,]))
  
}


#part b
adat <- read.table("day2_matrix.txt", sep = "\t", header = F)
osszeg <- 0

for (i in 1:nrow(adat)){
  sor <- unname(unlist(adat[i,]))
  for(sorelso in 1:length(sor)){
    for(sorfuto in 1:length(sor)){
      if(sor[sorelso]%%sor[sorfuto]==0 & sor[sorelso]/sor[sorfuto]!=1  ){
        osszeg <- osszeg +(sor[sorelso]/sor[sorfuto])
      }
    }
  }  
}
osszeg
