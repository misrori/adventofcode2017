adat<- fread('day_5.txt', header = F, stringsAsFactors = F)
szamok  <- adat$V1


poziciom <- 1
szamlalo <- 0
while(TRUE){
  #print(poziciom)
  ittvagyok <- poziciom
  poziciom <- poziciom + szamok[poziciom]  
  if(szamok[ittvagyok]>=3){
    szamok[ittvagyok] <- szamok[ittvagyok] - 1
  }else if(szamok[ittvagyok]<3){
    szamok[ittvagyok] <- szamok[ittvagyok] + 1
  }
  
  szamlalo <- szamlalo+1
  if(poziciom>length(szamok)){
    print("vege")
    print(szamlalo)
    break
  }
  
}
