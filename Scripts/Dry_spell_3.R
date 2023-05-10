
CDD_function=function(prcp,t,Nb) {
  
  prcp[length(prcp)+1]=10
  prcp[is.na(prcp)]=0
  spell <- ifelse(prcp > 1, 0, 0)
  SPELL=0
  for(i in 1:length(prcp)){
    if(prcp[i]<=t){
      SPELL=SPELL+1
      spell[i]=SPELL
    }
    else{
      SPELL=0
    }
  }
  
aa=as.data.frame(spell)

All=aa
CDD<-All  

n=length(CDD$spell)-1
for (i in 2:n) {
  if(All$spell[i+1]==0 & All$spell[i-1]>0){
    #print(All$spell[i])
  }else{
    CDD$spell[i]=0}
}


   CDD=cbind(CDD,aa$spell)
   names(CDD)[2]="Spell2"
   CDD$spell[1]=0
   CDD$Count<-ifelse(CDD$spell>=Nb,1,0)
   
   # Plus long sequence seche

   # Le nombre des equences seche superieur a 10 jours
   S10=sum(CDD$Count)
   
   # # Le nombre moyen de jour de sequence seche

    return(S10)
}




CDD_function_Max=function(prcp,t,Nb) {
  
  prcp[length(prcp)+1]=10
  prcp[is.na(prcp)]=0
  spell <- ifelse(prcp > 1, 0, 0)
  SPELL=0
  for(i in 1:length(prcp)){
    if(prcp[i]<=t){
      SPELL=SPELL+1
      spell[i]=SPELL
    }
    else{
      SPELL=0
    }
  }
  
  aa=as.data.frame(spell)
  
  All=aa
  CDD<-All  
  
  n=length(CDD$spell)-1
  for (i in 2:n) {
    if(All$spell[i+1]==0 & All$spell[i-1]>0){
      #print(All$spell[i])
    }else{
      CDD$spell[i]=0}
  }
  
  
  CDD=cbind(CDD,aa$spell)
  names(CDD)[2]="Spell2"
  CDD$spell[1]=0
  CDD$Count<-ifelse(CDD$spell>=Nb,1,0)
  
  # Plus long sequence seche
  S=max(CDD$spell)
  return(S)
}



CDD_function_Mean=function(prcp,t,Nb) {
  
  prcp[length(prcp)+1]=10
  prcp[is.na(prcp)]=0
  spell <- ifelse(prcp > 1, 0, 0)
  SPELL=0
  for(i in 1:length(prcp)){
    if(prcp[i]<=t){
      SPELL=SPELL+1
      spell[i]=SPELL
    }
    else{
      SPELL=0
    }
  }
  
  aa=as.data.frame(spell)
  
  All=aa
  CDD<-All  
  
  n=length(CDD$spell)-1
  for (i in 2:n) {
    if(All$spell[i+1]==0 & All$spell[i-1]>0){
      #print(All$spell[i])
    }else{
      CDD$spell[i]=0}
  }
  
  
  CDD=cbind(CDD,aa$spell)
  names(CDD)[2]="Spell2"
  CDD$spell[1]=0
  CDD$Count<-ifelse(CDD$spell>=Nb,1,0)
  
  
  # # Le nombre moyen de jour de sequence seche
  NbMoyen=filter(CDD,spell!=0)
  mean_speel=mean(NbMoyen$spell)
  # return(t(as.data.frame(c(S10,mean_speel,S))))
  return(mean_speel)
}