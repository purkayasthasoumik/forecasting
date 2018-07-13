############# LIBRARY #############
require(outliers)
require(plotly)

############# FIND BAD STORES #############
    ##### QUARTER 1 #####
analyseBad.q1<-function()
{
  
  #ONLY EASTER WEEK
  badStores.q1<-list()
  
  for(j in 10){
    badStores.q1<-c(badStores.q1, 
                    data.frame(q1$`Store IDs`[summary.q1$bc.mat[,j]>=quantile(summary.q1$bc.mat[,j], 0.865 )]))
  }
  names(badStores.q1)<-c(10)
  return(badStores.q1)
}
badStores.q1<-analyseBad.q1()

    ##### QUARTER 2:  #####
analyseBad.q2<-function()
{
  
  #ST. PATRICKS DAY, BUT NO BIGGIE.
  
  badStores.q2<-list()
  
  for(j in 14:26){
    
    if(j!=16) next
    
    badStores.q2<-c(badStores.q2, 
                    data.frame(q2$`Store IDs`[summary.q2$tc.mat[,j-13]>=quantile(summary.q2$tc.mat[,j-13], 0.9)]))
  }
  
  names(badStores.q2)<-c(16)
  return(badStores.q2)
}
badStores.q2<-analyseBad.q2()

    ##### QUARTER 3:  #####
analyseBad.q3<-function()
{
  
  badStores.q3<-list()
  
  for(j in 27:39){
    
    if(j!=33) next
    
    badStores.q3<-c(badStores.q3, 
                    data.frame( q3$`Store IDs`[which(summary.q3$bc.mat[,7]>=quantile(summary.q3$bc.mat[,7], 0.90 , na.rm=T))]))
  }
  
  names(badStores.q3)<-c(33)
  return(badStores.q3)
}
badStores.q3<-analyseBad.q3()
extra<-c(426, 537, 608, 1742, 2040, 3564, 3802) #stores affected by hurricanes but not reported.
badStores.q3$`33`<-badStores.q3$`33`[!badStores.q3$`33` %in% extra]

    ##### QUARTER 4:  #####
analyseBad.q4<-function()
{
  require(outliers)
  
  
  # thanksgiving and christmas
  
  badStores.q4<-list()
  
  for(j in c(43, 48)){
    
    
    if(j==43){badStores.q4<-c(badStores.q4, 
                              data.frame( q4$`Store IDs`[which(summary.q4$tc.mat[,j-39]>=
                                                                 quantile(summary.q4$tc.mat[,j-39], 0.76 ,
                                                                          na.rm=T))]))}else{
              badStores.q4<-c(badStores.q4, 
              data.frame( q4$`Store IDs`[which(summary.q4$tc.mat[,j-39]>=
              quantile(summary.q4$tc.mat[,j-39], 0.90 ,
              na.rm=T))]))                                                             
                                                                          }
    
    
  }
  
  names(badStores.q4)<-c(43, 48)
  return(badStores.q4)
}
badStores.q4<-analyseBad.q4()
