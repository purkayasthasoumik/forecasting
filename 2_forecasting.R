############# LIBRARY #############
require(bsts)
require(forecast)
require(lubridate)

############# WM DATE CONVERT #############
dateConvert<-function(choice=1, date)
{
  #1 if wm to grg, 2 if grg to wm
  if(choice==1){
    return(as.Date(week$wm_week_begin_date[week$wm_yr_wk_id==date]))
  }else{
    return(week$wm_yr_wk_id[as.Date(week$wm_week_begin_date)==as.Date(date)])
  }
}

############# FORECASTING #############

    ##### QUARTER 1: BSTS/TBATS + WM/CUSTOM #####
forecastingFinal.q1 <- function()
{
  
  st.id.q1<-numeric()
  observed.q1<-numeric()
  fc.bc.q1<-numeric() #forecast bsts custom q1
  fc.bw.q1<-numeric() #forecast walmart custom q1
  fc.tc.q1<-numeric() #forecast tbats custom q1
  fc.tw.q1<-numeric() #forecast tbats walmart q1
  
  
  for(i in store.id)
  {
    #if less than (training>=10 + 4) (test>=14 + 4) weeks of data, skip store
    if(length(getWeeksAndSales(i))<32) next
    
    #get data
    data<-getWeeksAndSales(i)
    
    #if training is less than 14 weeks (10 for training + 4 for burn), skip store
    if(length(window(data, end=as.Date("2017-12-23")))<14) next
    training<-window(data, start=time(data)[5], end=as.Date("2017-12-23"))
    
    #if test set is not 16 weeks, skip store
    if(length(window(data, start=as.Date("2017-12-30"), end=as.Date("2018-04-21")))<17) next
    test<-window(data, start=as.Date("2017-12-30"), end=as.Date("2018-04-21"))
    
    #no hurricane weeks in this zone, so we'll skip this part
    # #preparing test set for hurricanes, 
    # if(i %in% unique(hurr.info$store_nbr)){
    #   
    #   null.start<-min(filter(hurr.info, store_nbr==i)$start_week)-11736
    #   null.end<-max(filter(hurr.info, store_nbr==i)$end_week)-11736
    #   
    #   test[null.start:null.end]<-NA
    # }
    
    #if training and test conditions are met, include store id in the data set
    st.id.q1<-c(st.id.q1, i)
    
    #observation matrix
    observed.q1<-rbind(observed.q1, as.numeric(test))
    
    #use of multiplicative model
    training<-as.numeric(log(training))
    
    #seasonal indices for this period
    seas.custom<-c(seas[49:52], seas[1:13])
    seas.walmart<-c(seas.wm[49:52], seas.wm[1:13])
    
    #bsts trend
    bsts.comp <- AddSemilocalLinearTrend(list(), training)
    bsts.model <- bsts(training, state.specification = bsts.comp, niter = 500, ping=0, seed= round(as.numeric(Sys.time())%%100))
    burn <- SuggestBurn(0.1, bsts.model)
    trend.bsts<- exp(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
    
    fc.bc.q1<-rbind(fc.bc.q1, trend.bsts*seas.custom) #filling in bsts+custom fc
    fc.bw.q1<-rbind(fc.bw.q1, trend.bsts*seas.walmart) #filling in bsts+wm fc
    
    #tbats trend
    tbats.model<-tbats(training, use.trend = T,  use.damped.trend = T, use.box.cox = FALSE)
    trend.tbats<-exp(as.numeric(forecast(tbats.model, h=length(test))$mean))
    
    fc.tc.q1<-rbind(fc.tc.q1, trend.tbats*seas.custom) #filling in tbats+custom fc
    fc.tw.q1<-rbind(fc.tw.q1, trend.tbats*seas.walmart) #filling in tbats+wm fc
  }
  output<-list(st.id.q1, observed.q1[,c(5:17)], fc.bc.q1[,c(5:17)], fc.bw.q1[,c(5:17)], fc.tc.q1[,c(5:17)], fc.tw.q1[,c(5:17)])
  names(output)<-c("Store IDs", "Observed", "BC", "BW", "TC", "TW")
  return (output)
}
q1<-forecastingFinal.q1()

    ##### QUARTER 2: BSTS/TBATS + WM/CUSTOM #####
forecastingFinal.q2 <- function()
{
  

  st.id.q1<-numeric()
  observed.q1<-numeric()
  fc.bc.q1<-numeric() #forecast bsts custom q1
  fc.bw.q1<-numeric() #forecast walmart custom q1
  fc.tc.q1<-numeric() #forecast tbats custom q1
  fc.tw.q1<-numeric() #forecast tbats walmart q1
  
  
  for(i in store.id)
  {
    #if less than (training>=10 + 4) (test>=14 + 4) weeks of data, skip store
    if(length(getWeeksAndSales(i))<32) next
    
    #get data
    data<-getWeeksAndSales(i)
    
    #if training is less than 14 weeks (10 for training + 4 for burn), skip store
    if(length(window(data, end=as.Date("2017-03-25")))<14) next
    training<-window(data, start=time(data)[5], end=as.Date("2017-03-25"))
    
    #if test set is not 16 weeks, skip store
    if(length(window(data, start=as.Date("2017-04-01"), end=as.Date("2017-07-22")))<17) next
    test<-window(data, start=as.Date("2017-04-01"), end=as.Date("2017-07-22"))
    
    #no hurricane weeks in this zone, so we'll skip this part
    # #preparing test set for hurricanes, 
    # if(i %in% unique(hurr.info$store_nbr)){
    #   
    #   null.start<-min(filter(hurr.info, store_nbr==i)$start_week)-11736
    #   null.end<-max(filter(hurr.info, store_nbr==i)$end_week)-11736
    #   
    #   test[null.start:null.end]<-NA
    # }
    
    #if training and test conditions are met, include store id in the data set
    st.id.q1<-c(st.id.q1, i)
    
    #observation matrix
    observed.q1<-rbind(observed.q1, as.numeric(test))
    
    #use of multiplicative model
    training<-as.numeric(log(training))
    
    #seasonal indices for this period
    seas.custom<-c(seas[10:26])
    seas.walmart<-c(seas.wm[10:26])
    
    #bsts trend
    bsts.comp <- AddSemilocalLinearTrend(list(), training)
    bsts.model <- bsts(training, state.specification = bsts.comp, niter = 500, ping=0, seed= round(as.numeric(Sys.time())%%100))
    burn <- SuggestBurn(0.1, bsts.model)
    trend.bsts<- exp(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
    
    fc.bc.q1<-rbind(fc.bc.q1, trend.bsts*seas.custom) #filling in bsts+custom fc
    fc.bw.q1<-rbind(fc.bw.q1, trend.bsts*seas.walmart) #filling in bsts+wm fc
    
    #tbats trend
    tbats.model<-tbats(training, use.trend = T,  use.damped.trend = T, use.box.cox = FALSE)
    trend.tbats<-exp(as.numeric(forecast(tbats.model, h=length(test))$mean))
    
    fc.tc.q1<-rbind(fc.tc.q1, trend.tbats*seas.custom) #filling in tbats+custom fc
    fc.tw.q1<-rbind(fc.tw.q1, trend.tbats*seas.walmart) #filling in tbats+wm fc
  }
  output<-list(st.id.q1, observed.q1[,c(5:17)], fc.bc.q1[,c(5:17)], fc.bw.q1[,c(5:17)], fc.tc.q1[,c(5:17)], fc.tw.q1[,c(5:17)])
  names(output)<-c("Store IDs", "Observed", "BC", "BW", "TC", "TW")
  return (output)
}
q2<-forecastingFinal.q2()

    ##### QUARTER 3: BSTS/TBATS + WM/CUSTOM #####
forecastingFinal.q3 <- function()
{
  
  require(bsts)
  require(forecast)
  
  st.id.q1<-numeric()
  observed.q1<-numeric()
  fc.bc.q1<-numeric() #forecast bsts custom q1
  fc.bw.q1<-numeric() #forecast walmart custom q1
  fc.tc.q1<-numeric() #forecast tbats custom q1
  fc.tw.q1<-numeric() #forecast tbats walmart q1
  
  
  for(i in store.id)
  {
    #if less than (training>=10 + 4) (test>=14 + 4) weeks of data, skip store
    if(length(getWeeksAndSales(i))<32) next
    
    #get data
    data<-getWeeksAndSales(i)
    
    #if training is less than 14 weeks (10 for training + 4 for burn), skip store
    if(length(window(data, end=as.Date("2017-06-24")))<14) next
    training<-window(data, start=time(data)[5], end=as.Date("2017-06-24"))
    
    #if test set is not 16 weeks, skip store
    if(length(window(data, start=as.Date("2017-07-01"), end=as.Date("2017-10-21")))<17) next
    test<-window(data, start=as.Date("2017-07-01"), end=as.Date("2017-10-21"))
    
    #no hurricane weeks in this zone, so we'll skip this part
    #preparing test set for hurricanes,
    if(i %in% unique(hurricane$store_nbr)){

      null.start<-min(filter(hurricane, store_nbr==i)$start_week)-11722
      null.end<-max(filter(hurricane, store_nbr==i)$end_week)-11722

      test[null.start:null.end]<-NA
    }
    
    #if training and test conditions are met, include store id in the data set
    st.id.q1<-c(st.id.q1, i)
    
    #observation matrix
    observed.q1<-rbind(observed.q1, as.numeric(test))
    
    #use of multiplicative model
    training<-as.numeric(log(training))
    
    #seasonal indices for this period
    seas.custom<-c(seas[23:39])
    seas.walmart<-c(seas.wm[23:39])
    
    #bsts trend
    bsts.comp <- AddSemilocalLinearTrend(list(), training)
    bsts.model <- bsts(training, state.specification = bsts.comp, niter = 500, ping=0, seed= round(as.numeric(Sys.time())%%100))
    burn <- SuggestBurn(0.1, bsts.model)
    trend.bsts<- exp(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
    
    fc.bc.q1<-rbind(fc.bc.q1, trend.bsts*seas.custom) #filling in bsts+custom fc
    fc.bw.q1<-rbind(fc.bw.q1, trend.bsts*seas.walmart) #filling in bsts+wm fc
    
    #tbats trend
    tbats.model<-tbats(training, use.trend = T,  use.damped.trend = T, use.box.cox = FALSE)
    trend.tbats<-exp(as.numeric(forecast(tbats.model, h=length(test))$mean))
    
    fc.tc.q1<-rbind(fc.tc.q1, trend.tbats*seas.custom) #filling in tbats+custom fc
    fc.tw.q1<-rbind(fc.tw.q1, trend.tbats*seas.walmart) #filling in tbats+wm fc
  }
  output<-list(st.id.q1, observed.q1[,c(5:17)], fc.bc.q1[,c(5:17)], fc.bw.q1[,c(5:17)], fc.tc.q1[,c(5:17)], fc.tw.q1[,c(5:17)])
  names(output)<-c("Store IDs", "Observed", "BC", "BW", "TC", "TW")
  return (output)
}
q3<-forecastingFinal.q3()

    ##### QUARTER 4: BSTS/TBATS + WM/CUSTOM #####
forecastingFinal.q4 <- function()
{
  
  require(bsts)
  require(forecast)
  
  st.id.q1<-numeric()
  observed.q1<-numeric()
  fc.bc.q1<-numeric() #forecast bsts custom q1
  fc.bw.q1<-numeric() #forecast walmart custom q1
  fc.tc.q1<-numeric() #forecast tbats custom q1
  fc.tw.q1<-numeric() #forecast tbats walmart q1
  
  
  for(i in store.id)
  {
    #if less than (training>=10 + 4) (test>=14 + 4) weeks of data, skip store
    if(length(getWeeksAndSales(i))<32) next
    
    #get data
    data<-getWeeksAndSales(i)
    
    #if training is less than 14 weeks (10 for training + 4 for burn), skip store
    if(length(window(data, end=as.Date("2017-09-23")))<14) next
    training<-window(data, start=time(data)[5], end=as.Date("2017-09-23"))
    
    #if test set is not 16 weeks, skip store
    if(length(window(data, start=as.Date("2017-09-30"), end=as.Date("2018-01-20")))<17) next
    test<-window(data, start=as.Date("2017-09-30"), end=as.Date("2018-01-20"))
    
    # #no hurricane weeks in this zone, so we'll skip this part
    # #preparing test set for hurricanes,
    # if(i %in% unique(hurricane$store_nbr)){
    #   
    #   null.start<-min(filter(hurricane, store_nbr==i)$start_week)-11735
    #   null.end<-max(filter(hurricane, store_nbr==i)$end_week)-11735
    #   
    #   test[null.start:null.end]<-NA
    # }
    
    #if training and test conditions are met, include store id in the data set
    st.id.q1<-c(st.id.q1, i)
    
    #observation matrix
    observed.q1<-rbind(observed.q1, as.numeric(test))
    
    #use of multiplicative model
    training<-as.numeric(log(training))
    
    #seasonal indices for this period
    seas.custom<-c(seas[23:39])
    seas.walmart<-c(seas.wm[23:39])
    
    #bsts trend
    bsts.comp <- AddSemilocalLinearTrend(list(), training)
    bsts.model <- bsts(training, state.specification = bsts.comp, niter = 500, ping=0, seed= round(as.numeric(Sys.time())%%100))
    burn <- SuggestBurn(0.1, bsts.model)
    trend.bsts<- exp(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
    
    fc.bc.q1<-rbind(fc.bc.q1, trend.bsts*seas.custom) #filling in bsts+custom fc
    fc.bw.q1<-rbind(fc.bw.q1, trend.bsts*seas.walmart) #filling in bsts+wm fc
    
    #tbats trend
    tbats.model<-tbats(training, use.trend = T,  use.damped.trend = T, use.box.cox = FALSE)
    trend.tbats<-exp(as.numeric(forecast(tbats.model, h=length(test))$mean))
    
    fc.tc.q1<-rbind(fc.tc.q1, trend.tbats*seas.custom) #filling in tbats+custom fc
    fc.tw.q1<-rbind(fc.tw.q1, trend.tbats*seas.walmart) #filling in tbats+wm fc
  }
  output<-list(st.id.q1, observed.q1[,c(5:17)], fc.bc.q1[,c(5:17)], fc.bw.q1[,c(5:17)], fc.tc.q1[,c(5:17)], fc.tw.q1[,c(5:17)])
  names(output)<-c("Store IDs", "Observed", "BC", "BW", "TC", "TW")
  return (output)
}
q4<-forecastingFinal.q4()

