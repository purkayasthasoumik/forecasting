############# LIBRARY #############
require(bsts)
require(forecast)
require(plotly)

############# DATASETS #############
stWk<-read.csv("/home/rstudio/datasets/store_wk_og_orders_2018_05_15_00_00_16.csv")
week<-read.csv("/home/rstudio/datasets/wm_week_dim.csv")
markRes<-read.csv("/home/rstudio/datasets/sotc_feb2018_snapshot.csv")
hurricane<-read.csv("/home/rstudio/datasets/og_hurricane_impacted_stores_open_close_weeks.csv")



############# SALES DATA FETCH #############
getWeeksAndSales<-function(st.nm)
{
  require(dplyr)
  require(xts)
  data<-filter(stWk, store_nbr==st.nm)[complete.cases(filter(stWk, stWk$store_nbr==st.nm)),]
  week.id<-data[,3]
  dates<-as.Date(sort(unique((filter(week, wm_yr_wk_id %in% sort(week.id))[,8]))))
  return(xts(data[order(week.id),4], order.by=dates))
}

store.id<-sort(unique(stWk$store_nbr))

############## SALES DATA PLOT #############
plotStoreSales<-function(st.nm)
{
  
  #font for plot
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  
  x <- list(
    title = "Week",
    titlefont = f
  )
  y <- list(
    title = "Weekly Sales",
    titlefont = f
  )
  
  heading<-paste("Variation in weekly sales for store:", st.nm)
  
  data<-getWeeksAndSales(st.nm)
  
  p <- plot_ly(x =~ time(data), y =~as.numeric(data[,1, drop=T]), mode = 'lines', type = 'scatter', fill = 'tozeroy', line=list(color="#007dc6")) %>%  
    layout(title=heading,
           xaxis = x,
           yaxis = y)
  
  print(p)
}


############# MATURE STORES FOUND #############
# mature stores: 104 weeks data from Jan 2016 to Dec 2017.
getMatureStores<-function(){
  mature.id<-numeric()
  for(i in store.id)
  {
    data<-suppressWarnings(getWeeksAndSales(i))
    if(sum(time(data)>=as.Date("2016-01-02") & time(data)<=as.Date("2017-12-23"))==104){
      mature.id<-c(mature.id, i)
    }
    if(sum(time(data)>=as.Date("2015-01-03") & time(data)<=as.Date("2017-12-23"))==156){
      mature.id<-c(mature.id, i)
    }
  }
  mature.id<-unique(mature.id)
  return(mature.id)
}
mature.id<-getMatureStores()


############# BSTS #############

  ####### BSTS object: T + S #######
getBSTS<-function(training, test)
{
  require(lubridate)
  require(bsts)
  require(dplyr)
  
  bsts.comp <- AddLocalLinearTrend(list(), training)
  bsts.comp <- AddSeasonal(bsts.comp, training, nseasons = 52)
  bsts.model <- bsts(training, state.specification = bsts.comp, niter = 500, ping=0, seed=2016)
  burn <- SuggestBurn(0.1, bsts.model)
  bsts.pred <- predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))
  
  fitted.values<-as.numeric(as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+training))
  trend.values<-as.numeric(colMeans(bsts.model$state.contributions[-(1:burn),"trend",]))
  seas.values<-as.numeric(colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.52.1",]))
  pred.values<-as.numeric(exp(bsts.pred$mean))
  
  bsts.obj<-list(training, test, fitted.values, trend.values, seas.values, pred.values)
  names(bsts.obj)<-c("training","test", "f","t", "s", "p")
  
  return(bsts.obj)
}

  ####### BSTS SEASONALITY #######
getSeasBSTS<-function(){
  seas.bsts<-numeric()
  for(i in mature.id)
  {
    data<-suppressWarnings(getWeeksAndSales(i))
    training<-window(log(data), start=as.Date("2016-01-02"), end=as.Date("2017-12-23"))
    test<-window(data, start=as.Date("2017-12-30"), end=as.Date(time(data)[length(data)]))
    seas.bsts<-rbind(seas.bsts, getBSTS(training,test)$s)
  }
  
  seas.bsts<-0.5*(seas.bsts[,1:52]+seas.bsts[,53:104])
  return(seas.bsts)
}

  ####### BSTS SEASONALITY IN WM FORMAT #######
seas.bsts.mat<-getSeasBSTS()
seas.bsts.mat<-exp(seas.bsts.mat)
seas.bsts<-apply(seas.bsts.mat, 2, mean, trim=0.10)
seas.bsts<-c(seas.bsts[5:52], seas.bsts[1:4])


############# TBATS #############

  ####### TBATS object: T + S #######
getTBATS<-function(training, test)
{
  require(forecast)
  
  tbats.model<-tbats(training, use.trend = T,  
                     use.damped.trend = T,   
                     seasonal.periods = 52,
                     use.parallel=FALSE, 
                     use.box.cox = FALSE)
  
  fitted.values <- as.numeric(fitted.values(tbats.model))
  trend.values <- as.numeric(tbats.components(tbats.model)[,2]+tbats.components(tbats.model)[,3])
  if(ncol(tbats.components(tbats.model))==3){
    seas.values<-rep(NA, 104)
  }else{
    seas.values <- as.numeric(tbats.components(tbats.model)[,4])
  }
  pred.values <- as.numeric(exp(forecast(tbats.model, h=length(test))$mean))
  
  tbats.obj<-list(training, test, fitted.values, trend.values, seas.values, pred.values)
  names(tbats.obj)<-c("training","test","f","t", "s", "p")
  return(tbats.obj)
}
  ####### TBATS SEASONALITY #######
getSeasTBATS<-function(){
  require(forecast)
  seas.tbats<-numeric()
  for(i in unique(mature.id))
  {
    data<-suppressWarnings(getWeeksAndSales(i))
    training<-as.numeric(window(log(data), start=as.Date("2016-01-02"), end=as.Date("2017-12-23")))
    
    tbats.model<-tbats(training, use.trend = T,  
                       use.damped.trend = T,   
                       seasonal.periods = 52,
                       use.parallel=FALSE, 
                       use.box.cox = FALSE)
    
    if(ncol(tbats.components(tbats.model))<=3) next
    else{
      seas.tbats<-rbind(seas.tbats, tbats.components(tbats.model)[,4])
    }
  }
  
  seas.tbats<-0.5*(seas.tbats[,1:52]+seas.tbats[,53:104])
  return(seas.tbats)
}

  ####### TBATS SEASONALITY IN WM FORMAT#######
seas.tbats.mat<-getSeasTBATS()
seas.tbats.mat<-exp(seas.tbats.mat)
seas.tbats<-apply(seas.tbats.mat, 2, mean, trim=0.025)
seas.tbats<-c(seas.tbats[5:52], seas.tbats[1:4])



# dist.bsts<-as.matrix(dist(seas.bsts, method="euclidean") )
# dist.tbats<-as.matrix(dist(seas.tbats, method="euclidean"))
# 
# 
# diag(dist.bsts)<-rep(NA, 100)
# diag(dist.tbats)<-rep(NA, 60)





############# WM #############
seas.wm<-read.csv("/home/rstudio/datasets/SI_2018.csv", header=T)[,2]
seas.wm<-(1+seas.wm)







############# SEASONAL INDICES PLOT #############
require(plotly)
seas.ind<-plot_ly(x=~1:52)%>%
  add_trace(y=~seas.bsts , type="scatter", mode="lines+markers", name="BSTS")%>%
  add_trace(y=~seas.tbats, type="scatter", mode="lines+markers", name="TBATS")%>%
  add_trace(y=~seas.wm, type="scatter", mode="lines+markers", name="Walmart")%>%
  layout(title="Seasonal Indices", xaxis=list(title="Week Number", 
                                              linecolor= 'black',
                                              linewidth= 2,
                                              mirror= TRUE)
         , yaxis=list(title="Seasonality", 
                      linecolor= 'black',
                      linewidth= 2,
                      mirror= TRUE))

getSeasFinal<-function()
{
  weights.bsts<-c(0.8, 0.8, 0.8, 0.5, 
                  0.5, 0.5, 0.4, 0.4, 
                  0.4, 0.8, 0.6, 0.5, 
                  0.5, 0.8, 0.2, 0.2, 
                  0.2, 0.6, 0.6, 0.6, 
                  0.2, 0.5, 0.5, 0.5, 
                  0.8, 0.8, 0.8, 0.8, 
                  0.8, 0.8, 0.8, 0.5, 
                  0.5, 0.8, 0.8, 0.8, 
                  0.8, 0.7, 0.7, 0.7, 
                  0.7, 0.5, 0.8, 0.5,
                  0.8, 0.5, 0.8, 0.8, 
                  0.2, 0.2, 0.2, 0.2)
  weights.tbats<- 1-weights.bsts
  FINAL.SEASONAL.COMPONENT.WEIGHTED <- weights.bsts*seas.bsts + weights.tbats*seas.tbats
  return(FINAL.SEASONAL.COMPONENT.WEIGHTED)
}
seas<-getSeasFinal()
seas.ind<-add_trace(seas.ind, y=~seas, type="scatter", mode="lines+markers", name="Custom")
seas.ind
