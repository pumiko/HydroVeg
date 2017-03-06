library(RPostgreSQL)

#Zmienna - numer zlewni
gauges = c(7)

#Zmienna - dwa punkty czasowe
offset = 86400/4
date_time_1_no_offset = as.POSIXlt('2014-05-27 12:15:00',tz="") #moment osiagniecia progu maksymalne rtencji wodnej roslin
date_time_2_no_offset = as.POSIXlt('2014-05-27 10:15:00',tz="") #wyznaczony punkt przegiecia wodowskazu
date_1_no_offset = format(date_time_1_no_offset,"%Y-%m-%d")
time_1_no_offset = format(date_time_1_no_offset,"%H:%M:%S")
date_2_no_offset = format(date_time_2_no_offset,"%Y-%m-%d")
time_2_no_offset = format(date_time_2_no_offset,"%H:%M:%S")

date_time_1 = date_time_1_no_offset - offset
date_time_2 = date_time_2_no_offset + offset
date_1 = format(date_time_1,"%Y-%m-%d")
time_1 = format(date_time_1,"%H:%M:%S")
date_2 = format(date_time_2,"%Y-%m-%d")
time_2 = format(date_time_2,"%H:%M:%S")

seq_selected_points = c(date_time_1,date_time_2)
seq_selected_points_ok = format(seq_selected_points, format = "%Y-%m-%d %H:%M:%S", usetz = FALSE)
cat(seq_selected_points_ok,"\n")

drvP <- dbDriver("PostgreSQL")
conP <- dbConnect(drvP,dbname = "*****",user = "*****")

for (g in gauges)
{
  #Odczytanie z bazy danych PostgreSQL wysokosci stanow wody dla zadanego przedzialu czasu
  q = dbSendQuery(conP,paste("SELECT date+time as dt,wlev from data_processed.dp_15min natural join data_processed.time where id_g = ",g," and ((date ='",date_1,"' and time>='",time_1,"') or (date='",date_2,"' and time<='",time_2,"') or (date<'",date_2,"' and date>'",date_1,"')) order by dt",sep=""))
  obs = fetch(q,n=-1)
  script_workspace = "/home/obliczenia/hydror/"

  filename = paste(script_workspace,"figures/id_g_",g,"_date_time",date_time_1_no_offset,"_",date_time_2_no_offset,".png",sep="")
  png(file=filename,width=900,height=600)

  l1 = round(0.8*min(obs$wlev),-1)
  l2 = round(1.1*max(obs$wlev),-1) 
  ra = abs(l2-l1)/5

  #Przygotowanie wykresy stanow wody
  plot(obs$dt,obs$wlev,type="l",xlab="Time",ylab="H [cm]",ylim=c(l1,l2),xaxs="i",yaxs = "i")
  grid(NA, ra, lwd = 2) 
  legend("topleft",inset=0.05,pch=c(1,5,8),legend=c("Prediction based on multivariate autoregression","Prediction based on univariate autoregression","Prediction based on multimodel ensemble"))

  #odczytanie prognoz VAR, AR i ENS(97) z bazy danych dla obu punktow czasowych
  index_obs_1 = which(obs[,1]==date_time_1_no_offset)
  obs_1 = obs[index_obs_1,]

  qp1_m1 = dbSendQuery(conP,paste("SELECT date+time+length as dtl,water_level from predictions.prediction_lsop natural join data_processed.time natural join predictions.lead_time where id_m = 1 and id_l<=12 and id_g =",g," and date ='",date_1_no_offset,"' and time ='",time_1_no_offset,"' order by dtl",sep=""))
  p1_m1 = fetch(qp1_m1,n=-1)
  po1_m1 = rbind(setNames(obs_1,names(p1_m1)),p1_m1)

  qp1_m2 = dbSendQuery(conP,paste("SELECT date+time+length as dtl,water_level from predictions.prediction_lsop natural join data_processed.time natural join predictions.lead_time where id_m = 2 and id_l<=12 and id_g =",g," and date ='",date_1_no_offset,"' and time ='",time_1_no_offset,"' order by dtl",sep=""))
  p1_m2 = fetch(qp1_m2,n=-1)
  po1_m2 = rbind(setNames(obs_1,names(p1_m2)),p1_m2)

  qp1_m97 = dbSendQuery(conP,paste("SELECT date+time+length as dtl,water_level from predictions.prediction_lsop natural join data_processed.time natural join predictions.lead_time where id_m = 97 and id_l<=12 and id_g =",g," and date ='",date_1_no_offset,"' and time ='",time_1_no_offset,"' order by dtl",sep=""))
  p1_m97 = fetch(qp1_m97,n=-1)
  po1_m97 = rbind(setNames(obs_1,names(p1_m97)),p1_m97)

  index_obs_2 = which(obs[,1]==date_time_2_no_offset)
  obs_2 = obs[index_obs_2,]

  qp2_m1 = dbSendQuery(conP,paste("SELECT date+time+length as dtl,water_level from predictions.prediction_lsop natural join data_processed.time natural join predictions.lead_time where id_m = 1 and id_l<=12 and id_g =",g," and date ='",date_2_no_offset,"' and time ='",time_2_no_offset,"' order by dtl",sep=""))
  p2_m1 = fetch(qp2_m1,n=-1)
  po2_m1 = rbind(setNames(obs_2,names(p2_m1)),p2_m1)

  qp2_m2 = dbSendQuery(conP,paste("SELECT date+time+length as dtl,water_level from predictions.prediction_lsop natural join data_processed.time natural join predictions.lead_time where id_m = 2 and id_l<=12 and id_g =",g," and date ='",date_2_no_offset,"' and time ='",time_2_no_offset,"' order by dtl",sep=""))
  p2_m2 = fetch(qp2_m2,n=-1)
  po2_m2 = rbind(setNames(obs_2,names(p2_m2)),p2_m2)

  qp2_m97 = dbSendQuery(conP,paste("SELECT date+time+length as dtl,water_level from predictions.prediction_lsop natural join data_processed.time natural join predictions.lead_time where id_m = 97 and id_l<=12 and id_g =",g," and date ='",date_2_no_offset,"' and time ='",time_2_no_offset,"' order by dtl",sep=""))
  p2_m97 = fetch(qp2_m97,n=-1)
  po2_m97 = rbind(setNames(obs_2,names(p2_m97)),p2_m97)

  #Naniesienie wartosci prognoz na wykres
  points(po1_m1$dtl,po1_m1$water_level,pch=1)
  points(po1_m2$dtl,po1_m2$water_level,pch=5)
  points(po1_m97$dtl,po1_m97$water_level,pch=8)
  points(po2_m1$dtl,po2_m1$water_level,pch=1)
  points(po2_m2$dtl,po2_m2$water_level,pch=5)
  points(po2_m97$dtl,po2_m97$water_level,pch=8)

  #Dodanie grafiki do wykresu
  pred_97_3h = as.numeric(po2_m97$water_level[13])
  obs_plus_3h = as.numeric(obs$wlev[index_obs_2+12])
  delta = abs(obs_plus_3h - pred_97_3h)

  arrows(po1_m97$dtl[1], po1_m97$water_level[1] + (abs(range(obs$wlev)[2]-range(obs$wlev)[1])/3), po1_m97$dtl[1], po1_m97$water_level[1], col="black")
  arrows(po2_m97$dtl[1], po2_m97$water_level[1] + (abs(range(obs$wlev)[2]-range(obs$wlev)[1])/2), po2_m97$dtl[1], po2_m97$water_level[1], col="black")
  text(po1_m97$dtl[1], (po1_m97$water_level[1] + (abs(range(obs$wlev)[2]-range(obs$wlev)[1])/3) + 1.2),"Maximum canopy water storage")
  text(po2_m97$dtl[1], (po2_m97$water_level[1] + (abs(range(obs$wlev)[2]-range(obs$wlev)[1])/2) + 1.2),"Inflection point of hydrograph")

  dev.off()
  dbDisconnect(conP)
}
