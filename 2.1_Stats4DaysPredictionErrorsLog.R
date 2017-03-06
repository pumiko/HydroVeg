#Funkcje obliczajace statystyki bledow i jakosci prognoz
rmse = function(o,p) 
{
	return(round(sqrt(sum((o-p)^2)/length(o)),3))
}

mae = function(o,p) 
{
	return(round(sum(abs(o-p))/length(o),3))
}

#Comparison of different efficiency criteria for hydrological model assessment ADGEO 5, 89 97, 2005
nash_E = function(o,p) 
{
	licznik = sum((o-p)^2)
	o_mean = mean(o)
	mianownik = sum((o-o_mean)^2)	
	return(round(1-licznik/mianownik,3))
}

#Comparison of different efficiency criteria for hydrological model assessment ADGEO 5, 89 97, 2005
index_d = function(o,p) 
{
	licznik = sum((o-p)^2)
	o_mean = mean(o)
	mianownik = sum((abs(o-o_mean)+abs(p-o_mean))^2)	
	return(round(1-licznik/mianownik,3))
}

roundt2 = function(time)
{
  return( strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + floor(as.numeric(time)/900)*900 )
}


#Funkcja do obliczania statystyki prognoz na czas pomiedzy dwoma datami
#Funkcja wybiera obserwacje z tego przedzialu i prognozy z odpowiednich wczesniejszych punktow czasowych
statistics = function(experimental_gauges, id_m, date_start = NULL, date_end = NULL,log_fname)
{
	library(RPostgreSQL)
	drvP <- dbDriver("PostgreSQL")
	conP <- dbConnect(drvP,dbname = "*****",user = "*****", password='********************')
	
	q = dbSendQuery(conP,"SELECT * FROM predictions.lead_time order by id_l;")
	lead_times = fetch(q,n=-1)
	q = dbSendQuery(conP,"SELECT * FROM predictions.time_last_data order by id_t;")
	times_last_data = fetch(q,n=-1)
	
	dbDisconnect(conP)
	
  ds = as.POSIXct(date_start,tz="UTC")
  de = as.POSIXct(date_end,tz="UTC")
        
  next_day = de + 60*60*24 
  id_t = 1 #polnoc
	g = experimental_gauges
	
	drvP <- dbDriver("PostgreSQL")
	conP <- dbConnect(drvP,dbname = "*****",user = "*****", password='********************')

	#wybor podzbioru obserwacji ( od date_start do date_end wlacznie )	
	q = dbSendQuery(conP,paste("SELECT wlev,date,time FROM data_processed.dp_15min join data_processed.time using(id_t) where id_g =",g," AND (date >='",date_start,"' AND date <='",date_end,"') order by date,id_t;",sep=""))
	obserwacje = fetch(q,n=-1)

	matrix_stats = matrix(NA,nrow=4,ncol=12)

	for(j in 1:12)
	{
		lt = lead_times$id_l[j]

		#wybiaramy podzbior prognoz	
		#Przesuiecie okna o lead time (DZISIAJ, OSTATNI_ITD  -> ODEJMUJEMY LEAD TIME)
		terazP = next_day - as.difftime(lead_times$length[lt])
		pierwszaP = ds - as.difftime(lead_times$length[lt])

    DZISIAJP = format(terazP,"%Y-%m-%d")
		time_nowP = format(terazP,"%H:%M:%S")

		DATA_PIERWSZEJP = format(pierwszaP,"%Y-%m-%d")
		time_pierwszejP = format(pierwszaP,"%H:%M:%S")	
	
    q = dbSendQuery(conP,paste("SELECT water_level,date,time FROM predictions.prediction_lsop join predictions.time_last_data using(id_t) where id_g =",g," AND id_m =",id_m," AND id_l = ",lt," AND ((date ='",DZISIAJP,"' and time <'",time_nowP,"') OR (date <'",DZISIAJP,"' AND date >'",DATA_PIERWSZEJP,"') OR ( date ='",DATA_PIERWSZEJP,"' AND time >='",time_pierwszejP,"')) order by date,id_t;",sep=""))
    prognozy = fetch(q,n=-1)
    
    #Zapisywanie informacji o brakujacych danych do pliku dziennika (log file)
    if(is.null(prognozy$water_level)) {cat(paste("brak prognoz metody",id_m,"dla gauge",g,"w okresie",DATA_PIERWSZEJP,DZISIAJP ,"\n"),file=log_fname,append=TRUE); next}
        
    obserwacje_dt = as.POSIXct(strptime(paste(obserwacje$date,obserwacje$time), '%Y-%m-%d %H:%M:%S'),tz="UTC")
    prognozy_dt = as.POSIXct(strptime(paste(prognozy$date,prognozy$time), '%Y-%m-%d %H:%M:%S'),tz="UTC") + as.difftime(lead_times$length[lt])
      
    intersect_vals = intersect(obserwacje_dt,prognozy_dt)
        
    df = data.frame(obs=obserwacje$wlev[match(intersect_vals,obserwacje_dt)],pre=prognozy$water_level[match(intersect_vals,prognozy_dt)])
      
    #Obliczenie statystyk dla podzbiorow obserwacji i prognoz
    nE = nash_E(df$obs,df$pre)
    iD = index_d(df$obs,df$pre)
    rmse = rmse(df$obs,df$pre)
    mae = mae(df$obs,df$pre)
                
		matrix_stats[1,j] = rmse
		matrix_stats[2,j] = mae
		matrix_stats[3,j] = nE
		matrix_stats[4,j] = iD
						
	}
			
  dbDisconnect(conP)	
  
	matrix_stats
}

#Funkcja liczaca statystyki prognoz dla por roku, wykorzystujaca funkcje statistics()
#Wynikiem funkcji jest macierz ze statystykami rozkladu poszczegolnych bledow i jakosci prognoz
stats_prediction_errors = function(date_min,date_max,g,m,log_fname)
{
	library(moments)	
	stats_array = array(NA,dim=c(6,12,4))
	
  #Przygotowanie wektora dat
	dates4days = seq(as.Date(date_min),as.Date(date_max),4)
	year_min = as.numeric(format(as.Date(date_min), "%Y"))
	year_max = as.numeric(format(as.Date(date_max), "%Y"))
	first_jan_year = as.Date(paste(year_max,"-01-01",sep=""))
	last_dec_year = as.Date(paste(year_min,"-12-31",sep="")) 
	year_diff = year_max - year_min
  if (year_diff == 1 )
	{ 
	 	dates4days_part1 = seq(as.Date(date_min),last_dec_year,4)
		dates4days_part2 = seq(first_jan_year,as.Date(date_max),4)
		dates4days = c(dates4days_part1,dates4days_part2)
	} else {
		dates4days = seq(as.Date(date_min),as.Date(date_max),4)
	}
       
	ts_matrix_rmse_old = NULL
	ts_matrix_mae_old = NULL
	ts_matrix_nse_old = NULL
	ts_matrix_d_old = NULL
  
  #Obliczanie statystyk dla 4-dniowych przedzialow, zapis do macierzy
	for(s in 1:(length(dates4days)-1))
	{
		matrix_4day_stats = statistics(g,m, date_start = dates4days[s], date_end = (dates4days[s+1] -1),log_fname)

		ts_matrix_rmse = cbind(ts_matrix_rmse_old,matrix_4day_stats[1,])
		ts_matrix_rmse_old = ts_matrix_rmse

    ts_matrix_mae = cbind(ts_matrix_mae_old,matrix_4day_stats[2,])
    ts_matrix_mae_old = ts_matrix_mae

    ts_matrix_nse = cbind(ts_matrix_nse_old,matrix_4day_stats[3,])
    ts_matrix_nse_old = ts_matrix_nse

    ts_matrix_d = cbind(ts_matrix_d_old,matrix_4day_stats[4,])
    ts_matrix_d_old = ts_matrix_d
	}
	
  #Obliczenie statystyk rozkladu
	for (l in 1:12)
	{
		stats_array[1,l,1] = mean(ts_matrix_rmse[l,],na.rm=TRUE)
		stats_array[1,l,2] = mean(ts_matrix_mae[l,],na.rm=TRUE)
		stats_array[1,l,3] = mean(ts_matrix_nse[l,],na.rm=TRUE)
		stats_array[1,l,4] = mean(ts_matrix_d[l,],na.rm=TRUE)                        
		stats_array[2,l,1] = median(ts_matrix_rmse[l,],na.rm=TRUE)
    stats_array[2,l,2] = median(ts_matrix_mae[l,],na.rm=TRUE)
    stats_array[2,l,3] = median(ts_matrix_nse[l,],na.rm=TRUE)
    stats_array[2,l,4] = median(ts_matrix_d[l,],na.rm=TRUE)
    stats_array[3,l,1] = sd(ts_matrix_rmse[l,],na.rm=TRUE)
    stats_array[3,l,2] = sd(ts_matrix_mae[l,],na.rm=TRUE)
    stats_array[3,l,3] = sd(ts_matrix_nse[l,],na.rm=TRUE)
    stats_array[3,l,4] = sd(ts_matrix_d[l,],na.rm=TRUE)
    stats_array[4,l,1] = stats_array[3,l,1]/stats_array[1,l,1]
    stats_array[4,l,2] = stats_array[3,l,2]/stats_array[1,l,2]
    stats_array[4,l,3] = stats_array[3,l,3]/stats_array[1,l,3]
    stats_array[4,l,4] = stats_array[3,l,4]/stats_array[1,l,4]
    stats_array[5,l,1] = skewness(ts_matrix_rmse[l,],na.rm=TRUE)
    stats_array[5,l,2] = skewness(ts_matrix_mae[l,],na.rm=TRUE)
    stats_array[5,l,3] = skewness(ts_matrix_nse[l,],na.rm=TRUE)
    stats_array[5,l,4] = skewness(ts_matrix_d[l,],na.rm=TRUE)
    stats_array[6,l,1] = kurtosis(ts_matrix_rmse[l,],na.rm=TRUE)
    stats_array[6,l,2] = kurtosis(ts_matrix_mae[l,],na.rm=TRUE)
    stats_array[6,l,3] = kurtosis(ts_matrix_nse[l,],na.rm=TRUE)
    stats_array[6,l,4] = kurtosis(ts_matrix_d[l,],na.rm=TRUE)
	}
	stats_array
}

#Uruchomienie funkcji stats_prediction_errors() dla por roku
g_vect= c(1,3,4,5,7,8,15,18,19) 
m_vect = c(1,2,97)

date_s = '2014-06-06'
date_e = '2014-09-06'  
date_sd = format(as.Date(date_s),"%Y%m%d")
date_ed = format((as.Date(date_e)-1),"%Y%m%d")

log_fname = paste("/home/obliczenia/hydror/analysis/hydroveg/stats/log_",date_sd,"_",date_ed,".txt",sep="")

for (g in g_vect)
{
	for (m in m_vect)
	{
		x = stats_prediction_errors(date_s,date_e,g,m,log_fname)
		fname = paste("stats4days_",date_sd,"_",date_ed,"_id_g_",g,"_id_m_",m,".csv",sep="")
		cat(fname,"\n",file=log_fname,append=TRUE)
		write.csv(x,paste("/home/obliczenia/hydror/analysis/hydroveg/stats/",fname,sep=""))
	}
}