#Funkcja odczytujaca wysokosc stanow wody w podanym przedziale czasu
read_wlev_1or2days = function(date_min,time_min,date_max,time_max,g)
{
        library(RPostgreSQL)
        drvP <- dbDriver("PostgreSQL")
        conP <- dbConnect(drvP,dbname = "*****",user = "*****", password='*****')

	#cat(paste("SELECT wlev FROM data_processed.dp_15min natural join data_processed.time where id_g = ",g," and ((date = '",date_min,"' and time >= '",time_min,"') or ( date = '",date_max,"' and time <= '",time_max,"'));",sep=""))

	q = dbSendQuery(conP,paste("SELECT wlev FROM data_processed.dp_15min natural join data_processed.time where id_g = ",g," and ((date = '",date_min,"' and time >= '",time_min,"') or ( date = '",date_max,"' and time <= '",time_max,"')) order by date,time;",sep=""))
	q_f = fetch(q,n=-1)
	wlevel = as.vector(q_f$wlev)

    	dbDisconnect(conP)
  #standaryzacja danych - umozliwia porownanie pomiedzy zlewniami
	result_standardized = (wlevel - mean(wlevel))/sd(wlevel)
	result_standardized
}

#Zmienne - numer wodowskazu oraz przedzialu czasu
g_vect = c(1)
date_s = '2014-05-27'
time_s = '05:00:00'
date_e = '2014-05-28'
time_e = '05:00:00'

for (g in g_vect)
{
  #Uruchomienie funkcji odczytujacej stany wody
  wlevel = read_wlev_1or2days(date_s,time_s,date_e,time_e,g)
	inde = 1:length(wlevel)

  #Obliczenie punktu przegiecia wodowskazu
	library(inflection)
	inflection_matrix = NULL
  
	inflection_matrix = findiplist(cbind(inde),cbind(wlevel),0)
	inflection_point_1 = min(inflection_matrix[1,1],inflection_matrix[2,1])
 	inflection_point_2 = max(inflection_matrix[1,1],inflection_matrix[2,1])

	print(inflection_matrix)

  #Obliczenie wskaznika nachylenia krzywej standaryzowanych stanow wody dla kroku 15minutowego, 1-godzinnego i 3-godzinnego
	delta_wlev_15min_min = round(((wlevel[inflection_point_1 + 1] - wlevel[inflection_point_1])/1),3)
	delta_wlev_1h_min = round(((wlevel[inflection_point_1 + 4] - wlevel[inflection_point_1])/4),3)
	delta_wlev_3h_min = round(((wlevel[inflection_point_1 + 12] - wlevel[inflection_point_1])/12),3)

        delta_wlev_15min_max = round(((wlevel[inflection_point_2 + 1] - wlevel[inflection_point_2])/1),3)
        delta_wlev_1h_max = round(((wlevel[inflection_point_2 + 4] - wlevel[inflection_point_2])/4),3)
        delta_wlev_3h_max = round(((wlevel[inflection_point_2 + 12] - wlevel[inflection_point_2])/12),3)

	date_time = paste(gsub("-","",date_s),gsub(":","",time_s),gsub("-","",date_e),gsub(":","",time_e),sep="_")
	fname = paste("inflection_",date_time,"_id_g_",g,".png",sep="")

  #Przygotowanie i zapis wykresu do pliku .png
  png(file=fname,width=900,height=600)

	date_s_posix = as.POSIXct(paste(date_s,time_s,sep=" "),tz="UTC")
        date_e_posix = as.POSIXct(paste(date_e,time_e,sep=" "),tz="UTC")

	dates_plot = seq(date_s_posix,date_e_posix,by="15 min")
	
	plot(dates_plot,wlevel,type="l",xlab="Time",ylab="H [cm]")
	points(dates_plot[inflection_point_1],wlevel[inflection_point_1],pch=1)
	points(dates_plot[inflection_point_2],wlevel[inflection_point_2],pch=8)
        legend("topleft",inset=0.05,pch=c(1,NA,NA,NA,NA,NA,8,NA,NA,NA,NA),c(as.character(dates_plot[inflection_point_1]),"Rate of standardized water level rise (cm/15 min)",paste("   15 min forward: ", delta_wlev_15min_min,sep=""),paste("   1 hour forward: ",delta_wlev_1h_min,sep=""),paste("   3 hours forward: ",delta_wlev_3h_min,sep=""),NA,as.character(dates_plot[inflection_point_2]),"Rate of standardized water level rise (cm/15 min)",paste("   15 min forward: ",delta_wlev_15min_max,sep=""),paste("   1 hour forward: ",delta_wlev_1h_max,sep=""),paste("   3 hours forward: ",delta_wlev_3h_max,sep="")))

}

dev.off()
