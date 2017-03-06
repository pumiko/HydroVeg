library(raster)
library(moments)

# Funckje podajace daty
SMAX_date = function(raster_SMAX)
{
  substr(raster_SMAX, 6, 15)
}

# Funkcja wybor rastrow po datach - na tej podstawie wybierane sa rastry z ktorych liczymy statystyki (okreslone przedzialy czasowe)
Analyse_dates = function(dates)  # dates w formacie c('YYYY.MM.DD','YYYY.MM.DD')
{
  all_dates = NULL 
  for (raster in SMAX_lista)
  {
    all_dates[raster] = SMAX_date(raster)
  }
  
  all_dates1 = as.character(all_dates)
  all_dates2 = as.Date(all_dates1, "%Y.%m.%d")
  dates_limit = as.Date(dates, "%Y.%m.%d")
  dates_selection = all_dates2[which(all_dates2 >= dates_limit[1] & all_dates2 <= dates_limit[2])]
  dates_selection_2 = as.character(dates_selection)
  gsub('-', '.', dates_selection_2)
}

#------------------------------------------------#

# Odczytanie list rastrow w mapsetach
wd_input = "/path/to/HydroVeg/Rastry_S_max"
wd_output = "/path/to/HydroVeg/Statystyki_S_max"
SMAX_lista = list.files(paste(wd_input,"/bardo",sep=""))

#------------------------------------------------#

# Funkcja wlasciwa do tworzenia plikow ze statystykami roslinnosci dla kazdej zlewni osobno
Create_stats_csv = function(mainDir,dates)
{

  setwd(wd_output)

  mapsets = c("bardo", "bystrzyca", "klodzko", "ladek", "miedzylesie", "scinawka", "szalejow", "szczytna", "zelazno")
  
  analysed_raster_list = Analyse_dates(dates)
  
  stats_matrix = matrix(NA, nrow=6, ncol=length(analysed_raster_list)) 
  AVG_values = NULL
  MED_values = NULL
  STD_values = NULL
  SKW_values = NULL
  KRT_values = NULL
  
  mean_stats_matrix = matrix(NA, nrow=6, ncol=1)
  mean_stats_mx = matrix(NA, nrow=6, ncol=1)
  output_matrix = matrix(NA, nrow=6, ncol=9)
  output_mx = matrix(NA, nrow=6, ncol=9)
     
    for (mapset in mapsets) 
    {            
      for (raster_date in analysed_raster_list)
      {
        plik_rastrowy = raster(paste(wd_input,"/",mapset,"/SMAX.",raster_date,"_",mapset,".tif",sep=""))
	AVG_value = mean(as.vector(plik_rastrowy), na.rm = TRUE)
	MED_value = median(as.vector(plik_rastrowy), na.rm = TRUE)
        STD_value = sd(as.vector(plik_rastrowy), na.rm = TRUE)
        SKW_value = skewness(as.vector(plik_rastrowy), na.rm = TRUE)
        KRT_value = kurtosis(as.vector(plik_rastrowy), na.rm = TRUE)

        AVG_values[raster_date] = AVG_value
        MED_values[raster_date] = MED_value
        STD_values[raster_date] = STD_value
        SKW_values[raster_date] = SKW_value
        KRT_values[raster_date] = KRT_value
      }
      
      AVG_vect = as.numeric(AVG_values)
      MED_vect = as.numeric(MED_values)
      STD_vect = as.numeric(STD_values)
      SKW_vect = as.numeric(SKW_values)
      KRT_vect = as.numeric(KRT_values)
      
      stats_matrix[1,] = AVG_vect
      stats_matrix[2,] = MED_vect
      stats_matrix[3,] = STD_vect
      stats_matrix[4,] = (STD_vect / AVG_vect)
      stats_matrix[5,] = SKW_vect
      stats_matrix[6,] = KRT_vect
      
      
      ## Statystyki usrednione
      n = length(stats_matrix[1,])
      m = length(stats_matrix[,1])
      for ( row in 1:m ) 
      {
        mean_stats_mx[row] = sum(stats_matrix[row,])/n        
      }
      mean_stats_matrix = mean_stats_mx
      
      write.table(mean_stats_matrix,paste(mainDir,mapset,"_SMAX_",csv_date(dates),".csv",sep=""), sep=" ", quote=F, col.names=F, row.names=F)
      write.table(stats_matrix,paste(mainDir,mapset,"_SMAX_",csv_date(dates),"_raw.csv",sep=""), sep=" ", quote=F, col.names=F, row.names=F)    
    }
    
  }


#------------------------------------------------#

csv_date = function(dates)
{
  #dates w formacie c('YYYY.MM.DD', 'YYYY.MM.DD')
  a = dates[1]
  b = dates[2]
  fname_date = paste(gsub("[.]","",a),"_",gsub("[.]","",b),sep="")
  fname_date
}

# Funkcja wlasciwa do tworzenia jednego pliku ze statystykami roslinnosci do wybranej daty
Create_veg_stats_file = function(mainDir,dates)
{
  basins = c("bystrzyca", "szalejow", "ladek", "klodzko", "zelazno", "miedzylesie", "szczytna", "scinawka", "bardo")
  
  b = NA
  
  for (basin in basins)
  {
    b[basin] = read.csv(paste(mainDir,basin,"_SMAX_",csv_date(dates),".csv",sep=""), sep=" ", header = F)
  }
  output_matrix = cbind(b$bystrzyca,b$szalejow,b$ladek,b$klodzko,b$zelazno,b$miedzylesie,b$szczytna,b$scinawka,b$bardo)
  write.table(output_matrix,paste(mainDir,"SMAX_",csv_date(dates),".csv",sep=""), sep=" ", quote=F, col.names=F, row.names=F)
}

# Funkcja wlasciwa - spinajaca dwie powyzsze
StatsVegVsPred = function(mainDir,dates) # daty w formacie c('YYYY.MM.DD', 'YYYY.MM.DD'), prod w formacie 'LAI', 'NDVI' itp.
{  
  Create_stats_csv(mainDir,dates)
  Create_veg_stats_file(mainDir,dates)
}

mainDir = "/path/to/HydroVeg/Statystyki_S_max/"
#dates = c('2013.09.06','2013.12.06')
#StatsVegVsPred(mainDir,dates)
#dates = c('2013.12.07','2014.03.05')
#StatsVegVsPred(mainDir,dates)
#dates = c('2014.03.06','2014.06.05')
#StatsVegVsPred(mainDir,dates)
dates = c('2014.06.06','2014.09.05')
StatsVegVsPred(mainDir,dates)
