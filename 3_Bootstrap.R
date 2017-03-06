################################################################
#             SKRYPT POZIOMU 2 SYSTEMU HYDROVEG                #
#      DO OBLICZANIA KORELACJI BOOTSTRAP VEG VS. HYDRO         #
################################################################

# Czytanie bibliotek
library(boot)
options(digits=10)

#--------------------------------------

# Funkcja wykorzystana do bootstrapu
corr_function = function(data,indices)
{
data2 = data[indices,]
return(cor(data2$veg,data2$pred))
}

#--------------------------------------

# Funkcja podajaca daty plikow
csv_date = function(dates)
{
  #dates w formacie c('YYYY.MM.DD', 'YYYY.MM.DD')
  a = dates[1]
  b = dates[2]
  fname_date = paste(gsub("[.]","",a),"_",gsub("[.]","",b),sep="")
  fname_date
}

# mainDir w formacie ""/dir/dir/dir/dir/.../"
# dates w formacie: c('YYYY.MM.DD','YYYY.MM.DD')

#--------------------------------------

# Wlasciwa fukcja do bootstrapu

VegPredCorr = function(mainDir,dates)
{
  # Czytanie statystyk roslinnosci
  # smax = read.csv(file=paste(mainDir,"SMAX_",csv_date(dates),".csv",sep=""))
  smax = read.table(file=paste(mainDir,"SMAX_",csv_date(dates),".csv",sep=""))
  
  #--------------------------------------
  
  outfile = paste(mainDir,"Results_",csv_date(dates),".txt",sep="")
  
  #--------------------------------------
  
  pred_err_matrix = matrix(NA,nrow=27*6,ncol=48) #ROWS = 3 metody x 9 zlewni x 6 statystyk  #COLS = 4 statystyki bledow x 12 dlugosci prognoz #ZMIANY!!!
  
  g_vect= c(1,3,4,5,7,8,15,18,19) # do odczytywania plików wyników prognoz #ZMIANY
  m_vect = c(1,2,97) # do odczytywania plików wyników prognoz
  #---------------------
  #Do bootstrapu
  l_vect = 1:12
  p_val_thre = 0.1
  B = 1000
  
  #Zacytywanie plików i zapis do macierzy
  index = 1
  for (g in g_vect)
  {
    for (m in m_vect)
    {
      x = read.csv(paste(mainDir,"stats4days_",csv_date(dates),"_id_g_",g,"_id_m_",m,".csv",sep=""))[,2:49] #kolumny do zaczytania z pliku csv. 2:49 wynika z tego, ¿e w kolumnie 1 zapisa³ siê numer wiersza. #ZMIANY 49 na 12 lub 13 ??? 
      pred_err_matrix[((index-1)*6+1):(index*6),] = as.matrix(x) #ZMIANY 6 na 4 (?)
      index = index + 1
    }
  }
  
  
  # PARY VEG & PRED

  number_of_methods_and_statistics = length(m_vect)*length(x[,1])
  pred = vector(mode="numeric",length=length(g_vect))
  
  cat("\n","------------------------------------------------------------","\n","Analiza korelacji, jej istotnosci i symulacje bootsrap","\n","Na pierwszej osi jest zawsze indeks roslinnosci, na drugiej dokladnosc prognozy","\n","Czworki liczb: wspolczynnik korelacji, p-wartosc, sredni wspolczynnik korelacji boostrap, odchylenie standardowe wspolczynnika korelacji bootstrap","\n","Istotne korelacje oznaczono przerostkiem wiersza '!ISTOTNY!'","\n","------------------------------------------------------------","\n",file=outfile,append=TRUE)
  
  
  for (v in c(1))
  {
    cat("\n","%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%","\n","Numer indeksu wegetacji wynosi (1 - SMAX):", v,"\n","%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%","\n",file=outfile,append=TRUE)
    if (v == 1)
    {
      ind_veg = smax
    }

    
    for (si in 1:4)
    {
      cat("\n","@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n","Numer statystyki wynosi (1 - RMSE, 2 - MAE, 3 - NSE, 4 - d):", si,"\n","@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n",file=outfile,append=TRUE)
      for (li in 1:length(l_vect))
      {
        cat("\n","############################################################","\n","Dlugosc prognozy wynosi:", li,"\n","############################################################","\n",file=outfile,append=TRUE)
        ind = 1
        for (psi in 1:18) # co 6 zmiana modelu
        {
          veg = as.numeric(ind_veg[rep(1:6,3)[psi],])
          
          for (gi in 1:length(g_vect))
          {
            pred[gi] = pred_err_matrix[((gi-1)*number_of_methods_and_statistics+psi),(li+(si-1)*12)]
            if (is.infinite(pred[gi])) {
              cat(pred[gi])
              pred[gi] = as.numeric("NA")
              cat(pred[gi])
	    }
	  }
          
          if(psi%%6==1)
          {
            cat("Metoda nr (numeracja 1,2,3,...): ",ind,"\n",file=outfile,append=TRUE)
            ind = ind + 1
          }
          p_val = cor.test(veg,pred,use="pairwise.complete.obs")$p.value
          if (p_val < p_val_thre)
          {
            cat("!ISTOTNY! ",cor(veg,pred,use="pairwise.complete.obs")," ",p_val," ",file=outfile,append=TRUE)
          }else
          {
            cat(cor(veg,pred,use="pairwise.complete.obs")," ",p_val," ",file=outfile,append=TRUE)
          }
          
          veg_pred = data.frame(veg,pred)
          colnames(veg_pred) = c("veg","pred")
          boot_corr=boot(veg_pred,corr_function,R=B)
          mean(boot_corr$t,na.rm=TRUE)
          cat(mean(boot_corr$t,na.rm=TRUE)," ",sd(boot_corr$t,na.rm=TRUE),"\n",file=outfile,append=TRUE)
        }
      }
    }
  }
}


mainDir = "/path/to/HydroVeg/Statystyki_S_max_Boot/"
#dates = c('2013.09.06','2013.12.06')
#VegPredCorr(mainDir,dates)
#dates = c('2013.12.07','2014.03.05')
#VegPredCorr(mainDir,dates)
#dates = c('2014.03.06','2014.06.05')
#VegPredCorr(mainDir,dates)
dates = c('2014.06.06','2014.09.05')
VegPredCorr(mainDir,dates)
