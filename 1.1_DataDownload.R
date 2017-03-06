##################################################################
#                SKRYPT POZIOMU 2 SYSTEMU HYDROVEG               #
#            DO POBIERANIA DANYCH SATELITARNYCH MODIS            #
##################################################################

DataDownload = function(product,dates)
{
	# Sciagnij plik
	setwd("/home/hydroveg/Pulpit/mgr/HydroVeg/scripts")
	source('3_ModisDownload.R')
	#source('3_TimeSpan.R')
	#cat("\n")
	h = c(19,19)
	v = c(03,03)
	setwd("/home/hydroveg/Pulpit/mgr/HydroVeg/MODIS")
  
  #Przedzial czasu
	#dates = TimeSpan()
  
	if (product==1) 
	{
		cat("Laczenie z http MODIS - MOD13Q1","\n")
		try(ModisDownload(product,h,v,dates,MRTpath='/home/hydroveg/Pulpit/MRT/bin', mosaic=F, proj=T, UL=c(572500.0,5628000.0),LR=c(654900.0,5545000.0), resample_type="NEAREST_NEIGHBOR", bands_subset="1 0 1 0 0 0 0 0 0 0 0 1", proj_type="UTM", utm_zone=33, datum="WGS84", pixel_size=250))
	} 
	if (product==22) 
	{
		cat("Laczenie z http MODIS - MCD15A3","\n")
		try(ModisDownload(product,h,v,dates,MRTpath='/home/hydroveg/Pulpit/MRT/bin', mosaic=F, proj=T, UL=c(572500.0,5628000.0),LR=c(654900.0,5545000.0), resample_type="NEAREST_NEIGHBOR", bands_subset="0 1 1 1 0 0", proj_type="UTM", utm_zone=33, datum="WGS84", pixel_size=1000))
	}
}

# w celu wykluczenia pracy automatycznej nalezy zablokowac funcje TimeSpan()
# product: MOD13Q1 (1) lub MCD15A3 (22)
# dates: w formacie: c('YYYY.MM.DD', 'YYYY.MM.DD')

product = c(22) #MCD15A3
dates=c('2012.11.01','2013.07.31')
DataDownload(product,dates)

dates=c('2014.10.25','2014.11.05')
DataDownload(product,dates)

#product = c(1) #MOD13Q1
#dates=c('2014.03.23','2014.11.03')
#DataDownload(product,dates)
