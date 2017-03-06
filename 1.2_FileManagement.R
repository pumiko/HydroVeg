#################################################################
#                    SKRYPT SYSTEMU HYDROVEG                    #
#                    DO PORZADKOWANIA PLIKOW                    #
#################################################################

FileManagement = function()
{
  # Wylistuj istniejace foldery
  main_dir = paste("/home/hydroveg/Pulpit/mgr/HydroVeg/MODIS/")
  folders = list.dirs(main_dir,recursive = FALSE)
  
  # Wylistuj nowe pliki w folderze
  new_tif = list.files(main_dir,pattern = "tif")
  new_hdf = list.files(main_dir,pattern = "hdf")
  new_files = c(new_tif,new_hdf)
  
  for (folder in folders)
  {
    subfolders=list.dirs(folder)
    # cat(lista_podfolderow,"\n")
    for (subfolder in subfolders)
    {
      old_files_folders = list.files(subfolder)
      old_folders = list.dirs(subfolder,recursive = FALSE,full.names = FALSE)
      old_files = setdiff(old_files_folders,old_folders)
      # cat(old_files,"\n")
      # cat("\n") 
      
      for (file in new_files)
      {
        if(is.element(file,old_files)==TRUE) 
        {
          cat("Plik",file,"zostal wczesniej pobrany","\n")
          file.remove(paste(main_dir,file,sep=""))
          cat("Usuwam plik",file,"\n")
        } 
      } 
    }
  }
  
  # Rozdysponuj nowe pliki do odpowiednich folderow  
  new_tif = list.files(main_dir,pattern = "tif")
  new_hdf = list.files(main_dir,pattern = "hdf")
  new_files = c(new_tif,new_hdf)
  
  cat("Przenosze pliki do folderow","\n")
  
  for(file in new_files)
  {
    file_from = paste(main_dir,file,sep="")
    
    if (grepl("MCD15A3.A",file)==TRUE) {
      file_to_HDF = paste(main_dir,"MCD15A3/HDF/",file,sep="")
      file.rename(from=file_from, to=file_to_HDF)
    }
    
    if (grepl("Lai_1km",file)==TRUE) {
      file_to_Lai = paste(main_dir,"MCD15A3/Lai/",file,sep="")
      file.rename(from=file_from, to=file_to_Lai)
    }
    
    if (grepl("FparExtra_QC",file)==TRUE) {
      file_to_FparExtra_QC = paste(main_dir,"MCD15A3/FparExtra_QC/",file,sep="")
      file.rename(from=file_from, to=file_to_FparExtra_QC)
    }
    
    if (grepl("FparLai_QC",file)==TRUE) {
      file_to_FparLai_QC = paste(main_dir,"MCD15A3/FparLai_QC/",file,sep="")
      file.rename(from=file_from, to=file_to_FparLai_QC)
    }   
  } 
}
