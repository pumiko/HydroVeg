# Funkcja do nadawania plikom wyjsciowym z Programu TIMESAT nazw w oparciu o daty
lai_date_folder = "/path/to/HydroVeg/pliki_lai/"
lai_folder = "/path/to/HydroVeg/pliki_lai_fit/"

date_list = list.files(lai_date_folder, pattern = "*.rst")
non_rst_list = list.files(lai_date_folder, pattern = "*.rst.aux.xml")
date_list = setdiff(date_list,non_rst_list)
lai_list = list.files(lai_folder)

for (file in lai_list) {
  index = match(file,lai_list)
  new_name = date_list[index]
  file.rename(paste(lai_folder,file,sep=""),paste(lai_folder,new_name,sep=""))
}