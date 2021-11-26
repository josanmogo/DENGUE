#CODIGO DESCARGAR BASE ####
funcion_descargar_dengue<- function(){
  download.file(url="https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/etv/datos_abiertos_dengue.zip",
                destfile = "dengue.zip",
                method="auto")
  dengue<-unzip(zipfile = "dengue.zip",overwrite=T)
  file.rename(dengue,"dengue_2021.csv")
}