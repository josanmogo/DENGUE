library(tidyverse)
library(readxl)
source("FUNCION DESCARGAR BASE.R")

datos2021<-read_excel("pob_2021.xlsx") %>% 
  select(ENTIDAD=clave_ent,MUNICIPIO=mun,POB_TOTAL_2021=pob) %>% 
  unite(
    col = "IDENTIFICADOR",         # name of the new united column
    c("ENTIDAD", "MUNICIPIO"), # columns to unite
    sep = "- ",                   # separator to use in united column
    remove = F,                # if TRUE, removes input cols from the data frame
    na.rm = TRUE                  # if TRUE, missing values are removed before uniting
  )

funcion_descargar_dengue()

#AGREGAR DATOS DENGUE 2021####
dengue_2021<-read.csv("dengue_2021.csv") %>% 
  select(SEXO,EDAD=EDAD_ANOS,ENTIDAD_ASIG,MUNICIPIO_ASIG,INDIGENA,HABLA_LENGUA_INDIG,
         FECHA_SIGN_SINTOMAS,TIPO_PACIENTE,DEFUNCION,RESULTADO_PCR,ESTATUS_CASO) %>%
  rename(ENTIDAD=ENTIDAD_ASIG,MUNICIPIO=MUNICIPIO_ASIG) %>% 
  filter(ENTIDAD<=32) %>% 
  mutate(ENTIDAD=as.factor(ENTIDAD)) %>% 
  mutate(MUNICIPIO=as.factor(MUNICIPIO)) %>% 
  unite(
    col = "IDENTIFICADOR",         # name of the new united column
    c("ENTIDAD", "MUNICIPIO"), # columns to unite
    sep = "- ",                   # separator to use in united column
    remove = F,                # if TRUE, removes input cols from the data frame
    na.rm = TRUE                  # if TRUE, missing values are removed before uniting
  )

reportados_totales_nueva<-dengue_2021 %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(REPORTADOS_TOTALES=n) 

confirmados_nueva<-dengue_2021 %>% 
  filter(ESTATUS_CASO==2) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(CONFIRMADOS_TOTALES=n) 

defunciones_positivas_nueva<-dengue_2021 %>% 
  filter(ESTATUS_CASO==2) %>% 
  filter(DEFUNCION==1) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(DEFUNCIONES_CONF=n) 

descartados_nueva<-base_conjunta_nueva<-dengue_2021 %>% 
  filter(ESTATUS_CASO==3) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(DESCARTADOS_TOTALES=n) 


probables_nueva<-dengue_2021 %>% 
  filter(ESTATUS_CASO==1) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(PROBABLES_TOTALES=n) 

reportados_totales_nueva<-dengue_2021 %>%  
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(REPORTADOS_TOTALES=n) 

prueba_positiva_1a4_nueva<-dengue_2021 %>% 
  filter(RESULTADO_PCR<=4) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(PRUEBA_POSITIVA_1A4=n) 

sero_1_nueva<-dengue_2021 %>% 
  filter(RESULTADO_PCR==1) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(SERO_1=n) 

sero_2_nueva<-dengue_2021 %>% 
  filter(RESULTADO_PCR==2) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(SERO_2=n) 

sero_3_nueva<-dengue_2021 %>% 
  filter(RESULTADO_PCR==3) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(SERO_3=n) 

sero_4_nueva<-dengue_2021 %>% 
  filter(RESULTADO_PCR==4) %>% 
  count(IDENTIFICADOR) %>% # group data by week and count rows per group (creates column 'n')
  rename(SERO_4=n) 

#BASE DENGUE PARA TASAS 2021####
conteo_dengue_nueva<-merge(reportados_totales_nueva,confirmados_nueva,all.x=T)

conteo_dengue_nueva<-merge(conteo_dengue_nueva,reportados_totales_nueva,all.x=T)

conteo_dengue_nueva<-merge(conteo_dengue_nueva,confirmados_nueva,all.x=T)

conteo_dengue_nueva<-merge(conteo_dengue_nueva,defunciones_positivas_nueva,all.x=T)

conteo_dengue_nueva<-merge(conteo_dengue_nueva,descartados_nueva,all.x=T)

conteo_dengue_nueva<-merge(conteo_dengue_nueva,probables_nueva,all.x=T)

conteo_dengue_nueva<-merge(conteo_dengue_nueva,prueba_positiva_1a4_nueva,all.x=T)
conteo_dengue_nueva<-merge(conteo_dengue_nueva,sero_1_nueva,all.x=T)
conteo_dengue_nueva<-merge(conteo_dengue_nueva,sero_2_nueva,all.x=T)
conteo_dengue_nueva<-merge(conteo_dengue_nueva,sero_3_nueva,all.x=T)
conteo_dengue_nueva<-merge(conteo_dengue_nueva,sero_4_nueva,all.x=T)

base_tablero_dengue_2021<-dplyr::left_join(datos2021,conteo_dengue_nueva,by="IDENTIFICADOR")

base_tablero_dengue_2021<-base_tablero_dengue_2021 %>% 
  mutate(INC_ACUM=(CONFIRMADOS_TOTALES/POB_TOTAL_2021*100000)) %>% 
  mutate(TASA_MORT=(DEFUNCIONES_CONF/POB_TOTAL_2021*100000)) %>% 
  mutate(PORCENTAJE_POSITIVIDAD_2021=((CONFIRMADOS_TOTALES/(CONFIRMADOS_TOTALES+DESCARTADOS_TOTALES)*100))) %>%
  mutate(PROB_ESTIMADOS_2021="") %>% 
  mutate(TOTAL_ESTIMADOS_2021="") %>% 
  select(IDENTIFICADOR,POB_TOTAL_2021,REPORTADOS_TOTALES_2021=REPORTADOS_TOTALES,CONFIRMADOS_TOTALES_2021=CONFIRMADOS_TOTALES,
         DEFUNCIONES_CONF_2021=DEFUNCIONES_CONF,DESCARTADOS_TOTALES_2021=DESCARTADOS_TOTALES,PROBABLES_TOTALES_2021=PROBABLES_TOTALES,PORCENTAJE_POSITIVIDAD_2021,
         PROB_ESTIMADOS_2021,TOTAL_ESTIMADOS_2021,PRUEBA_POSITIVA_1A4_2021=PRUEBA_POSITIVA_1A4,
         SERO_1_2021=SERO_1,SERO_2_2021=SERO_2,SERO_3_2021=SERO_3,SERO_4_2021=SERO_4,INC_ACUM_2021=INC_ACUM,TASA_MORT_2021=TASA_MORT)

numerico<-c(1:2457)

for(i in numerico){
  base_tablero_dengue_2021$PROB_ESTIMADOS_2021<-((base_tablero_dengue_2021$PORCENTAJE_POSITIVIDAD_2021/100)*base_tablero_dengue_2021$PROBABLES_TOTALES_2021)
}
for(i in numerico){
  base_tablero_dengue_2021$TOTAL_ESTIMADOS_2021<-round(base_tablero_dengue_2021$PROB_ESTIMADOS_2021+base_tablero_dengue_2021$CONFIRMADOS_TOTALES_2021,0)
}

#MERGE FINAL####

base_tablero_dengue_2020<-read.csv("BASE_TRES_INDICADORES.csv")
base_tablero_dengue_final<-dplyr::left_join(base_tablero_dengue_2020,base_tablero_dengue_2021,by="IDENTIFICADOR")
base_tablero_dengue_final<-base_tablero_dengue_final %>% 
  distinct(X, .keep_all = TRUE) %>% 
  select(IDENTIFICADOR,ENTIDAD,MUNICIPIO,NOMBRE_ENT,NOMBRE_MUN,POB_TOTAL,POB_TOTAL_2021,REPORTADOS_TOTALES,CONFIRMADOS_TOTALES,DEFUNCIONES_CONF,
         DESCARTADOS_TOTALES,PROBABLES_TOTALES,PORCENTAJE_POSITIVIDAD,PROB_ESTIMADOS,TOTAL_ESTIMADOS,PRUEBA_POSITIVA_1A4,
         SERO_1,SERO_2,SERO_3,SERO_4,INC_ACUM,TASA_MORT,REPORTADOS_TOTALES_2021,CONFIRMADOS_TOTALES_2021,DEFUNCIONES_CONF_2021,
         DESCARTADOS_TOTALES_2021,PROBABLES_TOTALES_2021,PORCENTAJE_POSITIVIDAD_2021,PROB_ESTIMADOS_2021,TOTAL_ESTIMADOS_2021,PRUEBA_POSITIVA_1A4_2021,
         SERO_1_2021,SERO_2_2021,SERO_3_2021,SERO_4_2021,INC_ACUM_2021,TASA_MORT_2021,INDICE_REZAGO,INDICE_MARG,VUL1_E) %>%
  mutate(TASA_MORT= replace(TASA_MORT,is.na(TASA_MORT),0)) %>% 
  mutate(INC_ACUM= replace(INC_ACUM,is.na(INC_ACUM),0)) %>% 
  mutate(TASA_MORT_2021=replace(TASA_MORT_2021,is.na(TASA_MORT_2021),0)) %>% 
  mutate(INC_ACUM_2021=replace(INC_ACUM_2021,is.na(INC_ACUM_2021),0))


base_tablero_dengue_final<-base_tablero_dengue_final %>% 
  mutate(TASA_MORT_ESTR= replace(TASA_MORT,is.na(TASA_MORT),0)) %>% 
  mutate(INC_ACUM_ESTR= replace(INC_ACUM,is.na(INC_ACUM),0)) %>% 
  mutate(TASA_MORT_2021_ESTR=replace(TASA_MORT_2021,is.na(TASA_MORT_2021),0)) %>% 
  mutate(INC_ACUM_2021_ESTR=replace(INC_ACUM_2021,is.na(INC_ACUM_2021),0))

#x<-base_tablero_dengue_final$INC_ACUM_ESTR
#y<-base_tablero_dengue_final$TASA_MORT_ESTR
#strata.cumrootf(x, CV=0.05, Ls=4)
#strata.cumrootf(y, CV=0.05, Ls=3)
#quantile(base_tablero_dengue_final$TASA_MORT_ESTR)
#max()
base_tablero_dengue_final$INC_ACUM_GPO<-""
base_tablero_dengue_final$INC_ACUM_GPO[base_tablero_dengue_final$INC_ACUM_ESTR<1273.44]<-3
base_tablero_dengue_final$INC_ACUM_GPO[base_tablero_dengue_final$INC_ACUM_ESTR<254.49]<-2
base_tablero_dengue_final$INC_ACUM_GPO[base_tablero_dengue_final$INC_ACUM_ESTR<84.83]<-1
base_tablero_dengue_final$INC_ACUM_GPO[base_tablero_dengue_final$INC_ACUM_ESTR<21.21]<-0
base_tablero_dengue_final$INC_ACUM_GPO<-factor(base_tablero_dengue_final$INC_ACUM_GPO,levels = c(0,1,2,3),labels = c("Baja","Media","Alta","Muy alta")) 

base_tablero_dengue_final$TASA_MORT_GPO<-""
base_tablero_dengue_final$TASA_MORT_GPO[base_tablero_dengue_final$TASA_MORT_ESTR<82.17]<-1
base_tablero_dengue_final$TASA_MORT_GPO[base_tablero_dengue_final$TASA_MORT_ESTR<2.71]<-0
base_tablero_dengue_final$TASA_MORT_GPO<-factor(base_tablero_dengue_final$TASA_MORT_GPO,levels = c(0,1),labels = c("Baja","Alta")) 

#WRITE CSV####
write.csv(base_tablero_dengue_final,"BASE_TABLERO_DENGUE_2020_2021.csv")
