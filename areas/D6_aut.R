#================================================================
# Área: Salud pública
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#===================== 6001 / 6004 / 6005 =======================

#' Defunciones generales, Tasa de mortalidad en menores de 5 años, Mortalidad materna
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_REDG <- function(archivo, codigo_indicador, año, guardar=FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)


  # Archivo según indicador
  if (codigo_indicador == 6001){
    archivo_csv <- "1.2.5.csv"
    columna_n <- 2
  } else if (codigo_indicador == 6004) {
    archivo_csv <- "1.2.4.csv"
    columna_n <- 0
  } else if (codigo_indicador == 6005) {
    archivo_csv <- "1.2.2.csv"
    columna_n <- 0
  } 


  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo, archivo_csv),
                                locale = locale(encoding = 'latin1'), # UTF-8
                                delim = ",")
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    filter(!is.na(...2)) %>%
    filter(!(...1 %in% c('Región Sierra','Región Costa','Región Amazónica','Región Insular','Exterior','Región y provincia',
                         'Por provincia de residencia','Zonas no delimitadas','Reagión Amazónica'))) %>%
    select_if(~mean(is.na(.)) < 0.2) %>% # elimina columna según porcentaje de missing values
    select(1, (length(.)-columna_n):length(.)) %>% # selecciona últimas tres columnas
    mutate(...1 = ifelse(is.na(...1), 'Provincia', ...1),
           ...1 = ifelse(str_detect(...1, '^Total'), 'Total Nacional', ...1)) %>%
    `colnames<-` (.[1, ]) %>% # primera fila se convierte en nombre de columnas
    .[-1, ] # elimina primera columna
  
  
  # Correcciones según código de indicador
  if (codigo_indicador == 6001){
    nuevo_dato_provincia <- nuevo_dato %>%
      gather(key = 'Sexo', value = 'Dato Numérico', 2:4) %>%
      mutate(`Dato Numérico` = as.numeric(str_replace(`Dato Numérico`, ',', '')),
             Año = año)
  } else {
    nuevo_dato_provincia <- nuevo_dato %>%
      `colnames<-` (c('Provincia', 'Dato Numérico')) %>%
      mutate(`Dato Numérico` = as.numeric(`Dato Numérico`),
             Año = año)
  } 


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato_provincia, 'provincia_nombre')


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind

}

#================================================================

#======================== 6006 / 6007 ===========================

#' Tasa de muertes por suicidio, Tasa de muertes por VIH
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_REDG2 <- function(archivo, codigo_indicador, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador) %>%
    filter(Año < (año-1))
  

  # Archivo según indicador
  if (codigo_indicador == 6006){
    causa <- "63 Lesiones autoinflingidas intencionalmente (Suicidio)"
  } else if (codigo_indicador == 6007) {
    causa <- "7 Enfermedad por virus de la inmunodeficiencia (VIH)"
  }


  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo),
                                locale = locale(encoding = 'latin1'), # UTF-8
                                delim = ",")


  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    filter(!is.na(...3)) %>%
    select_if(~mean(is.na(.)) < 0.2) %>% # elimina columna según porcentaje de missing values
    mutate(...1 = ifelse(is.na(...1), "Causa", ...1)) %>%
    `colnames<-` (.[1, ]) %>% # primera fila se convierte en nombre de columnas
    .[-1, ] %>% # elimina primera columna
    filter(Causa == causa) %>%
    gather("Año", "Dato Numérico", -Causa) %>%
    mutate(Año = ifelse(Año == paste0(as.character(año), " (p**)"), as.character(año), Año),
           Año = as.numeric(Año),
           `Dato Numérico` = as.numeric(`Dato Numérico`)) %>%
    filter(Año >= (año-1)) %>%
    select(-Causa)


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#================== 6013 / 6014 / 6015 / 6016 ===================

#' Pruebas positivas de COVID-19, Muertes por COVID-19, Porcentaje de camas ocupadas para hospitalización (MSP), Vacunas COVID-19
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_covid <- function(archivo, codigo_indicador, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  if (codigo_indicador == 6014){
    key_w <- "Total Muertes"
  } else if (codigo_indicador == 6015){
    key_w <- "(MSP) Hospitalización Porcentaje Ocupadas"
  }
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo),
                                locale = locale(encoding = 'utf-8'),
                                delim = ",")
  
  if (codigo_indicador == 6016){
    
    archivo_inicial <-  archivo_inicial %>%
      mutate(Fecha = parse_date(created_at, "%d/%m/%Y"),
             mes = month(ymd(Fecha)),
             año_i = year(ymd(Fecha))) %>%
      rename(total = segunda_dosis) %>%
      group_by(provincia, año_i, mes) %>%
      filter(Fecha == max(Fecha)) %>% ungroup() %>% # para obtener último día del mes
      filter(año_i == año)
    
  } else if(codigo_indicador %in% c(6014,6015)) {
    
    archivo_inicial <-  archivo_inicial %>%
      filter(informacion == key_w) %>%
      gather(Fecha, total) %>%
      filter(total != key_w) %>%
      mutate(Fecha = parse_date(Fecha, "%d/%m/%Y"),
             mes = month(ymd(Fecha)),
             año_i = year(ymd(Fecha)),
             provincia = "Total Nacional",
             total = as.numeric(total)) %>%
      group_by(año_i, mes) %>%
      filter(Fecha == max(Fecha)) %>% ungroup() %>% # para obtener último día del mes
      filter(año_i == año) %>%
      filter(mes != max(mes))
             
  } 

  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    rename(Mes = mes,
           Provincia = provincia,
           `Dato Numérico` = total) %>%
    group_by(Mes) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~"Total Nacional"),
                        .groups = 'drop')) %>%
    mutate(Año = año) %>%
    select(Provincia, `Dato Numérico`, Mes, Año) %>% unique()


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, 'provincia_nombre')


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(as_tibble(nuevo_dato_provincia) %>%
                     arrange(desc(Mes), `Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind

}
