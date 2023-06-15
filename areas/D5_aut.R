#================================================================
# Área: Justicia y estado de derecho
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#========================= 5004 =================================

#' Índice de percepción de la corrupción
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_TI <- function(archivo, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(5004)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                skip = 1,
                                sheet = paste0("CPI ", as.character(año)))
  
  
  # Extraer dato por provincia
  nuevo_dato <- archivo_inicial %>%
    filter(ISO3 == "ECU") %>%
    rename("Dato Numérico" = paste0("CPI score ", as.character(año))) %>%
    mutate(Año = año) %>%
    select(`Dato Numérico`, Año)
    

  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
  

  # guardar indicador
  guardar_indicador(5004, ind, guardar)

  ind
  
}

#================================================================

#================== 5005 / 5006 / 5007 ==========================

#' Tasa de resolución / Tasa de pendencia / Tasa de congestión
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_CJ <- function(archivo, codigo_indicador, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  if (codigo_indicador == 5005){
    nombre_hoja = "Tasa_resolución"
  } else if (codigo_indicador == 5006) {
    nombre_hoja = "Tasa_pendencia"
  } else if (codigo_indicador == 5007) {
    nombre_hoja = "Tasa_congestión"
  }
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = nombre_hoja)
  
  
  # Extraer dato por sexo y por área
  nuevo_dato <- archivo_inicial %>%
    filter(!is.na(...3)) %>%
    select(1, length(archivo_inicial))
  
  names(nuevo_dato) <- c("Provincia", "Dato Numérico")
    
  nuevo_dato <- nuevo_dato %>%
    filter(!is.na(Provincia)) %>%
    mutate(Año = año)
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, 'provincia_nombre')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia %>%
                     arrange(`Código Provincia`),
                   ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}
