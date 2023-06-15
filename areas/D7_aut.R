#================================================================
# Área: Infraestructura y acceso a servicios sociales
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#================== 7001 / 7002 / 7003 / 7004 ===================

#' Instituciones educativas con sostenimiento fiscal / Instituciones educativas con sostenimiento fiscomisional /
#' Instituciones educativas con sostenimiento municipal / Instituciones educativas con sostenimiento particular
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_MINEDUC <- function(archivo, codigo_indicador, año, canton=FALSE, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "Registr_Estudiantes",
                                skip = 11)
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    contar_desagregacion <- c("Periodo", "Provincia", "Cantón", "Zona Inec")
    suma_desagregacion <- c("Año", "Provincia", "Cantón")
    formato <- 'canton_nombre'
  } else {
    contar_desagregacion <- c("Periodo", "Provincia", "Zona Inec")
    suma_desagregacion <- c("Año", "Provincia")
    formato <- 'provincia_nombre'
  }
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    mutate(Indicadores_cod = case_when(
      Sostenimiento == 'Fiscal' ~ 7001,
      Sostenimiento == 'Fiscomisional' ~ 7002,
      Sostenimiento == 'Municipal' ~ 7003,
      Sostenimiento == 'Particular' ~ 7004
    )) %>%
    filter(Indicadores_cod == codigo_indicador) %>%
    mutate(`Zona Inec` = ifelse(`Zona Inec` == 'Urbana', 'Urbano', `Zona Inec`)) %>%
    group_by(across(all_of(contar_desagregacion))) %>%  # ***
    summarize(`Dato Numérico` = n(), .groups = 'drop') %>%
    mutate(Periodo = as.numeric(str_remove(Periodo, '\\-(.*)'))) %>% # quitar caracteres después de guión
    rename(Año = Periodo,
           Área = `Zona Inec`) %>%
    group_by(across(all_of(suma_desagregacion))) %>% # ***
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(Área, ~ "Total"),
                        .groups = 'drop')) %>%
    group_by(Año, Área) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(Provincia, ~ "Total Nacional"),
                        .groups = 'drop'))
  
  if (canton == TRUE){
    nuevo_dato <- nuevo_dato %>%
      mutate(Cantón = ifelse(is.na(Cantón), "Total Nacional", Cantón))
  }


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, formato) %>%
    filter(Año == año,
           Provincia != 'Zona No Delimitada')
  


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#===================== 7013 / 7014 / 7016 =======================

#' Hogares con acceso a internet / Proporción de la población con acceso a electricidad /
#' Índice de percepción de la calidad de los servicios públicos en general
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_ENMH <- function(archivo, codigo_indicador, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Archivo según indicador
  if (codigo_indicador == 7013){
    archivo_csv <- "1.3.csv"
    skip_n <- 3
  } else if (codigo_indicador == 7014) {
    archivo_csv <- "1.7.csv"
    skip_n <- 7
  } else if (codigo_indicador == 7016) {
    archivo_csv <- "2.2.csv"
    skip_n <- 3
  } 
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo, archivo_csv),
                                locale = locale(encoding = 'latin1'), # UTF-8
                                delim = ";",
                                skip = skip_n)
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    fill("Periodo ", .direction = "down") %>%
    rename("Año" = "Periodo ",
           "Área" = "Desagregación",
           "Dato Numérico" = "Estimación\n(Porcentaje)") %>%
    filter(Año == 2020) %>%
    select(Año, Área, `Dato Numérico`) %>%
    mutate(Área = ifelse(Área %in% c("Nacional ", "Nacional"), "Total", Área),
           Área = ifelse(Área %in% c("Urbana ", "Urbana"), "Urbano", Área),
           Área = ifelse(Área %in% c("Rural ", "Rural"), "Rural", Área))

  
  if (codigo_indicador %in% c(7013, 7016)) {
    nuevo_dato <- nuevo_dato %>%
      mutate(`Dato Numérico` = `Dato Numérico`/100)
  } else {
    nuevo_dato <- nuevo_dato %>%
      mutate(`Dato Numérico` = str_replace(`Dato Numérico`, ",", ""),
             `Dato Numérico` = as.numeric(`Dato Numérico`)) %>%
      mutate(`Dato Numérico` = `Dato Numérico`/100)
  }
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}
