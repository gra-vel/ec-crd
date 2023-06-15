#================================================================
# Área: Igualdad de género
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#========================= 10001 ================================

#' Femicidios absolutos
#' Nota: Código corresponde a versión 2 del protocolo de manejo de información
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param mes numeric, mes de actualización 
#' @param año numeric, año de actualización 
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones 
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_CJ_fem <- function(archivo, mes, año, canton=FALSE, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(10001)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo),
                                locale = locale(encoding = 'UTF-8'), # UTF-8 / latin1
                                delim = ";")
  
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    formato <- 'canton_nombre'
    suma_desagregacion = c("Año", "Mes", "Provincia", "Cantón")
  } else {
    formato <- 'provincia_nombre'
    suma_desagregacion = c("Año", "Mes", "Provincia")
  }
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    filter(`Año I` == año) %>%
    select('Año I', 'Provincia', 'Canton', 'Tipo muerte SRMCE2', 'Cuenta Victima', 'Mes I') %>%
    mutate(Mes = month(ymd(parse_date(paste0(`Mes I`,`Año I`), "%B%Y", locale = locale("es")))),
           Mes = ifelse(is.na(Mes), as.numeric(`Mes I`), Mes)) %>%
    rename(Año = `Año I`,
           Cantón = Canton) %>%
    filter(`Tipo muerte SRMCE2` == 'FEMICIDIO',
           Año == año,
           Mes == mes)  %>%
    group_by(across(all_of(suma_desagregacion))) %>%
    summarise(`Dato Numérico` = sum(`Cuenta Victima`), .groups = 'drop') %>%
    group_by(Año, Mes) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~"Total Nacional"),
                        .groups = 'drop')) %>%
    ungroup()
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato, formato)


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico, ind) %>% 
    format_nuevo()


  # guardar indicador
  guardar_indicador(10001, ind, guardar)

  ind
  
}
