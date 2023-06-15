#================================================================
# Área: Seguridad alimentaria, agricultura y tierra
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#========================= 9004 =================================

#' Índice de precios al consumidor - Alimentos y bebidas no alcohólicas
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_IPC <- function(archivo, codigo_indicador, mes, año, guardar=FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)


  # Crear 'loop' por cada hoja del archivo xls
  for (location in c("1. NACIONAL", "4. Guayaquil", "5. Esmeraldas", "6. Machala", "7. Manta", "8. Sto. Domingo", "9. Quito", "10. Loja", "11. Cuenca", "12. Ambato")) {
    archivo_inicial <- read_excel(paste0("source data/", archivo),
                                  sheet = location,
                                  skip = 4) # saltar filas iniciales


    # Extraer el dato numérico de cada hoja
    nuevo_dato <- archivo_inicial %>%
      select(-c(NIVEL, `Descripción CCIF`)) %>%
      mutate(codigo_ind = case_when(
        `Cód. CCIF` == "01" ~ 9004,
        `Cód. CCIF` == "0111" ~ 9004.1,
        `Cód. CCIF` == "0112" ~ 9004.2,
        `Cód. CCIF` == "0113" ~ 9004.3,
        `Cód. CCIF` == "0114" ~ 9004.4,
        `Cód. CCIF` == "0115" ~ 9004.5,
        `Cód. CCIF` == "0116" ~ 9004.6,
        `Cód. CCIF` == "0117" ~ 9004.7
      )) %>%
      filter(codigo_ind == codigo_indicador) %>% # ***
      pivot_longer(!c(`Cód. CCIF`, codigo_ind), names_to = 'Fecha', values_to = 'Dato Numérico') %>%
      mutate(Fecha = gsub("\\bsep\\b", "sept", Fecha, ignore.case = TRUE),
             Fecha = parse_date(str_to_title(str_replace(Fecha, "-", ".")), "%b%y", locale = locale("es")),
             Mes = month(ymd(Fecha)),
             Año = year(ymd(Fecha)),
             Cantón = case_when(
               location == "1. NACIONAL" ~ 'Total Nacional',
               location == "11. Cuenca" ~ 'Cuenca',
               location == "10. Loja" ~ 'Loja',
               location == "9. Quito" ~ 'Quito',
               location == "12. Ambato" ~ 'Ambato',
               location == "6. Machala" ~ 'Machala',
               location == "5. Esmeraldas" ~ 'Esmeraldas',
               location == "4. Guayaquil" ~ 'Guayaquil',
               location == "7. Manta" ~ 'Manta',
               location == "8. Sto. Domingo" ~ 'Santo Domingo'
             ),
             Provincia = case_when(
               location == "1. NACIONAL" ~ 'Total Nacional',
               location == "11. Cuenca" ~ 'Azuay',
               location == "10. Loja" ~ 'Loja',
               location == "9. Quito" ~ 'Pichincha',
               location == "12. Ambato" ~ 'Tungurahua',
               location == "6. Machala" ~ 'El Oro',
               location == "5. Esmeraldas" ~ 'Esmeraldas',
               location == "4. Guayaquil" ~ 'Guayas',
               location == "7. Manta" ~ 'Manabí',
               location == "8. Sto. Domingo" ~ 'Sto Dgo Tsáchilas'
             )) %>%
      select(-c(Fecha,`Cód. CCIF`)) %>%
      filter((Mes == mes) & (Año == año))

    # Ordenar los datos extraídos. El primer dato corresponde al 'Total Nacional'. Las filas subsiguientes se unen a esta.
    if (location  == "1. NACIONAL"){
      nuevo_dato_mes <- nuevo_dato
    } else {
      nuevo_dato_mes <- rbind(nuevo_dato_mes, nuevo_dato)
    }
  }
  

    # Unir datos extraídos con base de datos geográficos para introducir código de provincia y cantón
    nuevo_dato_mes <- cambio_nombres(nuevo_dato_mes, 'canton_nombre')


    # Datos extraídos se unen a la base importada en el primer paso
    ind <- bind_rows(nuevo_dato_mes, ind) %>%
      format_nuevo()


    # guardar indicador
    guardar_indicador(codigo_indicador, ind, guardar)

    ind

}

#================================================================

#===================== 9005 / 9006 / 9007 =======================

#' Cultivos permanentes / Cultivos transitorios y barbecho / Pastos cultivados
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_ESPAC <- function(archivo, codigo_indicador, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                skip = 8,
                                sheet = 'T1')
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    rename("Provincia" = "...1") %>%
    filter(!is.na(Total),
           !Provincia %in% c('REGIÓN SIERRA','REGIÓN COSTA','REGIÓN AMAZÓNICA','ZONAS NO DELIMITADAS','REGIÓN ORIENTAL')) %>%
    pivot_longer(-Provincia, names_to = "Tipo", values_to = "Dato Numérico") %>%
    mutate(Año = año,
           Indicador = case_when(
      Tipo == "Cultivos Permanentes" ~ 9005,
      Tipo == "Cultivos Transitorios y Barbecho" ~ 9006,
      Tipo == "Pastos Cultivados" ~ 9007
    )) %>%
    filter(Indicador == codigo_indicador) %>%
    select(-c(Tipo, Indicador))
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, 'provincia_nombre')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#========================= 9014 =================================

#' Precio promedio ponderado de urea
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param mes numeric, mes de actualización }
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_SIPA <- function(archivo, mes, año, guardar=FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(9014)


  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                skip = 7,
                                sheet = 'Historico mensual',
                                col_types = 'text')


  # Extraer el dato numérico de cada hoja
  nuevo_dato <- archivo_inicial %>%
    filter(`Ingrediente Activo` == 'Urea') %>%
    pivot_longer(-c(Grupo, `Ingrediente Activo`, Concentración, Presentación), names_to = 'Fecha', values_to = 'Dato Numérico') %>%
    filter(!(Fecha %in% c('ene-22**', 'feb-22**', 'mar-22**', 'abr-22**', 'may-22**', 'jun-22**', 'jul-22**', 'ago-22**'))) %>% # ver base de datos para explicación
    mutate(Fecha = as.Date(as.numeric(Fecha), origin = '1899-12-30'),
           Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha)),
           `Dato Numérico` = as.numeric(`Dato Numérico`)) %>%
    select(-c(Fecha, Grupo, `Ingrediente Activo`, Concentración, Presentación)) %>%
    filter((Mes == mes) & (Año == año))


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(9014, ind, guardar)

  ind

}
