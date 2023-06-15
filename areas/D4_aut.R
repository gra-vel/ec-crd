#================================================================
# Área: Seguridad interna
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#========================= 4001 =================================

#' Homicidios intencionales
#' Nota: error al leer archivo se soluciona abriendo archivo de fuente y guardandolo de nuevo. No es necesario 
#' realizar ningún cambio.
#' @param archivo string, ubicación y nombre de archivo (xlsx-csv)
#' @param version numeric, version de documento original. 1 - corresponde a archivo de Ministerio de Gobierno; 2 - corresponde a archivo de Datos Abiertos
#' @param año numeric, año de actualización
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_homicidios <- function(archivo, version, año, canton = FALSE, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(4001)
  
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    columnas_orig <- c("año", "mes", "provincia", "cantón")
    columnas_final <- c("Año", "Mes", "Provincia", "Cantón")
    suma_desagregacion = c("Mes", "Provincia", "Cantón")
    formato <- "canton_nombre"
  } else {
    columnas_orig <- c("año", "mes", "provincia")
    columnas_final <- c("Año", "Mes", "Provincia")
    suma_desagregacion = c("Mes", "Provincia")
    formato <- "provincia_nombre"
  }
  
  
  # Diferencia entre versiones
  if (version == 1) {
    
    # Leer archivo con nueva información (versión ministerio de gobierno)
    archivo_inicial <- read_excel(paste0("source data/", archivo),
                                  sheet = "HOMICIDIOS INTENCIONALES",
                                  skip = 3)
    names(archivo_inicial) <- tolower(names(archivo_inicial))
    
    archivo_inicial <- archivo_inicial %>%
      rename(homicidios = `# homicidios`) %>%
      filter(!is.na(mes)) %>%
      mutate(mes = ifelse(mes == "Sep", "Sept", mes), # locale reconoce abreviatura de septiembre como Sept y no Sep
             Fecha = parse_date(paste0(mes, ".", año), "%b%Y", locale = locale("es")), # locale necesita que abreviatura de mes termine con un punto
             mes = month(ymd(Fecha)))
    
  } else if (version == 2) {
        
    # Leer archivo con nueva información (versión datos abiertos)
    archivo_inicial <- read_delim(paste0("source data/", archivo),
                                  locale = locale(encoding = 'latin1'),
                                  delim = ";")
    names(archivo_inicial) <- tolower(names(archivo_inicial))

    archivo_inicial <- archivo_inicial %>%
      rename(`tipo de muerte` = `tipo muert.`,
             `tipo de arma` = `tipo arma`,
             mes = mes_i,
             año = anio_i) %>%
      mutate(homicidios = 1) %>%
      select(`tipo de muerte`, provincia, cantón, mes, año, `tipo de arma`, sexo, homicidios) %>%
      filter(!is.na(mes)) %>%
      mutate(Fecha = parse_date(paste0(mes, año), "%B%Y", locale = locale("es")),
             mes = month(ymd(Fecha)))
    
  }
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    rename_at(vars(all_of(columnas_orig)), ~columnas_final) %>%
    filter(Año == año) %>%
    group_by(across(all_of(suma_desagregacion))) %>%
    summarise(`Dato Numérico` = sum(homicidios), .groups = 'drop') %>%
    group_by(Mes) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~"Total Nacional"),
                        .groups = 'drop')) %>%
    filter(!(Provincia %in% c('ZONA NO DELIMITADA', 'ZONAS NO DELIMITADAS'))) %>%
    mutate(Año = año)


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato, formato)
  if ("Cantón" %in% (names(nuevo_dato_geografico))) {
    print("Valores no encontrados para cantones")
    print(nuevo_dato_geografico %>% select(Provincia, Cantón) %>% filter(str_detect(Cantón, "_sn$")))
  }
  

  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico %>%
                     arrange(desc(Mes), `Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(4001, ind, guardar)

  ind
  
}

#================================================================

#========================= 4002 / 4003 ==========================

#' Personas detenidas / Personas detenidas por tráfico ilícito de sustancias catalogadas sujetas a fiscalización
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param version numeric, version de documento original. 1 - corresponde a archivo de Ministerio de Gobierno; 2 - corresponde a archivo de Datos Abiertos
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones 
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_detenidos <- function(archivo, version, codigo_indicador, año, canton = FALSE, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    suma_desagregacion <- c("Mes", "Provincia", "Cantón")
    formato <- "canton_nombre"
  } else {
    suma_desagregacion <- c("Mes", "Provincia")
    formato <- "provincia_nombre"
  }
  
  
  # Dato según codigo_indicador
  if (codigo_indicador == 4002){
    infr <- '[a-zA-Z]'
  } else if (codigo_indicador == 4003) {
    infr <- 'TRÁFICO ILÍCITO DE SUSTANCIAS CATALOGADAS SUJETAS A FISCALIZACIÓN'
  }
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = as.character(año))
  
  if (version == 2){
    archivo_inicial <- archivo_inicial %>%
      mutate(Zona = "0000") %>%
      rename(Edad = edad,
             Sexo = sexo,
             #Nacionalidad = nacionalidad,
             Cantón = nombre_canton,
             Subzona = nombre_provincia,
             `Fecha de Detención` = fecha_detencion_aprehension)
  }
  
  
  # Cambios en fecha y provincia
  nuevo_dato <- archivo_inicial %>%
    mutate(Mes = month(ymd(`Fecha de Detención`)),
           Año = year(ymd(`Fecha de Detención`)),
           Subzona = ifelse(Subzona == 'DMQ', 'PICHINCHA', Subzona),
           Subzona = ifelse(Subzona == 'DMG', 'GUAYAS', Subzona)) %>%
    select(-c(Zona, `Fecha de Detención`, Edad, nacionalidad)) %>%
    rename(Provincia = Subzona,
           Infraccion = presunta_subinfraccion)
  
  
  # Extraer dato numérico
  nuevo_dato <- nuevo_dato %>%
    filter(stringr::str_detect(Infraccion, infr)) %>%
    filter(!(Provincia %in% c('MAR TERRITORIAL'))) %>%
    group_by(across(all_of(suma_desagregacion))) %>%
    mutate(detenido = 1) %>%
    summarise(`Dato Numérico` = sum(detenido), .groups = 'drop') %>%
    group_by(Mes) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~ "Total Nacional"),
                        .groups = 'drop')) %>%
    mutate(Año = año)
  
  if (canton == TRUE){
    nuevo_dato <- nuevo_dato %>%
      filter(!(Cantón %in% c('MAR TERRITORIAL')))
  }
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato, formato)
  if ("Cantón" %in% (names(nuevo_dato_geografico))) {
    print("Valores no encontrados para cantones")
    print(nuevo_dato_geografico %>% select(Provincia, Cantón) %>% filter(str_detect(Cantón, "_sn$")))
  }


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico %>%
                     arrange(desc(Mes), `Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#================== 4004 / 4005 / 4006 ==========================

#' Denuncias de delitos de mayor incidencia / Denuncias de robos / Denuncias de violaciones
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_denuncias <- function(archivo, codigo_indicador, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Dato según codigo_indicador
  if (codigo_indicador == 4004){
    nom_denuncia <- '[a-zA-Z]'
  } else if (codigo_indicador == 4005) {
    nom_denuncia <- 'Robo a personas'
  } else if (codigo_indicador == 4006) {
    nom_denuncia <- 'Violaciones'
  }
    
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "4.- denuncias_absolutos",
                                skip = 3)
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    fill(Provincia, .direction = 'down') %>%
    filter(str_detect(`Delitos de mayor incidencia`, nom_denuncia)) %>%
    pivot_longer(!c(Provincia, `Delitos de mayor incidencia`), names_to = 'Fecha', values_to = 'Dato Numérico') %>%
    mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30"),
           Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha)),
           Provincia = ifelse(Provincia == 'Nacional', 'Total Nacional', Provincia)) %>%
    select(-c(Fecha, `Delitos de mayor incidencia`)) %>%
    group_by(Año, Mes, Provincia) %>%
    summarise(`Dato Numérico` = sum(`Dato Numérico`), .groups = 'drop') %>%
    filter(Año == año)
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, 'provincia_nombre')


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia %>%
                     arrange(desc(Mes), `Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#========================= 4007 / 4008 ==========================

#' Promedio de población penitenciaria / Porcentaje de hacinamiento
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_snai <- function(archivo, codigo_indicador, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Dato según codigo_indicador
  if (codigo_indicador == 4007){
    nom_datos <- c("TOTAL PPL \r\r\n (f)=c+d+e", "PPL HOMBRES", "PPL MUJERES") #TOTAL PPL\r\n(f)=c+d+e  /  TOTAL PPL \r\r\n (f)=c+d+e
    nom_columnas <- c('Mes', 'Total', 'Hombres', 'Mujeres')
  } else if (codigo_indicador == 4008) {
    nom_datos <- c("% HACINAMIENTO* \r\r\n (i)=((f/g)-1)*100") #% HACINAMIENTO*\r\n(i)=((f/g)-1)*100  /  % HACINAMIENTO* \r\r\n (i)=((f/g)-1)*100
    nom_columnas <- c('Mes', 'Total')
  }
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = paste0("Resumen PPL ", as.character(año)),
                                skip = 3)
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    filter(!is.na(`PPL HOMBRES`)) %>%
    slice(which(`FECHA DE REPORTE`=='Enero'):(n()-1)) %>%
    select(`FECHA DE REPORTE`, all_of(nom_datos))

  names(nuevo_dato) <- nom_columnas
  
  nuevo_dato <- nuevo_dato %>%
    pivot_longer(!c('Mes'), names_to = 'Sexo', values_to = 'Dato Numérico') %>%
    mutate(Fecha = parse_date(paste0(Mes, año), "%B%Y", locale = locale("es")),
           Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha)),
           `Dato Numérico` = as.numeric(`Dato Numérico`))

    if (codigo_indicador == 4008){
    nuevo_dato <- nuevo_dato %>%
      mutate(`Dato Numérico` = `Dato Numérico` * 100)
  }


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato %>%
                     arrange(desc(Mes)),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#========================= 4009 =================================

#' Armas ilícitas decomisadas
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_armas <- function(archivo, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(4009)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo),
                                locale = locale(encoding = 'latin1'),
                                delim = ";")
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    mutate(Fecha = as.Date(fecha_evento, "%d/%m/%Y"),
           Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha))) 
  
  nombre_mes <- lubridate::month(max(nuevo_dato$Mes), label=TRUE, abbr = FALSE) # mes de actualizacion

  nuevo_dato <- nuevo_dato %>%
    mutate(nombre_provincia = ifelse(nombre_provincia == 'DMQ', 'PICHINCHA', nombre_provincia),
           nombre_provincia = ifelse(nombre_provincia == 'DMG', 'GUAYAS', nombre_provincia)) %>%
    count(Año, nombre_provincia) %>% # agregar mes para dato mensual
    rename('Provincia' = 'nombre_provincia',
           'Dato Numérico' = 'n') %>%
    group_by(Año) %>% # agregar mes para dato mensual
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~ "Total Nacional"),
                        .groups = 'drop')) %>%
    filter(!(Provincia %in% c('MAR TERRITORIAL')))


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, 'provincia_nombre')


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(as_tibble(nuevo_dato_provincia) %>%
                     arrange(`Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo() %>%
    mutate(`Dato Cualitativo` = ifelse(Año == año, str_remove(`Dato Cualitativo`, " Agregado.*"), `Dato Cualitativo`)) %>%
    mutate(`Dato Cualitativo` = ifelse(Año == año, paste0(`Dato Cualitativo`, ' Agregado a ', nombre_mes, ' de ', as.character(año)),
                                       `Dato Cualitativo`))


  # guardar indicador
  guardar_indicador(4009, ind, guardar)

  ind
  
}

#================================================================

#========================= 4010 =================================

#' Cantidad depositada de sustancias sujetas a fiscalización
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_drogas <- function(archivo, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(4010)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "Hoja1")
  
  
  # Extraer dato numérico
  nuevo_dato <- archivo_inicial %>%
    mutate(Fecha = as.Date(FECHA_DEPOSITO, "%Y-%m/-%d/", tz='EST'),
           Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha)))
  
  nombre_mes <- lubridate::month(max(nuevo_dato$Mes), label=TRUE, abbr = FALSE) # mes de actualización
    
  nuevo_dato <- nuevo_dato %>%  
    group_by(PROVINCIA, Año) %>%
    summarize(`Dato Numérico` = sum(PESO_NETO)/1000, .groups = 'drop') %>%
    group_by(Año) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~ "Total Nacional"),
                        .groups = 'drop')) %>%
    rename(Provincia = PROVINCIA)
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato, 'provincia_nombre')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(as_tibble(nuevo_dato_provincia) %>%
                     arrange(`Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>%
    format_nuevo() %>%
    mutate(`Dato Cualitativo` = str_remove(`Dato Cualitativo`, " Agregado.*")) %>%
    mutate(`Dato Cualitativo` = ifelse(Año == año, paste0(`Dato Cualitativo`, ' Agregado a ', nombre_mes, ' de ', as.character(año)), 
                                       `Dato Cualitativo`))
  
  
  # guardar indicador
  guardar_indicador(4010, ind, guardar)
  
  ind
  
}
