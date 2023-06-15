#================================================================
# Área: Cohesión social, equidad y no discriminación
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")
options(dplyr.summarise.inform = FALSE) # elimina mensaje sobre .groups en 2008 / 2009 / 2010 / 2011

#================================================================

#================== 2001 / 2002 / 2003 ==========================

#' Pobreza
#' Pobreza por ingresos / Pobreza extrema por ingresos / Índice de Gini
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_pobreza <- function(archivo, codigo_indicador, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Hojas de trabajo de acuerdo a código de indicador
  if (codigo_indicador == 2001){
    area_indicador <- c("1.1.1.pobre_nacional", "1.1.2.pobre_urbana", "1.1.3.pobre_rural")
    columna_datos = 'Incidencia (1)'
  } else if (codigo_indicador == 2002) {
    area_indicador <- c("1.2.1.extpob_nacional", "1.2.2.extpob_urbana", "1.2.3.extpob_rural")
    columna_datos = 'Incidencia (1)'
  } else if (codigo_indicador == 2003) {
    area_indicador <- c("2.1. Desigualdad_nacional ", "2.2. Desigualdad_urbana", "2.3. Desigualdad_rural ")
    columna_datos = 'Índice de Gini'
  }
  
  
  # Crear 'loop' por cada hoja del archivo xlsx, según código de indicador
  for (area in area_indicador){
    
    archivo_inicial <- read_excel(paste0("source data/", archivo),
                                  sheet = area,
                                  skip = 8)
    
    # Extraer el dato numérico de cada hoja
    nuevo_dato <- archivo_inicial %>%
      rename('Mes' = 'Período',
             'Año' = '...2',
             'Dato Numérico' = all_of(columna_datos)) %>%
      filter(!is.na(Año)) %>%
      mutate(Área = str_replace_all(str_extract(area, "(?<=\\_)[a-z].*"), ' ', ''),
              `Dato Numérico` = as.numeric(str_replace_all(`Dato Numérico`, '-', '')),
              Año = as.numeric(str_replace_all(Año, '\\s\\([\\d]\\)', '')) ,
              Mes = case_when(
                Mes == 'Junio' ~ 6,
                Mes == 'Diciembre' ~ 12)) %>%
      fill('Mes', .direction = 'down') %>%
      select(Mes, Año, Área, `Dato Numérico`)
    
    # Ordenar los datos extraídos. El primer dato corresponde al 'Total Nacional'. Las filas subsiguientes se unen a esta.
    if (area  == area_indicador[1]){
      nuevo_dato_semestre <- nuevo_dato
    } else {
      nuevo_dato_semestre <- rbind(nuevo_dato_semestre, nuevo_dato) %>%
        arrange(desc(Año), desc(Mes))
    }
  }
  
  
  # Recolectar dato por cada semestre
  nuevo_dato_semestre <- nuevo_dato_semestre %>%
    mutate(Área = case_when(
      Área == 'nacional' ~ 'Total',
      Área == 'urbana' ~ 'Urbano',
      Área == 'rural' ~ 'Rural')) %>%
    filter(Año == año & Mes == mes)
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_semestre, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#================== 2004 / 2005 =================================

#' Pobreza multidimensional
#' Pobreza multidimensional / Pobreza extrema multidimensional
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_pobreza_multi <- function(archivo, codigo_indicador, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "1.1. Serie_componentes_IPM",
                                skip = 4)
  
  
  # Extraer el dato numérico
  nuevo_dato <- archivo_inicial %>%
    rename('Área' = ...1,
           'Año' = ...2,
           Dato_2005 = `Estimación\r\npuntual...4`,
           Dato_2004 = `Estimación\r\npuntual...7`) %>%
    select('Área', 'Año', paste0('Dato_', as.character(codigo_indicador))) %>%
    filter(!is.na(Año)) %>%
    fill(Área) %>%
    mutate(Área = ifelse(Área == 'Nacional', 'Total', Área)) %>%
    rename(`Dato Numérico` = paste0('Dato_', as.character(codigo_indicador))) %>%
    filter(Año == año)
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
    
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#================== 2008 / 2009 / 2010 / 2011 ===================

#' Usuarios de programas de inclusión social
#' Desarrollo Infantil Integral / Adultos mayores / Personas con discapacidad / Protección Especial
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización 
#' @param año numeric, año de actualización 
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones 
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_inclusion_MIES <- function(archivo, codigo_indicador, mes, año, canton=FALSE, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "Base")
  
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    grupo_desagregacion <- c("id_provincia", "id_canton", "Hombre", "Mujer\r\n(excluye mujeres gestantes)", "Total\r\n(excluye mujeres gestantes)")
    suma_desagregacion = c("id_provincia", "id_canton")
    columnas_orig <- c("id_provincia", "id_canton", "Mujer\r\n(excluye mujeres gestantes)", "Total\r\n(excluye mujeres gestantes)")
    columnas_final <- c("Código Provincia", "Código Cantón", "Mujer", "Total")
    columnas_pivot <- c("Código Provincia", "Código Cantón")
    formato <- 'canton_codigo'
  } else {
    grupo_desagregacion <- c("id_provincia", "Hombre", "Mujer\r\n(excluye mujeres gestantes)", "Total\r\n(excluye mujeres gestantes)")
    suma_desagregacion = c("id_provincia")
    columnas_orig <- c("id_provincia", "Mujer\r\n(excluye mujeres gestantes)", "Total\r\n(excluye mujeres gestantes)")
    columnas_final <- c("Código Provincia", "Mujer", "Total")
    columnas_pivot <- c("Código Provincia")
    formato <- 'provincia_codigo'
  }
  
  
  # Filtrar datos dependiendo del código de indicador señalado como argumento
  nuevo_dato <- archivo_inicial %>%
    select(c(Servicio, Provincia, id_provincia, Canton, id_canton, Modalidad, Hombre, `*Area urbana rural`,
             `Afroecuatoriano Hombre`, `Afroecuatoriano Mujer`, `Indígena Hombre`, `Indígena Mujer`, `Mestizo Hombre`, `Mestizo Mujer`, `Blanco Hombre`, `Blanco Mujer`, `Otro Hombre`, `Otro Mujer`,
             `Mujer\r\n(excluye mujeres gestantes)`, `Total\r\n(excluye mujeres gestantes)`, Venezuela, `Montuvio Hombre`, `Montuvio Mujer`)) %>%
    # para códigos geográficos
    mutate(id_provincia = as.character(str_pad(id_provincia, 2, pad = "0")),
           id_canton = as.character(str_pad(id_canton, 4, pad = "0"))) %>%
    # reemplazar nombre de servicios con códigos
    mutate(Servicio = case_when(
      Servicio == 'DESARROLLO INFANTIL INTEGRAL' ~ 2008,
      Servicio == 'PERSONAS ADULTOS MAYORES - PEJ' ~ 2009,
      Servicio == 'PERSONAS CON DISCAPACIDAD' ~ 2010,
      Servicio == 'PROTECCION ESPECIAL' ~ 2011
    )) %>%
    filter(Servicio == codigo_indicador) %>%
    select(all_of(grupo_desagregacion)) 
    
  
  # Recolectar datos por desagregación geográfica (provincias)
  nuevo_dato_geografico <- nuevo_dato %>%
    group_by(across(all_of(suma_desagregacion))) %>%
    # para eliminar valores NA
    mutate(Hombre = ifelse(is.na(Hombre), 0, Hombre),
           `Mujer\r\n(excluye mujeres gestantes)` = ifelse(is.na(`Mujer\r\n(excluye mujeres gestantes)`), 0, `Mujer\r\n(excluye mujeres gestantes)`),
           `Total\r\n(excluye mujeres gestantes)` = ifelse(is.na(`Total\r\n(excluye mujeres gestantes)`), 0, `Total\r\n(excluye mujeres gestantes)`)) %>%
    # sumatorias
    summarise(Hombre = sum(Hombre),
              `Mujer\r\n(excluye mujeres gestantes)` = sum(`Mujer\r\n(excluye mujeres gestantes)`),
              `Total\r\n(excluye mujeres gestantes)` = sum(`Total\r\n(excluye mujeres gestantes)`)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"00"))) %>%
    rename_at(vars(all_of(columnas_orig)), ~columnas_final) %>%
    # cambio en forma de base
    pivot_longer(!all_of(columnas_pivot), names_to = "Sexo", values_to = "Dato Numérico") %>%
    # variables de tiempo
    mutate(Mes = mes,
           Año = año) 
  

  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato_geografico, formato)
  
  # Corrección para cantones = 0000
  if (canton == TRUE){
    nuevo_dato_geografico <- nuevo_dato_geografico %>% ungroup() %>% 
      select(-c(PROVINCIA_CODIGO, Provincia)) %>% 
      mutate(`Código Cantón` = ifelse(`Código Cantón` == "00", "0000", `Código Cantón`),
             Cantón = ifelse(`Código Cantón` == "0000", "Total Nacional", Cantón)) %>%
      cambio_nombres(formato = 'provincia_codigo')
  }
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#========================= 2014 =================================

#' Deuda de gobierno central a IESS
#' @param archivo string, ubicación y nombre de archivo (xls/xlsx)
#' @param mes numeric, mes de actualización 
#' @param año numeric, año de actualización 
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_deuda_IESS <- function(archivo, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(2014)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "BD Interna")
  
  # Simplificar nombres de columnas
  names(archivo_inicial) <- gsub(" ", "", names(archivo_inicial), fixed = TRUE)
  names(archivo_inicial) <- gsub("\n", "", names(archivo_inicial), fixed = TRUE)
  names(archivo_inicial) <- gsub("\r", "", names(archivo_inicial), fixed = TRUE)
  archivo_inicial

  # Encontrar fecha relevante
  if(mes < 12){
    fecha <- format(as.Date(paste0(as.character(año), "-", as.character(mes+1), "-01"), by='months')-1, "%d-%m-%Y")
  } else {
    fecha <- format(as.Date(paste0(as.character(año+1), "-", as.character(1), "-01"), by='months')-1, "%d-%m-%Y") # para considerar cambios de año
  }
  

  # Filtrar y recolectar datos
  nuevo_dato <- archivo_inicial %>%
    select("NombredelAcreedor", paste0("Saldoal", fecha)) %>% # Nombre del Acreedor; Saldo al
    rename("Acreedor" = "NombredelAcreedor",
           "Dato" = paste0("Saldoal", fecha)) %>%
    filter(Acreedor %in% c("IESS", "CONVENIO  PAGO-IESS", "BIESS", "IESS 40%")) %>%
    summarise(`Dato Numérico` = sum(Dato)) %>%
    mutate(`Dato Numérico` = `Dato Numérico`/10^3,
           Mes = mes,
           Año = año)

  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()

  # guardar indicador
  guardar_indicador(2014, ind, guardar)

  ind
  
}

#================================================================

#======= 2017 / 4011 / 4012 / 6009 / 10002 / 10003 / 10004 ======

#' Emergencias ciudadanas
#' Control de manifestaciones y marchas / Seguridad ciudadana / Consumo de sustancias sujetas a fiscalización / Suicidio
#' Violencia intrafamiliar / Violencia contra la mujer o miembros del núcleo familiar física / Violencia contra la mujer o miembros del núcleo familiar sexual
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_ecu911 <- function(archivo, codigo_indicador, mes, año, canton=FALSE, guardar=FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)


  # Leer archivo con nueva información
  archivo_inicial <- read_delim(paste0("source data/", archivo),
                                locale = locale(encoding = 'latin1'), # latin1 / UTF-8
                                # col_types = "c",
                                delim = ";")
  
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    grupo_desagregacion <- c("Cod_Parroquia", "Parroquia")
    contar_desagregacion <- c("provincia", "Código Cantón", "Mes", "Año")
    formato <- 'canton_codigo'
    columnas_orig <- c("provincia", "Código Cantón", "n")
    columnas_final <- c("Provincia", "Código Cantón", "Dato Numérico")
    
    #Prepara código para cantón
    archivo_inicial <- archivo_inicial %>%
      mutate(`Código Cantón` = ifelse(nchar(as.character(Cod_Parroquia)) == 5, paste0("0", as.character(Cod_Parroquia)), as.character(Cod_Parroquia))) %>% # agrega 0 frente al código
      mutate(`Código Cantón` = substring(`Código Cantón`, 1, nchar(`Código Cantón`)-2))
  } else {
    grupo_desagregacion <- c("Canton", "Cod_Parroquia", "Parroquia")
    contar_desagregacion <- c("provincia", "Mes", "Año")
    formato <- 'provincia_nombre'
    columnas_orig <- c("provincia", "n")
    columnas_final <- c("Provincia", "Dato Numérico")
  }
  
  
  # Crear variable para codigo indicador
  if (codigo_indicador == 4011) {
    nuevo_dato <- archivo_inicial %>%
      select(-all_of(grupo_desagregacion)) %>%
      mutate(indicador = case_when(
        Servicio == "Seguridad Ciudadana" ~ 4011))
  } else {
    nuevo_dato <- archivo_inicial %>%
      select(-all_of(grupo_desagregacion)) %>%
      mutate(indicador = case_when(
        Subtipo == "Control de marchas" ~ 2017,
        Subtipo == "Control de manifestaciones" ~ 2017,
        Subtipo == "Consumo de sustancias sujetas a fiscalización" ~ 4012,  
        Subtipo == "Suicidio" ~ 6009,
        Subtipo == "Violencia intrafamiliar" ~ 10002,
        Subtipo == "Violencia contra la mujer o miembros del núcleo familiar física" ~ 10003,
        Subtipo == "Violencia contra la mujer o miembros del núcleo familiar sexual" ~ 10004))
  }


  # Filtrar y recolectar datos por desagregación geográfica (provincias)
  nuevo_dato_geografico <- nuevo_dato %>%
    mutate(Mes = month(dmy(Fecha)), #dmy / ymd
           Año = year(dmy(Fecha))) %>% 
    filter(indicador == codigo_indicador,
           Año == año,
           Mes == mes) %>%
    count(across(all_of(contar_desagregacion))) %>%
    bind_rows(summarise(.,
                        across(n, sum),
                        across(where(is.character), ~"0000"))) %>%
    mutate(provincia = ifelse(provincia == "0000", "Total Nacional", provincia)) %>%
    filter(!is.na(provincia)) %>%
    filter(provincia != 'ZONA NO DELIMITADA') %>% # Cantones: Las Golondrinas / El Piedrero
    rename_at(vars(all_of(columnas_orig)), ~columnas_final) %>%
    fill(Mes, Año,
         .direction = 'down')


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato_geografico, formato)


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#========================= 2018 =================================

#' Protestas registradas
#' @param archivo string, ubicación y nombre de archivo (csv)
#' @param mes numeric, mes de actualización 
#' @param año numeric, año de actualización 
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones 
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_protestas <- function(archivo, mes, año, canton=FALSE, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(2018)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_csv(paste0("source data/", archivo))
  
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    formato <- 'canton_nombre'
    contar_desagregacion <- c("Año", "Mes", "admin1", "admin2")
    columnas_orig <- c("admin1", "admin2", "n")
    columnas_final <- c("Provincia", "Cantón", "Dato Numérico")
  } else {
    formato <- 'provincia_nombre'
    contar_desagregacion <- c("Año", "Mes", "admin1")
    columnas_orig <- c("admin1", "n")
    columnas_final <- c("Provincia", "Dato Numérico")
  }
  
  
  # Filtrar datos iniciales
  nuevo_dato <- archivo_inicial %>%
    filter(event_type == "Protests") %>% # se excluyen 'riots' (disturbios)
    select(event_date, sub_event_type, admin1, admin2, fatalities) %>%
    # transformar fechas
    mutate(Mes = month(dmy(event_date)),
           Año = year(dmy(event_date))) %>%
    filter(Año == año)
  
  
  # Recolectar datos por desagregación geográfica (provincias)
  nuevo_dato_geografico <- nuevo_dato %>%
    count(across(all_of(contar_desagregacion))) %>%
    filter(!is.na(admin1)) %>%
    rename_at(vars(all_of(columnas_orig)), ~columnas_final) %>%
    group_by(Mes) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~"Total Nacional"),
                        across(Año, max))) %>%
    ungroup()
  

  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato_geografico, formato)
  if ("Cantón" %in% names(nuevo_dato_geografico)){
    print("Valores no encontrados para cantones")
    print(nuevo_dato_geografico %>% select(Provincia, Cantón) %>% filter(str_detect(Cantón, "_sn$")))
  }
  

  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico %>% arrange(desc(Mes)),
                   ind %>% filter(!Año == año)) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(2018, ind, guardar)

  ind
  
}
