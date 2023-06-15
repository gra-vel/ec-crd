#================================================================
# Área: Estabilidad económica
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(lubridate)
library(readr)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#================== 3025 / 3001 / 3002 / 3003 ===================

#' Tasa de participación global, Tasa de desempleo, Tasa de empleo adecuado
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_enemdu_empleo <- function(archivo, codigo_indicador, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "2. Tasas")
  
  
  # Extraer dato por sexo y por área
  nuevo_dato <- archivo_inicial %>%
    mutate(Indicadores_cod = case_when(
      Indicadores == 'Subempleo (%)' ~ 3025,
      Indicadores == 'Participación Global (%)' ~ 3001,
      Indicadores == 'Desempleo (%)' ~ 3002,
      Indicadores == 'Empleo Adecuado/Pleno (%)' ~ 3003
    )) %>%
    filter(Indicadores_cod == codigo_indicador) %>%
    rename(des1 = Área, # urbano
           des2 = ...6, # rural
           des3 = Sexo, # hombre
           des4 = ...8, # mujer
           des5 = Nacional) %>%
    select(Periodo, des1, des2, des3, des4, des5) %>%
    # mover base de forma horizontal a forma vertical
    pivot_longer(cols = c(des1, des2, des3, des4, des5), 
                 names_to = c("pr", "Des"),
                 names_pattern = "([a-zA-Z]+)(\\d+)",
                 values_to = "Dato Numérico") %>%
    # reemplazar valores
    mutate(`Dato Numérico` = as.numeric(`Dato Numérico`),
           Área = ifelse(Des == 1, "Urbano", ifelse(Des == 2, "Rural", "Total")),
           Sexo = ifelse(Des == 3, "Hombre", ifelse(Des == 4, "Mujer", "Total")),
           Periodo = str_replace_all(Periodo, c("-" = ".-", "sep" = "sept")), # %b reconoce a septiembre como sept
           Fecha = parse_date(Periodo, "%b-%y", locale = locale("es"))) %>%
    mutate(Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha))) %>%
    # extraer valor más reciente
    filter(Fecha == max(Fecha)) %>%
    select(-c(Periodo, pr, Des, Fecha))
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()

  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#================== 3004 / 3005 / 3006 / 3007 ===================

#' Deuda pública total, Deuda interna, Deuda externa, Deuda pública total (% PIB)
#' Nota: nombre de hoja de cálculo cambia regularmente. Renombrar en archivo de origen a formato "Indicador PIB-%B"
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_deuda_publica <- function(archivo, codigo_indicador, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = paste0("Indicador PIB-", str_to_title(lubridate::month(mes, label=TRUE, abbr = FALSE, locale='es'))),
                                skip = 3)
  
  
  # Filtrar y recolectar datos
  nuevo_dato <- archivo_inicial %>%
    rename("indicador" = "...1",
           "Dato Numérico" = toupper(lubridate::month(mes, label=TRUE, abbr = FALSE, locale='es'))) %>%
    mutate(indicador = case_when(
      indicador == 'DEUDA PÚBLICA TOTAL' ~ 3004,
      indicador == 'Total Deuda Externa' ~ 3005,
      indicador == 'Total Deuda Interna' ~ 3006,
      indicador == 'Indicador Deuda / PIB' ~ 3007
    )) %>%
    filter(indicador == codigo_indicador) %>%
    mutate(`Dato Numérico` = as.numeric(`Dato Numérico`),
           Mes = mes,
           Año = año) %>%
    select(`Dato Numérico`, Mes, Año)
  
  
  # Ajustar unidades
  if (codigo_indicador != 3007){
    nuevo_dato <- nuevo_dato %>%
      mutate(`Dato Numérico` = `Dato Numérico`/10^3)
  } else {
    nuevo_dato <- nuevo_dato %>%
      mutate(`Dato Numérico` = `Dato Numérico`*100)
  }
  

  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
  

  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#================== 3009 / 3010 / 3011 ==========================

#' Variación trimestral de PIB, Variación trimestral de PIB en agricultura, Variación trimestral de PIB en construcción
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización 
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_varPIB <- function(archivo, codigo_indicador, mes, año, guardar = FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)


  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "IEM-436-e",
                                skip=14,
                                col_names = FALSE)
  
  
  # Filtrar y recolectar datos
  nuevo_dato <- archivo_inicial %>%
    rename("indicador" = "...1",
           "Dato Numérico" = names(archivo_inicial[,ncol(archivo_inicial)])) %>%
    mutate(indicador = case_when(
      indicador == "PIB" ~ 3009,
      indicador == "Agricultura" ~ 3010,
      indicador == "Construcción" ~ 3011
    )) %>%
    filter(indicador == codigo_indicador) %>%
    mutate(`Dato Numérico` = as.numeric(`Dato Numérico`),
           Mes = mes,
           Año = año) %>%
    select(`Dato Numérico`, Mes, Año)
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#================== 3012 / 3013 / 3014 ==========================

#' Sector agricultura, ganadería, caza y silvicultura, Sector manufactura (sin refinación petróleo), Sector comercio
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_secPIB <- function(archivo, codigo_indicador, año, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "IEM-432-e",
                                skip=7,
                                col_names = FALSE)
  
  names(archivo_inicial) <- archivo_inicial[1,]
  
  inicio_tabla <- str_which(archivo_inicial$`Período / Industrias`, "Millones de USD")[2]
  fin_tabla <- str_which(archivo_inicial$`Período / Industrias`, "Tasa de")
  
  nuevo_dato <- archivo_inicial[(inicio_tabla+2):(fin_tabla-2),]
  
  
  # Filtrar y recolectar datos
  nuevo_dato <- nuevo_dato %>%
    rename("Año" = "Período / Industrias") %>%
    mutate(Año = as.numeric(str_remove(Año, "\\(.*")), # se puede usar split para dato cualitativo
           across(where(is.character), as.numeric)) %>% # cambiar columnas character a numeric
    mutate_at(vars(-Año), funs((. / PIB)*100)) %>% # calcula el valor porcentual
    pivot_longer(-c(Año), names_to = "nombre_ind", values_to = "Dato Numérico") %>%
    mutate(ind = case_when(
      nombre_ind == "Sector manufactura (sin refinación petróleo)" ~ 3012,
      nombre_ind == "Manufactura (excepto refinación de petróleo)" ~ 3012,
      nombre_ind == "Comercio" ~ 3013,
      nombre_ind == "Sector comercio" ~ 3013,
      nombre_ind == "Agricultura, ganadería, caza y silvicultura" ~ 3014,
      nombre_ind == "Sector agricultura, ganadería, caza y silvicultura" ~ 3014
    )) %>%
    filter(ind == codigo_indicador,
           Año >= año - 5) %>%
    select(Año, `Dato Numérico`)
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind %>%
                     filter(Año < año - 5)) %>%
    arrange(desc(Año)) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}


#================================================================

#================== 3015 / 3017 / 3018 ==========================

#' Balanza comercial (no petrolera), Precio de petróleo (Crudo Oriente), Precio de petróleo (Crudo Napo)
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_BCE <- function(archivo, codigo_indicador, año, guardar = FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)


  # Leer archivo con nueva información
  sheet_name = str_extract(archivo, "(?<=(Comercio\\/)|(Crudo\\/))(.*?)(?=_)")
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = sheet_name,
                                skip=10,
                                col_names = FALSE)

  
  # Hojas de trabajo de acuerdo a código de indicador
  if (codigo_indicador == 3015){
    columna_datos = '...10'
    diferencia = 2
  } else if (codigo_indicador == 3017) {
    columna_datos = '...2'
    diferencia = 1
  } else if (codigo_indicador == 3018) {
    columna_datos = '...3'
    diferencia = 1
  }
  
  
  # Limpiar archivo inicial y variable Año
  archivo_inicial <- archivo_inicial %>%
    mutate(Año = str_extract(...1, '\\d+')) %>% # crea variable de año 
    filter(!is.na(...2)) %>% # elimina filas vacías
    fill(Año, .direction = "down")
  
  año_inicial <- str_which(archivo_inicial$Año, as.character(año-diferencia))[2]

  nuevo_dato <- archivo_inicial[año_inicial:length(archivo_inicial$Año),]


  # Extraer el dato numérico
  nuevo_dato <- nuevo_dato %>%
    filter(!str_detect(...1, '\\d+')) %>%
    mutate(Fecha = parse_date(paste0(...1,Año), "%B%Y", locale = locale("es")),
           Mes = month(ymd(Fecha)),
           Año = as.integer(Año)) %>%
    filter(Año > (año-diferencia)) %>% # filtra datos que no son provisionales
    arrange(desc(Año), desc(Mes)) %>%
    rename(`Dato Numérico` = all_of(columna_datos)) %>%
    select(`Dato Numérico`, Mes, Año)


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind %>%
                     filter(Año <= (año-diferencia))) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
  
}

#================================================================

#========================= 3016 =================================

#' Índice de Actividad Económica Coyuntural
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_IDEAC <- function(archivo, año, guardar = FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(3016)


  # Leer archivo con nueva información
  sheet_name = str_extract(archivo, "(?<=(IDEAC\\/)|(Crudo\\/))(.*?)(?=_)")
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = sheet_name,
                                skip=7)


  # Extraer el dato numérico
  nuevo_dato <- archivo_inicial %>%
    filter(!is.na(`2021`)) %>%
    select(...1, as.character(año)) %>%
    mutate(...1 = ifelse(...1 == "Sep", "Sept", ...1), # locale reconoce abreviatura de septiembre como Sept y no Sep
           Fecha = parse_date(paste0(...1, ".", año), "%b%Y", locale = locale("es")), # locale necesita que abreviatura de mes termine con un punto
           Mes = month(ymd(Fecha)),
           Año = año) %>%
    rename("Dato Numérico" = as.character(año)) %>%
    filter(!is.na(`Dato Numérico`)) %>%
    select(Mes, Año, `Dato Numérico`) %>%
    arrange(desc(Año), desc(Mes))
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind %>%
                     filter(Año < año)) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(3016, ind, guardar)
  
  ind

}

#================================================================

#========================= 3019 =================================

#' Recaudación de impuestos
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param año numeric, año de actualización
#' @param canton boolean, en TRUE recolecta datos a nivel de cantones
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_SRI <- function(archivo, año, canton=FALSE, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(3019)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "BDD")
  
  
  # Define variables para provincia y cantón
  if (canton == TRUE){
    suma_desagregacion = c("Provincia", "Cantón", "Mes")
    formato <- 'canton_nombre'
    
    # archivo_inicial <- archivo_inicial %>%
    #   mutate(Cantón = str_to_title(Cantón))
  } else {
    suma_desagregacion = c("Provincia", "Mes")
    formato <- 'provincia_nombre'
  }
  
  
  # Extraer dato por provincia
  nuevo_dato <- archivo_inicial %>%
    mutate(Mes = str_replace(Mes, "^[^_]*_", "")) %>% # elimina valores numéricos y subguión (01_, 02_, etc.)
    filter(!is.na(AÑO)) %>%
    mutate(Fecha = parse_date(paste0(Mes, AÑO), "%B%Y", locale = locale("es")),
           Mes = month(ymd(Fecha))) %>%
    group_by(across(all_of(suma_desagregacion))) %>%
    summarise(`Dato Numérico` = sum(Recaudación), .groups = 'drop') %>%
    group_by(Mes) %>%
    bind_rows(summarise(.,
                        across(`Dato Numérico`, sum),
                        across(where(is.character), ~"Total Nacional"),
                        .groups = 'drop')) %>%
    filter(Provincia != 'SIN DOMICILIO ASIGNADO') %>%
    mutate(Año = año)


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_geografico <- cambio_nombres(nuevo_dato, formato)
  if ("Cantón" %in% (names(nuevo_dato_geografico))){
    print("Valores no encontrados para cantones")
    print(nuevo_dato_geografico %>% select(Provincia, Cantón) %>% filter(str_detect(Cantón, "_sn$")))
  }
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_geografico %>%
                     filter(!is.na(`Código Provincia`)) %>%  # elimina datos inconsistentes (ej. La Concordia en Esmeraldas)
                     arrange(desc(Mes), `Código Provincia`),
                   ind %>%
                     filter(Año != año)) %>% ungroup() %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(3019, ind, guardar)

  ind
  
}

#================================================================

#========================= 3024 =================================

#' Canasta familiar básica
#' Nota: En ocasiones, existe un error en la fila con meses y años del archivo original. Se debe corregir manualmente
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_CFB <- function(archivo, codigo_indicador, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Crear 'loop' por cada hoja del archivo xls
  for (location in c("1. NACIONAL", "3. CUENCA", "4. LOJA", "5. QUITO", "6. AMBATO", "8. MACHALA", "9. ESMERALDAS", "10. GUAYAQUIL", "11. MANTA", "12. STO. DOMINGO")) {
    archivo_inicial <- read_excel(paste0("source data/", archivo),
                                  sheet = location,
                                  skip = 12) # saltar filas iniciales
    
    # Extraer el dato numérico de cada hoja
    nuevo_dato <- archivo_inicial %>%
      mutate(codigo_ind = case_when(
        `No.\nOrden` == 1 ~ 3024,
        `No.\nOrden` == 2 ~ 3024.1,
        `No.\nOrden` == 16 ~ 3024.2,
        `No.\nOrden` == 21 ~ 3024.3,
        `No.\nOrden` == 26 ~ 3024.4
      )) %>%
      filter(codigo_ind == codigo_indicador) %>% # *** 
      select(`Costo Actual en Dólares`) %>% # ***
      rename(`Dato Numérico` = `Costo Actual en Dólares`) %>%
      mutate(Mes = mes,
             Año = año,
             Cantón = case_when(
               location == "1. NACIONAL" ~ 'Total Nacional',
               location == "3. CUENCA" ~ 'Cuenca',
               location == "4. LOJA" ~ 'Loja',
               location == "5. QUITO" ~ 'Quito',
               location == "6. AMBATO" ~ 'Ambato',
               location == "8. MACHALA" ~ 'Machala',
               location == "9. ESMERALDAS" ~ 'Esmeraldas',
               location == "10. GUAYAQUIL" ~ 'Guayaquil',
               location == "11. MANTA" ~ 'Manta',
               location == "12. STO. DOMINGO" ~ 'Santo Domingo'
             ),
             Provincia = case_when(
               location == "1. NACIONAL" ~ 'Total Nacional',
               location == "3. CUENCA" ~ 'Azuay',
               location == "4. LOJA" ~ 'Loja',
               location == "5. QUITO" ~ 'Pichincha',
               location == "6. AMBATO" ~ 'Tungurahua',
               location == "8. MACHALA" ~ 'El Oro',
               location == "9. ESMERALDAS" ~ 'Esmeraldas',
               location == "10. GUAYAQUIL" ~ 'Guayas',
               location == "11. MANTA" ~ 'Manabí',
               location == "12. STO. DOMINGO" ~ 'Sto Dgo Tsáchilas'
             ))
    
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
