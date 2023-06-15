#================================================================
# Área: Desplazamiento y migración
#================================================================

# librerías
library(tidyverse)
library(readxl)
library(pdftools)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#========================= 1003 / 1004 ==========================

#' Víctimas de trata de personas / Víctimas de tráfico ilícito de migrantes
#' Nota: Formato de documento original cambia regularmente (nombres de variables y hojas). Código corresponde a "versión 2" 
#' de protocolo de manejo de información
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param hoja string, nombre de hoja de cálculo de archivo
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_seguridad_migracion <- function(archivo, hoja, codigo_indicador, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = hoja)
  names(archivo_inicial) <- tolower(names(archivo_inicial)) # minúsculas para nombres de columnas
  names(archivo_inicial) <- gsub('ó', 'o', names(archivo_inicial)) # elimina acentos en nombres de columnas
  
  
  # Filtrar datos dependiendo del código de indicador
  nuevo_dato <- archivo_inicial %>%
    rename_all(~str_replace_all(., '\\s+', '')) %>% # borra espacios en nombres de columnas
    mutate(Tipo = ifelse(presuntodelito == "COIP. ART. 91 TRATA DE PERSONAS",1003,1004)) %>%
    select(-c(presuntodelito, fechainfraccion, rango_edad, nacionalidad)) %>% 
    filter(Tipo == codigo_indicador)


  # Recolectar datos por género a nivel nacional
  nuevo_dato_genero <- nuevo_dato %>%
    count(genero) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total"))) %>%
    filter(!is.na(genero)) %>%
    filter(genero == 'MASCULINO' | genero == 'FEMENINO' | genero == 'Total') %>%
    mutate(genero = replace(genero, genero %in% c("MASCULINO", "FEMENINO"), c("Hombre", "Mujer")),
           Provincia = "Total Nacional") %>%
    rename(Sexo = genero,
           `Dato Numérico` = n)


  # Recolectar datos por desagregación geográfica (provincias)
  nuevo_dato_provincia <- nuevo_dato %>%
    mutate(provincia = ifelse(provincia == 'D.M. QUITO', 'PICHINCHA', provincia), # correcciones para casos específicos
           provincia = ifelse(provincia == 'D.M. GUAYAQUIL', 'GUAYAS', provincia)) %>%
    count(provincia) %>%
    filter(!is.na(provincia)) %>%
    filter(provincia != 'S/D') %>%
    rename(Provincia = provincia,
           `Dato Numérico` = n)


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_genero <- cambio_nombres(nuevo_dato_genero, 'provincia_nombre')
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato_provincia, 'provincia_nombre')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_genero, nuevo_dato_provincia, ind) %>%
    # remover información de mes anterior
    filter(Año != año | is.na(Año)) %>%
    # completar datos en filas incompletas
    mutate(Año = ifelse(is.na(Año), año, Año),
           Sexo = ifelse(is.na(Sexo),"Total",Sexo)) %>%
    fill('Código Indicador', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 'Indicador', 'Área', 'Sexo', 'Población', 
         'Fuente', 'Unidad de medida', 'Grupo', 'Dato Cualitativo', 'ODS',
         .direction = 'up') %>%
    # modificar `Dato Cualitativo` para indicar el mes al que está agregada la información
    mutate(`Dato Cualitativo` = str_remove(`Dato Cualitativo`, " Agregado.*")) %>%
    mutate(`Dato Cualitativo` = ifelse(Año == año, paste0(`Dato Cualitativo`, ' Agregado a ', mes, ' de ', as.character(año)), `Dato Cualitativo`)) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}

#================================================================

#========================= 1008 =================================

#' Casos de atención y protección a venezolanos en vulnerabilidad
#' Nota: Experimental! Formato de pdf cambia mensualmente. Función genera output para verificar si datos procesados
#' coinciden con el documento original. El nombre de la primera nacionalidad es importante para delimitar la extensión de la tabla
#' @param archivo string, ubicación y nombre de archivo (pdf)
#' @param primera_nacionalidad string, nombre de primera nacionalidad en tabla procesada
#' @param mes numeric, mes de actualización
#' @param año numeric, mes de actualización
#' @param boolean boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_APCEVTE <- function(archivo, primera_nacionalidad, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(1008)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- map(paste0("source data/", archivo), pdf_text)
  
  
  # Formatear documento inicial
  archivo_formateado <- map(archivo_inicial, ~ str_split(.x, "\\n") %>% unlist()) # elimina caracteres especiales (line break: \n)
  archivo_formateado <- reduce(archivo_formateado, c) # consolida las páginas
  
  
  # Especificar inicio y fin de tabla
  inicio_tabla <- str_which(tolower(archivo_formateado), primera_nacionalidad) # identifica primera fila de tabla
  inicio_tabla <- inicio_tabla[length(inicio_tabla)]
  fin_tabla <- str_which(tolower(archivo_formateado), "totales") # identifica última fila de tabla
  fin_tabla <- fin_tabla[length(fin_tabla)]
  
  
  # Construir tabla (genera csv temporal para reimportarlo como dataframe)
  tabla_pre <- archivo_formateado[(inicio_tabla):(fin_tabla-1)]
  tabla_pre <- str_replace_all(tabla_pre, "\\s{2,}", "|") # reemplaza caracteres especiales
  tabla_pre <- str_replace_all(tabla_pre, "\\.", "")
  text_con <- textConnection(tabla_pre)
  tabla_final <- read.csv(text_con, header=FALSE, sep = "|")
  tabla_final <- tabla_final[c(1:2, (length(tabla_final)-6):(length(tabla_final)))] # remueve columnas innecesarias
  colnames(tabla_final) <- c('Nacionalidad', 'Casos', 'Femenino','Masculino','NN','A','J','PA','PAM') # asigna nombres a columnas
  
  
  # Correcciones
  tabla_final <- tabla_final %>%
    filter(!is.na(PAM)) %>% # elimina valores NA para poder generar sumatoria
    mutate(Nacionalidad = ifelse(Nacionalidad == " e", " Estadounidense", Nacionalidad)) # casos específicos con errores
  
  
  # Calcular totales y verificar datos
  nuevo_dato <- tabla_final %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))
  print('Verificar valor total de la tabla con original')
  print(nuevo_dato)
  
  
  # Recolectar datos por género
  nuevo_dato_mes = nuevo_dato %>%
    filter(Nacionalidad %in% c(' Venezolana', 'Venezolana', 'Total')) %>%
    mutate(Nacionalidad = case_when(
      Nacionalidad == ' Venezolana' ~ 'Venezuela',
      Nacionalidad == 'Venezolana' ~ 'Venezuela',
      Nacionalidad == 'Total' ~ 'Total'),
      Total = Femenino + Masculino,
      Mes = mes,
      Año = año) %>%
    select(Nacionalidad, Femenino, Masculino, Total, Mes, Año) %>%
    rename(Mujer = Femenino,
           Hombre = Masculino) %>%
    pivot_longer(cols = c(Mujer, Hombre, Total),
                 names_to = 'Sexo',
                 values_to = 'Dato Numérico')
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(1008, ind, guardar)
  
  ind
  
}

#================================================================

#================== 1010 / 1011/ 1012 ===========================

#' Peticionarios de refugio de nacionalidad venezolana / Solicitantes de refugio de nacionalidad venezolana / Personas de nacionalidad venezolana que recibieron estatus de refugiado
#' Nota: Experimental! Formato de pdf cambia mensualmente. Función genera output para verificar si datos procesados
#' coinciden con el documento original. El nombre de la última nacionalidad es importante para delimitar la extensión de la tabla
#' @param archivo string, ubicación y nombre de archivo (pdf)
#' @param codigo_indicador numeric, código de indicador
#' @param ultima_nacionalidad string, nombre de última nacionalidad en tabla procesada
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_PI <- function(archivo, codigo_indicador, ultima_nacionalidad, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Límites de tablas según indicador
  if (codigo_indicador == 1010){
    primera_palabra <- "peticionarios"
    ultima_palabra <- "solicitantes"
    ultima_linea <- 1
  } else if (codigo_indicador == 1011) {
    primera_palabra <- "solicitantes"
    ultima_palabra <- "refugio"
    ultima_linea <- 1
  } else if (codigo_indicador == 1012) {
    primera_palabra <- "refugio"
    ultima_palabra <- ultima_nacionalidad
    ultima_linea <- 0
  }
  
  
  # Leer archivo con nueva información
  archivo_inicial <- map(paste0("source data/", archivo), pdf_text)
  
  
  # Formatear documento inicial
  archivo_formateado <- map(archivo_inicial, ~ str_split(.x, "\\n") %>% unlist()) # elimina caracteres especiales
  archivo_formateado <- reduce(archivo_formateado, c) # consolida las páginas
  
  
  # Especificar inicio y fin de tabla
  inicio_tabla <- str_which(tolower(archivo_formateado), primera_palabra) # identifica primera fila de tabla
  inicio_tabla <- inicio_tabla[length(inicio_tabla)]
  fin_tabla <- str_which(tolower(archivo_formateado), ultima_palabra) # identifica última fila de tabla
  fin_tabla <- fin_tabla[length(fin_tabla)]
  
  
  # Construir tabla
  tabla_pre <- archivo_formateado[(inicio_tabla):(fin_tabla-ultima_linea)]
  tabla_pre <- str_replace_all(tabla_pre, "\\s{2,}", "|") # reemplaza caracteres especiales
  tabla_pre <- str_replace_all(tabla_pre, "\\.", "")
  text_con <- textConnection(tabla_pre)
  tabla_final <- read.csv(text_con, header=FALSE, sep = "|") # remueve columnas innecesarias
  colnames(tabla_final) <- c('Nacionalidad','NNF','AF','JF','PAF','PAMF','Femenino','NNM','AM','JM','PAM','PAMM','Masculino','NNO','AO','JO','PAO','PAMO','Otro')
  
  
  # Calcular totales y verificar datos
  nuevo_dato <- tabla_final %>%
    filter(Nacionalidad != str_to_title(primera_palabra)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))
  print('Verificar valor total de la tabla con original')
  print(nuevo_dato)
  
  
  # Recolectar datos por género
  nuevo_dato_mes = nuevo_dato %>%
    filter(Nacionalidad %in% c('Venezuela', 'Total')) %>%
    mutate(Total = Femenino + Masculino,
           Mes = mes,
           Año = año) %>%
    select(Nacionalidad, Femenino, Masculino, Total, Mes, Año) %>%
    rename(Mujer = Femenino,
           Hombre = Masculino) %>%
    pivot_longer(cols = c(Mujer, Hombre, Total),
                 names_to = 'Sexo',
                 values_to = 'Dato Numérico')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    fill('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 'Indicador', 
         'Área', 'Sexo', 'Población', 'Fuente', 'Unidad de medida', 'Grupo', 'Dato Cualitativo', 'ODS', 'Nacionalidad',
         .direction = 'up')
  
  ind <- ind[, c('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 
                    'Indicador', 'Área', 'Sexo', 'Dato Numérico', 'Población', 'Dato Cualitativo', 'Año', 'Mes', 'Fuente', 'Unidad de medida', 
                    'Grupo', 'ODS', 'Nacionalidad')]
    
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#========================= 1013 =================================

#' Certificados de Migrantes Retornados
#' Nota: Experimental! Formato de pdf cambia mensualmente. Función genera output para verificar si datos procesados
#' coinciden con el documento original. El nombre de la primera nacionalidad es importante para delimitar la extensión de la tabla
#' @param archivo string, ubicación y nombre de archivo (pdf)
#' @param primera_nacionalidad string, nombre de primera nacionalidad en tabla procesada
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_MR <- function(archivo, primera_nacionalidad, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(1013)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- map(paste0("source data/", archivo), pdf_text)
  
  
  # Formatear documento inicial
  archivo_formateado <- map(archivo_inicial, ~ str_split(.x, "\\n") %>% unlist()) # elimina caracteres especiales
  archivo_formateado <- reduce(archivo_formateado, c) # consolida las páginas
  
  
  # Especificar inicio y fin de tabla
  inicio_tabla <- str_which(tolower(archivo_formateado), primera_nacionalidad)
  inicio_tabla <- inicio_tabla[length(inicio_tabla)]
  fin_tabla <- str_which(tolower(archivo_formateado), "total general")
  fin_tabla <- fin_tabla[length(fin_tabla)-1]


  # Construir tabla
  tabla_pre <- archivo_formateado[(inicio_tabla):(fin_tabla-1)]
  tabla_pre <- str_replace_all(tabla_pre, "\\s{2,}", "|") # reemplaza caracteres especiales
  tabla_pre <- str_replace_all(tabla_pre, "\\.", "")
  text_con <- textConnection(tabla_pre)
  tabla_final <- read.csv(text_con, header=FALSE, sep = "|")
  colnames(tabla_final) <- c('Nacionalidad','Total','Femenino','Masculino','J','PA','PAM')

  
  # Calcular totales y verificar datos
  nuevo_dato <- tabla_final %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))
  print('Verificar valor total de la tabla con original')
  print(nuevo_dato)


  # Recolectar datos por género
  nuevo_dato_mes = nuevo_dato %>%
    filter(Nacionalidad %in% c('Total')) %>%
    mutate(Total = Femenino + Masculino,
           Mes = mes,
           Año = año) %>%
    select(Femenino, Masculino, Total, Mes, Año) %>%
    rename(Mujer = Femenino,
           Hombre = Masculino) %>%
    pivot_longer(cols = c(Mujer, Hombre, Total),
                 names_to = 'Sexo',
                 values_to = 'Dato Numérico')


  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(1013, ind, guardar)

  ind
  
}

#================================================================

#========================= 1014 =================================

#' Visas emitidas a personas de nacionalidad venezolana
#' Nota: Experimental! Formato de pdf cambia mensualmente. Función genera output para verificar si datos procesados
#' coinciden con el documento original. El nombre de la primera nacionalidad es importante para delimitar la extensión de la tabla
#' @param archivo string, ubicación y nombre de archivo (pdf)
#' @param primera_nacionalidad string, nombre de primera nacionalidad en tabla procesada
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_VI <- function(archivo, primera_nacionalidad, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(1014)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- map(paste0("source data/", archivo), pdf_text)
  
  
  # Formatear documento inicial
  archivo_formateado <- map(archivo_inicial, ~ str_split(.x, "\\n") %>% unlist()) # elimina caracteres especiales
  archivo_formateado <- reduce(archivo_formateado, c) # consolida las páginas
  
  
  # Especificar inicio y fin de tabla
  inicio_tabla <- str_which(tolower(archivo_formateado), primera_nacionalidad)
  inicio_tabla <- inicio_tabla[length(inicio_tabla)]
  fin_tabla <- str_which(tolower(archivo_formateado), "totales")
  fin_tabla <- fin_tabla[length(fin_tabla)]
  
  
  # Construir tabla
  tabla_pre <- archivo_formateado[(inicio_tabla):(fin_tabla-1)]
  tabla_pre <- str_replace_all(tabla_pre, "\\s{2,}", "|") # reemplaza caracteres especiales
  tabla_pre <- str_replace_all(tabla_pre, "\\.", "")
  text_con <- textConnection(tabla_pre)
  tabla_final <- read.csv(text_con, header=FALSE, sep = "|")
  tabla_final <- tabla_final[c(1:2, (length(tabla_final)-7):(length(tabla_final)))] # elimina columnas innecesarias
  colnames(tabla_final) <- c('Nacionalidad','Total','Femenino','Masculino','PD','NN','A','J','PA','PAM')
  
  
  # Calcular totales y verificar datos
  nuevo_dato <- tabla_final %>%
    filter(!is.na(PAM)) %>% # elimina filas innecesarias (se pierde nombre de ciertos países)
    mutate_at(c(2:9), as.numeric) %>% # transforma columnas chr a numéricas para cálculo de total
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))
  print('Verificar valor total de la tabla con original')
  print(nuevo_dato)
  
  
  # Recolectar datos por género
  nuevo_dato_mes = nuevo_dato %>%
    filter(Nacionalidad %in% c('Venezuela','Total')) %>%
    mutate(Mes = mes,
           Año = año) %>%
    select(Nacionalidad, Femenino, Masculino, Total, Mes, Año) %>%
    rename(Mujer = Femenino,
           Hombre = Masculino) %>%
    pivot_longer(cols = c(Mujer, Hombre, Total),
                 names_to = 'Sexo',
                 values_to = 'Dato Numérico')
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    fill('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 'Indicador', 
         'Área', 'Sexo', 'Población', 'Fuente', 'Unidad de medida', 'Grupo', 'Dato Cualitativo', 'ODS', 'Nacionalidad',
         .direction = 'up')
  
  ind <- ind[, c('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 
                 'Indicador', 'Área', 'Sexo', 'Dato Numérico', 'Población', 'Dato Cualitativo', 'Año', 'Mes', 'Fuente', 'Unidad de medida', 
                 'Grupo', 'ODS', 'Nacionalidad')]
  
  
  # guardar indicador
  guardar_indicador(1014, ind, guardar)
  
  ind
  
}

#================================================================

#================== 1015 / 1016 / 1017 / 1018 ===================

#' Entrada de ecuatorianos / Salida de ecuatorianos / Entrada de extranjeros / Salida de extranjeros
#' Nota: requiere de un paso manual previo, descrito en el protocolo de manejo de información.
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_flujo_migratorio <- function(archivo, codigo_indicador, mes, guardar=FALSE) {
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "Sheet1")
  
  
  # Recolectar datos por género y generar total
  nuevo_dato_mes <- archivo_inicial %>%
    select(-Indicador) %>%
    filter(Mes == mes) %>%
    bind_rows(summarise(., 
                        across(`Dato Numérico`, sum),
                        across(`Sexo`, ~"Total"),
                        across(Año, max),
                        across(Mes, max)))
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#================== 1019 / 1020 / 1021 / 1022 ===================

#' Usuarios de programas de inclusión social - Venezuela
#' Desarrollo Infantil Integral / Adultos mayores / Personas con discapacidad / Protección Especial
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param codigo_indicador numeric, código de indicador
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_inclusion_MIES_nac <- function(archivo, codigo_indicador, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "Base")
  
  
  # Filtrar datos dependiendo del código de indicador señalado como argumento
  nuevo_dato <- archivo_inicial %>%
    select(c(Servicio, Provincia, id_provincia, Canton, id_canton, Parroquia, id_parroquia, Venezuela)) %>%
    # para códigos geográficos
    mutate(id_provincia = as.character(str_pad(id_provincia, 2, pad = "0")),
           id_canton = as.character(str_pad(id_canton, 4, pad = "0"))) %>%
    # reemplazar nombre de servicios con códigos
    mutate(Servicio = case_when(
      Servicio == 'DESARROLLO INFANTIL INTEGRAL' ~ 1019,
      Servicio == 'PERSONAS ADULTOS MAYORES - PEJ' ~ 1020,
      Servicio == 'PERSONAS CON DISCAPACIDAD' ~ 1021,
      Servicio == 'PROTECCION ESPECIAL' ~ 1022
    )) %>%
    filter(Servicio == codigo_indicador) %>%
    select(c(id_provincia, Venezuela)) # ***


  # Recolectar datos por desagregación geográfica (provincias)
  nuevo_dato_provincia <- nuevo_dato %>%
    group_by(id_provincia) %>%
    # para eliminar valores NA
    mutate(Venezuela = ifelse(is.na(Venezuela), 0, Venezuela)) %>%
    # sumatorias
    summarise(Venezuela = sum(Venezuela), .groups = 'drop') %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"00"))) %>%
    rename(`Código Provincia` = id_provincia,
           `Dato Numérico` = Venezuela) %>%
    # variables de tiempo
    mutate(Mes = mes,
           Año = año)


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato_provincia, 'provincia_codigo')


  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)

  ind
  
}


#================================================================

#========================= 1026 =================================

#' Asistencia a refugiados y migrantes por parte del GT Educación
#' Nota: requiere de un paso manual previo, descrito en el protocolo de manejo de información.
#' @param archivo string, ubicación y nombre de archivo (xlsx)
#' @param mes numeric, mes de actualización
#' @param año numeric, año de actualización
#' @param guardar boolean, función para exportar base actualizada como csv
#' @return dataframe, datos recolectados de indicador
act_GTRM <- function(archivo, mes, año, guardar=FALSE){

  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(1026)


  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "Total",
                                range = cell_cols("C:BW"))


  # Recolectar datos por género y generar total
  nuevo_dato_mes <- archivo_inicial %>%
    select(Value, DPA_DESCAN, DPA_DESPRO, mes, `poblacion meta`) %>%
    separate(mes, c("Año", "Mes"), sep = "-") %>%
    mutate(Mes = as.numeric(Mes),
           Año = as.numeric(Año)) %>%
    filter(`poblacion meta` == "Refugiados y migrantes",
           Mes == mes,
           Año == año) %>%
    rename(`Dato Numérico` = Value,
           Cantón = DPA_DESCAN,
           Provincia = DPA_DESPRO) %>%
    group_by(Año, Mes, Provincia, Cantón) %>%
    summarise(`Dato Numérico` = sum(`Dato Numérico`), .groups = 'drop')


  # Unir datos extraídos con base de datos geográficos para introducir código de provincia y cantón
  nuevo_dato_mes <- cambio_nombres(nuevo_dato_mes, 'canton_nombre')


  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    format_nuevo()


  # guardar indicador
  guardar_indicador(1026, ind, guardar)

  ind

}
