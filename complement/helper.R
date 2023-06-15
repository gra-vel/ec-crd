# librerías
library(tidyverse)
library(openxlsx)


# áreas de riesgo
areas_D <- c('1' = 'D1_Displacement and Migration', 
             '2' = 'D2_Social Cohesion, equality & nondiscrimination',
             '3' = 'D3_Economic stability',
             '4' = 'D4_Internal security',
             '5' = 'D5_Justice and rule of law',
             '6' = 'D6_Health',
             '7' = 'D7_Infrastructure and access to social services',
             '8' = 'D8_Environment and climate',
             '9' = 'D9_Food security, agriculture & land',
             '10' = 'D10_Gender equality',
             '11' = 'D11_Political stability')

#####
#' importar_indicador
#' Importa archivo individual (csv) de cada indicador según su código. Vector 'areas_D' permite identificar
#' carpeta en la que se encuentra el archivo csv
#' @param codigo_indicador numérico, código de cuatro cifras asignado a cada indicador (5 para área D10)
#' @param areas_riesgo vector, áreas de riesgo. default es 'areas_D'
#' @return dataframe, datos recolectados de indicador
importar_indicador <- function(codigo_indicador, areas_riesgo=areas_D) {
  
  # Revisar el número de caracteres del código para identificar área de riesgo (4 o 5)
  if (nchar(codigo_indicador) == 5){
    area = gsub('(?<=^\\d{2}).*', '', as.character(codigo_indicador), perl = TRUE)
  } else {
    area = gsub('(?<=^\\d{1}).*', '', as.character(codigo_indicador), perl = TRUE)
  }
  
  # Importar indicador
  ind <- read.csv(paste0('final data/', areas_riesgo[area], '/', as.character(codigo_indicador), '.csv'),
                  dec = ",",
                  check.names = FALSE,
                  fileEncoding = 'latin1',
                  colClasses = c(`Código Provincia` = 'character', # mantiene código de dos cifras
                                 `Código Cantón` = 'character'))  # mantiene código de cuatro cifras
  
}

#####
#' guardar_indicador
#' Guarda archivo individual (csv) de cada indicador según su código. También registra entrada en archivo log
#' con cambios realizados a archivo csv. Archivo log solo se activa si guardar es TRUE. Vector 'areas_D' permite 
#' identificar carpeta en la que se encuentra el archivo csv
#' @param codigo_indicador numérico, código de cuatro cifras asignado a cada indicador (5 para área D10)
#' @param ind, dataframe, datos del indicador después de haber sido modificados
#' @param guardar boolean, si es TRUE, guarda el indicador csv con las modificaciones realizadas. Puede sobrescribir datos!
#' @param areas_riesgo vector, áreas de riesgo. default es 'areas_D'
guardar_indicador <- function(codigo_indicador, ind, guardar, areas_riesgo=areas_D){
  
  if (guardar == TRUE) {
    
    # Revisar el número de caracteres del código para identificar área de riesgo
    if (nchar(codigo_indicador) == 5){
      area = gsub('(?<=^\\d{2}).*', '', as.character(codigo_indicador), perl = TRUE)
    } else {
      area = gsub('(?<=^\\d{1}).*', '', as.character(codigo_indicador), perl = TRUE)
    }
    
    
    # Exportar datos
    write.table(ind %>%
                  mutate(`Dato Numérico` = as.character(`Dato Numérico`)) %>% 
                  mutate(`Dato Numérico` = stringr::str_replace(`Dato Numérico`, '\\.', ',')), # para conservar decimales con coma
                file = paste0('final data/', areas_riesgo[area], '/', as.character(codigo_indicador), '.csv'),
                fileEncoding = "latin1", # conserva caracteres especiales (áéíóúñÑ)
                sep = ',',
                qmethod = "double", # especifica como almacenar comillas
                row.names = FALSE)
    
    # Crear registro a partir de primera fila de dataframe exportado
    function_name <- as.character(sys.call(-1))[1] # recoge nombre de función en nivel superior
    ind <- ind %>%
      select(`Código Indicador`, Mes, Año) %>%
      mutate(function_name = function_name,
             date = Sys.time(),
             observations = nrow(.),
             comment = "") %>%
      rename(indicator_code = `Código Indicador`,
             update_month = Mes,
             update_year = Año) %>%
      slice(1)
    
    ind <- ind[, c("function_name", "date", "observations", "indicator_code", "update_month", "update_year", "comment")]
    
    
    # Verificar existencia de archivo log
    if (file.exists("log.xlsx")) {
      # Cargar archivo y modificarlo
      wb <- loadWorkbook("log.xlsx")
      writeData(wb, sheet = "log", ind, colNames = FALSE, startRow = nrow(readWorkbook("log.xlsx"))+2)
    } else {
      # Crear archivo nuevo
      wb <- createWorkbook()
      addWorksheet(wb, sheetName = "log") # crear hoja de cálculo
      writeData(wb, sheet = "log", ind, colNames = TRUE)
    }
    
    # Guardar archivo log
    saveWorkbook(wb, file="log.xlsx", overwrite = TRUE)
    
  } else {
    
    
  }
  
  
}

#####
#' format_nuevo
#' Completa información en columnas de dataframe y ordena columnas de dataframe.
#' @param nuevo_dato dataframe, datos del indicador después de haber sido modificados
#' @return dataframe, datos recolectados de indicador
format_nuevo <- function(nuevo_dato){
  
  # Completar información en columnas de dataframe
  format_df <- nuevo_dato %>%
    fill('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 'Indicador', 
         'Área', 'Sexo', 'Población', 'Fuente', 'Unidad de medida', 'Grupo', 'Dato Cualitativo', 'ODS',
         .direction = 'up')
  
  # Ordenar columnas de dataframe
  format_df <- format_df[, c('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 
                             'Indicador', 'Área', 'Sexo', 'Dato Numérico', 'Población', 'Dato Cualitativo', 'Año', 'Mes', 'Fuente', 'Unidad de medida', 
                             'Grupo', 'ODS')]
  
}

#####
#' exportar_xlsx
#' Compila todos los indicadores de un área (csv) y los exporta como una base individual por área (xlsx)
#' @param area numérico, número de área de riesgo
#' @param areas_riesgo vector, áreas de riesgo. default es 'areas_D'
exportar_xlsx <- function(area, areas_riesgo=areas_D){
  
  # Identificar archivos csv en carpeta de área
  area_dataset <- list.files(path = paste0('final data/', areas_riesgo[area], "/"), pattern = "*.csv", full.names = TRUE) %>%
    map_df(~read.csv(., dec = ",",
                     check.names = FALSE,
                     fileEncoding = "latin1",
                     colClasses = c(`Código Provincia` = 'character',
                                    `Código Cantón` = 'character',
                                    `Código Indicador` = 'character')))
  
  # Exportar xlsx
  write.xlsx(area_dataset, file = paste0('final data/', 'D', area, '.xlsx'), sheetName='Hoja1')
  
}
