# librerías
library(tidyverse)

#' cambio_nombres
#' Estandariza los nombres de localidades geográficas (provincias y cantones) y añade los códigos oficiales para las localidades. 
#' Códigos se determinan según 'Catálogo de objetos censales' del INEC. Dependiendo de los datos originales se debe escoger el 
#' formato en el que se estandarizará. En caso de existir nombres y códigos, es preferible utilizar el formato por códigos
#' provincia_nombre: en caso de que fuente original disponga de nombres de provincias
#' provincia_codigo: en caso de que fuente original disponga de códigos de provincias
#' canton_nombre: en caso de que fuente original disponga de nombres de provincias y cantones. Importante: se añade "_sn" a cantones no identificados
#' canton_codigo: en caso de que fuente original disponga de códigos de provincias y cantones
#' @param data dataframe, datos con nombres o códigos de localidades, según fuente original
#' @param formato string, formato o método con el que se estandarizan nombres (provincia_nombre, canton_nombre, provincia_codigo, canton_codigo)
#' @return dataframe, datos recolectados de indicador con modificaciones en nombres de localidades y códigos geográficos
cambio_nombres <- function(data, formato){
  
  # Estandarización de nombres en fuente original (solo aplica para canton_nombre y provincia_nombre)
  # Aqui se corrige problema de cantones con "_sn"
  if ('Cantón' %in% (names(data))) {
    data <- data %>% 
      mutate(Cantón = str_to_title(Cantón)) %>%
      mutate(Cantón = replace(Cantón, Cantón %in% c('Alfredo Baquerizo Moreno'), 'A.Baquerizo Moreno'),
             Cantón = replace(Cantón, Cantón %in% c('Carlos Julio Arosemena Tola'), 'C. J. Arosemena Tola'),
             Cantón = replace(Cantón, Cantón %in% c('Baños De Agua Santa', 'Banos De Agua Santa'), 'Baños'),
             Cantón = replace(Cantón, Cantón %in% c('Canar'), 'Cañar'),
             Cantón = replace(Cantón, Cantón %in% c('Chahuarpamba'), 'Chaguarpamba'),
             Cantón = replace(Cantón, Cantón %in% c('Coronel Marcelino Maridueña', 'Crnel. Marcelino Maridueña'), 'Crnl. Marcelino Maridueñas'),
             Cantón = replace(Cantón, Cantón %in% c('Empalme'), 'El Empalme'),
             Cantón = replace(Cantón, Cantón %in% c('Francisco De Orellana', 'Orellana'), 'Fco.De Orellana'),
             Cantón = replace(Cantón, Cantón %in% c('General Antonio Elizalde', 'Gnral. Antonio Elizalde'), 'Gral. A. Elizalde'),
             Cantón = replace(Cantón, Cantón %in% c('La Joya De Los Sachas'), 'Joya De Los Sachas'),
             Cantón = replace(Cantón, Cantón %in% c('Limon - Indanza'), 'Limon Indanza'), # Limón Indanza (con acento en oficial)
             Cantón = replace(Cantón, Cantón %in% c('Logroðo'), 'Logroño'),
             Cantón = replace(Cantón, Cantón %in% c('Nobol (Vicente Piedrahita)', 'Nobol'), 'Nobol / Piedrahita'),
             Cantón = replace(Cantón, Cantón %in% c('Ona'), 'Oña'),
             Cantón = replace(Cantón, Cantón %in% c('San Pedro De Pelileo'), 'Pelileo'),
             Cantón = replace(Cantón, Cantón %in% c('Santiago De Pillaro'), 'Pillaro'),
             Cantón = replace(Cantón, Cantón %in% c('Playas (General Villamil)'), 'Playas'),
             Cantón = replace(Cantón, Cantón %in% c('Puebloviejo'), 'Pueblo Viejo'),
             Cantón = replace(Cantón, Cantón %in% c('Rioverde'), 'Rio Verde'), # Río Verde (con acento en oficial)
             Cantón = replace(Cantón, Cantón %in% c('Rumiðahui','Ruminahui'), 'Rumiñahui'),
             Cantón = replace(Cantón, Cantón %in% c('San Miguel De Salcedo'), 'Salcedo'),
             Cantón = replace(Cantón, Cantón %in% c('San Miguel De Urcuqui'), 'Urcuqui'), # Urcuquí (con acento en oficial)
             Cantón = replace(Cantón, Cantón %in% c('San Jacinto De Yaguachi'), 'Yaguachi'),
             Cantón = replace(Cantón, Cantón %in% c('Yantzaza', 'Yantzaza (Yanzatza)'), 'Yanzatza'))
  } 
  
  if ('Provincia' %in% (names(data))) {
    data <- data %>%
      mutate(Provincia = str_to_title(Provincia)) %>%
      mutate(Provincia = replace(Provincia, Provincia %in% c('Bolivar'), 'Bolívar'),
             Provincia = replace(Provincia, Provincia %in% c('Canar'), 'Cañar'),
             Provincia = replace(Provincia, Provincia %in% c('Galapagos'), 'Galápagos'),
             Provincia = replace(Provincia, Provincia %in% c('Los Rios'), 'Los Ríos'),
             Provincia = replace(Provincia, Provincia %in% c('Manabi'), 'Manabí'),
             Provincia = replace(Provincia, Provincia %in% c('Santo Domingo De Los Tsachilas', 'Santo Domingo De Los Tsáchilas', 'Sto. Domingo Tsáchilas', 'Sto Dgo Tsáchilas'), 'Santo Domingo de los Tsáchilas'),
             Provincia = replace(Provincia, Provincia %in% c('Sucumbíos'), 'Sucumbios'),
             Provincia = replace(Provincia, Provincia %in% c('Nacional'), 'Total Nacional'))
  }
  
  
  # Datos geográficos con nombres estandarizados y códigos oficiales
  geocodigos <- read.csv("complement/geocodigos.csv",
                         sep = ";",
                         check.names = FALSE,
                         fileEncoding = 'latin1',
                         colClasses = c(CANTON_CODIGO = 'character',
                                        PROVINCIA_CODIGO = 'character'))
  
  
  # Reglas especiales para canton_nombre
  reglas_acentos <- "ñ > \\~; Ñ > \\^; ::Latin-ASCII; \\~ > ñ; \\^ > Ñ" # para conservar letra "ñ"
  
  
  # Estandarizar por nombres de provincia
  if (formato == 'provincia_nombre'){
    
    data <- data %>%
      left_join(geocodigos %>%
                  select(-c(CANTON_CODIGO, CANTON_NOMBRE)) %>%
                  unique, by = c('Provincia' = 'PROVINCIA_NOMBRE')) %>%
      mutate(`Código Provincia` = PROVINCIA_CODIGO) %>% # añade código de provincia
      select(-c(PROVINCIA_CODIGO))
    
  # Estandarizar por nombres de provincia y cantón
  } else if (formato == 'canton_nombre'){
    
    data <- data %>%
      left_join(geocodigos %>%
                  mutate(CANTON_NOMBRE2 = stringi::stri_trans_general(CANTON_NOMBRE, id = reglas_acentos, rules = TRUE)), #elimina acentos (áéíóú)
                by = c('Provincia' = 'PROVINCIA_NOMBRE', 'Cantón' = 'CANTON_NOMBRE2')) %>%  
      mutate(`Código Provincia` = PROVINCIA_CODIGO,
             `Código Cantón` = CANTON_CODIGO,
             Cantón = ifelse(is.na(CANTON_NOMBRE), paste0(Cantón, "_sn"), CANTON_NOMBRE)) %>% #IMPORTANTE: si no hay un match, se añade "_sn" al cantón no identificado. Corregir nombre en primer paso
      select(-c(CANTON_CODIGO, PROVINCIA_CODIGO, CANTON_NOMBRE))
    
  # Estandarizar por códigos de provincia
  } else if (formato == 'provincia_codigo'){
    
    data <- data %>%
      left_join(geocodigos %>%
                  select(-c(CANTON_CODIGO, CANTON_NOMBRE)) %>%
                  unique, by = c('Código Provincia' = 'PROVINCIA_CODIGO')) %>%
      rename(Provincia = PROVINCIA_NOMBRE) 
    
    # Estandarizar por códigos de cantón
  } else if (formato == 'canton_codigo'){
    
    if ("Provincia" %in% names(data)){
      data <- data %>% select(-Provincia)
    }
    
    data <- data %>% 
      left_join(geocodigos, by = c('Código Cantón' = 'CANTON_CODIGO')) %>%
      #`Código Provincia` = PROVINCIA_CODIGO,
      rename(Provincia = PROVINCIA_NOMBRE,
             Cantón = CANTON_NOMBRE)
  }
  
}

