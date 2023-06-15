
source("areas/D1_aut.R", encoding = "UTF-8")
source("areas/D2_aut.R", encoding = "UTF-8")
source("areas/D3_aut.R", encoding = "UTF-8")
source("areas/D4_aut.R", encoding = "UTF-8")
source("areas/D5_aut.R", encoding = "UTF-8")
source("areas/D6_aut.R", encoding = "UTF-8")
source("areas/D7_aut.R", encoding = "UTF-8")
source("areas/D9_aut.R", encoding = "UTF-8")
source("areas/D10_aut.R", encoding = "UTF-8")

library(readxl)
library(tidyverse)

# Desplazamiento y migración

nuevo_1003 <- act_seguridad_migracion(archivo = "003_Trata/mdg_tratadepersonasytraficoilicitodemigrantes_pm_2023_enero_febrero.xlsx", hoja = 'BASE DE DATOS', codigo_indicador = 1003, mes = "febrero", año = 2023, guardar=FALSE)
nuevo_1004 <- act_seguridad_migracion(archivo = "003_Trata/mdg_tratadepersonasytraficoilicitodemigrantes_pm_2023_enero_febrero.xlsx", hoja = 'BASE DE DATOS', codigo_indicador = 1004, mes = "febrero", año = 2023, guardar=FALSE)
# nuevo_1008 <- act_APCEVTE(archivo = "005_MREMH/APCEVTE_2022/Agosto-3.pdf", primera_nacionalidad = "china", mes = 8, año = 2022, guardar = FALSE)
# nuevo_1010 <- act_PI(archivo = "005_MREMH/PI_2022/Agosto-8.pdf", codigo_indicador = 1010, ultima_nacionalidad = 'venezuela', mes = 8, año = 2022, guardar = FALSE)
# nuevo_1011 <- act_PI(archivo = "005_MREMH/PI_2022/Agosto-8.pdf", codigo_indicador = 1011, ultima_nacionalidad = 'venezuela', mes = 8, año = 2022, guardar = FALSE)
# nuevo_1012 <- act_PI(archivo = "005_MREMH/PI_2022/Agosto-8.pdf", codigo_indicador = 1012, ultima_nacionalidad = 'venezuela', mes = 8, año = 2022, guardar = FALSE)
# nuevo_1013 <- act_MR(archivo = "005_MREMH/MR_2022/Agosto-6.pdf", primera_nacionalidad = 'argentina', mes = 8, año = 2022, guardar = FALSE)
# nuevo_1014 <- act_VI(archivo = "005_MREMH/VI_2022/Agosto.pdf", primera_nacionalidad = 'albania', mes = 8, año = 2022, guardar = FALSE)
nuevo_1015 <- act_flujo_migratorio(archivo = "007_ESI/entrada_ec_2023.xlsx", codigo_indicador = 1015, mes = 4, guardar = FALSE)
nuevo_1016 <- act_flujo_migratorio(archivo = "007_ESI/salida_ec_2023.xlsx", codigo_indicador = 1016, mes = 4, guardar = FALSE)
nuevo_1017 <- act_flujo_migratorio(archivo = "007_ESI/entrada_ex_2023.xlsx", codigo_indicador = 1017, mes = 4, guardar = FALSE)
nuevo_1018 <- act_flujo_migratorio(archivo = "007_ESI/salida_ex_2023.xlsx", codigo_indicador = 1018, mes = 4, guardar = FALSE)
nuevo_1019 <- act_inclusion_MIES_nac(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 1019, mes = 5, año = 2023, guardar = FALSE)
nuevo_1020 <- act_inclusion_MIES_nac(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 1020, mes = 5, año = 2023, guardar = FALSE)
nuevo_1021 <- act_inclusion_MIES_nac(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 1021, mes = 5, año = 2023, guardar = FALSE)
nuevo_1022 <- act_inclusion_MIES_nac(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 1022, mes = 5, año = 2023, guardar = FALSE)
nuevo_1026 <- act_GTRM(archivo = "017_ACNUR/211022_GTRM_GT_educacion.xlsx", mes = 3, año = 2023, guardar = FALSE)

# Cohesión social, equidad y no-discriminación

nuevo_2001 <- act_pobreza(archivo = "008_ENEMDU_Pob/202212_Tabulados_pobreza_diciembre_2022/202212_Tabulados_pobreza_EXCEL.xlsx", codigo_indicador = 2001, mes = 12, año = 2022, guardar = FALSE)
nuevo_2002 <- act_pobreza(archivo = "008_ENEMDU_Pob/202212_Tabulados_pobreza_diciembre_2022/202212_Tabulados_pobreza_EXCEL.xlsx", codigo_indicador = 2002, mes = 12, año = 2022, guardar = FALSE)
nuevo_2003 <- act_pobreza(archivo = "008_ENEMDU_Pob/202212_Tabulados_pobreza_diciembre_2022/202212_Tabulados_pobreza_EXCEL.xlsx", codigo_indicador = 2003, mes = 12, año = 2022, guardar = FALSE)
nuevo_2004 <- act_pobreza_multi(archivo = "008_ENEMDU_Pob/Tabulados IPM-dic 22.xlsx", codigo_indicador = 2004, año = 2022, guardar = FALSE)
nuevo_2005 <- act_pobreza_multi(archivo = "008_ENEMDU_Pob/Tabulados IPM-dic 22.xlsx", codigo_indicador = 2005, año = 2022, guardar = FALSE)
nuevo_2008 <- act_inclusion_MIES(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 2008, mes = 5, año = 2023, guardar = FALSE)
nuevo_2009 <- act_inclusion_MIES(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 2009, mes = 5, año = 2023, guardar = FALSE)
nuevo_2010 <- act_inclusion_MIES(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 2010, mes = 5, año = 2023, guardar = FALSE)
nuevo_2011 <- act_inclusion_MIES(archivo = "010_MIES/01_DII/2023 USUARIOS DE LA UNIDAD DE ATENCION DEL SIIMIES MAYO.xlsx", codigo_indicador = 2011, mes = 5, año = 2023, guardar = FALSE)
nuevo_2014 <- act_deuda_IESS(archivo = "012_MEF/Base-de-Datos-Boletin-de-Deuda-FEB-2023.xls", mes = 2, año = 2023, guardar = FALSE)
nuevo_2017 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 2017, mes = 5, año = 2023, guardar = FALSE)
nuevo_2018 <- act_protestas(archivo = "025_ACLED/2017-01-01-2023-05-31-South_America-Ecuador.csv", mes = 5, año = 2023, guardar = FALSE)
  
# Estabilidad económica

nuevo_3001 <- act_enemdu_empleo(archivo = "002_ENEMDU/202303_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3001, guardar = FALSE)
nuevo_3002 <- act_enemdu_empleo(archivo = "002_ENEMDU/202303_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3002, guardar = FALSE)
nuevo_3003 <- act_enemdu_empleo(archivo = "002_ENEMDU/202303_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3003, guardar = FALSE)
nuevo_3004 <- act_deuda_publica(archivo = "012_MEF/Base-de-Datos-Boletin-de-Deuda-FEB-2023.xls", codigo_indicador = 3004, mes = 2, año = 2023, guardar = FALSE)
nuevo_3005 <- act_deuda_publica(archivo = "012_MEF/Base-de-Datos-Boletin-de-Deuda-FEB-2023.xls", codigo_indicador = 3005, mes = 2, año = 2023, guardar = FALSE)
nuevo_3006 <- act_deuda_publica(archivo = "012_MEF/Base-de-Datos-Boletin-de-Deuda-FEB-2023.xls", codigo_indicador = 3006, mes = 2, año = 2023, guardar = FALSE)
nuevo_3007 <- act_deuda_publica(archivo = "012_MEF/Base-de-Datos-Boletin-de-Deuda-FEB-2023.xls", codigo_indicador = 3007, mes = 2, año = 2023, guardar = FALSE)
nuevo_3009 <- act_varPIB(archivo = "013_BCE/PIB/IEM-436-e_mar_23.xlsx", codigo_indicador = 3009, mes = 12, año = 2022, guardar = FALSE)
nuevo_3010 <- act_varPIB(archivo = "013_BCE/PIB/IEM-436-e_mar_23.xlsx", codigo_indicador = 3010, mes = 12, año = 2022, guardar = FALSE)
nuevo_3011 <- act_varPIB(archivo = "013_BCE/PIB/IEM-436-e_mar_23.xlsx", codigo_indicador = 3011, mes = 12, año = 2022, guardar = FALSE)
nuevo_3012 <- act_secPIB(archivo = "013_BCE/PIB/IEM-432-e_mar_23.xlsx", codigo_indicador = 3012, año = 2022, guardar = FALSE)
nuevo_3013 <- act_secPIB(archivo = "013_BCE/PIB/IEM-432-e_mar_23.xlsx", codigo_indicador = 3013, año = 2022, guardar = FALSE)
nuevo_3014 <- act_secPIB(archivo = "013_BCE/PIB/IEM-432-e_mar_23.xlsx", codigo_indicador = 3014, año = 2022, guardar = FALSE)
nuevo_3015 <- act_BCE(archivo = "013_BCE/Comercio/IEM-322-e_may_23.xlsx", codigo_indicador = 3015, año = 2023, guardar = FALSE)
nuevo_3016 <- act_IDEAC(archivo = "013_BCE/IDEAC/IEM-451-e_may_23.xlsx", año = 2023, guardar = FALSE)
nuevo_3017 <- act_BCE(archivo = "013_BCE/Crudo/IEM-412b-e_may_23.xlsx", codigo_indicador = 3017, año = 2023, guardar = FALSE)
nuevo_3018 <- act_BCE(archivo = "013_BCE/Crudo/IEM-412b-e_may_23.xlsx", codigo_indicador = 3018, año = 2023, guardar = FALSE)
nuevo_3019 <- act_SRI(archivo = "014_SRI/Recaudación por impuesto provincia y cantón_mayo2023.xlsx", año = 2023, guardar = FALSE)
nuevo_3024 <- act_CFB(archivo = "001_CFB/4. Ipc_canastabasica_nacional_ciudades_may_2023.xls", codigo_indicador = 3024, mes = 5, año = 2023, guardar = FALSE)
nuevo_3024_1 <- act_CFB(archivo = "001_CFB/4. Ipc_canastabasica_nacional_ciudades_may_2023.xls", codigo_indicador = 3024.1, mes = 5, año = 2023, guardar = FALSE)
nuevo_3024_2 <- act_CFB(archivo = "001_CFB/4. Ipc_canastabasica_nacional_ciudades_may_2023.xls", codigo_indicador = 3024.2, mes = 5, año = 2023, guardar = FALSE)
nuevo_3024_3 <- act_CFB(archivo = "001_CFB/4. Ipc_canastabasica_nacional_ciudades_may_2023.xls", codigo_indicador = 3024.3, mes = 5, año = 2023, guardar = FALSE)
nuevo_3024_4 <- act_CFB(archivo = "001_CFB/4. Ipc_canastabasica_nacional_ciudades_may_2023.xls", codigo_indicador = 3024.4, mes = 5, año = 2023, guardar = FALSE)
nuevo_3025 <- act_enemdu_empleo(archivo = "002_ENEMDU/202303_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3025, guardar = FALSE)

# Seguridad interna

nuevo_4001 <- act_homicidios(archivo = "018_MG/mdg_homicidiosintencionales_pm_2023_enero_abril.csv", version = 2, año = 2023, guardar = FALSE)
nuevo_4002 <- act_detenidos(archivo = "018_MG/Detenidos/mdg_detenidosaprehendidos_pm_2023_enero_febrero.xlsx", version = 2, codigo_indicador = 4002, año = 2023, guardar = FALSE)
nuevo_4003 <- act_detenidos(archivo = "018_MG/Detenidos/mdg_detenidosaprehendidos_pm_2023_enero_febrero.xlsx", version = 2, codigo_indicador = 4003, año = 2023, guardar = FALSE)
nuevo_4004 <- act_denuncias(archivo = "019_CESCJ/052022_Tabulados Seguridad.xlsx", codigo_indicador = 4004, año = 2022, guardar = FALSE)
nuevo_4005 <- act_denuncias(archivo = "019_CESCJ/052022_Tabulados Seguridad.xlsx", codigo_indicador = 4005, año = 2022, guardar = FALSE)
nuevo_4006 <- act_denuncias(archivo = "019_CESCJ/052022_Tabulados Seguridad.xlsx", codigo_indicador = 4006, año = 2022, guardar = FALSE)
nuevo_4007 <- act_snai(archivo = "020_SNAI/Reporte-mensual-PPL-abril-2023.xlsx", codigo_indicador = 4007, año = 2023, guardar = FALSE)
nuevo_4008 <- act_snai(archivo = "020_SNAI/Reporte-mensual-PPL-abril-2023.xlsx", codigo_indicador = 4008, año = 2023, guardar = FALSE)
nuevo_4009 <- act_armas(archivo = "018_MG/Armas/mdg_armasilicitas_pm_2023_enero-marzo.csv", año = 2023, guardar = FALSE)
nuevo_4010 <- act_drogas(archivo = "018_MG/Drogas/mdi_sustanciasdepositadas_pm_2023_enero_febrero.xlsx", año = 2023, guardar = FALSE)
nuevo_4011 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 4011, mes = 5, año = 2023, guardar = FALSE)
nuevo_4012 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 4012, mes = 5, año = 2023, guardar = FALSE)

# Justicia y estado de derecho

nuevo_5004 <- act_TI(archivo = "022_TI/CPI2022_GlobalResultsTrends.xlsx", año = 2022, guardar = FALSE)
nuevo_5005 <- act_CJ(archivo = "023_CJ/TASAS JUDICIALES ANUALES 2012 - 2022.xlsx", codigo_indicador = 5005, año = 2022, guardar = FALSE)
nuevo_5006 <- act_CJ(archivo = "023_CJ/TASAS JUDICIALES ANUALES 2012 - 2022.xlsx", codigo_indicador = 5006, año = 2022, guardar = FALSE)
nuevo_5007 <- act_CJ(archivo = "023_CJ/TASAS JUDICIALES ANUALES 2012 - 2022.xlsx", codigo_indicador = 5007, año = 2022, guardar = FALSE)

# Salud pública

nuevo_6001 <- act_REDG(archivo = "026_REDG/Tabulados_y_series_EDG_2021/", codigo_indicador = 6001, año = 2021, guardar = FALSE)
nuevo_6004 <- act_REDG(archivo = "026_REDG/Tabulados_y_series_EDG_2021/", codigo_indicador = 6004, año = 2021, guardar = FALSE)
nuevo_6005 <- act_REDG(archivo = "026_REDG/Tabulados_y_series_EDG_2021/", codigo_indicador = 6005, año = 2021, guardar = FALSE)
nuevo_6006 <- act_REDG2(archivo = "026_REDG/Tabulados_y_series_EDG_2021/1.1.7.csv", codigo_indicador = 6006, año = 2021, guardar = FALSE)
nuevo_6007 <- act_REDG2(archivo = "026_REDG/Tabulados_y_series_EDG_2021/1.1.7.csv", codigo_indicador = 6007, año = 2021, guardar = FALSE)
nuevo_6009 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 6009, mes = 5, año = 2023, guardar = FALSE)
nuevo_6013 <- act_covid(archivo = "030_ECUACOVID/casos_provincia_2022.txt", codigo_indicador = 6013, año = 2022, guardar = FALSE)
nuevo_6014 <- act_covid(archivo = "030_ECUACOVID/muertes_nacionales.txt", codigo_indicador = 6014, año = 2022, guardar = FALSE)
nuevo_6015 <- act_covid(archivo = "030_ECUACOVID/camas_nacionales.txt", codigo_indicador = 6015, año = 2022, guardar = FALSE)
nuevo_6016 <- act_covid(archivo = "030_ECUACOVID/vacunas_provincia.txt", codigo_indicador = 6016, año = 2022, guardar = FALSE)

# Infraestructura y acceso a servicios sociales

nuevo_7001 <- act_MINEDUC(archivo = "031_ME/Tabulados_Instituciones_Historico-Inicio.xlsx", codigo_indicador = 7001, año = 2022, guardar = FALSE)
nuevo_7002 <- act_MINEDUC(archivo = "031_ME/Tabulados_Instituciones_Historico-Inicio.xlsx", codigo_indicador = 7002, año = 2022, guardar = FALSE)
nuevo_7003 <- act_MINEDUC(archivo = "031_ME/Tabulados_Instituciones_Historico-Inicio.xlsx", codigo_indicador = 7003, año = 2022, guardar = FALSE)
nuevo_7004 <- act_MINEDUC(archivo = "031_ME/Tabulados_Instituciones_Historico-Inicio.xlsx", codigo_indicador = 7004, año = 2022, guardar = FALSE)
nuevo_7013 <- act_ENMH(archivo = "027_ENMH/202012_Tabulados_Multipropósito_CSV/", codigo_indicador = 7013, año = 2020, guardar = FALSE) 
nuevo_7014 <- act_ENMH(archivo = "027_ENMH/202012_Tabulados_Multipropósito_CSV/", codigo_indicador = 7014, año = 2020, guardar = FALSE)
nuevo_7016 <- act_ENMH(archivo = "027_ENMH/202012_Tabulados_Multipropósito_CSV/", codigo_indicador = 7016, año = 2020, guardar = FALSE)

# Seguridad alimentaria, agricultura y tierra

nuevo_9004 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_1 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.1, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_2 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.2, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_3 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.3, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_4 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.4, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_5 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.5, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_6 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.6, mes = 5, año = 2023, guardar = FALSE)
nuevo_9004_7 <- act_IPC(archivo = "037_IPC/ipc_ind_nac_reg_ciud_emp_clase_05_2023.xlsx", codigo_indicador = 9004.7, mes = 5, año = 2023, guardar = FALSE)
nuevo_9005 <- act_ESPAC(archivo = "038_ESPAC/Tabulados ESPAC 2022.xlsx", codigo_indicador = 9005, año = 2022, guardar = FALSE)
nuevo_9006 <- act_ESPAC(archivo = "038_ESPAC/Tabulados ESPAC 2022.xlsx", codigo_indicador = 9006, año = 2022, guardar = FALSE)
nuevo_9007 <- act_ESPAC(archivo = "038_ESPAC/Tabulados ESPAC 2022.xlsx", codigo_indicador = 9007, año = 2022, guardar = FALSE)
nuevo_9014 <- act_SIPA(archivo = "044_SIPA/202305_precios-agroquimicos-fertilizantes.xlsx", mes = 5, año = 2023, guardar = FALSE)
  
# Igualdad de género

nuevo_10001 <- act_CJ_fem(archivo = "023_CJ/Femicidios/total_Datos completos_data.csv", mes = 5, año = 2023, guardar = FALSE)
nuevo_10002 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 10002, mes = 5, año = 2023, guardar = FALSE)
nuevo_10003 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 10003, mes = 5, año = 2023, guardar = FALSE)
nuevo_10004 <- act_ecu911(archivo = "021_ECU911/emergencias_mayo_2023.csv", codigo_indicador = 10004, mes = 5, año = 2023, guardar = FALSE)


### Exportar xlsx

exportar_xlsx("1") # Desplazamiento y migración
exportar_xlsx("2") # Cohesión social, equidad y no-discriminación
exportar_xlsx("3") # Estabilidad económica
exportar_xlsx("4") # Seguridad interna
exportar_xlsx("5") # Justicia y estado de derecho
exportar_xlsx("6") # Salud pública
exportar_xlsx("7") # Infraestructura y acceso a servicios sociales
exportar_xlsx("9") # Seguridad alimentaria, agricultura y tierra
exportar_xlsx("10") # Igualdad de género
