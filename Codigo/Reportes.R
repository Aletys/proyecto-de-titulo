library(dplyr) #Para unir los conjuntos de datos
library(stringr)
library(ggplot2)
library(tidyr)


setwd("C:/Users/aleja/OneDrive/Escritorio/Alejandra/UCM/2023/Segundo Semestre/Práctica/Reportes")

#######Preprocesamiento

# Crear una lista para almacenar los data frames
reportes <- list()

# Lista de nombres de archivos
archivos <- c("Reporte 1 - 09-2022.csv", "Reporte 2 - 10-2022.csv",
              "Reporte 3 - 11-2022.csv", "Reporte 4 - 02-2023.csv",
              "Reporte 5 - 03-2023.csv", "Reporte 6 - 04-2023.csv",
              "Reporte 7 - 05-2023.csv", "Reporte 8 - 07-2023.csv")

# Leer los archivos y asignar nombres de columnas
for (archivo in archivos) {
  df <- read.csv(archivo, header = FALSE, skip = 1)
  colnames(df) <- read.csv(archivo, nrows = 1)
  df <- df[-1, ]
  reportes[[archivo]] <- df
}

# Realizar el outer join
all_data <- reportes[[1]]

for (i in 2:length(reportes)) {
  all_data <- merge(all_data, reportes[[i]], all = TRUE)
}

#Comprobar que los datos estén unidos mediante un archivo exportado para lograr ver todos los datos

#write.csv(datos, "all_data.csv", row.names = FALSE)

# Dejar todas las columnas antes de los antibioticos juntas
 datos <- all_data %>%
  select(Origen, `Número de la Orden`, `Código de barras `, Edad, `Fecha de Nacimiento del Paciente`,
        `Localización de paciente (Cód)`, `Localización de paciente`, Estudio,
        `Tipo de Muestra`, `Fecha de Extracción de la Muestra`, `Comentario de la muestra en la orden`,
         Resultado, `Microorganismo `, `Recuento de colonias`, `Tiempo de Detección`,
        `Fecha de Positividad`, `Fecha del estado`, `Estado del estudio`, everything())


#############################################


#Resumnen de los datos

str(datos)
summary(datos)
head(datos)
tail(datos)
is.null(datos)


#Tratamiento de los valores nulos y eliminación de columnas

#Verificar las columnas que no tienen valores

columnas_con_vacios <- datos %>%
   select_if(~all(. == ""))

#Verificar las filas sin antibioticos

# Filtrar las filas en las que todas las columnas de "Interpretación" estén en blanco
ant_blancos <- datos %>%
  filter(`Microorganismo ` != "" &
           rowSums(across(ends_with("Interpretación"), ~ .x == "")) == 0)

#Eliminacion de columnas
datos <- datos %>%
  mutate_all(~ ifelse(is.na(.), "", .)) %>%
  select(-Origen, -`Fecha de Nacimiento del Paciente`, -`Número de la Orden` ,
         -`Localización de paciente (Cód)`, -`Estado del estudio`,
         -`Comentario de la muestra en la orden`, -ends_with("CIM"))


##################################

#Estudios

unique(datos$Estudio)

#Cultivo corriente
datos <- datos %>%
  mutate(Estudio = ifelse(str_detect(Estudio, "^CULTIVO CORRIENTE"), "CULTIVO CORRIENTE", Estudio))

#Cultivo hongos
datos$Estudio <- str_trim(datos$Estudio)
datos <- datos %>%
  mutate(Estudio = str_replace(Estudio, "CULTIVO DE HONGOS", "CULTIVO HONGOS"),
         Estudio = str_replace(Estudio, "CULTIVO HONGOS \\s*\\(RASPADO\\)", "CULTIVO HONGOS"),
         Estudio = str_replace(Estudio, "CULTIVO HONGOS \\s*\\(UÑA\\)", "CULTIVO HONGOS"))

#Cultivo genococo
datos <- datos %>%
  mutate(Estudio = str_replace(Estudio, "CULTIVO DE GONOCOCO", "CULTIVO GONOCOCO") ,
         Estudio = str_replace(Estudio, "CULTIVO GONOCOCO \\s*\\(FLUJO VAGINAL\\)", "CULTIVO GONOCOCO"),
         Estudio = str_replace(Estudio, "CULTIVO GONOCOCO \\s*\\(URETRAL\\)", "CULTIVO GONOCOCO"))

#Estudio Bacteriologico de TBC Pulmonar
datos$Estudio <- toupper(datos$Estudio)
datos <- datos %>%
  mutate(Estudio = str_replace(Estudio, " Estudio Bacteriologico de TBC Pulmonar", "Estudio Bacteriologico de TBC Pulmonar"),
         Estudio = ifelse(str_detect(Estudio, "^ESTUDIO BACTERIOLOGICO DE TBC"), "ESTUDIO BACTERIOLOGICO DE TBC", Estudio))

##################################


#Limpiar los tipos de muestras

# Función para limpiar la columna de muestras
limpiar_muestra <- function(muestra) {
  primera_palabra <- str_extract(muestra, "\\S+")
  primera_palabra <- str_remove(primera_palabra, "-.*")
  return(primera_palabra)
}

# Aplicar la función a la columna Muestras
datos <- datos %>%
  mutate(Muestra = limpiar_muestra(`Tipo de Muestra`))


#Eliminar Tipo de Muestra
datos <- datos %>%
  select(-`Tipo de Muestra`)






###################################



# Patrones de localidades
comunas <- c("SAN CLEMENTE", "PENCAHUE", "CUREPTO", "SAN RAFAEL", "RIO CLARO",
            "EMPEDRADO", "LINARES", "MOLINA", "MAULE", "SANTIAGO",
            "CONSTITUCION", "CAUQUENES", "PARRAL", "COLBUN", "LONGAVI", "SAN JAVIER",
            "vILLA ALEGRE", "YERBAS BUENAS", "CURICO", "HUALAÑE", "LICANTEN", "RAUCO",
            "ROMERAL", "SAGRADA FAMILIA", "TENO", "VICHUQUEN", "CHANCO", "PELLUHUE",
            "PELARCO")


datos <- datos %>%
  mutate(`Localización de paciente` = str_replace(`Localización de paciente`, "SN. CLEMENTE", "SAN CLEMENTE"))

# Función para limpiar la columna de localidades
limpiar_localidad <- function(localidad) {
  localidad_limpia <- str_extract(localidad, paste(comunas, collapse = "|"))
  ifelse(is.na(localidad_limpia), "TALCA", localidad_limpia)
}

# Aplicar la función a la columna Localizacion del paciente
datos <- datos %>%
  mutate(Comuna = limpiar_localidad(`Localización de paciente`))


datos <- datos %>%
  select( `Código de barras ` , Edad, `Localización de paciente`, Comuna,
         Estudio, Muestra, everything())



###################################





# Función para reemplazar los datos en la columna EDAD
datos$Edad <- sub("^([[:digit:]]+ [[:alpha:]]+) .*", "\\1", datos$Edad)

#Cantidad de edades diferentes
length(unique(datos$Edad))






#############################################


#Escherichia Coli
datos <- datos %>%
  mutate(`Microorganismo ` = str_replace(`Microorganismo `, "Escherichia coli \\s*\\(ECO\\)", "Escherichia coli"))

#Klebsiella pneumoniae
datos <- datos %>%
  mutate(`Microorganismo ` = str_replace(`Microorganismo `, "Klebsiella pneumoniae ssp pneumoniae", "Klebsiella pneumoniae"))

#Bacilo gram
datos <- datos %>%
  mutate(`Microorganismo ` = str_replace(`Microorganismo `, "Bacilo Gram", "Bacilo gram"))


#############################################




#Microorganismo

datos <- datos %>%
  mutate(Resultado = ifelse(Resultado == "" & `Microorganismo ` != "", "Hubo desarrollo de", Resultado),
         Resultado = str_replace(Resultado, "Cultivo continua en estudio", "Cultivo en estudio"))




#############################################

# Filtrar las filas en las que todas las columnas de "Interpretación" no estén en blanco
datos <- datos %>%
  filter(`Microorganismo ` != "" &
           rowSums(across(ends_with("Interpretación"), ~ .x != "")) > 0)

#############################################

#Categorización de Edad

unique(datos$Edad)

# Función para convertir las edades en días
Convertir_a_dias <- function(Edad) {
  parts <- str_extract_all(Edad, "\\d+")[[1]]
  if (grepl("DIAS", Edad)) {
    return(as.numeric(parts))
  } else if (grepl("MESES", Edad)) {
    return(as.numeric(parts) * 30)
  } else if (grepl("AÑOS", Edad)) {
    return(as.numeric(parts) * 365)
  }
}


# Función para asignar categoría de edad
asignacion_de_categoria <- function(Edad) {
  dias <- Convertir_a_dias(Edad)
  if (dias < 365) {
    return("Lactante")
  } else if (dias < 365 * 12) {
    return("Niñez")
  } else if (dias < 365 * 18) {
    return("Adolescecia")
  } else if (dias < 365 * 30) {
    return("Juventud")
  } else if (dias < 365 * 60) {
    return("Adultez")
  } else {
    return("Vejez")
  }
}

# Agregar una columna con la categoría de edad
datos <- datos %>%
  mutate(Rango_etario = sapply(Edad, asignacion_de_categoria)) %>%
  select(`Código de barras `, Edad,Rango_etario, everything())


#############################################


write.csv(datos, "datos_reportes.csv")

