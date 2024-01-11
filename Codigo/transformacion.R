setwd("C:/Users/aleja/OneDrive/Escritorio/Alejandra/UCM/2023/Segundo Semestre/Tesis/Datos")
df <- read.csv("bin_5_2000Da_10000Da_talca_e_coli_v2.csv",sep = ",", header = TRUE)

# Verificar si hay presencia de valores nulos
any(is.na(df))

# Si muestra los valores, entonces no hay valores nulos
#na.fail(df)

#Solo para algunos antibióticos
a <- df$CIPROFLOXACINO #definir la variable objetivo
df <- df[!is.na(a), ] # Elimina las filas con NA solo de un antibiótico
#df <- df[complete.cases(df[, c("CIPROFLOXACINO", "GENTAMICINA")]), ] #Para más de un antibiótico


df <- replace(df, is.na(df), 0)

library(dplyr)
#Eliminar las columnas que contienen solo 0
df <- df %>%
  select_if(~ !all(. == 0))

# Obtén el índice de la columna 'Especie'
indice_especie <- which(colnames(df) == "especie")

# Obtén las columnas que están después de 'especie'
antibioticos <- colnames(df)[(indice_especie + 1):ncol(df)]

# Crear una nueva columna 'antibioticos' que combine los valores de las columnas
df$antibioticos <- do.call(paste0, as.data.frame(df[, antibioticos]))

# Eliminar las columnas en 'antibioticos'
df <- df[, -(which(names(df) %in% antibioticos)), drop = FALSE]

# Seleccionar todas las columnas que no comienzan con 'X'
columnas_a_mantener <- c(grep("^X", names(df), value = TRUE), "antibioticos")

# Mantener solo las columnas seleccionadas
df <- df[, columnas_a_mantener, drop = FALSE]

df$antibioticos <- as.factor(df$antibioticos)

table(df$antibioticos)

#df$objetivo <- ifelse(substr(df$antibioticos, 1, 4) == "1111", 1, 0) #Para los casos que todos son 1
df$objetivo <- as.integer(grepl("^1", df$antibioticos)) #Primera posición
#df$objetivo <- as.integer(grepl("^.1", df$antibioticos)) #Otras posiciones

# Eliminar la columnna de los antibioticos
df <- df %>%
  select(-antibioticos)

write.csv(df, file="ecipro.csv")
