# Instalar y cargar la librería openxlsx si aún no está instalada
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

datosTXT1 <- read.table(file.choose(), header = TRUE)


# Especificar el nombre del archivo Excel de salida
nombre_archivo <- "datos_exportados.xlsx"

# Escribir los datos en un archivo Excel
write.xlsx(datosTXT1, file = nombre_archivo)
