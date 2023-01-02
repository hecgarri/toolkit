#' Imputa datos perdidos en una serie de tiempo
#'
#' Esta función recibe una lista de archivos Excel y los convierte a
#'  archivos CSV, omitiendo la primera 5 filas de cada archivo y la última
#'  fila que contenga datos. Para lograr esto, primero se itera sobre la lista
#'  de archivos utilizando un ciclo for. Luego, se usa la función read_excel
#'  del paquete readxl para leer el archivo y almacenarlo en una variable.
#'  A continuación, se utiliza la función nrow para determinar el último índice
#'  que contiene datos en el data frame y se almacena en una variable local.
#'  Finalmente, se usa la función write_csv del paquete readxl para escribir el
#'  archivo CSV, indicando que se deben omitir las primeras 5 filas y la última
#'  fila que contenga datos.
#'
#' @param data Una serie de tiempo que contenga datos perdidos
#'
#' @returns Un objeto de tipo `ts` con los datos imputados.
#' 
#' @seealso [unique()], [stringi::stri_unique()] which this function wraps.
#' 
#' 
#' @export

convertir_a_csv <- function(archivos) {
  # Recorre la lista de archivos
  for (archivo in archivos) {
    # Lee el archivo Excel
    datos <- readxl::read_excel(archivo)
    # Elimina la primera fila de datos
    datos <- datos[,]
    print(datos)
    # Encuentra el último índice con datos
    ultimo_indice <- max(which(!is.na(datos[,1]))) - 1
    # Elimina la última fila de datos
    datos <- datos[1:ultimo_indice,]
    # Crea un nombre de archivo CSV
    nombre_csv <- gsub("\\.xlsx$", ".csv", archivo)
    # Escribe el archivo CSV
    write.csv(datos, nombre_csv, row.names = FALSE)
  }
}