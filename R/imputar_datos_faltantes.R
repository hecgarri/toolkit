#' Imputa datos perdidos en una serie de tiempo
#'
#' La función imputar_datos_faltantes es una función que recibe como entrada una
#'  serie de tiempo y utiliza el módulo imputeTS de R para imputar los valores
#'  faltantes en la serie. Para ello, primero encuentra los índices de los valores
#'  faltantes en la serie utilizando la función which y luego utiliza la función
#'  na_kalman del módulo imputeTS para imputar los valores faltantes utilizando
#'  un modelo ARIMA y el filtro de Kalman. Finalmente, la función retorna la serie
#'  con los valores faltantes imputados.
#'
#' @param data Una serie de tiempo que contenga datos perdidos
#'
#' @returns Un objeto de tipo `ts` con los datos imputados.
#' 
#' @seealso [unique()], [stringi::stri_unique()] which this function wraps.
#' 
#' 
#' @export

imputar_datos_faltantes <- function(serie) {
  # Encuentra los índices de los valores faltantes
  indices_na <- which(is.na(serie))
  # Crea una copia de la serie
  y1 <- serie
  # Imputa los valores faltantes utilizando el modelo ARIMA y el filtro de Kalman
  y1[indices_na] <- imputeTS::na_kalman(y1, model = 'auto.arima')
  return(y1)
}


