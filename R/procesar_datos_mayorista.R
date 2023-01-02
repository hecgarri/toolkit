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
#' 

procesar_datos_mayorista <- function(wholesaler, path) {
wholesaler_list <- data.table::rbindlist(
  lapply(wholesaler, function(x) {
    data.table::fread(paste0(path, x), skip = 5) %>%
      mutate(year = gsub("[^[:digit:]]", "", x))
  })
) %>%
  mutate(Desde = gsub("/","-",Desde)) %>%
  separate(
    col = "Desde",
    into = c("day", "month", "year"),
    sep = "-",
    extra = "drop",
    remove = FALSE
  ) %>%
  mutate(
    `Unidad de comercialización` = recode(
      as.character(`Unidad de comercialización`),
      "$/bandeja 10 kilos" = 10,
      "$/bandeja 4 kilos embalada" = 4,
      "$/bandeja 8 kilos" = 8,
      "$/caja 12 kilos" = 12,
      "$/kilo (en bins de 400 kilos)" = 1,
      "$/kilo (en bins de 450 kilos)" = 1,
      "$/kilo (en caja de 15 kilos)" = 1,
      "$/kilo (en caja de 17 kilos)" = 1,
      "$/kilo (en caja de 20 kilos)" = 1,
      "$/kilo (en caja de 8 kilos )" = 1
    )
  ) %>%
  mutate(
    precio_definitivo = ifelse(
      `Unidad de comercialización` == 10 |
      `Unidad de comercialización` == 4 |
      `Unidad de comercialización` == 8 |
      `Unidad de comercialización` == 12,
      `Precio promedio`/`Unidad de comercialización`,
      `Precio promedio`
    )
  ) %>% 
  rename(start = Desde)  %>%
  mutate(day = as.numeric(day),
   month = as.numeric(month),
    year = as.numeric(year), 
         start = paste(year, month, day, sep="-"), 
         start = as.Date(start)) %>%
         group_by(start) %>%
  summarise(price = mean(precio_definitivo, na.rm = TRUE))
}
