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
procesar_datos_consumidor <- function(consumer, path) {
  consumer_list = lapply(consumer,
   function(x) data.table::fread(paste0(path, x), skip = 3)  %>% 
  mutate(year = gsub("[^[:digit:]]", "", x)))  %>%
  data.table::rbindlist() %>%
  filter(Variedad=="Hass" & Calidad=="Primera") %>% 
    `colnames<-` (c("week", "start", "end","sector", "place","product",
                    "variety","quality","unity","min","max","price", "year")) %>% 
    mutate(price=price/1.19,
    start = as.Date(start, origin = "1899-12-30"),
    end = as.Date(end, origin = "1899-12-30")) %>%
    separate(col = "start",into = c("year", "month", "day"),sep = "\\-",
     remove = FALSE, extra="drop") %>%
     group_by(start) %>%
     summarise(price = mean(price))
}
