% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputar_datos_faltantes.R
\name{imputar_datos_faltantes}
\alias{imputar_datos_faltantes}
\title{Imputa datos perdidos en una serie de tiempo}
\usage{
imputar_datos_faltantes(serie)
}
\arguments{
\item{data}{Una serie de tiempo que contenga datos perdidos}
}
\value{
Un objeto de tipo \code{ts} con los datos imputados.
}
\description{
La función imputar_datos_faltantes es una función que recibe como entrada una
serie de tiempo y utiliza el módulo imputeTS de R para imputar los valores
faltantes en la serie. Para ello, primero encuentra los índices de los valores
faltantes en la serie utilizando la función which y luego utiliza la función
na_kalman del módulo imputeTS para imputar los valores faltantes utilizando
un modelo ARIMA y el filtro de Kalman. Finalmente, la función retorna la serie
con los valores faltantes imputados.
}
\seealso{
\code{\link[=unique]{unique()}}, \code{\link[stringi:stri_unique]{stringi::stri_unique()}} which this function wraps.
}
