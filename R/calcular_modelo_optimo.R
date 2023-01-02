#' Remove duplicated strings
#'
#'  La función "calcular_modelo_optimo" tiene como objetivo encontrar
#'  el modelo VECM (Vector Error Correction Model) óptimo para un conjunto
#'  de datos y un número determinado de lags. Para ello, crea una lista
#'  de modelos VECM con diferentes valores de K utilizando la función "ca.jo"
#'  y a continuación crea una lista de modelos VAR (Vector Autoregressive Model)
#'  a partir de los modelos VECM. A continuación, se calcula una matriz de p-values
#'  utilizando la función "serial.test" y se modifican los elementos de la matriz
#'  con una sentencia if-else. Finalmente, se calcula el número óptimo de lags
#'  utilizando la función "which.max" y se calcula el modelo VECM óptimo con la función
#'  "ca.jo" nuevamente. La función retorna una lista con el modelo VECM óptimo y
#'  el número óptimo de lags.
#'  
#'
#' @param string Input vector. Either a character vector, or something
#'  coercible to one.
#' @param ... Other options used to control matching behavior between duplicate
#'   strings. Passed on to [stringi::stri_opts_collator()].
#' @returns A character vector, usually shorter than `string`.
#' @seealso [unique()], [stringi::stri_unique()] which this function wraps.
#' @examples
#' str_unique(c("a", "b", "c", "b", "a"))
#'
#' # Use ... to pass additional arguments to stri_unique()
#' str_unique(c("motley", "mötley", "pinguino", "pingüino"))
#' str_unique(c("motley", "mötley", "pinguino", "pingüino"), strength = 1)
#' @export
#' 
#' 
calcular_modelo_optimo <- function(datos, nlag) {
  # Crea una lista de modelos VECM con diferentes valores de K
  modelos_vecm <- vector("list", length = nlag)
  for (k in 2:(nlag+1)) {
    modelos_vecm[[k - 1]] <- ca.jo(datos, type = "trace", ecdet = "const", K = k, spec = "transitory")
  }

  # Crea una lista de modelos VAR a partir de los modelos VECM
  modelos_var <- vector("list", length = nlag)
  for (i in 1:nlag) {
    modelos_var[[i]] <- vec2var(modelos_vecm[[i]], r = 1)
  }

  # Calcula la matriz de p-values
  matriz_pvalues <- matrix(nrow = nlag, ncol = nlag)
  for (i in 1:nlag) {
    for (j in 1:nlag) {
      matriz_pvalues[i,j] <- serial.test(modelos_var[[i]], lags.pt = i+j, type = "PT.adjusted")$serial$p.value
    }
  }

  matrix <- matrix(nrow = nlag, ncol = nlag)
  # Modifica los elementos de la matriz
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (matriz_pvalues[i,j] > 0.1) {
        matrix[i,j] <- 1
      } else {
        matrix[i,j] <- 0
      }
    }
  }

  # Calcula el número óptimo de lags
  nlag_opt <- which.max(rowSums(matrix))

  # Calcula el modelo VECM óptimo
  modelo_optimo <- ca.jo(datos, type = c("trace"), ecdet = c("const"), K=nlag_opt, spec = c("transitory"))

  # Retorna el modelo y el rezago óptimo
  return(list(modelo = modelo_optimo, nlag_opt = nlag_opt))
}
