#' @title Selección paso a paso hacia atrás de combinaciones SEM
#' @name stepwise_backward_sem_combn
#' @description Una clase para realizar la selección paso a paso hacia atrás de variables en los modelos SEM y almacenar el modelo final y las estadísticas de ajuste
#' @slot datos Un data frame que contiene los datos
#' @slot vars_pos Un vector de caracteres que contiene las variables a considerar en el análisis
#' @slot estadisticos_ajuste Un data frame que contiene las estadísticas de ajuste para cada combinación de variables consideradas
#' @slot modelo_final La combinación de variables seleccionadas con las mejores estadísticas de ajuste
#' @export
#' @examples
#' data(holzingerSwineford1939)
#' vars_pos <- names(holzingerSwineford1939)[1:8]
#' modelo <- stepwise_backward_sem_combn(holzingerSwineford1939, vars_pos)
#' print(modelo)
#' @rdname stepwise_backward_sem_combn
#' @import lavaan
#' @importFrom combn combn
#' @exportClass stepwise_backward_sem_combn
#' @method initialize function to initialize the class
#' @method print function to print the final model and fit statistics
#' @method estadisticos_ajuste function to extract the fit statistics data frame
#' @exportMethod stepwise_backward_sem_combn function to perform stepwise backward selection of variables in SEM models
#' @exportMethod estadisticos_ajuste
#' @param datos A data frame containing the data
#' @param vars_pos A character vector containing the variables to consider in the analysis
#' @return An object of class stepwise_backward_sem_combn which contains the final model, fit statistics and input data
#' @details
#' The stepwise_backward_sem_combn function performs a stepwise backward selection of variables in SEM models by generating all possible combinations of variables, fitting a SEM model for each combination, and calculating fit statistics such as CFI, TLI, and RMSEA. The combination with the best fit statistics is selected as the final model. The function returns an object of class stepwise_backward_sem_combn which contains the final model, fit statistics, and input data. The object also has print, estadisticos_ajuste methods.
#' @note
#' The stepwise_backward_sem_combn function uses the 'lavaan' and 'combn' packages. Make sure to have them installed before using the function.
#' The function assumes that the input data is in the correct format, it's the user's responsibility to check the input before using the function.


setClass("stepwise_backward_sem_combn", representation(
  datos = "data.frame",
  vars_pos = "character",
  estadisticos_ajuste = "data.frame",
  modelo_final = "character"
))

setMethod("initialize", "stepwise_backward_sem_combn", function(.Object, datos, vars_pos) {
  # Verifica que los datos sean un data frame
  if (!is.data.frame(datos)) {
    stop("Los datos deben ser un data frame.")
  }
  
  # Verifica que vars_pos sea un vector de caracteres
  if (!is.character(vars_pos)) {
    stop("vars_pos debe ser un vector de caracteres.")
  }
  
  # Asigna los datos y variables a las slots del objeto
  .Object@datos <- datos
  .Object@vars_pos <- vars_pos
  
  # Inicializa un data frame para almacenar los resultados de los estadísticos de bondad de ajuste
  estadisticos_ajuste <- data.frame(modelo = character(), CFI = numeric(), TLI = numeric(), RMSEA = numeric())
  
  # Genera todas las combinaciones posibles de variables
  combinaciones <- expand.grid(vars_pos)
  
  # Itera sobre las combinaciones
  for (i in 1:ncol(combinaciones)) {
    combinacion <- combinaciones[, i]
    # Crea la fórmula para el modelo SEM con la combinación actual
    formula <- paste(combinacion, collapse = " ~ ")
    # Ajusta el modelo SEM con los datos
    modelo <- sem(formula, data = datos)

    # Obtiene las estadísticas de bondad de ajuste
    cfi <- lavaan::fitMeasures(modelo)$cfi
    tli <- lavaan::fitMeasures(modelo)$tli
    rmsea <- lavaan::fitMeasures(modelo)$rmsea
    # Guarda las estadísticas en el data frame
    estadisticos_ajuste <- rbind(estadisticos_ajuste, data.frame(modelo = paste(combinacion, collapse = " ~ "), CFI = cfi, TLI = tli, RMSEA = rmsea))
  }
  
  # Selecciona la combinación con las mejores estadísticas de ajuste
  mejor_combinacion <- estadisticos_ajuste[which.max(estadisticos_ajuste$CFI), "modelo"]
  
  .Object@estadisticos_ajuste <- estadisticos_ajuste
  .Object@modelo_final <- mejor_combinacion
  
  return(.Object)
})



setMethod("print", "stepwise_backward_sem_combn", function(x) {
  cat("Modelo final: ", x@modelo_final, "\n")
  cat("Estadísticos de bondad de ajuste: \n")
  print(x@estadisticos_ajuste)
})


setMethod("estadisticos_ajuste", "stepwise_backward_sem_combn", function(x) {
  x@estadisticos_ajuste
})

setMethod("modelo_final", "stepwise_backward_sem_combn", function(x) {
  return(x@modelo_final)
})

setMethod("plot", "stepwise_backward_sem_combn", function(object) {
  library(ggplot2)
  ggplot(object@estadisticos_ajuste, aes(x = modelo, y = CFI, fill = modelo)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_bar(aes(x = modelo, y = TLI, fill = modelo), stat = "identity", position = "dodge") +
    geom_bar(aes(x = modelo, y = RMSEA, fill = modelo), stat = "identity", position = "dodge") +
    ylim(0,1) +
    labs(x = "Modelo", y = "Estadística de ajuste", fill = "Modelo") +
    theme_bw()
})


