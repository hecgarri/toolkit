#' @title Stepwise backward selection of SEM combinations
#' @description A class for performing stepwise backward selection of variables in SEM models and storing the final model and fit statistics.
#' @param datos A data frame containing the data
#' @param vars_pos A character vector containing the variables to consider in the analysis.
#' @export stepwise_backward_sem_combn
#' @export plot
#' @return An object of class stepwise_backward_sem_combn which contains the final model, fit statistics and input data.
#' @method print stepwise_backward_sem_combn
#' @method estadisticos_ajuste stepwise_backward_sem_combn
#' @method plot stepwise_backward_sem_combn
#' @import "lavaan"
#' @import "ggplot2"
#' @import "cowplot"
#' @note The function uses the 'lavaan', 'ggplot2', and 'cowplot' package. Make sure to have it installed before using the function.
#' @examples
#' data(holzingerSwineford1939)
#' vars_pos <- names(holzingerSwineford1939)[1:8]
#' modelo <- stepwise_backward_sem_combn(holzingerSwineford1939, vars_pos)
#' print(modelo)
#' plot(modelo)


setClass("stepwise_backward_sem_combn", representation(
  datos = "data.frame",
  vars_pos = "character",
  estadisticos_ajuste = "data.frame",
  modelo_final = "character"
))

setGeneric("stepwise_backward_sem_combn", function(datos, vars_pos, formula,...) standardGeneric("stepwise_backward_sem_combn"))

setMethod("stepwise_backward_sem_combn", "stepwise_backward_sem_combn", function(datos, vars_pos, formula,...) {

  # Verifica que los datos sean un data frame
  if (!is.data.frame(datos)) {
    stop("Los datos deben ser un data frame.")
  }
  
  # Verifica que vars_pos sea un vector de caracteres
  if (!is.character(vars_pos)) {
    stop("vars_pos debe ser un vector de caracteres.")
  }
  # Verifica que formula sea un vector de caracteres
  if (!is.character(vars_pos)) {
    stop("formula debe ser un vector de caracteres.")
  }

    # Verifica que las variables especificadas en vars_pos existan en el data frame
  vars_inexistentes <- setdiff(vars_pos, colnames(datos))
  if (length(vars_inexistentes) > 0) {
    stop(paste("Las siguientes variables no existen en el data frame: ", paste(vars_inexistentes, collapse = ", ")))
  }

  # Verifica que las variables especificadas en vars_pos tengan valores numéricos válidos
  vars_no_numéricas <- vars_pos[sapply(datos[vars_pos], is.numeric) == FALSE]
  if (length(vars_no_numéricas) > 0) {
    stop(paste("Las siguientes variables no son numéricas: ", paste(vars_no_numéricas, collapse = ", ")))
  }
  
  # Inicializa un data frame para almacenar los resultados de los estadísticos de bondad de ajuste
  estadisticos_ajuste <- data.frame(modelo = character(), CFI = numeric(), TLI = numeric(), RMSEA = numeric())
  
# Genera todas las combinaciones posibles de variables
vars_combn <- list()
for (i in 2:9) {
  combinaciones <- vars_combn[[i-1]]
  for (j in 1:ncol(combinaciones)) {
    combinacion <- combinaciones[, j]
    # Crea la fórmula para el modelo SEM con la combinación actual
    formula <- paste("POS", "=~", paste(combinacion, collapse = "+"), "\n", formula)
    # Ajusta el modelo SEM con los datos
    modelo <- sem(formula, data = datos,...)

    # Obtiene las estadísticas de bondad de ajuste
    cfi <- lavaan::fitMeasures(modelo)$cfi
    rmsea <- lavaan::fitMeasures(modelo)$rmsea
    # Guarda las estadísticas en el data frame
    estadisticos_ajuste <- rbind(estadisticos_ajuste, data.frame(modelo = paste(combinacion, collapse = " ~ "), CFI = cfi, RMSEA = rmsea))
  }
}
  
  # Selecciona la combinación con las mejores estadísticas de ajuste
  modelo_final <- estadisticos_ajuste[which.max(estadisticos_ajuste$CFI), "modelo"]
  
  # Guardar el modelo final y las estadísticas de ajuste en un objeto de la clase stepwise_backward_sem_combn
  objeto <- new("stepwise_backward_sem_combn", datos = datos, vars_pos = vars_pos, estadisticos_ajuste = estadisticos_ajuste, modelo_final = modelo_final)
  
  return(objeto)
  
})

setMethod("print", "stepwise_backward_sem_combn", function(x) {
  cat("Modelo final: ", x@modelo_final, "\n")
  cat("Estadísticos de bondad de ajuste: \n")
  print(x@estadisticos_ajuste)
})

setGeneric("estadisticos_ajuste", function(x) standardGeneric("estadisticos_ajuste"))
setGeneric("modelo_final", function(x) standardGeneric("modelo_final"))

setMethod("estadisticos_ajuste", "stepwise_backward_sem_combn", function(x) {
  x@estadisticos_ajuste
})

setMethod("modelo_final", "stepwise_backward_sem_combn", function(x) {
  return(x@modelo_final)
})

setGeneric("plot", function(x, ...) standardGeneric("plot"))

setMethod("plot", "stepwise_backward_sem_combn", function(x, object) {
    library(ggplot2)
    library(cowplot)

    # Create separate plots for each statistic
    plot_CFI <- ggplot(object@estadisticos_ajuste, aes(x = modelo, y = CFI, fill = modelo)) +
        geom_bar(stat = "identity", position = "dodge") +
        ylim(0, 1) +
        labs(x = "Modelo", y = "CFI", fill = "Modelo") +
        theme_bw()

    plot_TLI <- ggplot(object@estadisticos_ajuste, aes(x = modelo, y = TLI, fill = modelo)) +
        geom_bar(stat = "identity", position = "dodge") +
        ylim(0, 1) +
        labs(x = "Modelo", y = "TLI", fill = "Modelo") +
        theme_bw()

    plot_RMSEA <- ggplot(object@estadisticos_ajuste, aes(x = modelo, y = RMSEA, fill = modelo)) +
        geom_bar(stat = "identity", position = "dodge") +
        ylim(0, 1) +
        labs(x = "Modelo", y = "RMSEA", fill = "Modelo") +
        theme_bw()

    # Combine the plots using cowplot
    plot_grid(plot_CFI, plot_TLI, plot_RMSEA, ncol = 3, align = "h", axis = "tb", axis_size = unit(0, "cm"))
})


