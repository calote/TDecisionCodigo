
## Graficos de los metodos multicriterio ----

# Nombres de columnas usados en dplyr::summarise() y ggplot2::aes(), que se
# evaluan con evaluacion no estandar.
utils::globalVariables(c("Alternativas", "Metodos", "Rangos", "SumaRangos",
                         "Valoraciones", "SumaValoraciones", "Criterios",
                         "Alternativa", "Orden", "Valores", "y_left", "y_right"))

#' Tabla larga de rangos por metodo
#'
#' Convierte una tabla con los rangos que cada metodo de decision
#' multicriterio asigna a cada alternativa en un data frame en formato
#' largo, con una fila por alternativa y metodo. La tabla resultante es la
#' que consumen \code{\link{plot_barras_acumuladas_rangos}} y
#' \code{\link{plot_segmentos_rangos}}.
#'
#' @param d1 Data frame o matriz con los rangos de cada metodo: una columna
#'   por metodo y una fila por alternativa. Los nombres de las alternativas
#'   deben ser los nombres de las filas.
#'
#' @return Data frame en formato largo con las columnas
#'   \code{Alternativas}, \code{Metodos}, \code{Rangos} y
#'   \code{SumaRangos} (suma de los rangos de la alternativa, que se usa
#'   para ordenar las alternativas de mejor a peor).
#'
#' @examples
#' rangos <- data.frame(RangosAHP = c(1, 5, 3), RangosPromethee = c(1, 5, 4))
#' rownames(rangos) <- c("Coche", "Bus1", "Bus2")
#' func_tabla_rangos_metodos(rangos)
#'
#' @export
func_tabla_rangos_metodos = function(d1) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function. Please install it with install.packages(\"dplyr\").", call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package \"tidyr\" needed for this function. Please install it with install.packages(\"tidyr\").", call. = FALSE)
  }
  if (!is.data.frame(d1) && !is.matrix(d1)) {
    stop("d1 debe ser un data frame o una matriz con los rangos de cada metodo.", call. = FALSE)
  }
  if (is.matrix(d1) && is.null(rownames(d1))) {
    stop("d1 debe tener los nombres de las alternativas como nombres de las filas.", call. = FALSE)
  }
  d1 = as.data.frame(d1, stringsAsFactors = FALSE)
  if (ncol(d1) < 1) {
    stop("d1 debe tener al menos una columna con los rangos de un metodo.", call. = FALSE)
  }

  tabla = dplyr::bind_cols(dplyr::tibble(Alternativas = rownames(d1)), d1)
  tabla_larga = tidyr::pivot_longer(tabla, cols = -1, names_to = "Metodos",
                                    values_to = "Rangos")
  sumas = tabla_larga |>
      dplyr::group_by(Alternativas) |>
      dplyr::summarise(SumaRangos = sum(Rangos), .groups = "drop")
  tabla_larga = dplyr::left_join(tabla_larga, sumas, by = "Alternativas")

  return(tabla_larga)

}


#' Grafico de flujos de PROMETHEE I
#'
#' Dibuja un grafico con los flujos entrantes (\eqn{\Phi^+}) y salientes
#' (\eqn{\Phi^-}) de cada alternativa en los extremos de un eje, unidos por
#' un segmento. Es la representacion habitual de PROMETHEE I, en la que las
#' alternativas mejor valoradas tienen un flujo entrante mayor y un flujo
#' saliente menor.
#'
#' @param phi_plus Vector numerico con el flujo entrante de cada alternativa.
#' @param phi_minus Vector numerico con el flujo saliente de cada alternativa.
#' @param alt_names Vector de character strings con los nombres de las
#'   alternativas. Por defecto se toman de los nombres de \code{phi_plus}.
#' @param normalize_by_n Logical. Si es \code{TRUE} los flujos se dividen
#'   entre el numero de alternativas menos uno, de modo que se situan en el
#'   intervalo \eqn{[0,1]}.
#' @param invert_right Logical. Si es \code{TRUE} el flujo saliente se
#'   representa invertido (0 arriba), como es habitual en estos graficos.
#' @param title Character string con el titulo del grafico.
#'
#' @return Objeto \code{ggplot} con el grafico.
#'
#' @examples
#' \dontrun{
#' X <- multicriterio.crea.matrizdecision(c(10, 20, 5, 8, 15, 12), 2, 3)
#' fpref <- data.frame(tipo = c(1, 1, 1), qi = c(0, 0, 0),
#'                     pi = c(5, 3, 2), si = c(1, 1, 1))
#' res <- multicriterio.metodo.promethee_windows(X, fpref, c(0.5, 0.3, 0.2))
#' plot_promethee_I(res$Acciones$Phi.mas, res$Acciones$Phi.menos)
#' }
#'
#' @export
plot_promethee_I = function(phi_plus,
                            phi_minus,
                            alt_names = names(phi_plus),
                            normalize_by_n = TRUE,
                            invert_right = TRUE,
                            title = "PROMETHEE I \u2014 \u03A6+ vs \u03A6-") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function. Please install it with install.packages(\"ggplot2\").", call. = FALSE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package \"ggrepel\" needed for this function. Please install it with install.packages(\"ggrepel\").", call. = FALSE)
  }
  if (length(phi_plus) != length(phi_minus)) {
    stop("phi_plus y phi_minus deben tener la misma longitud.", call. = FALSE)
  }
  n = length(phi_plus)
  if (n < 2) {
    stop("Se necesitan al menos dos alternativas.", call. = FALSE)
  }
  if (normalize_by_n) {
    phi_plus = phi_plus/(n - 1)
    phi_minus = phi_minus/(n - 1)
  }
  if (is.null(alt_names)) {
    alt_names = paste0("A", seq_len(n))
  }
  if (length(alt_names) != n) {
    stop("alt_names debe tener tantos elementos como alternativas.", call. = FALSE)
  }

  df = data.frame(Alternativa = alt_names,
                  y_left = as.numeric(phi_plus),
                  y_right = if (invert_right) 1 - as.numeric(phi_minus) else as.numeric(phi_minus),
                  stringsAsFactors = FALSE)

  grafico = ggplot2::ggplot(df) +
      ggplot2::geom_vline(xintercept = 0, linewidth = 1, color = "grey30") +
      ggplot2::geom_vline(xintercept = 1, linewidth = 1, color = "grey30") +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 1, y = y_left, yend = y_right,
                                         color = Alternativa),
                            linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(x = 0, y = y_left, color = Alternativa), size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = 1, y = y_right, color = Alternativa), size = 2) +
      ggrepel::geom_label_repel(ggplot2::aes(x = 0, y = y_left, label = Alternativa,
                                             color = Alternativa),
                                direction = "y", nudge_x = -0.05, size = 3,
                                show.legend = FALSE, seed = 1) +
      ggrepel::geom_label_repel(ggplot2::aes(x = 1, y = y_right, label = Alternativa,
                                             color = Alternativa),
                                direction = "y", nudge_x = 0.05, size = 3,
                                show.legend = FALSE, seed = 1) +
      ggplot2::scale_x_continuous(breaks = c(0, 1),
                                   labels = c("\u03A6+", "\u03A6- (0 arriba)")) +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
      ggplot2::labs(title = title, x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     panel.grid.major.x = ggplot2::element_blank())

  return(grafico)

}


#' Grafico de barras de un vector de valores
#'
#' Dibuja un diagrama de barras con los valores de un vector con el nombre de
#' las alternativas en el eje horizontal, ordenando las alternativas segun el
#' criterio indicado.
#'
#' @param v_datos Vector numerico con los valores de cada alternativa. Debe
#'   tener nombres, que se usan como etiquetas de las alternativas.
#' @param titulo Character string con el titulo del grafico.
#' @param subtitulo Character string con el subtitulo del grafico.
#' @param Ordenados Numerico. \code{-1} para orden decreciente (mayor valor
#'   primero), \code{1} para orden creciente y \code{0} para mantener el
#'   orden original del vector.
#'
#' @return Objeto \code{ggplot} con el grafico.
#'
#' @examples
#' \dontrun{
#' valores <- c(0.5, 0.3, 0.2)
#' names(valores) <- c("A1", "A2", "A3")
#' plot_barras_vector(valores)
#' }
#'
#' @export
plot_barras_vector = function(v_datos,
                              titulo = "Ordenaci\u00F3n de Alternativas",
                              subtitulo = "",
                              Ordenados = -1) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function. Please install it with install.packages(\"ggplot2\").", call. = FALSE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package \"ggrepel\" needed for this function. Please install it with install.packages(\"ggrepel\").", call. = FALSE)
  }
  if (!is.numeric(v_datos) || length(v_datos) == 0) {
    stop("v_datos debe ser un vector numerico no vacio.", call. = FALSE)
  }
  if (is.null(names(v_datos))) {
    stop("v_datos debe tener nombres: son las etiquetas de las alternativas.", call. = FALSE)
  }
  if (!Ordenados %in% c(-1, 0, 1)) {
    stop("Ordenados debe ser -1 (decreciente), 0 (original) o 1 (creciente).", call. = FALSE)
  }

  if (Ordenados == -1) {
    v_orden = -v_datos
  } else if (Ordenados == 1) {
    v_orden = v_datos
  } else {
    v_orden = 1:length(v_datos)
  }

  datos = data.frame(Valores = v_datos,
                     Alternativas = names(v_datos),
                     Orden = v_orden,
                     stringsAsFactors = FALSE)

  grafico = datos %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(Alternativas, Orden),
                                    y = Valores, fill = Alternativas)) +
      ggplot2::geom_col() +
      ggrepel::geom_label_repel(ggplot2::aes(x = Alternativas, y = Valores,
                                             label = Alternativas),
                                direction = "y", nudge_x = -0.05, size = 3,
                                show.legend = TRUE, seed = 1) +
      ggplot2::labs(title = titulo, subtitle = subtitulo,
                    y = "Valores", x = "Alternativas") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

  return(grafico)

}


#' Grafico de lineas de comparacion de rangos
#'
#' Dibuja un grafico de lineas con el rango que cada metodo de decision
#' multicriterio asigna a cada alternativa, lo que permite comparar las
#' ordenaciones obtenidas por distintos metodos.
#'
#' @param d1 Data frame o matriz con los rangos de cada metodo: una columna
#'   por metodo y una fila por alternativa. Los nombres de las alternativas
#'   deben ser los nombres de las filas.
#'
#' @return Objeto \code{ggplot} con el grafico.
#'
#' @examples
#' \dontrun{
#' rangos <- data.frame(RangosAHP = c(1, 5, 3), RangosPromethee = c(1, 5, 4))
#' rownames(rangos) <- c("Coche", "Bus1", "Bus2")
#' plot_lineas_rangos(rangos)
#' }
#'
#' @export
plot_lineas_rangos = function(d1) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function. Please install it with install.packages(\"ggplot2\").", call. = FALSE)
  }

  d1lb = func_tabla_rangos_metodos(d1)

  grafico = d1lb %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(Alternativas, SumaRangos),
                                    y = Rangos, group = Metodos, colour = Metodos)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::labs(title = "Comparaci\u00F3n de Rangos",
                    subtitle = "Cuanto menor valor es MEJOR",
                    y = "Rangos", x = "Alternativas") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.5,
                                                          face = "bold"),
                     panel.grid.minor = ggplot2::element_blank())

  return(grafico)

}


#' Grafico de barras apiladas de comparacion de rangos
#'
#' Dibuja un diagrama de barras apiladas con los rangos que cada metodo de
#' decision multicriterio asigna a cada alternativa, lo que permite comparar
#' visualmente las ordenaciones obtenidas por distintos metodos.
#'
#' @param df_l_AltMetRangos Data frame en formato largo con las columnas
#'   \code{Alternativas}, \code{Metodos}, \code{Rangos} y
#'   \code{SumaRangos}, como el que devuelve
#'   \code{\link{func_tabla_rangos_metodos}}.
#'
#' @return Objeto \code{ggplot} con el grafico.
#'
#' @examples
#' \dontrun{
#' rangos <- data.frame(RangosAHP = c(1, 5, 3), RangosPromethee = c(1, 5, 4))
#' rownames(rangos) <- c("Coche", "Bus1", "Bus2")
#' plot_barras_acumuladas_rangos(func_tabla_rangos_metodos(rangos))
#' }
#'
#' @export
plot_barras_acumuladas_rangos = function(df_l_AltMetRangos) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function. Please install it with install.packages(\"ggplot2\").", call. = FALSE)
  }
  if (!is.data.frame(df_l_AltMetRangos) ||
      !all(c("Alternativas", "Metodos", "Rangos", "SumaRangos") %in% names(df_l_AltMetRangos))) {
    stop("df_l_AltMetRangos debe ser la tabla larga que devuelve func_tabla_rangos_metodos().", call. = FALSE)
  }

  grafico = df_l_AltMetRangos %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = stats::reorder(Alternativas, SumaRangos),
                                     y = Rangos, fill = Metodos),
                        position = "stack") +
      ggplot2::labs(title = "Comparaci\u00F3n de Rangos",
                    subtitle = "Cuanto menor valor es MEJOR",
                    y = "Rangos", x = "Alternativas") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.5,
                                                          face = "bold"),
                     panel.grid.minor = ggplot2::element_blank())

  return(grafico)

}


#' Grafico de segmentos de comparacion de rangos
#'
#' Dibuja un grafico de segmentos en el que cada alternativa aparece en la
#' posicion que le asigna cada metodo, formando un segmento vertical entre
#' su mejor y su peor posicion. Sirve para visualizar hasta donde discrepan
#' las ordenaciones obtenidas por distintos metodos.
#'
#' @param df_l_AltMetRangos Data frame en formato largo con las columnas
#'   \code{Alternativas}, \code{Metodos}, \code{Rangos} y
#'   \code{SumaRangos}, como el que devuelve
#'   \code{\link{func_tabla_rangos_metodos}}.
#'
#' @return Objeto \code{ggplot} con el grafico.
#'
#' @examples
#' \dontrun{
#' rangos <- data.frame(RangosAHP = c(1, 5, 3), RangosPromethee = c(1, 5, 4))
#' rownames(rangos) <- c("Coche", "Bus1", "Bus2")
#' plot_segmentos_rangos(func_tabla_rangos_metodos(rangos))
#' }
#'
#' @export
plot_segmentos_rangos = function(df_l_AltMetRangos) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function. Please install it with install.packages(\"ggplot2\").", call. = FALSE)
  }
  if (!is.data.frame(df_l_AltMetRangos) ||
      !all(c("Alternativas", "Metodos", "Rangos", "SumaRangos") %in% names(df_l_AltMetRangos))) {
    stop("df_l_AltMetRangos debe ser la tabla larga que devuelve func_tabla_rangos_metodos().", call. = FALSE)
  }

  grafico = ggplot2::ggplot(df_l_AltMetRangos,
                            ggplot2::aes(x = stats::reorder(Alternativas, SumaRangos),
                                         y = Rangos)) +
      ggplot2::geom_line(ggplot2::aes(group = Alternativas), color = "#D55E00",
                         linewidth = 1) +
      ggplot2::geom_point(color = "#D55E00", size = 3) +
      ggplot2::geom_segment(ggplot2::aes(x = Alternativas, xend = Alternativas,
                                          y = min(Rangos), yend = max(Rangos)),
                            linetype = "dotted", color = "gray50", linewidth = 0.3) +
      ggplot2::labs(title = "Intervalo de Rangos para ordenaci\u00F3n alternativas",
                    subtitle = "Comparaci\u00F3n m\u00E9todos decisi\u00F3n multicriterio",
                    x = "Alternativas", y = "Rangos") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(size = 16, face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 12),
                     axis.title.x = ggplot2::element_text(size = 11,
                                                          margin = ggplot2::margin(t = 10)))

  return(grafico)

}


#' Grafico de barras apiladas de comparacion de valoraciones
#'
#' Dibuja un diagrama de barras apiladas con las valoraciones de cada
#' alternativa, desagregadas por criterio. El grafico es mas informativo
#' cuando la matriz de valoraciones esta homogeneizada, por ejemplo con
#' \code{\link{multicriterio.homogeneizacion.nadir}}.
#'
#' @param Mvalor Matriz numerica de valoracion (alternativas x criterios). Se
#'   espera que los valores sean comparables entre criterios.
#'
#' @return Objeto \code{ggplot} con el grafico.
#'
#' @examples
#' \dontrun{
#' Mvalor <- matrix(c(8, 25, 20, 10, 75, 5,
#'                    12, 35, 17, 25, 100, 7), nrow = 2, ncol = 6, byrow = TRUE)
#' rownames(Mvalor) <- c("A1", "A2")
#' plot_barras_acumuladas_Mvaloraciones(
#'   multicriterio.homogeneizacion.nadir(Mvalor) * 100)
#' }
#'
#' @export
plot_barras_acumuladas_Mvaloraciones = function(Mvalor) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function. Please install it with install.packages(\"ggplot2\").", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function. Please install it with install.packages(\"dplyr\").", call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package \"tidyr\" needed for this function. Please install it with install.packages(\"tidyr\").", call. = FALSE)
  }
  if (!is.matrix(Mvalor) && !is.data.frame(Mvalor)) {
    stop("Mvalor debe ser una matriz numerica de valoracion.", call. = FALSE)
  }
  Mvalor = as.matrix(Mvalor)
  if (is.null(rownames(Mvalor))) {
    rownames(Mvalor) = paste0("a", 1:nrow(Mvalor))
  }
  if (is.null(colnames(Mvalor))) {
    colnames(Mvalor) = paste0("C", 1:ncol(Mvalor))
  }

  df_homog = as.data.frame(t(Mvalor), stringsAsFactors = FALSE)
  df_homog2 = dplyr::bind_cols(dplyr::tibble(Criterios = rownames(df_homog)), df_homog)
  df_homog2_l = tidyr::pivot_longer(df_homog2, names_to = "Alternativas",
                                     cols = -"Criterios", values_to = "Valoraciones")
  v1t = df_homog2_l |>
      dplyr::group_by(Alternativas) |>
      dplyr::summarise(SumaValoraciones = sum(Valoraciones), .groups = "drop")
  df_homog2_lb = dplyr::left_join(df_homog2_l, v1t, by = "Alternativas")

  grafico = df_homog2_lb %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = stats::reorder(Alternativas, -SumaValoraciones),
                                     y = Valoraciones, fill = Criterios),
                        position = "stack") +
      ggplot2::labs(title = "Comparaci\u00F3n de Alternativas",
                    subtitle = "Cuanto mayor valor es MEJOR",
                    y = "Valoraciones", x = "Alternativas") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.5,
                                                          face = "bold"),
                     panel.grid.minor = ggplot2::element_blank())

  return(grafico)

}
