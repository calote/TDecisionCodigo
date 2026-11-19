#
# PROBLEMAS DE ARBOLES DE DECISION EN AMBIENTE DE RIESGO ------
#

## Funciones para especificar, resolver y dibujar arboles de decision.
## Incluye un mini-lenguaje de especificacion (arb_T(), arb_P(),
## arb_R(), arb_D()), validacion del arbol, induccion hacia atras
## (rollback), tablas de decision (esperanza, REMA, REIP, VEIP) y salida
## a Graphviz, Mermaid y tablas Typst anidadas.


# Especificacion (DSL) ------------

#' Nodo terminal de un arbol de decision
#'
#' Crea un nodo terminal (triangulo) con su pago.
#'
#' @param payoff Numero con el pago del nodo terminal.
#'
#' @return Objeto de clase `arboles_nodo` de tipo terminal.
#'
#' @examples
#' arb_T(15000)
#'
#' @export
arb_T = function(payoff) {
  stopifnot(length(payoff) == 1L, is.numeric(payoff))
  structure(list(type = "T", payoff = payoff), class = "arboles_nodo")
}

#' Asocia una probabilidad a una rama de azar
#'
#' Envuelve un nodo para indicar que la rama que lleva hasta el ocurre con
#' probabilidad `prob`. Solo tiene sentido en ramas de nodos de riesgo.
#'
#' @param prob Numero entre 0 y 1 con la probabilidad de la rama.
#' @param node Nodo creado con `arb_T()`, `arb_R()` o `arb_D()`.
#'
#' @return El mismo nodo con la probabilidad de llegada anotada.
#'
#' @examples
#' arb_P(0.6, arb_T(15000))
#'
#' @export
arb_P = function(prob, node) {
  stopifnot(inherits(node, "arboles_nodo"), length(prob) == 1L,
            is.numeric(prob), prob >= 0, prob <= 1)
  node$prob = prob
  node
}

#' Nodo de riesgo (azar) de un arbol de decision
#'
#' Crea un nodo de riesgo (circulo). Cada argumento nombrado es una rama y
#' debe ir envuelto en `arb_P()`; las probabilidades de un mismo nodo deben
#' sumar 1 (se comprueba al compilar).
#'
#' @param label Etiqueta del nodo (debe ser unica en el arbol).
#' @param ... Ramas nombradas `etiqueta = arb_P(prob, nodo)`.
#'
#' @return Objeto de clase `arboles_nodo` de tipo riesgo.
#'
#' @examples
#' arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000)))
#'
#' @export
arb_R = function(label, ...) {
  branches = list(...)
  if (is.null(names(branches)) || any(names(branches) == ""))
    stop("arb_R(): cada rama necesita etiqueta, p. ej. arb_R(\"NR1\", w1 = arb_P(0.6, arb_T(5)))")
  structure(list(type = "R", label = label, branches = branches), class = "arboles_nodo")
}

#' Nodo de decision de un arbol de decision
#'
#' Crea un nodo de decision (cuadrado). Cada argumento nombrado es una
#' alternativa y **no** lleva probabilidad. El hijo de cada alternativa
#' puede ser un nodo terminal (`arb_T()`), de riesgo (`arb_R()`) o incluso
#' otro nodo de decision (decisiones secuenciales).
#'
#' @param label Etiqueta del nodo (debe ser unica en el arbol).
#' @param ... Alternativas nombradas `etiqueta = nodo`.
#'
#' @return Objeto de clase `arboles_nodo` de tipo decision.
#'
#' @examples
#' arb_D("ND1", A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000))))
#'
#' @export
arb_D = function(label, ...) {
  branches = list(...)
  if (is.null(names(branches)) || any(names(branches) == ""))
    stop("arb_D(): cada rama necesita etiqueta, p. ej. arb_D(\"ND1\", A = ...)")
  structure(list(type = "D", label = label, branches = branches), class = "arboles_nodo")
}


# Compilacion y validacion ------------

#' Compila un arbol especificado con el mini-lenguaje a nodos + aristas
#'
#' Convierte la especificacion anidada (`arb_D()` / `arb_R()` / `arb_T()` /
#' `arb_P()`) al modelo interno validado: tabla de nodos, tabla de aristas y
#' vector de pagos. Falla con mensaje claro si la especificacion no es valida.
#'
#' @param root Nodo raiz creado con `arb_D()` o `arb_R()`.
#'
#' @return Objeto de clase `dtree` con `nodes` (data.frame `id`, `type`,
#'   `label`), `edges` (data.frame `from`, `to`, `label`, `prob`) y `payoff`
#'   (vector numerico nombrado por id de terminal).
#'
#' @examples
#' t31 <- arb_D("ND1",
#'   A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000))),
#'   B = arb_R("NR2", w1 = arb_P(0.6, arb_T(11500)), w2 = arb_P(0.4, arb_T(10500))))
#' tr31 <- arboles_compile_tree(t31)
#' tr31$nodes
#'
#' @export
arboles_compile_tree = function(root) {
  stopifnot(inherits(root, "arboles_nodo"))
  nodes = data.frame(id = character(), type = character(), label = character(),
                     stringsAsFactors = FALSE)
  edges = data.frame(from = character(), to = character(), label = character(),
                     prob = numeric(), stringsAsFactors = FALSE)
  pay = stats::setNames(numeric(), character())
  n = 0L
  walk = function(node, parent = NA_character_, elabel = NA_character_,
                  eprob = NA_real_) {
    if (node$type == "T") {
      n <<- n + 1L
      id = paste0("t", n)
      disp = arboles_format_num(node$payoff)
      nodes <<- rbind(nodes, data.frame(id = id, type = "T", label = disp,
                                        stringsAsFactors = FALSE))
      pay[id] <<- node$payoff
    } else {
      id = node$label
      if (id %in% nodes$id) stop("Etiqueta de nodo duplicada: ", id)
      nodes <<- rbind(nodes, data.frame(id = id, type = node$type, label = id,
                                        stringsAsFactors = FALSE))
      nm = names(node$branches)
      for (i in seq_along(node$branches)) {
        b = node$branches[[i]]
        if (!inherits(b, "arboles_nodo")) stop("Rama '", nm[i], "' no es un nodo arb_T/arboles_R/arb_D")
        walk(b, parent = id, elabel = nm[i],
             eprob = if (!is.null(b$prob)) b$prob else NA_real_)
      }
    }
    if (!is.na(parent))
      edges <<- rbind(edges, data.frame(from = parent, to = id, label = elabel,
                                        prob = eprob, stringsAsFactors = FALSE))
    invisible(id)
  }
  root_id = walk(root)
  tree = structure(list(nodes = nodes, edges = edges, payoff = pay,
                        root = root_id), class = "dtree")
  arboles_validate_tree(tree)
  tree
}

#' Construye un arbol validado a partir de tablas planas
#'
#' Segunda puerta de entrada al mismo modelo que
#' [arboles_compile_tree()]: nodos + aristas como data.frames (aptos para
#' CSV o generacion programatica) en vez de especificacion anidada.
#' Imprescindible en arboles grandes, donde anidar parentesis es propenso
#' a errores.
#'
#' @param nodes Data.frame con columnas `id`, `type` (`"D"`, `"R"` o `"T"`)
#'   y opcionalmente `label`.
#' @param edges Data.frame con columnas `from`, `to`, `label` y `prob`
#'   (`NA` en aristas de decision).
#' @param payoff Vector numerico nombrado con el pago de cada nodo `T`.
#'
#' @return Objeto de clase `dtree`, igual que [arboles_compile_tree()].
#'
#' @examples
#' nd <- data.frame(id = c("D1", "C1", "t1", "t2"),
#'                  type = c("D", "R", "T", "T"),
#'                  stringsAsFactors = FALSE)
#' ed <- data.frame(from = c("D1", "C1", "C1"),
#'                  to = c("C1", "t1", "t2"),
#'                  label = c("jugar", "cara", "cruz"),
#'                  prob = c(NA, 0.5, 0.5),
#'                  stringsAsFactors = FALSE)
#' tr <- arboles_from_dataframes(nd, ed, c(t1 = 10, t2 = -5))
#' arboles_rollback(tr)$rema
#'
#' @export
arboles_from_dataframes = function(nodes, edges, payoff) {
  stopifnot(all(c("id", "type") %in% names(nodes)),
            all(c("from", "to", "label") %in% names(edges)))
  if (!"label" %in% names(nodes)) nodes$label = nodes$id
  if (!"prob" %in% names(edges)) edges$prob = NA_real_
  roots = setdiff(nodes$id, edges$to)
  if (length(roots) != 1L) stop("Debe haber exactamente un nodo ra\u00edz")
  tree = structure(list(nodes = nodes, edges = edges, payoff = payoff,
                        root = roots), class = "dtree")
  arboles_validate_tree(tree)
  tree
}

#' Valida un arbol de decision
#'
#' Comprueba: tipos validos (`"D"`, `"R"`, `"T"`), aristas con extremos
#' existentes, terminales sin salida y con pago, nodos de riesgo con
#' probabilidades entre 0 y 1 que sumen 1, y nodos de decision con al menos
#' una rama. La llaman [arboles_compile_tree()] y
#' [arboles_from_dataframes()]; tambien puede usarse directamente.
#'
#' @param tree Objeto de clase `dtree`.
#'
#' @return El mismo arbol, invisiblemente (falla si algo no cuadra).
#' @noRd
arboles_validate_tree = function(tree) {
  nodes = tree$nodes
  edges = tree$edges
  if (!all(nodes$type %in% c("D", "R", "T"))) stop("type debe ser D, R o T")
  if (!all(edges$from %in% nodes$id, edges$to %in% nodes$id))
    stop("Arista con extremo desconocido")
  for (id in nodes$id) {
    tp = nodes$type[nodes$id == id]
    out = edges[edges$from == id, , drop = FALSE]
    if (tp == "T") {
      if (nrow(out) > 0) stop("Nodo terminal con salida: ", id)
      if (!id %in% names(tree$payoff)) stop("Terminal sin payoff: ", id)
    } else if (tp == "R") {
      if (nrow(out) == 0) stop("Nodo de riesgo sin ramas: ", id)
      if (any(is.na(out$prob))) stop("Rama de riesgo sin probabilidad en: ", id)
      if (abs(sum(out$prob) - 1) > 1e-8)
        stop("Probabilidades de ", id, " suman ", sum(out$prob), " (deben sumar 1)")
    } else {
      if (nrow(out) == 0) stop("Nodo de decisi\u00f3n sin ramas: ", id)
    }
  }
  invisible(tree)
}


# Rollback ------------

#' Induccion hacia atras (rollback) en un arbol de decision
#'
#' Resuelve el arbol de derecha a izquierda: en nodos de riesgo pondera
#' por probabilidades y en nodos de decision toma el maximo. Vale para
#' arboles de una y de N etapas (decisiones secuenciales).
#'
#' @param tree Objeto de clase `dtree` (ver [arboles_compile_tree()]).
#'
#' @return Lista con `values` (esperanza por nodo), `optimal` (hijos
#'   optimos de cada nodo de decision), `rema` (valor del nodo raiz) y
#'   `root` (id de la raiz).
#'
#' @importFrom stats setNames
#'
#' @examples
#' t31 <- arb_D("ND1",
#'   A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000))),
#'   B = arb_R("NR2", w1 = arb_P(0.6, arb_T(11500)), w2 = arb_P(0.4, arb_T(10500))))
#' rb <- arboles_rollback(arboles_compile_tree(t31))
#' rb$rema      # 12200
#' rb$optimal   # $ND1: "NR1"
#'
#' @export
arboles_rollback = function(tree) {
  stopifnot(inherits(tree, "dtree"))
  nodes = tree$nodes
  edges = tree$edges
  values = stats::setNames(vector("list", nrow(nodes)), nodes$id)
  optimal = list()
  visit = function(id) {
    if (!is.null(values[[id]])) return(values[[id]])
    tp = nodes$type[nodes$id == id]
    if (tp == "T") {
      values[[id]] <<- unname(tree$payoff[[id]])
    } else {
      out = edges[edges$from == id, , drop = FALSE]
      v = vapply(out$to, visit, numeric(1))
      if (tp == "R") {
        values[[id]] <<- sum(out$prob * v)
      } else {
        m = max(v)
        values[[id]] <<- m
        optimal[[id]] <<- out$to[v == m]
      }
    }
    values[[id]]
  }
  rema = visit(tree$root)
  list(values = stats::setNames(unlist(values), nodes$id),
       optimal = optimal, rema = rema, root = tree$root)
}


# Tablas de decision ------------

#' Tabla de decision: esperanzas, REMA, REIP y VEIP
#'
#' Dada la matriz de pagos (alternativas en filas, estados en columnas) y
#' el vector de probabilidades, calcula la esperanza de cada alternativa,
#' la mejor (REMA), el resultado esperado con informacion perfecta (REIP)
#' y su valor (VEIP = REIP - REMA). Con `maximize = FALSE` minimiza
#' (costes): REMA es el minimo y VEIP = REIP - REMA.
#'
#' Las tablas "anidadas" no necesitan funcion propia: cada celda interior
#' es otra llamada con probabilidades condicionales.
#'
#' @param payoff Matriz numerica de pagos (filas = alternativas).
#' @param probs Vector numerico de probabilidades de los estados (suma 1).
#' @param maximize Logico: `TRUE` maximiza (beneficios), `FALSE` minimiza
#'   (costes).
#'
#' @return Lista con `E` (esperanzas), `best` (indice de la mejor),
#'   `rema`, `colmax` (maximos por columna), `reip` y `veip`.
#'
#' @examples
#' m <- matrix(c(15000, 11500, 8000, 10500), nrow = 2, byrow = FALSE)
#' dt <- arboles_decision_table(m, c(0.6, 0.4))
#' dt$E; dt$rema; dt$reip; dt$veip
#'
#' @export
arboles_decision_table = function(payoff, probs, maximize = TRUE) {
  stopifnot(is.matrix(payoff), is.numeric(probs),
            ncol(payoff) == length(probs), abs(sum(probs) - 1) < 1e-8)
  E = as.numeric(payoff %*% probs)
  if (!is.null(rownames(payoff))) names(E) = rownames(payoff)
  best = if (maximize) which.max(E) else which.min(E)
  colmax = if (maximize) apply(payoff, 2L, max) else apply(payoff, 2L, min)
  reip = sum(colmax * probs)
  list(E = E, best = best, rema = E[best],
       colmax = colmax, reip = reip, veip = reip - E[best])
}


# Dibujo con Graphviz ------------

#' Terminal estrella: maximo entre hijos terminales de un nodo de riesgo
#'
#' Marca con `*` el mejor hijo terminal de cada nodo de riesgo cuyos hijos
#' sean todos terminales. (Los maximos por columna de una tabla viven en
#' `colmax` de [arboles_decision_table()], no en el arbol.)
#'
#' @param tree Objeto de clase `dtree`.
#'
#' @return Vector de ids de terminales estrella.
#' @noRd
arboles_starred_terminals = function(tree) {
  starred = character()
  for (id in tree$nodes$id[tree$nodes$type == "R"]) {
    kids = tree$edges$to[tree$edges$from == id]
    kt = tree$nodes$type[match(kids, tree$nodes$id)]
    if (length(kids) > 0 && all(kt == "T")) {
      pv = tree$payoff[kids]
      starred = c(starred, kids[pv == max(pv)])
    }
  }
  starred
}

#' Formatea numeros sin notacion cientifica
#'
#' @param x Vector numerico.
#' @param digits Digitos significativos (ver `format()`).
#'
#' @return Vector de caracteres.
#' @noRd
arboles_format_num = function(x, digits = 6) format(x, digits = digits, trim = TRUE,
                                                   scientific = FALSE)

#' Genera codigo DOT (Graphviz) de un arbol de decision
#'
#' Cuadrado = decision, circulo = riesgo (con su media (mu) calculada),
#' triangulo = pago terminal; en rojo las ramas optimas de decision y el
#' nodo de riesgo al que llevan. Con `mark_starred = TRUE` se rellenan
#' en amarillo y se marcan con `*` los mejores hijos terminales de cada
#' nodo de riesgo (util para el calculo del REIP); por defecto no se
#' marcan porque graficamente se usa poco.
#'
#' El codigo DOT también puede visualizarse sin instalar Graphviz con
#' `DiagrammeR::grViz()`, que lo muestra en el visor de RStudio o en el
#' navegador como pagina HTML autocontenida. Para guardar esa vista en
#' un fichero PNG se necesita ademas \pkg{webshot2} y un navegador
#' Chrome o Chromium, igual que en la exportacion de tablas a PNG.
#'
#' Con `show_values = FALSE` y `mark_optimal = FALSE` no aparece ningun
#' calculo ni marca de optimo: util para entregar el arbol como ejercicio.
#'
#' @param tree Objeto de clase `dtree`.
#' @param rb Resultado de [arboles_rollback()] (se calcula si hace falta).
#' @param title Titulo opcional del grafico.
#' @param rankdir `"LR"` (horizontal) o `"TB"` (vertical; mejor para
#'   arboles secuenciales altos en pagina portrait).
#' @param show_values Logico: mostrar las esperanzas calculadas.
#' @param mark_optimal Logico: marcar en rojo la decision optima y el
#'   nodo de riesgo al que lleva.
#' @param mark_starred Logico: resaltar en amarillo y con `*` el mejor
#'   hijo terminal de cada nodo de riesgo (para el REIP). Por defecto
#'   `FALSE`.
#'
#' @return Cadena de caracteres con el codigo DOT.
#'
#' @examples
#' t31 <- arb_D("ND1",
#'   A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000))))
#' cat(arboles_to_dot(arboles_compile_tree(t31)))
#' cat(arboles_to_dot(arboles_compile_tree(t31), mark_starred = TRUE))
#' cat(arboles_to_dot(arboles_compile_tree(t31),
#'                    show_values = FALSE, mark_optimal = FALSE))
#'
#' @export
arboles_to_dot = function(tree, rb = NULL, title = NULL, rankdir = "LR",
                          show_values = TRUE, mark_optimal = TRUE,
                          mark_starred = FALSE) {
  stopifnot(inherits(tree, "dtree"))
  if ((show_values || mark_optimal) && is.null(rb)) rb = arboles_rollback(tree)
  nodes = tree$nodes
  edges = tree$edges
  opt_children = if (mark_optimal) unique(unlist(rb$optimal)) else character()
  starred = if (mark_starred) arboles_starred_terminals(tree) else character()
  L = c("digraph dtree {", paste0("  rankdir=", rankdir, ";"),
        '  graph [fontname="DejaVu Sans"];',
        '  node [fontname="DejaVu Sans", fontsize=11];',
        '  edge [fontname="DejaVu Sans", fontsize=10];')
  if (!is.null(title))
    L = c(L, sprintf('  labelloc=top; label="%s"; fontsize=14;', title))
  for (i in seq_len(nrow(nodes))) {
    id = nodes$id[i]
    tp = nodes$type[i]
    if (tp == "D") {
      L = c(L, sprintf('  "%s" [shape=box, style=filled, fillcolor="#dbeafe", label="%s"];', id, id))
    } else if (tp == "R") {
      col = if (mark_optimal && id %in% opt_children) ", color=red, penwidth=2.5" else ""
      lab = id
      if (show_values)
        lab = paste0(lab, "\\n\u03bc = ", arboles_format_num(rb$values[[id]]))
      L = c(L, sprintf('  "%s" [shape=circle%s, label="%s"];', id, col, lab))
    } else {
      lab = arboles_format_num(tree$payoff[[id]])
      st = if (id %in% starred) ', style=filled, fillcolor="#fef9c3"' else ""
      if (id %in% starred) lab = paste0(lab, " *")
      L = c(L, sprintf('  "%s" [shape=triangle%s, label="%s"];', id, st, lab))
    }
  }
  for (e in seq_len(nrow(edges))) {
    fr = edges$from[e]
    to = edges$to[e]
    lab = edges$label[e]
    if (!is.na(edges$prob[e])) lab = sprintf("%s, %s", lab, arboles_format_num(edges$prob[e]))
    red = mark_optimal && fr %in% names(rb$optimal) && to %in% rb$optimal[[fr]]
    st = if (red) ", color=red, penwidth=2.5" else ""
    L = c(L, sprintf('  "%s" -> "%s" [label="%s"%s];', fr, to, lab, st))
  }
  paste(c(L, "}"), collapse = "\n")
}

#' Dibuja un arbol de decision con Graphviz a SVG o PNG
#'
#' Compila el arbol a DOT con [arboles_to_dot()] y lo convierte con el
#' programa `dot` (Graphviz). Guarda el `.dot` junto a la imagen, para
#' transparencia y reproducibilidad. Recomendado para docencia: un solo
#' `apt install graphviz`, sin navegador ni Node.
#'
#' @section Dependencia externa:
#' Solo el dibujo necesita Graphviz; los cálculos
#' ([arboles_compile_tree()], [arboles_rollback()],
#' [arboles_decision_table()]) funcionan sin él. Instalación:
#'
#' * Linux (Debian/Ubuntu): `sudo apt install graphviz`
#'   (Fedora: `sudo dnf install graphviz`; Arch: `sudo pacman -S graphviz`).
#' * macOS (Homebrew): `brew install graphviz`.
#' * Windows: `winget install graphviz.graphviz` o el instalador de
#'   <https://graphviz.org/download/> (marcar la casilla de añadir
#'   Graphviz al PATH).
#'
#' Tras instalar, reinicia RStudio y comprueba en la consola R que
#' `Sys.which("dot")` devuelve una ruta (si sigue vacío en macOS,
#' abre RStudio desde el terminal con `open -a RStudio`).
#'
#' @param tree Objeto de clase `dtree`.
#' @param file Fichero de salida (extension segun `format`).
#' @param format `"svg"` o `"png"`. Si se omite, se deduce de la
#'   extensión de `file` (por defecto `"svg"`).
#' @param title Titulo opcional del grafico.
#' @param rankdir `"LR"` (horizontal) o `"TB"` (vertical).
#' @param dot_file Donde guardar el `.dot` (por defecto, junto a `file`).
#' @param show_values Logico: mostrar las esperanzas calculadas.
#' @param mark_optimal Logico: marcar en rojo la decision optima.
#'   Con ambos en `FALSE` el dibujo no contiene ningun calculo: util
#'   como enunciado de ejercicio para el alumnado.
#' @param mark_starred Logico: resaltar en amarillo y con `*` el mejor
#'   hijo terminal de cada nodo de riesgo (para el REIP). Por defecto
#'   `FALSE`.
#' @param dpi Resolucion para PNG.
#'
#' @return `file`, invisiblemente. Falla con mensaje claro si `dot` no
#'   esta en el PATH.
#'
#' @examples
#' \dontrun{
#' tr <- arboles_compile_tree(arb_D("ND1", A = arb_T(10)))
#' arboles_draw_tree(tr, tempfile(fileext = ".svg"))
#' }
#'
#' @export
arboles_draw_tree = function(tree, file, format = c("svg", "png"),
                             title = NULL, rankdir = "LR", dot_file = NULL,
                             dpi = 150, show_values = TRUE, mark_optimal = TRUE,
                             mark_starred = FALSE) {
  if (missing(format)) {
    bn = basename(file)
    ext = if (grepl(".", bn, fixed = TRUE)) tolower(sub("^.*\\.([^.]+)$", "\\1", bn)) else ""
    if (ext %in% c("svg", "png")) format = ext
  }
  format = match.arg(format)
  bn = basename(file)
  ext = if (grepl(".", bn, fixed = TRUE)) tolower(sub("^.*\\.([^.]+)$", "\\1", bn)) else ""
  if (nzchar(ext) && ext != format) {
    warning("La extensi\u00f3n de `file` ('", ext, "') no coincide con ",
            "`format = \"", format, "\"`; el fichero se escribe en formato ", format, ".")
  }
  if (Sys.which("dot") == "") {
    stop("Graphviz (`dot`) no est\u00e1 en el PATH. ",
         "Inst\u00e1lalo (Linux: `sudo apt install graphviz`; ",
         "macOS: `brew install graphviz`; ",
         "Windows: `winget install graphviz.graphviz`), ",
         "reinicia RStudio y reintenta. Ver `?arboles_draw_tree`.")
  }
  dot = arboles_to_dot(tree, title = title, rankdir = rankdir,
                       show_values = show_values, mark_optimal = mark_optimal,
                       mark_starred = mark_starred)
  if (is.null(dot_file)) dot_file = sub("\\.[^.]+$", ".dot", file)
  writeLines(dot, dot_file)
  args = c(paste0("-T", format), dot_file, "-o", file)
  if (format == "png") args = c(paste0("-Gdpi=", dpi), args)
  st = system2("dot", args)
  if (st != 0) stop("`dot` fall\u00f3 con c\u00f3digo ", st)
  invisible(file)
}


# Dibujo con Mermaid ------------

#' Genera codigo `flowchart` Mermaid de un arbol de decision
#'
#' Equivalente a [arboles_to_dot()] en sintaxis Mermaid: `D -> [...]`,
#' `R -> ((...))` con su media (mu), `T -> [/.../]`; aristas optimas en rojo
#' (`linkStyle`). Sin triangulos (Mermaid no los tiene): los terminales
#' son paralelogramos.
#'
#' Con `show_values = FALSE` y `mark_optimal = FALSE` no aparece ningun
#' calculo ni marca de optimo: util para entregar el arbol como ejercicio.
#'
#' @param tree Objeto de clase `dtree`.
#' @param rb Resultado de [arboles_rollback()] (se calcula si falta).
#' @param title Titulo opcional (cabecera YAML del `.mmd`).
#' @param rankdir `"LR"` (-> `flowchart LR`) o `"TB"` (-> `flowchart TD`).
#' @param show_values Logico: mostrar las esperanzas calculadas.
#' @param mark_optimal Logico: marcar en rojo la decision optima.
#' @param mark_starred Logico: marcar con `*` el mejor hijo terminal de
#'   cada nodo de riesgo (para el REIP). Por defecto `FALSE`.
#'
#' @return Cadena de caracteres con el codigo Mermaid.
#'
#' @examples
#' t31 <- arb_D("ND1",
#'   A = arb_R("NR1", w1 = arb_P(0.6, arb_T(15000)), w2 = arb_P(0.4, arb_T(8000))))
#' cat(arboles_to_mermaid(arboles_compile_tree(t31)))
#' cat(arboles_to_mermaid(arboles_compile_tree(t31), mark_starred = TRUE))
#' cat(arboles_to_mermaid(arboles_compile_tree(t31),
#'                        show_values = FALSE, mark_optimal = FALSE))
#'
#' @export
arboles_to_mermaid = function(tree, rb = NULL, title = NULL, rankdir = "LR",
                               show_values = TRUE, mark_optimal = TRUE,
                               mark_starred = FALSE) {
  stopifnot(inherits(tree, "dtree"))
  if ((show_values || mark_optimal) && is.null(rb)) rb = arboles_rollback(tree)
  nodes = tree$nodes
  edges = tree$edges
  L = sprintf("flowchart %s", if (rankdir == "TB") "TD" else "LR")
  if (!is.null(title)) L = c("---", sprintf("title: %s", title), "---", L)
  starred = if (mark_starred) arboles_starred_terminals(tree) else character()
  for (i in seq_len(nrow(nodes))) {
    id = nodes$id[i]
    tp = nodes$type[i]
    if (tp == "D") {
      L = c(L, sprintf("  %s[%s]", id, id))
    } else if (tp == "R") {
      lab = id
      if (show_values)
        lab = paste0(lab, "<br/>\u03bc = ", arboles_format_num(rb$values[[id]]))
      L = c(L, sprintf("  %s((%s))", id, lab))
    } else {
      lab = arboles_format_num(tree$payoff[[id]])
      if (id %in% starred) lab = paste0(lab, " *")
      L = c(L, sprintf("  %s[/%s/]", id, lab))
    }
  }
  idx = 0L
  styles = character()
  for (e in seq_len(nrow(edges))) {
    fr = edges$from[e]
    to = edges$to[e]
    lab = edges$label[e]
    if (!is.na(edges$prob[e])) lab = sprintf("%s, %s", lab, arboles_format_num(edges$prob[e]))
    L = c(L, sprintf("  %s -->|%s| %s", fr, lab, to))
    if (mark_optimal && fr %in% names(rb$optimal) && to %in% rb$optimal[[fr]])
      styles = c(styles, sprintf("  linkStyle %d stroke:#d00,stroke-width:3px", idx))
    idx = idx + 1L
  }
  paste(c(L, styles), collapse = "\n")
}

#' Dibuja un arbol de decision con Mermaid a PNG, SVG o PDF
#'
#' Guarda el `.mmd` junto a la imagen y lo compila con `mermaid-cli`
#' (`mmdc` si existe, si no `npx @mermaid-js/mermaid-cli`). Necesita
#' Chrome/Chromium; en contenedores usa `PUPPETEER_EXECUTABLE_PATH` mas
#' `--no-sandbox` (se configura solo).
#'
#' @param tree Objeto de clase `dtree`.
#' @param file Fichero de salida.
#' @param format `"png"`, `"svg"`, `"pdf"` o `"mmd"` (solo guarda el
#'   codigo fuente).
#' @param title Titulo opcional del grafico.
#' @param rankdir `"LR"` o `"TB"`.
#' @param mmd_file Donde guardar el `.mmd` (por defecto, junto a `file`).
#' @param background Color de fondo (`-b` de mmdc).
#' @param scale Escala para PNG (`--scale` de mmdc).
#' @param show_values Lógico: mostrar las esperanzas calculadas.
#' @param mark_optimal Lógico: marcar en rojo la decisión óptima.
#'   Con ambos en `FALSE` no hay ningún cálculo ni marca: útil como
#'   enunciado de ejercicio para el alumnado.
#' @param mark_starred Lógico: marcar con `*` el mejor hijo terminal de
#'   cada nodo de riesgo (para el REIP). Por defecto `FALSE`.
#'
#' @return `file` (o el `.mmd` si `format = "mmd"`), invisiblemente.
#'
#' @examples
#' \dontrun{
#' tr <- arboles_compile_tree(arb_D("ND1", A = arb_T(10)))
#' arboles_draw_mermaid(tr, tempfile(fileext = ".png"))
#' }
#'
#' @export
arboles_draw_mermaid = function(tree, file, format = c("png", "svg", "pdf", "mmd"),
                                title = NULL, rankdir = "LR", mmd_file = NULL,
                                background = "white", scale = 2,
                                show_values = TRUE, mark_optimal = TRUE,
                                mark_starred = FALSE) {
  format = match.arg(format)
  code = arboles_to_mermaid(tree, title = title, rankdir = rankdir,
                             show_values = show_values, mark_optimal = mark_optimal,
                             mark_starred = mark_starred)
  if (is.null(mmd_file))
    mmd_file = paste0(arboles_sin_extension(file), ".mmd")
  writeLines(code, mmd_file)
  if (format == "mmd") return(invisible(mmd_file))
  mmdc = Sys.which("mmdc")
  if (mmdc != "") {
    cmd = mmdc
    args = character()
  } else {
    cmd = "npx"
    args = c("-y", "@mermaid-js/mermaid-cli")
  }
  if (Sys.getenv("PUPPETEER_EXECUTABLE_PATH") == "" &&
      file.exists("/usr/bin/google-chrome"))
    Sys.setenv(PUPPETEER_EXECUTABLE_PATH = "/usr/bin/google-chrome")
  cfg = tempfile(fileext = ".json")
  writeLines('{"args": ["--no-sandbox", "--disable-setuid-sandbox"]}', cfg)
  args = c(args, "-i", mmd_file, "-o", file, "-b", background,
           "--puppeteerConfigFile", cfg)
  if (format == "png") args = c(args, "--scale", as.character(scale))
  st = system2(cmd, args)
  if (st != 0) stop("mermaid-cli fall\u00f3 con c\u00f3digo ", st)
  invisible(file)
}


# Tablas anidadas (2 niveles) emitidas desde R ------------

#' Celda de texto para [arboles_emit_nested_table()]
#'
#' @param text Texto de la celda.
#'
#' @return Lista que describe la celda.
#'
#' @examples
#' arboles_tcell_txt("100")
#'
#' @export
arboles_tcell_txt = function(text) list(kind = "txt", text = as.character(text))

#' Celda con mini-matriz para [arboles_emit_nested_table()]
#'
#' Mini-matriz de 1 fila (p. ej. dos escenarios condicionales) con sus
#' probabilidades; su REMA se calcula solo con
#' [arboles_decision_table()].
#'
#' @param cols Encabezados de columna de la mini-matriz.
#' @param vals Pagos de la unica fila.
#' @param probs Probabilidades condicionales (suman 1).
#' @param star Logico: marca `*` (optimo) junto al resultado.
#'
#' @return Lista que describe la celda.
#'
#' @examples
#' arboles_tcell_mat(c("AC (0.4)", "no AC (0.6)"), c(120, 150), c(0.4, 0.6),
#'                   star = TRUE)
#'
#' @export
arboles_tcell_mat = function(cols, vals, probs, star = FALSE) {
  stopifnot(length(vals) == length(cols), length(probs) == length(cols),
            abs(sum(probs) - 1) < 1e-8)
  list(kind = "mat", cols = cols, vals = vals, probs = probs, star = star)
}

#' REMA de una celda con mini-matriz
#'
#' @param cell Celda creada con [arboles_tcell_mat()].
#'
#' @return Numero con el valor esperado de la mini-matriz.
#' @noRd
arboles_cell_rema = function(cell)
  unname(arboles_decision_table(matrix(cell$vals, nrow = 1), cell$probs)$rema)

#' Escapa `*` sueltos para Typst
#'
#' En Typst, `*` abre enfasis: un asterisco de "optimo" sin cerrar rompe
#' la compilacion ("unclosed delimiter").
#'
#' @param x Vector de caracteres.
#'
#' @return Vector de caracteres con `\*` donde correspondia `*` suelto.
#' @noRd
arboles_esc_star = function(x) gsub("(?<!\\\\)\\*", "\\\\*", x, perl = TRUE)

#' Celda a codigo Typst (interno)
#'
#' @param cell Celda creada con [arboles_tcell_txt()] o
#'   [arboles_tcell_mat()].
#'
#' @return Fragmento Typst de la celda.
#' @noRd
arboles_cell_typst = function(cell) {
  if (cell$kind == "txt") return(sprintf("[%s]", arboles_esc_star(cell$text)))
  n = length(cell$cols)
  sprintf(paste0("[#table(columns: %d, align: center, stroke: 0.5pt, ",
                 "fill: rgb(\"#f1f5f9\"), %s) \\ %s%s]"),
          n,
          paste(sprintf("[%s]", c(cell$cols, arboles_format_num(cell$vals))),
                collapse = ", "),
          arboles_format_num(arboles_cell_rema(cell)),
          if (cell$star) " \\*" else "")
}

#' Celda a markdown aplanado (interno)
#'
#' @param cell Celda creada con [arboles_tcell_txt()] o
#'   [arboles_tcell_mat()].
#'
#' @return Texto de la celda para la tabla HTML de reserva.
#' @noRd
arboles_cell_md = function(cell) {
  if (cell$kind == "txt") return(cell$text)
  paste0(paste(cell$cols, collapse = " / "), ": ",
         paste(arboles_format_num(cell$vals), collapse = ", "),
         " \u2192 ", arboles_format_num(arboles_cell_rema(cell)),
         if (cell$star) "*" else "")
}

#' Emite una tabla de decision con matrices anidadas
#'
#' Construye una tabla de dos niveles (mini-matrices dentro de celdas,
#' estilo 3.7/3.8) y la emite en dos versiones: `#table` Typst real para
#' PDF (dentro de `content-visible when-format="typst"`) y su equivalente
#' markdown aplanado para HTML (`unless-format="typst"`). Pensada para
#' chunks Quarto con `results: asis`.
#'
#' @param header Vector con los encabezados de columna (puede incluir
#'   matematicas `$...$`).
#' @param rows Lista de filas; cada una es `list(first, cells, result)`
#'   donde `cells` es una lista de celdas ([arboles_tcell_txt()] o
#'   [arboles_tcell_mat()]) y `first`/`result` son textos (admiten `*`
#'   de optimo: se escapa solo).
#' @param maxrow Lista `list(first, cells, result)` con la fila de
#'   maximos (celdas ya en texto plano).
#' @param caption_typst Leyenda para la figura Typst (math nativo: los
#'   identificadores multiletra van entrecomillados, p. ej. `$"VIP"_2$`).
#' @param caption_md Leyenda para la version HTML (puede ser `""`).
#'
#' @return Nada (invisible); imprime el bloque con `cat()`.
#'
#' @examples
#' \dontrun{
#' # En un chunk Quarto con `results: asis`:
#' arboles_emit_nested_table(
#'   header = c("VE2", "AA (0.5)", "ES (0.3)"),
#'   rows = list(list(first = "A1",
#'     cells = list(arboles_tcell_txt("100"), arboles_tcell_txt("100")),
#'     result = "100")),
#'   maxrow = list(first = "max.", cells = c("100", "138"), result = "REIP"),
#'   caption_typst = "Ejemplo.", caption_md = "Ejemplo.")
#' }
#'
#' @export
arboles_emit_nested_table = function(header, rows, maxrow, caption_typst,
                                     caption_md = "") {
  ncol = length(header)
  t = c("#figure(",
        sprintf("  table(\n    columns: %d,", ncol),
        "    align: center + horizon,",
        "    stroke: 0.5pt,",
        paste0("    table.header(",
               paste(sprintf("[%s]", header), collapse = ", "), "),"))
  for (r in rows) {
    cells = vapply(r$cells, arboles_cell_typst, character(1))
    t = c(t, paste0("    [", arboles_esc_star(r$first), "], ", paste(cells, collapse = ", "),
                    ", [", arboles_esc_star(r$result), "],"))
  }
  t = c(t, paste0("    [", arboles_esc_star(maxrow$first), "], ",
                  paste(sprintf("[%s]", arboles_esc_star(maxrow$cells)), collapse = ", "),
                  ", [", arboles_esc_star(maxrow$result), "],"),
        "  ),",
        sprintf("  caption: [%s],", arboles_esc_star(caption_typst)), ")")
  md = c(paste0("| ", paste(header, collapse = " | "), " |"),
         paste0("|", paste(rep("---", ncol), collapse = "|"), "|"))
  for (r in rows) {
    cells = vapply(r$cells, arboles_cell_md, character(1))
    md = c(md, paste0("| ", paste(c(r$first, cells, r$result),
                                 collapse = " | "), " |"))
  }
  md = c(md, paste0("| ", paste(c(maxrow$first, maxrow$cells, maxrow$result),
                                collapse = " | "), " |"))
  out = c("::: {.content-visible when-format=\"typst\"}", "```{=typst}",
          paste(t, collapse = "\n"), "```", ":::",
          "", "::: {.content-visible unless-format=\"typst\"}", "", md)
  if (nzchar(caption_md)) out = c(out, "", paste0("*", caption_md, "*"))
  out = c(out, "", ":::")
  cat(paste(out, collapse = "\n"))
  invisible(NULL)
}


# Exportar fragmentos Typst a imagen ------------

#' Localiza el binario de Typst
#'
#' Busca en `TYPST_BIN`, en el empaquetado con Quarto/RStudio y en el PATH.
#'
#' @return Ruta al binario (falla si no lo encuentra).
#' @noRd
arboles_find_typst = function() {
  cand = c(Sys.getenv("TYPST_BIN"),
           "/usr/lib/rstudio-server/bin/quarto/bin/tools/x86_64/typst",
           Sys.which("typst"))
  cand = unname(cand[cand != "" & file.exists(cand)])
  if (!length(cand))
    stop("No se encontr\u00f3 `typst` (TYPST_BIN, Quarto ni el PATH)")
  cand[1]
}

#' Quita la extension de un fichero (interno)
#' @noRd
arboles_sin_extension = function(x) sub("\\.[^.]+$", "", x)

#' Compila un fragmento Typst a imagen PNG, SVG o PDF
#'
#' Envuelve el codigo (p. ej. una `#figure` con `#table`) en un documento
#' de pagina de tamano automatico -la pagina recorta justo la figura- y
#' lo convierte. Ideal para reutilizar las tablas en Word, HTML u otros
#' documentos.
#'
#' @param code Cadena con el fragmento Typst (p. ej. `#figure(...)`).
#' @param file Fichero de salida.
#' @param format `"png"`, `"svg"` o `"pdf"`.
#' @param dpi Resolucion para PNG.
#' @param fontsize Tamano de letra del documento (pt).
#' @param font Familia tipografica.
#'
#' @return `file`, invisiblemente.
#'
#' @examples
#' \dontrun{
#' arboles_typst_table_image("#figure(table(columns: 2, [a], [b], [1], [2]))",
#'                           tempfile(fileext = ".png"))
#' }
#'
#' @export
arboles_typst_table_image = function(code, file, format = c("png", "svg", "pdf"),
                                     dpi = 300, fontsize = 10,
                                     font = "Liberation Sans") {
  format = match.arg(format)
  typst = arboles_find_typst()
  if (format == "png" && Sys.which("pdftoppm") == "")
    stop("Se necesita `pdftoppm` (poppler-utils) para PNG")
  if (format == "svg" && Sys.which("pdftocairo") == "")
    stop("Se necesita `pdftocairo` (poppler-utils) para SVG")
  dir = dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  doc = sprintf(paste0("#set page(width: auto, height: auto, margin: 12pt)\n",
                       "#set text(lang: \"es\", font: \"%s\", size: %gpt)\n%s"),
                font, fontsize, code)
  base = tempfile()
  writeLines(doc, paste0(base, ".typ"))
  st = system2(typst, c("compile", paste0(base, ".typ"), paste0(base, ".pdf")))
  if (st != 0) stop("`typst compile` fall\u00f3")
  if (format == "pdf") {
    file.copy(paste0(base, ".pdf"), file, overwrite = TRUE)
  } else if (format == "png") {
    st = system2("pdftoppm", c("-png", "-r", as.character(dpi), "-singlefile",
                               paste0(base, ".pdf"), sub("\\.png$", "", file)))
    if (st != 0) stop("`pdftoppm` fall\u00f3")
  } else {
    st = system2("pdftocairo", c("-svg", paste0(base, ".pdf"), file))
    if (st != 0) stop("`pdftocairo` fall\u00f3")
  }
  invisible(file)
}


# Arboles aleatorios (generador de problemas) ------------

#' Genera un arbol de decision aleatorio reproducible
#'
#' Crea un problema completo de arbol de decision a partir de unos pocos
#' parametros: numero de etapas, numero (minimo y maximo) de alternativas
#' por nodo de decision y de resultados por nodo de riesgo, probabilidad
#' de continuar a la etapa siguiente y rango de pagos. Con la misma
#' `seed` se genera siempre el mismo arbol (ideal para enunciados con su
#' solucion); variando la semilla o los parametros se gradua la dificultad
#' de ejercicios sencillos a complejos.
#'
#' La estructura alterna decision -> riesgo -> (terminal o decision de la
#' etapa siguiente). Los pagos se interpretan como beneficios y el arbol
#' se resuelve maximizando (ver [arboles_rollback()]).
#'
#' @param seed Semilla para reproducibilidad (`NULL` = aleatorio no
#'   reproducible). No altera el estado previo de `.Random.seed`.
#' @param netapas Numero de etapas de decision (>= 1).
#' @param min_alternativas,max_alternativas Minimo y maximo de
#'   alternativas por nodo de decision (entre 2 y 26; se etiquetan
#'   `A`, `B`, ...).
#' @param min_resultados,max_resultados Minimo y maximo de resultados
#'   por nodo de riesgo (>= 2; se etiquetan `e1`, `e2`, ...).
#' @param p_continua Probabilidad (0-1) de que cada resultado de una
#'   etapa no final continue a un nodo de decision de la etapa
#'   siguiente en vez de terminar en un pago. Con `p_continua = 0` el
#'   arbol es de una sola etapa efectiva.
#' @param pago_min,pago_max Rango de los pagos terminales (enteros ->
#'   pagos enteros; si no, uniformes redondeados a `decimales`).
#' @param decimales Decimales de las probabilidades (>= 0). Siempre
#'   suman exactamente 1 en cada nodo de riesgo.
#' @param max_nodos Tope de seguridad de nodos de decision + riesgo
#'   (evita arboles gigantes por despiste en los parametros).
#' @param mark_starred Logico: resaltar en amarillo y con `*` el mejor
#'   hijo terminal de cada nodo de riesgo en `dot`/`mermaid` (para el
#'   REIP). Por defecto `FALSE`.
#'
#' @return Lista de clase `arboles_aleatorio` con `semilla`, `parametros`,
#'   `especificacion` (objeto `arb_D()` anidado), `tree` (clase `dtree`,
#'   ver [arboles_compile_tree()]), `rollback` (solucion, ver
#'   [arboles_rollback()]), `dot` (codigo Graphviz para
#'   [arboles_draw_tree()]) y `mermaid` (codigo para bloques `{mermaid}`
#'   de Quarto). Al imprimir muestra un resumen con dimensiones, REMA
#'   y alternativa optima de la raiz.
#'
#' @examples
#' # Problema sencillo de una etapa (enunciado + solucion)
#' a1 <- arboles_aleatorio(seed = 7)
#' a1                       # resumen: dimensiones, REMA y optimo
#' a1$rollback$rema
#' a1$rollback$optimal
#'
#' # Mas complejo: dos etapas y pagos hasta 200
#' a2 <- arboles_aleatorio(seed = 7, netapas = 2, max_alternativas = 3,
#'                         pago_max = 200)
#' a2
#'
#' \dontrun{
#' # Dibujo del enunciado (requiere Graphviz)
#' arboles_draw_tree(a2$tree, tempfile(fileext = ".svg"))
#' }
#'
#' @export
arboles_aleatorio = function(seed = NULL, netapas = 1L,
                             min_alternativas = 2L, max_alternativas = 3L,
                             min_resultados = 2L, max_resultados = 3L,
                             p_continua = 0.5,
                             pago_min = 0, pago_max = 100,
                             decimales = 2L, max_nodos = 1000L,
                             mark_starred = FALSE) {
  if (!is.null(seed) && (length(seed) != 1L || !is.numeric(seed)))
    stop("`seed` debe ser un n\u00famero o NULL")
  netapas = as.integer(netapas)
  min_alternativas = as.integer(min_alternativas)
  max_alternativas = as.integer(max_alternativas)
  min_resultados = as.integer(min_resultados)
  max_resultados = as.integer(max_resultados)
  decimales = as.integer(decimales)
  max_nodos = as.integer(max_nodos)
  if (netapas < 1L) stop("`netapas` debe ser >= 1")
  if (min_alternativas < 2L || max_alternativas > 26L ||
      min_alternativas > max_alternativas)
    stop("Alternativas: 2 <= min <= max <= 26")
  if (min_resultados < 2L || min_resultados > max_resultados)
    stop("Resultados: 2 <= min <= max")
  if (p_continua < 0 || p_continua > 1) stop("`p_continua` debe estar en [0, 1]")
  if (pago_min > pago_max) stop("`pago_min` debe ser <= `pago_max`")
  if (decimales < 0L) stop("`decimales` debe ser >= 0")
  if (max_nodos < 1L) stop("`max_nodos` debe ser >= 1")

  ## Semilla reproducible sin alterar el estado previo del generador
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      semilla_previa = get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", semilla_previa, envir = .GlobalEnv),
              add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed)
  }

  n_decision = 0L
  n_riesgo = 0L

  cuenta_nodo = function() {
    if (n_decision + n_riesgo >= max_nodos)
      stop("Se super\u00f3 `max_nodos` (", max_nodos, "): reduce `netapas`, ",
           "`max_alternativas`, `max_resultados` o `p_continua`")
  }

  ## Probabilidades que suman exactamente 1 (enteros de 10^decimales)
  probs_azar = function(k) {
    m = 10^decimales
    if (m < k) m = k
    w = stats::runif(k)
    (rep(1, k) + as.vector(stats::rmultinom(1, m - k, w))) / m
  }

  pago_azar = function() {
    if (pago_min == floor(pago_min) && pago_max == floor(pago_max))
      sample.int(pago_max - pago_min + 1L, 1L) + pago_min - 1L
    else round(stats::runif(1, pago_min, pago_max), decimales)
  }

  nueva_D = function(etapa) {
    cuenta_nodo()
    n_decision <<- n_decision + 1L
    id = paste0("D", n_decision)
    k = sample.int(max_alternativas - min_alternativas + 1L, 1L) +
      min_alternativas - 1L
    ramas = lapply(seq_len(k), function(i) nueva_R(etapa))
    ramas = stats::setNames(ramas, LETTERS[seq_len(k)])
    do.call(arb_D, c(list(id), ramas))
  }

  nueva_R = function(etapa) {
    cuenta_nodo()
    n_riesgo <<- n_riesgo + 1L
    id = paste0("R", n_riesgo)
    k = sample.int(max_resultados - min_resultados + 1L, 1L) +
      min_resultados - 1L
    p = probs_azar(k)
    ramas = lapply(seq_len(k), function(i) {
      hijo = if (etapa < netapas && stats::runif(1) < p_continua)
        nueva_D(etapa + 1L)
      else
        arb_T(pago_azar())
      arb_P(p[i], hijo)
    })
    ramas = stats::setNames(ramas, paste0("e", seq_len(k)))
    do.call(arb_R, c(list(id), ramas))
  }

  raiz = nueva_D(1L)
  tree = arboles_compile_tree(raiz)
  rb = arboles_rollback(tree)
  titulo = if (!is.null(seed))
    sprintf("\u00c1rbol aleatorio (semilla %s)", seed)
  else
    "\u00c1rbol aleatorio"
  out = list(semilla = seed,
             parametros = list(netapas = netapas,
                               min_alternativas = min_alternativas,
                               max_alternativas = max_alternativas,
                               min_resultados = min_resultados,
                               max_resultados = max_resultados,
                               p_continua = p_continua,
                               pago_min = pago_min, pago_max = pago_max,
                               decimales = decimales,
                               mark_starred = mark_starred),
             especificacion = raiz,
             tree = tree,
             rollback = rb,
             dot = arboles_to_dot(tree, rb, title = titulo,
                                  mark_starred = mark_starred),
             mermaid = arboles_to_mermaid(tree, rb, title = titulo,
                                          mark_starred = mark_starred))
  class(out) = "arboles_aleatorio"
  out
}

#' Imprime un arbol de decision aleatorio
#'
#' Muestra un resumen de [arboles_aleatorio()]: dimensiones del arbol,
#' REMA de la raiz y alternativa optima.
#'
#' @param x Objeto de clase `arboles_aleatorio`.
#' @param ... Ignorado.
#'
#' @return `x`, invisiblemente.
#'
#' @export
print.arboles_aleatorio = function(x, ...) {
  nodos = x$tree$nodes
  aristas = x$tree$edges
  raiz = x$rollback$root
  opt_ids = x$rollback$optimal[[raiz]]
  opt_alt = aristas$label[aristas$from == raiz & aristas$to %in% opt_ids]
  cat("Arbol de decisi\u00f3n aleatorio",
      if (!is.null(x$semilla)) sprintf(" (semilla %s)", x$semilla),
      "\n", sep = "")
  cat(sprintf("  Decisiones: %d | Riesgo: %d | Terminales: %d\n",
              sum(nodos$type == "D"), sum(nodos$type == "R"),
              sum(nodos$type == "T")))
  cat(sprintf("  REMA (%s): %s | \u00d3ptimo: %s\n",
              raiz, arboles_format_num(x$rollback$rema),
              paste(opt_alt, collapse = ", ")))
  invisible(x)
}
