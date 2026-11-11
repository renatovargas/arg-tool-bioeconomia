# Tablas en HTML y reactable
# ==========================

num <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
pct <- function(x, d = 1) ifelse(is.na(x), "–", paste0(num(100 * x, d), " %"))
pp <- function(x, d = 1) { r <- round(100 * x, d); ifelse(is.na(x), "–", paste0(ifelse(r > 0, "+", ""), num(r + 0, d), " p. p.")) }
celda <- function(x) ifelse(is.na(x) | abs(x) < 0.5, "", num(x))

pill <- function(cat) {
  k <- match(cat, niveles_bio)
  ifelse(is.na(k), "", paste0("<span class='pill p", k, "'>", cat, "</span>"))
}

pill_corta <- function(cat) {
  k <- match(cat, niveles_bio)
  ifelse(is.na(k), "", paste0("<span class='pill p", k, "' title='", cat, "'>", c("Bio", "Parcial", "Fuera")[k], "</span>"))
}

# Cuadro bioeconómico de oferta o utilización
cuadro_bio_html <- function(t, va = NULL) {
  otras <- setdiff(names(t), c("Bio", "Fila", COLS_ACT, "Total"))
  nums <- c(COLS_ACT, otras, "Total")
  etq <- str_replace(COLS_ACT, ".* \\| ", "") |> str_remove("^[A-D]-?[A-S]? ")
  cab1 <- paste0(
    "<tr><th rowspan='2' class='izq'></th>",
    "<th colspan='2' class='hc1'>", B_BIO, "</th><th colspan='2' class='hc2'>", B_PAR, "</th><th colspan='3' class='hc3'>", B_FUE, "</th>",
    paste0("<th rowspan='2'>", otras, "</th>", collapse = ""), "<th rowspan='2'>Total</th></tr>"
  )
  cab2 <- paste0("<tr>", paste0("<th class='hc", c(1, 1, 2, 2, 3, 3, 3), " sub'>", etq, "</th>", collapse = ""), "</tr>")
  fila <- function(et, v, cl) paste0("<tr class='", cl, "'><td class='izq'>", et, "</td>", paste0("<td>", celda(v), "</td>", collapse = ""), "</tr>")
  cuerpo <- character()
  for (k in seq_along(niveles_bio)) {
    g <- t[t$Bio == niveles_bio[k], ]
    cuerpo <- c(cuerpo, fila(niveles_bio[k], colSums(g[, nums]), paste0("subtotal s", k)))
    for (i in seq_len(nrow(g))) cuerpo <- c(cuerpo, fila(paste0("&ensp;", g$Fila[i]), as.numeric(g[i, nums]), ""))
  }
  r <- t[t$Bio == "No aplica", ]
  for (i in seq_len(nrow(r))) cuerpo <- c(cuerpo, fila(r$Fila[i], as.numeric(r[i, nums]), ""))
  cuerpo <- c(cuerpo, fila(if (is.null(va)) "Oferta total" else "Utilización total", colSums(t[, nums]), "total"))
  if (!is.null(va)) {
    v <- coalesce(va$v[match(COLS_ACT, va$Col)], 0)
    cuerpo <- c(cuerpo, fila("Valor agregado bruto", c(v, rep(NA, length(otras)), sum(v)), "total"))
  }
  HTML(paste0("<div class='tabla-scroll'><table class='cou bio'><thead>", cab1, cab2, "</thead><tbody>", paste(cuerpo, collapse = ""), "</tbody></table></div>"))
}

cuadro_va_ci_html <- function(t) {
  ind <- function(et, x, cl = "", f = celda, tot = sum(x)) paste0("<tr class='", cl, "'><td class='izq'>", et, "</td>", paste0("<td>", f(x), "</td>", collapse = ""), "<td>", f(tot), "</td></tr>")
  HTML(paste0(
    "<div class='tabla-scroll'><table class='cou'><thead><tr><th class='izq'></th>",
    paste0("<th>", t$Seccion, "<br>", t$Nombre, "</th>", collapse = ""), "<th>Total</th></tr></thead><tbody>",
    ind("Consumo intermedio", t$ci),
    ind(paste("de los cuales, insumos de", tolower(B_BIO)), t$ci_bio),
    ind("% bioeconómico del consumo intermedio", t$pct_bio, "subtotal s1", pct, sum(t$ci_bio) / sum(t$ci)),
    ind("Valor agregado bruto", t$va),
    ind("VA bioeconómico vía consumo intermedio", t$va_bio, "total"),
    ind("% del VA de la sección", t$va_bio / t$va, "", pct, sum(t$va_bio) / sum(t$va)),
    "</tbody></table></div>"
  ))
}

cuadro_va_act_html <- function(v) {
  fila <- function(et, x, cl = "", f = celda) paste0("<tr class='", cl, "'><td class='izq'>", et, "</td>", paste0("<td>", f(x), "</td>", collapse = ""), "<td>", f(sum(x)), "</td></tr>")
  HTML(paste0(
    "<div class='tabla-scroll'><table class='cou'><thead><tr><th class='izq'>Actividades</th>",
    paste0("<th class='hc", seq_along(v$Bio), "'>", v$Bio, "</th>", collapse = ""), "<th>Total</th></tr></thead><tbody>",
    fila("(+) Valor bruto de producción", v$Producción),
    fila("(−) Consumo intermedio", v$`Consumo intermedio`),
    fila("(=) Valor agregado bruto", v$`Valor agregado`, "total"),
    fila("Porcentaje", v$pct_va, "subtotal s1", pct),
    "</tbody></table></div>"
  ))
}

# Tema de reactable
tema_tabla <- reactableTheme(
  color = "#1F2A2E", borderColor = "#E3E8E6", stripedColor = "#F7FAF9", highlightColor = "#EEF5F2",
  headerStyle = list(background = "#0B4A3A", color = "#fff", fontWeight = 600, borderColor = "#0B4A3A"),
  searchInputStyle = list(borderRadius = "999px", width = "100%", padding = "0.35rem 0.9rem"),
  cellPadding = "0.42rem 0.6rem"
)

col_pill <- function(nombre = "Categoría", minWidth = 190) {
  colDef(name = nombre, html = TRUE, minWidth = minWidth, cell = function(v) pill(v),
         filterInput = NULL)
}

barra_peso <- function(max_s) {
  function(value, index) {
    w <- if (max_s > 0) 100 * value / max_s else 0
    htmltools::div(class = "peso-celda",
      htmltools::span(class = "mini-barra", htmltools::span(style = sprintf("width:%.1f%%", w))),
      htmltools::span(pct(value)))
  }
}
