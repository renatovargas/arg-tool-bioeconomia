# Mosaico de productos y actividades
# ==================================
# Un recuadro por código, agrupado por grupo CPC o sección CIIU, con el color de
# su categoría y una barra con su peso en la producción. Al hacer clic se envía
# input$tile = list(tipo, cod) para reclasificarlo.

orden_grupos <- c(
  "Productos agrícolas", "Agricultura", "Productos mineros", "Minería", "Manufacturas",
  "Servicios básicos", "Servicios básicos y agua", "Construcción", "Comercio", "Otros servicios"
)

mosaico_html <- function(L, clasif_orig, etiqueta) {
  if (!nrow(L)) return(tags$p(class = "vacio", "Ningún código coincide con la búsqueda y las categorías elegidas."))
  max_s <- max(L$s, 1e-9)
  L <- L |> arrange(match(grupo, orden_grupos), orden)
  grupos <- split(L, factor(L$grupo, unique(L$grupo)))
  tags$div(class = "mosaico", lapply(names(grupos), function(g) {
    xs <- grupos[[g]]
    n_cat <- table(factor(xs$cat, niveles_bio))
    tags$section(class = "grupo",
      tags$header(
        tags$h4(g), tags$span(class = "g-n", sprintf("%d %s", nrow(xs), etiqueta)),
        tags$span(class = "g-mini", lapply(seq_along(niveles_bio), \(k) tags$i(class = paste0("c", k), style = sprintf("flex-grow:%d", n_cat[[k]]))))
      ),
      tags$div(class = "tiles", lapply(seq_len(nrow(xs)), function(i) {
        x <- xs[i, ]
        k <- match(x$cat, niveles_bio)
        cambiado <- x$cat != clasif_orig[[x$cod]]
        tags$button(
          type = "button", class = paste0("tile c", k, if (cambiado) " cambiado"),
          onclick = sprintf("Shiny.setInputValue('tile', {tipo: '%s', cod: '%s', n: Math.random()})", x$tipo, x$cod),
          `aria-label` = paste(x$cod, x$nombre, x$cat),
          tags$span(class = "t-cod", x$cod, if (cambiado) tags$b(class = "nuevo", "cambiado")),
          tags$span(class = "t-nom", x$nombre),
          tags$span(class = "t-peso", style = sprintf("width:%.1f%%", max(2, 100 * x$s / max_s))),
          tags$span(class = "tip",
            tags$b(x$cod), " · ", x$cat, if (cambiado) paste0(" (antes: ", clasif_orig[[x$cod]], ")"), tags$br(),
            x$completo, tags$br(),
            tags$em(sprintf("%s de la producción · clic para reclasificar", pct(x$s))))
        )
      }))
    )
  }))
}
