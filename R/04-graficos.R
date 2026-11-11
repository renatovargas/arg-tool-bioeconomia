# Gráficos interactivos (plotly)
# ==============================
# Colores por categoría fijos (validados para daltonismo), líneas de 2 px,
# rejilla tenue, leyenda siempre visible y tooltip al pasar el cursor.

fuente <- list(family = "Source Sans 3, Segoe UI, sans-serif", color = "#1F2A2E", size = 13)

estilo_plotly <- function(p, y_pct = TRUE, leyenda = TRUE, ytitulo = NULL) {
  p |>
    layout(
      font = fuente,
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 10, t = 10, b = 40),
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, tickfont = list(color = "#5B676B"), fixedrange = TRUE),
      yaxis = list(title = ytitulo %||% "", gridcolor = "#E3E8E6", zeroline = FALSE, tickformat = if (y_pct) ".0%" else ",",
                   tickfont = list(color = "#5B676B"), fixedrange = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.12, yanchor = "top", traceorder = "normal", font = list(size = 13), entrywidth = 185, entrywidthmode = "pixels"),
      showlegend = leyenda,
      hoverlabel = list(bgcolor = "#1F2A2E", bordercolor = "#1F2A2E", font = list(color = "#fff", family = fuente$family)),
      separators = ",."
    ) |>
    config(displayModeBar = FALSE, locale = "es")
}

# Participación de cada categoría en un agregado, año a año
g_serie_part <- function(s, componente) {
  d <- s |> filter(Indicador == componente, Grupo == "Participación en") |> mutate(Bio = factor(Bio, niveles_bio))
  plot_ly(d, x = ~Año, y = ~valor, color = ~Bio, colors = COL_BIO, type = "scatter", mode = "lines+markers",
          line = list(width = 2.5), marker = list(size = 9, line = list(color = "#fff", width = 2)),
          text = ~paste0("<b>", Bio, "</b><br>", Año, ": ", pct(valor)), hoverinfo = "text") |>
    estilo_plotly() |> layout(xaxis = list(dtick = 1))
}

# Barras apiladas al 100 %: participación de cada categoría en cada agregado
g_participacion <- function(p) {
  d <- p |> mutate(Componente = factor(Componente, rev(COMPONENTES)), Bio = factor(Bio, niveles_bio))
  plot_ly(d, y = ~Componente, x = ~pct, color = ~Bio, colors = COL_BIO, type = "bar", orientation = "h",
          marker = list(line = list(color = "#fff", width = 2)),
          text = ~ifelse(pct > 0.05, pct(pct, 0), ""), textposition = "inside", insidetextanchor = "middle",
          textfont = list(color = "#fff", size = 12),
          hovertext = ~paste0("<b>", Bio, "</b><br>", Componente, ": ", pct(pct)), hoverinfo = "text") |>
    estilo_plotly() |>
    layout(barmode = "stack", bargap = 0.32, xaxis = list(tickformat = ".0%", range = c(0, 1)), yaxis = list(tickformat = NULL, gridcolor = "rgba(0,0,0,0)"))
}

# Original frente a escenario, por indicador (una categoría)
g_comparacion <- function(cmp, cat) {
  d <- cmp |> filter(Bio == cat) |> mutate(Indicador = factor(Indicador, rev(unique(Indicador))))
  col <- COL_BIO[[cat]]
  plot_ly(d, y = ~Indicador) |>
    add_segments(x = ~Original, xend = ~Escenario, yend = ~Indicador, line = list(color = "#B9C3BF", width = 3), showlegend = FALSE, hoverinfo = "none") |>
    add_markers(x = ~Original, name = "Original", marker = list(size = 11, color = "#fff", line = list(color = col, width = 2.5)),
                text = ~paste0("<b>", Indicador, "</b><br>Original: ", pct(Original)), hoverinfo = "text") |>
    add_markers(x = ~Escenario, name = "Escenario", marker = list(size = 11, color = col, line = list(color = "#fff", width = 1.5)),
                text = ~paste0("<b>", Indicador, "</b><br>Escenario: ", pct(Escenario), "<br>Cambio: ", pp(Escenario - Original)), hoverinfo = "text") |>
    estilo_plotly() |> layout(xaxis = list(tickformat = ".0%", zeroline = TRUE, zerolinecolor = "#C9D3CF"), yaxis = list(tickformat = NULL))
}

# Serie de un indicador, original y escenario
g_serie_cmp <- function(s0, s1, indicador, cat) {
  f <- function(s, nombre) s |> filter(Indicador == indicador, Bio == cat) |> mutate(Clasificación = nombre)
  d <- bind_rows(f(s0, "Original"), f(s1, "Escenario"))
  col <- COL_BIO[[cat]]
  plot_ly(d, x = ~Año, y = ~valor, linetype = ~Clasificación, linetypes = c(Original = "dot", Escenario = "solid"),
          type = "scatter", mode = "lines+markers", line = list(color = col, width = 2.5),
          marker = list(color = col, size = 8, line = list(color = "#fff", width = 1.5)),
          text = ~paste0("<b>", Clasificación, "</b><br>", Año, ": ", pct(valor)), hoverinfo = "text") |>
    estilo_plotly() |> layout(xaxis = list(dtick = 1))
}

# Ranking de multiplicadores
g_multiplicadores <- function(m, n = 25) {
  d <- m |> arrange(desc(m_total)) |> slice_head(n = n) |>
    mutate(Etiqueta = str_trunc(paste(Código, Actividad, sep = " · "), 48), Etiqueta = factor(Etiqueta, rev(Etiqueta)), Bio = factor(Bio, niveles_bio))
  plot_ly(d, y = ~Etiqueta, x = ~m_total, color = ~Bio, colors = COL_BIO, type = "bar", orientation = "h",
          text = ~num(m_total, 2), textposition = "outside", textfont = list(size = 11, color = "#1F2A2E"),
          hovertext = ~paste0("<b>", Actividad, "</b><br>", Grupo, "<br>Bioeconomía: ", num(m_bio, 2), " · Parcial: ", num(m_par, 2), "<br>Total: ", num(m_total, 2)),
          hoverinfo = "text") |>
    estilo_plotly(y_pct = FALSE) |>
    layout(bargap = 0.25, xaxis = list(tickformat = ".1f", range = c(0, max(d$m_total) * 1.14)), yaxis = list(tickformat = NULL, gridcolor = "rgba(0,0,0,0)", tickfont = list(size = 11)))
}
