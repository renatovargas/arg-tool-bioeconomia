# Herramienta de bioeconomía de Argentina
# =======================================
# Explora la cuenta temática de bioeconomía construida con los cuadros de oferta
# y utilización de INDEC (2018 a 2023), y permite reclasificar productos y
# actividades para ver cómo cambian los agregados.

# Los paquetes, datos y funciones se cargan desde R/ (Shiny lo hace solo, en
# orden alfabético): 00-paquetes, 01-datos, 02-calculos, 03-tablas,
# 04-graficos y 05-mosaico.

d_orig <- aplicar_clasif(scn, clasif_original)
serie_orig <- serie_resumen(d_orig)

tema <- bs_theme(
  version = 5, bg = "#FBFBF8", fg = "#1F2A2E", primary = "#0F7B5F", secondary = "#5B676B",
  base_font = font_google("Source Sans 3", local = FALSE),
  heading_font = font_google("Roboto", wght = c(500, 700), local = FALSE),
  code_font = font_google("Space Mono", local = FALSE),
  "border-radius" = "0.6rem"
)

sel_anio <- function(id) radioButtons(id, NULL, choices = ANIOS, selected = ANIO_MAX, inline = TRUE)

leyenda_cat <- tags$div(class = "leyenda", lapply(seq_along(niveles_bio), \(k) tags$span(class = paste0("pill p", k), niveles_bio[k])))

ui <- page_navbar(
  title = tags$span(class = "marca", tags$b("Bioeconomía"), " · Argentina"),
  id = "nav", theme = tema, fillable = FALSE,
  navbar_options = navbar_options(bg = "#0B4A3A", theme = "dark"),
  header = tags$head(tags$link(rel = "stylesheet", href = "estilos.css"), tags$title("Bioeconomía de Argentina")),

  # 1. Panorama ---------------------------------------------------------------
  nav_panel(
    "Panorama", icon = icon("chart-line"),
    tags$div(class = "barra-anio", tags$span("Año"), sel_anio("anio_pan"), uiOutput("aviso_pan", inline = TRUE)),
    uiOutput("cajas_pan"),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Participación de cada categoría en los agregados"), plotlyOutput("g_part", height = "380px"),
           card_footer("Productos clasificados por su categoría. Precios corrientes.")),
      card(card_header(tags$div(class = "cab-sel", "Evolución de la participación en",
                                selectInput("comp_pan", NULL, COMPONENTES, "Exportaciones", width = "260px"))),
           plotlyOutput("g_serie", height = "380px"),
           card_footer("Las participaciones a precios corrientes también reflejan precios relativos y tipo de cambio."))
    )
  ),

  # 2. Cuadros ----------------------------------------------------------------
  nav_panel(
    "Cuadros", icon = icon("table"),
    tags$div(class = "barra-anio", tags$span("Año"), sel_anio("anio_cua"),
             downloadButton("baja_cuadros", "Descargar en Excel", class = "btn-sm btn-outline-primary")),
    navset_card_underline(
      nav_panel("Oferta bioeconómica", uiOutput("t_oferta"), tags$p(class = "nota", "Miles de millones de pesos corrientes. Columnas: categoría de la actividad y sector; filas: categoría y grupo del producto.")),
      nav_panel("Utilización bioeconómica", uiOutput("t_util"), tags$p(class = "nota", "Miles de millones de pesos corrientes.")),
      nav_panel("Valor agregado por actividades", uiOutput("t_va_act"), tags$p(class = "nota", "Método tradicional: se suma el valor agregado de las actividades de cada categoría.")),
      nav_panel("Valor agregado vía consumo intermedio", uiOutput("t_va_ci"), tags$p(class = "nota", "Cada sección aporta valor agregado en proporción a sus insumos de bioeconomía (Vargas, Mondaini y Rodríguez, 2023)."))
    )
  ),

  # 3. Clasificación ------------------------------------------------------------
  nav_panel(
    "Clasificación", icon = icon("grip"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        radioButtons("cl_anio", "Año", ANIOS, ANIO_MAX, inline = TRUE),
        radioButtons("cl_tipo", "Mostrar", c("Productos", "Actividades"), inline = TRUE),
        checkboxGroupInput("cl_cats", "Categorías", niveles_bio, niveles_bio),
        textInput("cl_q", NULL, placeholder = "Buscar: carne, 1514, papel…"),
        radioButtons("cl_vista", "Vista", c("Mosaico", "Tabla"), inline = TRUE),
        tags$p(class = "nota", "Haga clic en un recuadro para cambiar su categoría. El cambio entra en su escenario.")
      ),
      uiOutput("cl_cajas"),
      uiOutput("cl_cuerpo")
    )
  ),

  # 4. Reclasificar ---------------------------------------------------------------
  nav_panel(
    "Reclasificar", icon = icon("shuffle"),
    layout_sidebar(
      sidebar = sidebar(
        width = 360, title = "Su escenario",
        radioButtons("rc_tipo", "1. ¿Qué quiere mover?", c("Productos", "Actividades"), inline = TRUE),
        selectizeInput("rc_cods", "2. Elija uno o varios códigos", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Escriba un código o un nombre")),
        radioButtons("rc_destino", "3. Muévalos a", niveles_bio, B_FUE),
        actionButton("rc_aplicar", "Aplicar al escenario", icon = icon("check"), class = "btn-primary w-100"),
        tags$hr(),
        tags$div(class = "atajos-t", "Ejemplos para empezar"),
        actionButton("pre_abonos", "Sacar abonos y plaguicidas (346)", class = "btn-sm btn-outline-secondary w-100 mb-1"),
        actionButton("pre_comida", "Servicio de comida (632) como parcial", class = "btn-sm btn-outline-secondary w-100 mb-1"),
        actionButton("pre_regla", "Regla: actividad = su producto principal", class = "btn-sm btn-outline-secondary w-100 mb-1"),
        tags$hr(),
        tags$div(class = "d-flex gap-2 mb-2",
          actionButton("rc_enlace", "Compartir enlace", icon = icon("link"), class = "btn-sm btn-outline-primary flex-fill"),
          downloadButton("rc_baja", "Guardar", class = "btn-sm btn-outline-primary flex-fill")),
        tags$div(class = "d-flex gap-2",
          actionButton("rc_reset", "Restablecer", icon = icon("rotate-left"), class = "btn-sm btn-outline-danger flex-fill")),
        fileInput("rc_sube", NULL, buttonLabel = "Cargar escenario…", placeholder = "Archivo .xlsx guardado aquí", accept = ".xlsx")
      ),
      tags$div(class = "barra-anio", tags$span("Año"), sel_anio("anio_rc")),
      uiOutput("rc_estado"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header(tags$div(class = "cab-sel", "Original y escenario para",
                                  selectInput("rc_cat", NULL, niveles_bio, B_BIO, width = "230px"))),
             plotlyOutput("rc_g", height = "430px"),
             card_footer("Círculo vacío: clasificación original. Círculo lleno: su escenario.")),
        card(card_header("Cambios del escenario"), reactableOutput("rc_cambios"))
      ),
      card(card_header("Todos los indicadores"), reactableOutput("rc_tabla")),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header(tags$div(class = "cab-sel", "En el tiempo:",
                                  selectInput("rc_ind", NULL, NULL, width = "360px"))),
             plotlyOutput("rc_serie", height = "330px")),
        card(card_header("Actividades que cambian de grupo de multiplicador"), reactableOutput("rc_mult"))
      )
    )
  ),

  # 5. Multiplicadores --------------------------------------------------------------
  nav_panel(
    "Multiplicadores", icon = icon("diagram-project"),
    tags$div(class = "barra-anio", tags$span("Año"), sel_anio("anio_mu"), uiOutput("aviso_mu", inline = TRUE)),
    uiOutput("mu_cajas"),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Las 25 actividades con mayor uso de insumos de bioeconomía"), plotlyOutput("mu_g", height = "640px"),
           card_footer("Pesos de insumos de bioeconomía y bioeconomía parcial usados, directa e indirectamente, por peso de demanda final.")),
      card(card_header("Todas las actividades"), reactableOutput("mu_t"))
    )
  ),

  # 6. Método -----------------------------------------------------------------------
  nav_panel(
    "Método", icon = icon("book-open"),
    tags$div(class = "metodo", markdown(readLines("metodo.md", encoding = "UTF-8")))
  ),

  nav_spacer(),
  nav_item(tags$div(class = "esc-nav", uiOutput("badge_esc", inline = TRUE), input_switch("usar_esc", "Mi escenario", FALSE)))
)

server <- function(input, output, session) {
  esc <- reactiveVal(clasif_original)
  d_esc <- reactive(aplicar_clasif(scn, esc()))
  serie_esc <- reactive(serie_resumen(d_esc()))
  cambios <- reactive(cambios_clasif(esc()))
  usar <- reactive(isTRUE(input$usar_esc))
  d_act <- reactive(if (usar()) d_esc() else d_orig)
  clasif_act <- reactive(if (usar()) esc() else clasif_original)

  # Enlaces: ?tab=Reclasificar&anio=2022&p=632:2,346:3&a=552:3
  # p y a son cambios de productos y actividades, código:categoría (1, 2 o 3)
  codificar <- function(cl) {
    x <- cambios_clasif(cl)
    f <- \(t) { y <- x[x$tipo == t, ]; if (!nrow(y)) return(NULL); paste0(y$cod, ":", match(y$despues, niveles_bio), collapse = ",") }
    q <- c(p = f("Producto"), a = f("Actividad"))
    paste0(names(q), "=", vapply(q, utils::URLencode, "", reserved = TRUE), collapse = "&")
  }
  decodificar <- function(q) {
    cl <- clasif_original
    for (k in c("p", "a")) {
      if (is.null(q[[k]]) || q[[k]] == "") next
      pares <- str_split_fixed(str_split(q[[k]], ",")[[1]], ":(?=[123]$)", 2)
      lista <- if (k == "p") "productos" else "actividades"
      ok <- pares[, 1] %in% names(cl[[lista]]) & pares[, 2] %in% c("1", "2", "3")
      cl[[lista]][pares[ok, 1]] <- niveles_bio[as.integer(pares[ok, 2])]
    }
    cl
  }
  observe({
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$p) || !is.null(q$a)) {
      cl <- decodificar(q)
      if (nrow(cambios_clasif(cl))) { esc(cl); update_switch("usar_esc", value = TRUE) }
    }
    if (!is.null(q$anio) && as.integer(q$anio) %in% ANIOS) anio(as.integer(q$anio))
    if (!is.null(q$tab)) nav_select("nav", q$tab)
  }) |> bindEvent(session$clientData$url_search, once = TRUE)
  observeEvent(input$rc_enlace, {
    cd <- session$clientData
    base <- paste0(cd$url_protocol, "//", cd$url_hostname, if (nzchar(cd$url_port)) paste0(":", cd$url_port), cd$url_pathname)
    url <- paste0(base, "?tab=Reclasificar&anio=", anio(), if (nrow(cambios())) paste0("&", codificar(esc())))
    showModal(modalDialog(title = "Enlace a su escenario", easyClose = TRUE,
      tags$p("Quien abra este enlace verá la herramienta con sus cambios:"),
      tags$textarea(class = "form-control enlace", rows = 3, readonly = NA, onclick = "this.select()", url),
      footer = tagList(tags$button(type = "button", class = "btn btn-primary", onclick = "navigator.clipboard.writeText(document.querySelector('.enlace').value)", icon("copy"), " Copiar"), modalButton("Cerrar"))))
  })

  # Un mismo año para todas las pantallas
  ids_anio <- c("anio_pan", "anio_cua", "cl_anio", "anio_rc", "anio_mu")
  anio <- reactiveVal(ANIO_MAX)
  observeEvent(anio(), for (o in ids_anio) if (!identical(as.integer(input[[o]]), anio())) updateRadioButtons(session, o, selected = anio()), ignoreInit = TRUE)
  lapply(ids_anio, \(id) observeEvent(input[[id]], {
    a <- as.integer(input[[id]])
    if (!identical(a, anio())) {
      anio(a)
      for (o in setdiff(ids_anio, id)) updateRadioButtons(session, o, selected = a)
    }
  }, ignoreInit = TRUE))

  modificar <- function(tipo, cods, destino, mensaje = NULL) {
    cl <- esc()
    k <- if (tipo == "Producto") "productos" else "actividades"
    cods <- intersect(cods, names(cl[[k]]))
    if (!length(cods)) return(invisible())
    cl[[k]][cods] <- destino
    esc(cl)
    update_switch("usar_esc", value = TRUE)
    showNotification(mensaje %||% sprintf("%d %s %s a %s", length(cods), tolower(tipo), if (length(cods) > 1) "s pasan" else " pasa", destino),
                     type = "message", duration = 4)
  }

  output$badge_esc <- renderUI({
    n <- nrow(cambios())
    if (!n) return(tags$span(class = "badge-esc vacio", "Escenario sin cambios"))
    actionLink("ir_rc", tags$span(class = "badge-esc", sprintf("%d %s en su escenario", n, if (n == 1) "cambio" else "cambios")))
  })
  observeEvent(input$ir_rc, nav_select("nav", "Reclasificar"))

  aviso <- reactive(if (usar() && nrow(cambios())) tags$span(class = "aviso-esc", icon("shuffle"), "Con su escenario") else NULL)
  output$aviso_pan <- renderUI(aviso())
  output$aviso_mu <- renderUI(aviso())

  # Panorama --------------------------------------------------------------------
  output$cajas_pan <- renderUI({
    a <- anio(); r <- resumen_anio(d_act(), a)
    v <- \(ind, b = B_BIO) r$valor[r$Indicador == ind & r$Bio == b]
    caja <- function(titulo, valor, sub, k = 1) value_box(title = paste(titulo, "· bioeconomía"), value = pct(valor), p(sub), class = paste0("vb c", k))
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      caja("Producción", v("Producción"), sprintf("de la producción es de bioeconomía; %s parcial", pct(v("Producción", B_PAR)))),
      caja("Exportaciones", v("Exportaciones"), sprintf("de las exportaciones; %s parcial", pct(v("Exportaciones", B_PAR)))),
      caja("Impuestos netos", v("Impuestos menos subvenciones"), sprintf("de los impuestos sobre los productos; tasa implícita de %s", pct(v("Tasa impositiva implícita")))),
      caja("Valor agregado", v("Valor agregado (método de actividades)"), sprintf("por actividades; %s vía consumo intermedio", pct(v("Valor agregado (método del consumo intermedio)"))))
    )
  })
  output$g_part <- renderPlotly(g_participacion(participaciones(d_act(), anio())))
  output$g_serie <- renderPlotly(g_serie_part(if (usar()) serie_esc() else serie_orig, input$comp_pan))

  # Cuadros ---------------------------------------------------------------------
  output$t_oferta <- renderUI(cuadro_bio_html(cuadro_bio(d_act(), anio(), "Oferta")))
  output$t_util <- renderUI(cuadro_bio_html(cuadro_bio(d_act(), anio(), "Utilización"), va = va_bio_cols(d_act(), anio())))
  output$t_va_act <- renderUI(cuadro_va_act_html(va_actividades(d_act(), anio())))
  output$t_va_ci <- renderUI(cuadro_va_ci_html(va_via_ci(d_act(), anio())))
  output$baja_cuadros <- downloadHandler(
    filename = \() sprintf("ARG_cuadros_bioeconomia_%d%s.xlsx", anio(), if (usar() && nrow(cambios())) "_escenario" else ""),
    content = function(file) {
      a <- anio(); d <- d_act()
      hojas <- list(
        "Oferta bio" = cuadro_bio(d, a, "Oferta"), "Utilización bio" = cuadro_bio(d, a, "Utilización"),
        "VA actividades" = va_actividades(d, a), "VA vía CI" = va_via_ci(d, a),
        "Multiplicadores" = multiplicadores(d, a) |> select(-mediana, -dam)
      )
      if (nrow(cambios())) hojas[["Cambios"]] <- cambios()
      writexl::write_xlsx(hojas, file)
    }
  )

  # Clasificación ---------------------------------------------------------------
  cl_lista <- reactive({
    req(input$cl_tipo)
    a <- anio(); tipo <- if (input$cl_tipo == "Productos") "Producto" else "Actividad"
    lk <- if (tipo == "Producto") lk_productos else lk_actividades
    k <- if (tipo == "Producto") "productos" else "actividades"
    w <- pesos(d_orig, a) |> filter(tipo == !!tipo)
    lk |> filter(desde <= a, is.na(hasta) | hasta >= a) |>
      mutate(cat = unname(clasif_act()[[k]][cod]), s = coalesce(w$s[match(cod, w$cod)], 0))
  })
  cl_filtrada <- reactive({
    req(input$cl_tipo)
    q <- str_to_lower(stringi::stri_trans_general(str_squish(input$cl_q %||% ""), "Latin-ASCII"))
    L <- cl_lista() |> filter(cat %in% input$cl_cats)
    if (q == "") return(L)
    L |> filter(str_detect(str_to_lower(stringi::stri_trans_general(paste(cod, completo, grupo), "Latin-ASCII")), fixed(q)))
  })
  output$cl_cajas <- renderUI({
    L <- cl_lista(); etq <- tolower(input$cl_tipo)
    layout_columns(col_widths = c(4, 4, 4), !!!lapply(seq_along(niveles_bio), function(k) {
      x <- L[L$cat == niveles_bio[k], ]
      tags$div(class = paste0("tarjeta c", k),
        tags$div(class = "t-nombre", tags$i(), niveles_bio[k]),
        tags$div(class = "t-fila", tags$span(class = "t-n", nrow(x)), tags$span(class = "t-l", etq),
                 tags$span(class = "t-p", sprintf("%s de la producción de %d", pct(sum(x$s)), anio()))))
    }))
  })
  output$cl_cuerpo <- renderUI({
    k <- if (input$cl_tipo == "Productos") "productos" else "actividades"
    if (input$cl_vista == "Mosaico") return(mosaico_html(cl_filtrada(), clasif_original[[k]], tolower(input$cl_tipo)))
    reactableOutput("cl_tabla")
  })
  output$cl_tabla <- renderReactable({
    L <- cl_filtrada() |> arrange(desc(s)) |> select(cod, completo, grupo, cat, s, desde)
    reactable(L, theme = tema_tabla, defaultPageSize = 25, compact = TRUE, highlight = TRUE,
      columns = list(cod = colDef("Código", minWidth = 90, class = "cod"), completo = colDef("Nombre de INDEC", minWidth = 340),
                     grupo = colDef("Grupo", minWidth = 140), cat = col_pill(), s = colDef("Peso en la producción", minWidth = 170, cell = barra_peso(max(cl_lista()$s))),
                     desde = colDef("Desde", minWidth = 70)),
      onClick = JS("function(row) { Shiny.setInputValue('tile', {tipo: 'auto', cod: row.original.cod, n: Math.random()}) }"))
  })

  # Clic en un recuadro: diálogo para moverlo
  pendiente <- reactiveVal(NULL)
  observeEvent(input$tile, {
    t <- input$tile
    tipo <- if (identical(t$tipo, "auto")) (if (input$cl_tipo == "Productos") "Producto" else "Actividad") else t$tipo
    lk <- if (tipo == "Producto") lk_productos else lk_actividades
    k <- if (tipo == "Producto") "productos" else "actividades"
    x <- lk[lk$cod == t$cod, ]
    actual <- esc()[[k]][[t$cod]]
    pendiente(list(tipo = tipo, cod = t$cod))
    showModal(modalDialog(
      title = tags$span(tags$code(t$cod), x$nombre), easyClose = TRUE, size = "m",
      tags$p(class = "nota", x$completo),
      tags$p("Categoría en su escenario: ", HTML(pill(actual)),
             if (actual != clasif_original[[k]][[t$cod]]) tags$span(class = "nota", " (original: ", clasif_original[[k]][[t$cod]], ")")),
      tags$div(class = "mover", lapply(seq_along(niveles_bio), \(i)
        actionButton(paste0("mover_", i), niveles_bio[i], class = paste0("btn-cat c", i), disabled = niveles_bio[i] == actual))),
      footer = modalButton("Cancelar")
    ))
  })
  lapply(seq_along(niveles_bio), \(i) observeEvent(input[[paste0("mover_", i)]], {
    p <- pendiente(); req(p)
    modificar(p$tipo, p$cod, niveles_bio[i])
    removeModal()
  }))

  # Reclasificar -------------------------------------------------------------------
  observe({
    req(input$rc_tipo)
    lk <- if (input$rc_tipo == "Productos") lk_productos else lk_actividades
    k <- if (input$rc_tipo == "Productos") "productos" else "actividades"
    cat_ <- unname(esc()[[k]][lk$cod])
    updateSelectizeInput(session, "rc_cods", choices = setNames(lk$cod, sprintf("%s · %s (%s)", lk$cod, lk$nombre, cat_)), server = TRUE)
  })
  observeEvent(input$rc_aplicar, {
    req(length(input$rc_cods))
    modificar(if (input$rc_tipo == "Productos") "Producto" else "Actividad", input$rc_cods, input$rc_destino)
    updateSelectizeInput(session, "rc_cods", selected = character())
  })
  observeEvent(input$pre_abonos, modificar("Producto", "346", B_FUE))
  observeEvent(input$pre_comida, modificar("Producto", "632", B_PAR))
  observeEvent(input$pre_regla, {
    pp_ <- d_orig |> filter(Año == ANIO_MAX, Componente == "Producción", `Tipo fila` == "Producto") |>
      group_by(cod = `Código columna`) |> slice_max(Valor, n = 1, with_ties = FALSE) |> ungroup()
    nuevo <- setNames(unname(esc()$productos[pp_$`Código fila`]), pp_$cod)
    dif <- names(nuevo)[nuevo != esc()$actividades[names(nuevo)]]
    if (!length(dif)) return(showNotification("Todas las actividades ya tienen la categoría de su producto principal."))
    cl <- esc(); cl$actividades[dif] <- nuevo[dif]; esc(cl)
    update_switch("usar_esc", value = TRUE)
    showNotification(sprintf("%d actividades toman la categoría de su producto principal en %d", length(dif), ANIO_MAX), type = "message")
  })
  observeEvent(input$rc_reset, {
    esc(clasif_original)
    showNotification("Escenario restablecido a la clasificación original", type = "warning")
  })
  output$rc_baja <- downloadHandler(
    filename = \() sprintf("escenario_bioeconomia_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      cl <- esc()
      writexl::write_xlsx(list(
        cambios = cambios(),
        productos = lk_productos |> transmute(cod, nombre, original = cat, escenario = unname(cl$productos[cod])),
        actividades = lk_actividades |> transmute(cod, nombre, original = cat, escenario = unname(cl$actividades[cod]))
      ), file)
    }
  )
  observeEvent(input$rc_sube, {
    r <- try({
      cl <- clasif_original
      for (k in c("productos", "actividades")) {
        x <- readxl::read_excel(input$rc_sube$datapath, sheet = k, col_types = "text")
        x <- x[x$cod %in% names(cl[[k]]) & x$escenario %in% niveles_bio, ]
        cl[[k]][x$cod] <- x$escenario
      }
      cl
    }, silent = TRUE)
    if (inherits(r, "try-error")) return(showNotification("No se pudo leer el archivo. Use un escenario guardado con esta herramienta.", type = "error"))
    esc(r); update_switch("usar_esc", value = TRUE)
    showNotification(sprintf("Escenario cargado: %d cambios", nrow(cambios_clasif(r))), type = "message")
  })

  comparacion <- reactive({
    a <- anio()
    inner_join(
      resumen_anio(d_orig, a) |> rename(Original = valor),
      resumen_anio(d_esc(), a) |> rename(Escenario = valor),
      by = c("Indicador", "Grupo", "Bio", "Año")
    ) |> mutate(Cambio = Escenario - Original)
  })
  output$rc_estado <- renderUI({
    n <- nrow(cambios())
    if (!n) return(tags$div(class = "estado vacio", icon("circle-info"),
      "Su escenario es igual a la clasificación original. Mueva códigos con el panel de la izquierda, con los ejemplos, o haciendo clic en el mosaico de la pantalla Clasificación."))
    c0 <- comparacion() |> filter(Grupo != "Impuestos") |> slice_max(abs(Cambio), n = 1, with_ties = FALSE)
    tags$div(class = "estado", icon("shuffle"),
      sprintf("%d %s. En %d, el mayor efecto es en %s de %s: %s → %s (%s).", n, if (n == 1) "cambio" else "cambios", anio(),
              tolower(c0$Indicador), tolower(c0$Bio), pct(c0$Original), pct(c0$Escenario), pp(c0$Cambio)))
  })
  # La categoría del gráfico sigue al mayor cambio del escenario
  observeEvent(comparacion(), {
    x <- comparacion() |> filter(Grupo %in% c("Participación en", "Valor agregado"))
    if (max(abs(x$Cambio), na.rm = TRUE) > 1e-6) updateSelectInput(session, "rc_cat", selected = x$Bio[which.max(abs(x$Cambio))])
  })
  output$rc_g <- renderPlotly(g_comparacion(comparacion() |> filter(Grupo %in% c("Participación en", "Valor agregado")) |>
    mutate(Indicador = recode(Indicador, "Valor agregado (método de actividades)" = "VA, método de actividades",
                              "Valor agregado (método del consumo intermedio)" = "VA, método del consumo intermedio",
                              "Impuestos menos subvenciones" = "Impuestos netos", "Formación bruta de capital" = "Formación de capital")), input$rc_cat))
  output$rc_cambios <- renderReactable({
    x <- cambios()
    if (!nrow(x)) x <- tibble(tipo = character(), cod = character(), nombre = character(), antes = character(), despues = character())
    x <- x |> transmute(cod, nombre = paste0("<span class='tipo-mini'>", tipo, "</span>", nombre),
                        cambio = paste0(pill(antes), "<span class='flecha'>→</span>", pill(despues)))
    reactable(x, theme = tema_tabla, compact = TRUE, pagination = nrow(x) > 10, defaultPageSize = 10,
      language = reactableLang(noData = "Sin cambios todavía"),
      columns = list(cod = colDef("Código", minWidth = 85, class = "cod"),
                     nombre = colDef("Nombre", minWidth = 150, html = TRUE),
                     cambio = colDef("Antes → ahora", minWidth = 190, html = TRUE, class = "cambio-celda")))
  })
  output$rc_tabla <- renderReactable({
    x <- comparacion() |> mutate(Indicador = if_else(Grupo == "Participación en", paste("Participación en:", Indicador), Indicador)) |>
      select(Indicador, Bio, Original, Escenario, Cambio)
    mx <- max(abs(x$Cambio), 1e-9)
    reactable(x, theme = tema_tabla, compact = TRUE, groupBy = "Indicador", defaultExpanded = TRUE, pagination = FALSE,
      rowStyle = function(i) if (!is.na(x$Cambio[i]) && abs(x$Cambio[i]) > 5e-4) list(fontWeight = 600),
      columns = list(
        Indicador = colDef("Indicador", minWidth = 280),
        Bio = col_pill("Categoría", 200),
        Original = colDef(cell = \(v) pct(v), align = "right", aggregate = NULL),
        Escenario = colDef(cell = \(v) pct(v), align = "right"),
        Cambio = colDef("Cambio", minWidth = 200, align = "left", cell = function(v) {
          if (is.na(v)) return("–")
          w <- 50 * abs(v) / mx
          htmltools::div(class = "dif",
            htmltools::span(class = "dif-eje", htmltools::span(class = if (v >= 0) "pos" else "neg", style = sprintf("width:%.1f%%;%s", w, if (v >= 0) "left:50%" else sprintf("left:%.1f%%", 50 - w)))),
            htmltools::span(class = "dif-n", pp(v)))
        })
      ))
  })
  observe({
    ind <- unique(serie_orig$Indicador)
    updateSelectInput(session, "rc_ind", choices = ind, selected = isolate(input$rc_ind) %||% "Exportaciones")
  })
  output$rc_serie <- renderPlotly({
    req(input$rc_ind)
    cat_ <- if (input$rc_ind == "Valor agregado (método del consumo intermedio)") B_BIO else input$rc_cat
    g_serie_cmp(serie_orig, serie_esc(), input$rc_ind, cat_)
  })
  output$rc_mult <- renderReactable({
    a <- anio()
    m0 <- multiplicadores(d_orig, a); m1 <- multiplicadores(d_esc(), a)
    x <- inner_join(m0 |> select(Código, Actividad, g0 = Grupo, t0 = m_total), m1 |> select(Código, g1 = Grupo, t1 = m_total), by = "Código") |>
      filter(g0 != g1) |> arrange(g1)
    reactable(x |> transmute(Código, Actividad, Antes = g0, Ahora = g1, `Mult.` = paste(num(t0, 2), "→", num(t1, 2))),
      theme = tema_tabla, compact = TRUE, language = reactableLang(noData = "Ninguna actividad cambia de grupo"),
      columns = list(Código = colDef(minWidth = 70, class = "cod"), Actividad = colDef(minWidth = 170)))
  })

  # Multiplicadores ------------------------------------------------------------------
  mult <- reactive(multiplicadores(d_act(), anio()))
  output$mu_cajas <- renderUI({
    m <- mult()
    layout_columns(col_widths = c(4, 4, 4),
      value_box("Mediana", num(m$mediana[1], 2), p("pesos de insumos por peso de demanda final"), class = "vb neutra"),
      value_box("Desviación absoluta mediana", num(m$dam[1], 2), p("umbral de los grupos: mediana + 1, 2 y 3 DAM"), class = "vb neutra"),
      value_box("Dependencia muy alta", sum(m$Grupo == "1. Muy alta"), p(sprintf("actividades, con %s del valor agregado", pct(sum(m$va[m$Grupo == "1. Muy alta"]) / sum(m$va)))), class = "vb neutra"))
  })
  output$mu_g <- renderPlotly(g_multiplicadores(mult()))
  output$mu_t <- renderReactable({
    m <- mult() |> arrange(desc(m_total)) |> select(Código, Actividad, Bio, m_bio, m_par, m_total, Grupo)
    f2 <- \(v) num(v, 2)
    reactable(m, theme = tema_tabla, compact = TRUE, searchable = TRUE, defaultPageSize = 20,
      language = reactableLang(searchPlaceholder = "Buscar actividad"),
      columns = list(Código = colDef(minWidth = 70, class = "cod"), Actividad = colDef(minWidth = 170),
                     Bio = col_pill("Categoría", 165), m_bio = colDef("Bio.", cell = f2, minWidth = 50, align = "right"), m_par = colDef("Parcial", cell = f2, minWidth = 58, align = "right"),
                     m_total = colDef("Total", cell = f2, minWidth = 52, align = "right", style = list(fontWeight = 700)), Grupo = colDef(minWidth = 120, cell = \(v) str_remove(v, "^\\d\\. "))))
  })
}

shinyApp(ui, server)
