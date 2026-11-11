# Indicadores de la cuenta de bioeconomía
# =======================================
# Mismas fórmulas que scripts/02-cuadros-y-resultados.R del sitio del taller.
# Todas reciben la base `d` ya clasificada (aplicar_clasif) y un año.

ESCALA <- 1e3 # de millones de pesos a miles de millones de pesos

COMPONENTES <- c(
  "Producción", "Consumo intermedio", "Importaciones", "Exportaciones",
  "Impuestos menos subvenciones", "Consumo final", "Formación bruta de capital"
)
SECTORES <- c("A-B Sector primario", "C Manufacturas", "D-S Servicios")
NIVELES_CPC <- c(
  "Productos agrícolas", "Productos mineros", "Manufacturas",
  "Servicios básicos", "Construcción", "Comercio", "Otros servicios"
)
SECCIONES <- c("A", "B", "C", "D - E", "F", "G", "H - S")
COLS_ACT <- c(
  paste(B_BIO, SECTORES[1:2], sep = " | "),
  paste(B_PAR, SECTORES[2:3], sep = " | "),
  paste(B_FUE, SECTORES, sep = " | ")
)

del_anio <- function(d, a) d[d$Año == a, ]

# Participación de cada categoría de producto en cada agregado
participaciones <- function(d, a) {
  del_anio(d, a) |>
    filter(`Bioeconomía Productos` %in% niveles_bio, Componente %in% COMPONENTES) |>
    group_by(Componente, Bio = `Bioeconomía Productos`) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    complete(Componente, Bio = niveles_bio, fill = list(v = 0)) |>
    group_by(Componente) |>
    mutate(pct = v / sum(v), Año = a) |>
    ungroup()
}

tasa_implicita <- function(d, a) {
  del_anio(d, a) |>
    filter(`Bioeconomía Productos` %in% niveles_bio, Componente %in% c("Producción", "Impuestos menos subvenciones")) |>
    group_by(Bio = `Bioeconomía Productos`) |>
    summarise(
      imp = sum(Valor[Componente == "Impuestos menos subvenciones"]),
      vbp = sum(Valor[Componente == "Producción"]),
      .groups = "drop"
    ) |>
    complete(Bio = niveles_bio, fill = list(imp = 0, vbp = 0)) |>
    mutate(tasa = ifelse(vbp > 0, imp / vbp, NA_real_), Año = a)
}

va_actividades <- function(d, a) {
  del_anio(d, a) |>
    filter(Componente %in% c("Producción", "Consumo intermedio", "Valor agregado")) |>
    group_by(Bio = `Bioeconomía Actividades`, Componente) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Componente, values_from = v, values_fill = 0) |>
    right_join(tibble(Bio = niveles_bio), by = "Bio") |>
    mutate(across(where(is.numeric), \(x) coalesce(x, 0)), pct_va = `Valor agregado` / sum(`Valor agregado`)) |>
    arrange(match(Bio, niveles_bio))
}

# Valor agregado vía consumo intermedio, sección a sección (Vargas y otros, 2023)
va_via_ci <- function(d, a) {
  x <- del_anio(d, a)
  ci <- x |>
    filter(Componente == "Consumo intermedio") |>
    group_by(Seccion = `CIIU Compacto`, Nombre = `CIIU Compacto Descripción`) |>
    summarise(
      ci = sum(Valor) / ESCALA,
      ci_bio = sum(Valor[`Bioeconomía Productos` == B_BIO]) / ESCALA,
      .groups = "drop"
    )
  va <- x |> filter(Componente == "Valor agregado") |> group_by(Seccion = `CIIU Compacto`) |> summarise(va = sum(Valor) / ESCALA, .groups = "drop")
  ci |> left_join(va, by = "Seccion") |>
    mutate(pct_bio = ci_bio / ci, va_bio = pct_bio * va) |>
    arrange(match(Seccion, SECCIONES))
}

comercio <- function(d, a) {
  x <- del_anio(d, a)
  pib <- sum(x$Valor[x$Componente %in% c("Valor agregado", "Impuestos menos subvenciones")])
  x |>
    filter(`Bioeconomía Productos` %in% niveles_bio, Componente %in% c("Exportaciones", "Importaciones")) |>
    group_by(Bio = `Bioeconomía Productos`) |>
    summarise(X = sum(Valor[Componente == "Exportaciones"]) / ESCALA, M = sum(Valor[Componente == "Importaciones"]) / ESCALA, .groups = "drop") |>
    complete(Bio = niveles_bio, fill = list(X = 0, M = 0)) |>
    mutate(Saldo = X - M, saldo_pib = Saldo / (pib / ESCALA), Año = a)
}

# Indicadores de un año en una sola tabla larga: lo que compara el laboratorio
resumen_anio <- function(d, a) {
  p <- participaciones(d, a) |> transmute(Indicador = Componente, Grupo = "Participación en", Bio, valor = pct)
  t <- tasa_implicita(d, a) |> transmute(Indicador = "Tasa impositiva implícita", Grupo = "Impuestos", Bio, valor = tasa)
  v <- va_actividades(d, a) |> transmute(Indicador = "Valor agregado (método de actividades)", Grupo = "Valor agregado", Bio, valor = pct_va)
  ci <- va_via_ci(d, a)
  vc <- tibble(Indicador = "Valor agregado (método del consumo intermedio)", Grupo = "Valor agregado", Bio = B_BIO, valor = sum(ci$va_bio) / sum(ci$va))
  s <- comercio(d, a) |> transmute(Indicador = "Saldo comercial (% del PIB)", Grupo = "Comercio exterior", Bio, valor = saldo_pib)
  bind_rows(p, t, v, vc, s) |> mutate(Año = a)
}

serie_resumen <- function(d) bind_rows(lapply(ANIOS, \(a) resumen_anio(d, a)))

# Multiplicadores de uso de insumos bioeconómicos (tecnología de la industria)
#   B = U inv(diag(x)), D = V inv(diag(q)), L = inv(I - D B); multiplicador = b' L
multiplicadores <- function(d, a) {
  x <- del_anio(d, a)
  orden <- lk_actividades |> arrange(orden)
  act <- x |>
    filter(Componente == "Valor agregado") |>
    transmute(Código = `Código Clasificación Actividades`, va = Valor)
  act <- orden |> transmute(Código = cod, Actividad = nombre, Seccion = grupo) |>
    left_join(act, by = "Código") |> mutate(va = coalesce(va, 0))
  act$Bio <- unname(x$`Bioeconomía Actividades`[match(act$Código, x$`Código columna`)])
  dd <- x |> filter(`Tipo fila` == "Producto", `Tipo columna` == "Actividad", Cuadro != "Valor Agregado")
  prod <- sort(unique(dd$`Código fila`))
  n_a <- nrow(act); n_p <- length(prod)
  V <- matrix(0, n_a, n_p); U <- matrix(0, n_p, n_a)
  pr <- dd |> filter(Componente == "Producción")
  V[cbind(match(pr$`Código columna`, act$Código), match(pr$`Código fila`, prod))] <- pr$Valor
  ci <- dd |> filter(Componente == "Consumo intermedio")
  U[cbind(match(ci$`Código fila`, prod), match(ci$`Código columna`, act$Código))] <- ci$Valor
  xx <- rowSums(V); q <- colSums(V)
  inv <- function(v) ifelse(v > 0, 1 / v, 0)
  B <- sweep(U, 2, inv(xx), "*")
  L <- solve(diag(n_a) - sweep(V, 2, inv(q), "*") %*% B)
  clase <- dd$`Bioeconomía Productos`[match(prod, dd$`Código fila`)]
  m_bio <- as.vector(colSums(B[clase == B_BIO, , drop = FALSE]) %*% L)
  m_par <- as.vector(colSums(B[clase == B_PAR, , drop = FALSE]) %*% L)
  res <- act |> mutate(m_bio = m_bio, m_par = m_par, m_total = m_bio + m_par, vbp = xx / ESCALA, va = va / ESCALA)
  med <- median(res$m_total); dam <- median(abs(res$m_total - med))
  res |> mutate(
    Grupo = case_when(
      m_total > med + 3 * dam ~ "1. Muy alta",
      m_total > med + 2 * dam ~ "2. Alta",
      m_total > med + 1 * dam ~ "3. Sobre la mediana",
      TRUE ~ "4. Mediana o abajo"
    ),
    mediana = med, dam = dam
  )
}

# Cuadros bioeconómicos (filas: grupo de producto; columnas: categoría | sector)
cuadro_bio <- function(d, a, cuadro) {
  comp_act <- if (cuadro == "Oferta") "Producción" else "Consumo intermedio"
  finales <- if (cuadro == "Oferta") c("Importaciones", "Ajuste CIF/FOB", "Impuestos menos subvenciones", "Márgenes de distribución") else c("Exportaciones", "Consumo final", "Formación bruta de capital")
  t <- del_anio(d, a) |>
    filter(Cuadro == cuadro) |>
    mutate(
      Bio = `Bioeconomía Productos`, Fila = `Grupo fila`,
      Col = if_else(Componente == comp_act, paste(`Bioeconomía Actividades`, `Sector agregado`, sep = " | "), Componente)
    ) |>
    group_by(Bio, Fila, Col) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Col, values_from = v, values_fill = 0)
  cols <- c(COLS_ACT, finales)
  for (cc in setdiff(cols, names(t))) t[[cc]] <- 0
  t |>
    select(Bio, Fila, all_of(cols)) |>
    mutate(Total = rowSums(across(all_of(cols)))) |>
    arrange(match(Bio, c(niveles_bio, "No aplica")), match(Fila, c(NIVELES_CPC, "Ajuste CIF/FOB", "Compras directas")))
}

va_bio_cols <- function(d, a) {
  del_anio(d, a) |>
    filter(Cuadro == "Valor Agregado") |>
    mutate(Col = paste(`Bioeconomía Actividades`, `Sector agregado`, sep = " | ")) |>
    group_by(Col) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop")
}

# Peso de cada código en la producción del año
pesos <- function(d, a) {
  x <- del_anio(d, a) |> filter(Componente == "Producción")
  p <- x |> filter(`Tipo fila` == "Producto") |> group_by(cod = `Código fila`) |> summarise(v = sum(Valor), .groups = "drop") |> mutate(s = v / sum(v), tipo = "Producto")
  ac <- x |> group_by(cod = `Código columna`) |> summarise(v = sum(Valor), .groups = "drop") |> mutate(s = v / sum(v), tipo = "Actividad")
  bind_rows(p, ac)
}
