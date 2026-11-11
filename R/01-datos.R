# Datos y clasificación
# =====================
# La base plana (data/arg_scn.rds) y los lookups (data/arg_lookups.xlsx) vienen
# del repositorio taller-bioeconomia; scripts/actualizar-datos.R los copia.

ruta_lookups <- "data/arg_lookups.xlsx"

categorias <- readxl::read_excel(ruta_lookups, sheet = "categorias", col_types = "text")
niveles_bio <- categorias$Categoría
B_BIO <- niveles_bio[1]
B_PAR <- niveles_bio[2]
B_FUE <- niveles_bio[3]
COL_BIO <- setNames(categorias$Color, niveles_bio)
TINTE_BIO <- setNames(c("#DCEFE8", "#FBEFD9", "#E3EDF8"), niveles_bio)

sin_codigo <- function(x) str_squish(str_remove(x, "^[^ ]+( [A-Z]+)? - "))
corto <- function(x, n = 60) {
  x <- str_remove(sin_codigo(x), "\\.\\s?\\d{2,}.*$")
  str_trunc(str_split_fixed(x, "\\. |; | / ", 2)[, 1], n)
}

lk_productos <- readxl::read_excel(ruta_lookups, sheet = "productos", col_types = "text") |>
  transmute(
    tipo = "Producto",
    cod = `Código Nomenclatura Local de Productos`,
    nombre = corto(`Nomenclatura Local de Productos`),
    completo = sin_codigo(`Nomenclatura Local de Productos`),
    grupo = `CPC Grupos`,
    cat = niveles_bio[as.integer(`Código Bioeconomía Productos`)],
    desde = as.integer(Desde), hasta = as.integer(Hasta),
    orden = as.integer(Orden)
  ) |>
  mutate(nombre = if_else(cod == "217/218", "Tortas oleaginosas y borras de algodón", nombre))

lk_actividades <- readxl::read_excel(ruta_lookups, sheet = "actividades", col_types = "text") |>
  transmute(
    tipo = "Actividad",
    cod = `Código Clasificación Actividades`,
    nombre = corto(str_replace_all(`Descripción Actividades`, "\\b\\d{2,5}\\s*-\\s*", "")),
    completo = `Descripción Actividades`,
    grupo = `CIIU Compacto Descripción`,
    cat = niveles_bio[as.integer(`Código Bioeconomía Actividades`)],
    desde = as.integer(Desde), hasta = as.integer(Hasta),
    orden = as.integer(Orden)
  )

scn <- readRDS("data/arg_scn.rds")
ANIOS <- sort(unique(scn$Año))
ANIO_MAX <- max(ANIOS)

# Clasificación original: dos vectores con nombre, código -> categoría
clasif_original <- list(
  productos = setNames(lk_productos$cat, lk_productos$cod),
  actividades = setNames(lk_actividades$cat, lk_actividades$cod)
)

# Aplica una clasificación a la base: reemplaza las categorías por código
aplicar_clasif <- function(d, clasif) {
  es_p <- d$`Tipo fila` == "Producto"
  es_a <- d$`Tipo columna` == "Actividad"
  bp <- d$`Bioeconomía Productos`
  ba <- d$`Bioeconomía Actividades`
  bp[es_p] <- unname(clasif$productos[d$`Código fila`[es_p]])
  ba[es_a] <- unname(clasif$actividades[d$`Código columna`[es_a]])
  d$`Bioeconomía Productos` <- bp
  d$`Bioeconomía Actividades` <- ba
  d
}

# Diferencias entre una clasificación y la original
cambios_clasif <- function(clasif) {
  f <- function(nuevo, orig, lk, tipo) {
    k <- names(nuevo)[nuevo != orig[names(nuevo)]]
    if (!length(k)) return(tibble())
    lk |> filter(cod %in% k) |> transmute(tipo = tipo, cod, nombre, antes = cat, despues = unname(nuevo[cod]))
  }
  bind_rows(
    f(clasif$productos, clasif_original$productos, lk_productos, "Producto"),
    f(clasif$actividades, clasif_original$actividades, lk_actividades, "Actividad")
  )
}
