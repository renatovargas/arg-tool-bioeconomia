# Copia la base y los lookups desde el repositorio del taller
# ===========================================================
# Ejecutar desde la raíz de este proyecto, después de correr los scripts del
# taller (../taller-bioeconomia/scripts/01 y 02):
#   Rscript scripts/actualizar-datos.R

origen <- "../taller-bioeconomia/data"
archivos <- c(file.path(origen, "bd/arg_scn.rds"), file.path(origen, "config/arg_lookups.xlsx"))
stopifnot(all(file.exists(archivos)))
file.copy(archivos, "data/", overwrite = TRUE)
cat("Datos actualizados:\n")
print(file.info(list.files("data", full.names = TRUE))[, c("size", "mtime")])
