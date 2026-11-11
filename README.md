# Herramienta de bioeconomía de Argentina

Aplicación de Shiny para explorar la cuenta temática de bioeconomía de Argentina, construida con los cuadros de oferta y utilización (COU) de INDEC de 2018 a 2023, y para **reclasificar productos y actividades** y ver cómo cambian los agregados.

Acompaña los materiales del [taller de bioeconomía](https://renatovargas.github.io/taller-bioeconomia/) ([repositorio](https://github.com/renatovargas/taller-bioeconomia)).

## Pantallas

| Pantalla | Qué muestra |
|---|---|
| Panorama | Participación de la bioeconomía en producción, exportaciones, impuestos y valor agregado, y su evolución desde 2018 |
| Cuadros | Oferta y utilización bioeconómicas y valor agregado por los dos métodos, con descarga en Excel |
| Clasificación | Mosaico y tabla de productos y actividades por año. Un clic en un recuadro lo reclasifica |
| Reclasificar | Su escenario frente a la clasificación original: participaciones, tasa implícita, valor agregado, saldo comercial y multiplicadores |
| Multiplicadores | Uso directo e indirecto de insumos de bioeconomía por actividad, y grupos según la desviación absoluta mediana |
| Método | Categorías, indicadores, límites y fuentes |

El interruptor **Mi escenario** muestra todas las pantallas con su clasificación. Los escenarios se pueden guardar en Excel, volver a cargar o compartir con un enlace (`?tab=Reclasificar&p=632:2,346:3`, donde `p` son productos, `a` actividades y 1, 2 y 3 son Bioeconomía, Bioeconomía parcial y Fuera de la bioeconomía).

## Estructura

- `app.R`: interfaz y servidor.
- `R/`: Shiny los carga solos, en orden: paquetes, datos y clasificación, cálculos (las mismas fórmulas del script `02-cuadros-y-resultados.R` del taller), tablas, gráficos y mosaico.
- `data/`: `arg_scn.rds` (base plana, millones de pesos) y `arg_lookups.xlsx` (clasificación por código con vigencia por año). Vienen del repositorio del taller.
- `metodo.md`: texto de la pantalla Método.
- `www/estilos.css`: estilos.
- `manifest.json`: dependencias para Posit Connect Cloud.

## Ejecutar

```r
shiny::runApp()
```

Paquetes: shiny, bslib, dplyr, tidyr, stringr, reactable, plotly, readxl, writexl.

## Actualizar los datos

Después de correr los scripts del taller:

```bash
Rscript scripts/actualizar-datos.R
Rscript -e 'rsconnect::writeManifest()'
```

## Publicar en Posit Connect Cloud

En [connect.posit.cloud](https://connect.posit.cloud), elija *Publish* → *Shiny*, seleccione este repositorio, la rama `main` y el archivo principal `app.R`. Connect Cloud lee `manifest.json` para instalar los paquetes. Con *Automatically publish on push*, cada push a `main` actualiza la aplicación.

Las cifras son elaboración propia a partir de datos oficiales de INDEC y no constituyen estadísticas oficiales.
