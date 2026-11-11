## Cómo funciona esta herramienta

La herramienta parte de los **cuadros de oferta y utilización (COU) de INDEC, 2018 a 2023**, convertidos en una sola base plana: cada fila es una celda del cuadro, con su año, cuadro, producto, actividad y valor en millones de pesos corrientes. A esa base se le agrega la clasificación bioeconómica por código.

### Tres categorías

Las categorías siguen la terminología del SCN 2025 para **cuentas temáticas** (capítulo 38). Una cuenta temática se organiza alrededor de una *actividad clave* con una frontera precisa:

- **Bioeconomía:** dentro de la frontera. Productos de origen biológico y actividades cuya producción principal son esos productos.
- **Bioeconomía parcial:** parcialmente dentro de la frontera. Productos y actividades con contenido biológico y no biológico que el COU no permite separar.
- **Fuera de la bioeconomía:** todo lo demás.

Productos y actividades se clasifican por separado. Así se ven los productos bioeconómicos que fabrica una actividad que no lo es, y los insumos biológicos que esta compra.

### Su escenario

Cada cambio que haga en **Clasificación** o en **Reclasificar** entra en *su escenario*, que empieza igual a la clasificación original:

- Con el interruptor **Ver con mi escenario**, todas las pantallas muestran los resultados con su clasificación.
- **Reclasificar** siempre compara su escenario con la original.
- El escenario se puede guardar en Excel y volver a cargar.

### Indicadores

- **Participación:** la parte de cada agregado (producción, consumo intermedio, importaciones, exportaciones, impuestos netos, consumo final y formación de capital) que corresponde a cada categoría de producto.
- **Tasa impositiva implícita:** impuestos menos subvenciones sobre los productos de una categoría, entre su valor bruto de producción.
- **Valor agregado, método de actividades:** la suma del valor agregado de las actividades de cada categoría.
- **Valor agregado, método del consumo intermedio:** cada sección de la CIIU aporta valor agregado en proporción a sus insumos de bioeconomía.
- **Multiplicadores:** pesos de insumos de bioeconomía y de bioeconomía parcial usados, directa e indirectamente, por peso de demanda final de cada actividad. Usan el supuesto de tecnología de la industria y valores a precios de comprador, sin separar insumos importados. Las actividades se agrupan según su distancia a la mediana, medida en desviaciones absolutas medianas (DAM).

### Límites

- Los cuadros están a precios corrientes, en un periodo de alta inflación. Las participaciones mezclan volúmenes y precios.
- El COU no incluye empleo ni datos ambientales.
- La clasificación es una decisión técnica discutible, y esta herramienta existe para discutirla.

Las cifras son elaboración propia a partir de datos oficiales de INDEC y no constituyen estadísticas oficiales.

### Fuentes

- INDEC. Matrices de oferta y de utilización a precios de comprador (CIIU Rev. 3/CPC 1.1), 2018 a 2023.
- Vargas, R., Alvarado, I., Rodríguez, M., Rodríguez, A. y Wander, P. (2022). *Cuenta satélite de bioeconomía para Costa Rica: propuesta metodológica y aplicación práctica*. CEPAL.
- Vargas, R., Mondaini, A. y Rodríguez, A. G. (2023). *Cuentas satélite de bioeconomía para 13 países de América Latina y el Caribe: metodología y resultados*. CEPAL.
- Naciones Unidas y otros (2025). *System of National Accounts 2025*, capítulo 38: *Thematic and Extended Accounts*.

Materiales del taller: [renatovargas.github.io/taller-bioeconomia](https://renatovargas.github.io/taller-bioeconomia/). Código: [github.com/renatovargas/arg-tool-bioeconomia](https://github.com/renatovargas/arg-tool-bioeconomia).
