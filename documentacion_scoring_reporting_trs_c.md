# BASC-3 TRS-C: scoring y reporting por forma

Este documento explica primero la forma TRS-C: entrada, recodificación, escalas principales, escalas compuestas, escalas de contenido, índices de validez, índices clínicos, índices de funcionamiento ejecutivo y reporting. Después recoge las diferencias entre las demás formas del BASC-3 a nivel de escalas principales, escalas compuestas, escalas de contenido, índices e ítems.

## Alcance de TRS-C

TRS-C es la forma para profesores/tutores en edad infantil.

La entrada esperada son 156 respuestas. Las opciones se codifican como:

- Nunca = 0
- A veces = 1
- Frecuentemente = 2
- Casi siempre = 3

Los ítems invertidos se transforman con `3 - valor`. En TRS-C son: `1, 21, 64, 55, 98, 144, 20, 22, 32, 139`.

## Flujo de scoring TRS-C

1. Seleccionar baremos.
   Para obtener puntuaciones T, percentiles y SEM se consulta `BASC3_ES_ESES_NORM`. Las columnas relevantes son `TEST_ID`, `SCALE`, `RAWSCORE`, `AGE_MIN`, `AGE_MAX`, `GENDER_NORM` y las columnas de T, PR y SEM del baremo seleccionado. Para diferencias ipsativas, significaciones, comparaciones entre compuestos y frecuencia/tasa base se consulta `BASC3DIFF_ES_ESES_NORM`; ahí se usan `TEST_ID`, `SCALE`, `AGE_MIN`, `AGE_MAX`, `RAWSCORE_MIN`, `RAWSCORE_MAX`, `PERCENTILE`, `NORMALIZED_SCORE1` y `NORMALIZED_SCORE2`.

2. Determinar el baremo aplicable.
   La selección normativa se realiza combinando edad, sexo y tipo de baremo. El reporte TRS-C utiliza el baremo combinado por sexo para las tablas principales, aunque el scoring puede calcular también el baremo específico cuando está disponible.

3. Validar entrada.
   Debe comprobarse que existe una fila normativa aplicable, que llegan las 156 respuestas esperadas, que la edad está presente y que pertenece al rango de la forma.

4. Recodificar respuestas.
   Se invierten los ítems de la lista TRS-C.

   ```r
   reversed <- c(1, 21, 64, 55, 98, 144, 20, 22, 32, 139)

   reverse <- function(value, index) {
     if (index < 0) {
       return(2 - value)
     }
     return(3 - value)
   }

   Score$items <- Score$items - 1
   for (i in 1:length(Score$items)) {
     if (i %in% reversed) {
       Score$items[i] <- reverse(Score$items[i], i)
     }
   }
   ```

5. Calcular puntuación directa de escalas.
   Para cada escala se suman los ítems válidos. Si una escala tiene más de 2 omisiones o valores 99, la escala queda no puntuable. Si tiene 1 o 2 omisiones, se imputa `n_omisiones * constante_de_omisión` antes de obtener la puntuación directa.

   ```r
   score_scale <- function(datos_items, omit_constant) {
     n_nas <- sum(is.na(datos_items) | datos_items == 99)

     if (n_nas > 2) {
       return(NA)
     }

     sum(datos_items[datos_items < 99 & !is.na(datos_items)] +
         (n_nas * omit_constant))
   }
   ```

6. Transformar por baremos.
   Cada puntuación directa se busca en `BASC3_ES_ESES_NORM`. La fila normativa devuelve la puntuación T, el percentil y el SEM de la escala o compuesto para el grupo normativo seleccionado.

7. Calcular compuestos.
   Primero se suman las puntuaciones T de las escalas incluidas en cada compuesto. Después esa suma se transforma en la tabla `BASC3_ES_ESES_NORM` para obtener T, percentil y SEM del compuesto.

   ```r
   epi_scales <- c("hyp", "agg", "cop")
   ipi_scales <- c("anx", "dep", "som")
   spi_scales <- c("atp", "lep")
   asi_scales <- c("ada", "sos", "lea", "sus", "com")
   BSI_scales <- c("hyp", "agg", "dep", "atp", "aty", "wit")

   BSI <- sum(T_hyp, T_agg, T_dep, T_atp, T_aty, T_wit)
   BSI_mean <- round2(BSI / 6)
   asi_mean <- round2(asi / 5)
   ```

8. Calcular diferencias ipsativas.
   Las escalas clínicas/escolares se comparan contra la media T del BSI. Las adaptativas se comparan contra la media T de Habilidades adaptativas. Para obtener la frecuencia/tasa base, se calcula la diferencia y se busca en `BASC3DIFF_ES_ESES_NORM` la fila en la que esa diferencia cae dentro del rango definido por `RAWSCORE_MIN` y `RAWSCORE_MAX`. El `PERCENTILE` de esa fila es la frecuencia/tasa base, y `NORMALIZED_SCORE1` se usa como umbral de significación de la diferencia.

   ```r
   if (escala %in% c("ad", "so", "le", "sk", "fc")) {
     diff <- T_escala - asi_mean
   } else {
     diff <- T_escala - BSI_mean
   }

   fila_diff <- dplyr::filter(
     norm_diff,
     SCALE == escala &
       abs(diff) > RAWSCORE_MIN &
       abs(diff) <= RAWSCORE_MAX
   )

   sig <- abs(diff) > fila_diff$NORMALIZED_SCORE1
   frecuencia_tasa_base <- fila_diff$PERCENTILE
   ```

9. Calcular comparaciones entre compuestos.
   Se comparan Problemas de exteriorización frente a Problemas de interiorización, Problemas de exteriorización frente a Problemas escolares, y Problemas de interiorización frente a Problemas escolares. Para cada comparación se calcula la diferencia y se localiza en `BASC3DIFF_ES_ESES_NORM` el rango `RAWSCORE_MIN`-`RAWSCORE_MAX` en el que cae. El `PERCENTILE` de esa fila da la frecuencia/tasa base, y `NORMALIZED_SCORE2` se usa como umbral de significación para comparaciones entre compuestos.

   ```r
   comp_diff <- setNames(
     c(epi_T - ipi_T, epi_T - spi_T, ipi_T - spi_T),
     c("epiipi", "epispi", "ipispi")
   )

   fila_comp <- dplyr::filter(
     norm_diff,
     SCALE == name &
       abs(comp_diff[name]) > RAWSCORE_MIN &
       abs(comp_diff[name]) <= RAWSCORE_MAX
   )

   sig_comp <- abs(comp_diff[name]) > fila_comp$NORMALIZED_SCORE2
   frecuencia_tasa_base_comp <- fila_comp$PERCENTILE
   ```

10. Calcular escalas de contenido e índices clínicos.
    Se calculan por suma directa de sus ítems y se transforman con las normas igual que el resto de escalas.

## Escalas TRS-C y mapeo de ítems

| Escala | ítems | Constante omisiones |
|---|---:|---:|
| Hiperactividad | 4, 11, 30, 33, 40, 93, 103, 110, 126, 137, 154 | 1 |
| Agresividad | 6, 10, 52, 61, 73, 82, 90, 111, 124, 138 | 0 |
| Problemas de conducta | 23, 35, 43, 48, 70, 85, 121, 135, 149 | 0 |
| Ansiedad | 8, 15, 26, 68, 79, 83, 95, 106, 112 | 0 |
| Depresión | 12, 81, 91, 97, 114, 118, 133, 142, 146, 153, 156 | 0 |
| Somatización | 34, 56, 76, 80, 105, 131, 134 | 0 |
| Problemas de atención | 1R, 14, 21R, 53, 64R, 88, 107, 152 | 1 |
| Problemas de aprendizaje | 28, 44, 55R, 72, 117, 120, 130, 147 | 1 |
| Atipicidad | 9, 50, 63, 87, 125, 128, 132, 145, 151 | 0 |
| Retraimiento | 16, 37, 62, 96, 98R, 115, 123, 144R | 0 |
| Adaptabilidad | 3, 20R, 24, 38, 42, 47, 59, 67, 69 | 2 |
| Habilidades sociales | 5, 19, 31, 45, 104, 113, 116, 127, 141, 150 | 2 |
| Liderazgo | 25, 41, 49, 58, 86, 92, 102 | 2 |
| Habilidades académicas | 7, 77, 94, 122, 129, 143, 148, 155 | 2 |
| Comunicación funcional | 2, 22R, 32R, 39, 60, 71, 74, 89, 119, 139R | 2 |

## Compuestos TRS-C

| Compuesto | Escalas que suma |
|---|---|
| Problemas de exteriorización | Hiperactividad, Agresividad, Problemas de conducta |
| Problemas de interiorización | Ansiedad, Depresión, Somatización |
| Problemas escolares | Problemas de atención, Problemas de aprendizaje |
| Habilidades adaptativas | Adaptabilidad, Habilidades sociales, Liderazgo, Habilidades académicas, Comunicación funcional |
| Índice de síntomas conductuales | Hiperactividad, Agresividad, Depresión, Problemas de atención, Atipicidad, Retraimiento |

La media T del BSI se calcula como la suma de las T de sus seis escalas dividida por 6. La media T de Habilidades adaptativas se calcula como la suma de sus cinco escalas dividida por 5.

## Escalas de contenido e índices clínicos TRS-C

| Escala/Índice | ítems |
|---|---|
| Control de la ira | 6, 61, 65, 75, 93, 111, 142 |
| Acoso escolar | 35, 36, 48, 57, 61, 90, 109, 121, 124 |
| Trastornos del desarrollo social | 2, 16, 38, 50, 62, 63, 66, 71, 89, 100, 115, 127, 132, 136, 144 |
| Autocontrol emocional | 6, 15, 29, 51, 67, 69, 81, 91, 93, 111, 142, 154 |
| Funcionamiento ejecutivo | 1, 6, 14, 17, 18, 33, 39, 40, 51, 55, 58, 69, 86, 93, 103, 107, 108, 111, 122, 126, 142, 143, 148, 154 |
| Emocionalidad negativa | 6, 20, 46, 78, 91, 118, 133, 142 |
| Resiliencia | 3, 17, 38, 39, 41, 42, 47, 49, 67, 84, 92, 101, 140 |
| Índice de probabilidad de TDAH | 4, 6, 13, 14, 22, 25, 41, 53, 64, 70, 88, 91, 107, 111, 125, 135, 136, 139, 143, 148, 154 |
| Índice de probabilidad de autismo | 5, 9, 45, 55, 58, 60, 62, 63, 86, 87, 91, 92, 100, 106, 108, 111, 119, 132, 141, 145, 151, 154 |
| Índice de probabilidad de comportamiento perturbador | 10, 12, 23, 35, 52, 57, 61, 62, 70, 73, 85, 90, 91, 111, 118, 125, 133, 138, 139, 148, 154, 156 |
| Índice de deterioro funcional | 1, 2, 5, 15, 22, 23, 28, 32, 39, 45, 50, 53, 60, 62, 69, 71, 72, 78, 80, 81, 83, 86, 89, 91, 93, 94, 96, 98, 103, 105, 106, 111, 117, 120, 125, 126, 128, 130, 139, 143, 144, 146, 147, 155 |

## índices de validez TRS-C

Índice F: cuenta respuestas "Casi siempre" en los ítems `8, 12, 20, 26, 35, 37, 50, 57, 61, 73, 78, 80, 82, 97, 109, 123, 128, 132, 149, 156`.

- 0-1: Aceptable
- 2: Cautela
- 3 o más: Suma cautela

Índice de patrón de respuestas: cuenta cuántas veces una respuesta difiere de la respuesta inmediatamente anterior.

- 0-45: Precaución-Bajo
- 46-123: Aceptable
- 124-155: Precaución-Alto

Índice de consistencia: suma diferencias absolutas entre pares:

`(70,23), (131,105), (135,82), (110,40), (9,63), (17,58), (75,111), (147,28), (107,53), (88,14), (102,155), (29,91), (16,96), (85,35), (64R,21R), (150,113), (44,117), (124,90), (116,141), (77,94)`.

- 0-11: Aceptable
- 12-13: Cautela
- 14 o más: Suma cautela

## índices de funcionamiento ejecutivo en el reporte

El reporte TRS-C añade cinco índices ejecutivos calculados desde respuestas de ítems, no desde baremos de escalas:

| Índice | ítems |
|---|---|
| Resolución de problemas | 17, 18, 39, 55, 58, 86, 108, 143, 148 |
| Control atencional | 1, 14, 21, 53, 64, 88, 107, 152 |
| Control conductual | 30, 33, 40, 93, 103, 126, 154 |
| Control emocional | 6, 29, 51, 69, 75, 111, 142 |
| Funcionamiento ejecutivo global | 1, 6, 14, 17, 18, 21, 29, 30, 33, 39, 40, 51, 53, 55, 58, 64, 69, 75, 86, 88, 93, 103, 107, 108, 111, 126, 142, 143, 148, 152, 154 |

Rangos:

| Índice | No elevado | Elevado | Muy elevado |
|---|---:|---:|---:|
| Resolución de problemas | <=19 | <=24 | <=27 |
| Control atencional | <=15 | <=19 | <=24 |
| Control conductual | <=9 | <=13 | <=21 |
| Control emocional | <=7 | <=12 | <=21 |
| Funcionamiento ejecutivo global | <=48 | <=61 | <=93 |

## Reporting TRS-C

Antes de montar las tablas, se calculan los intervalos de confianza,  la frecuencia/tasa base:

```r
IC90_MIN <- factors$T - round(qnorm(0.95) * factors$SEM)
IC90_MAX <- factors$T + round(qnorm(0.95) * factors$SEM)

factors$IC90 <- paste0(IC90_MIN, "-", IC90_MAX)
factors$BR <- paste0("â‰¤", factors$BR, "%")
factors$SIG[factors$SIG == 99 & !is.na(factors$SIG)] <- "NS"
```

Las medias del Índice de síntomas conductuales y de Habilidades adaptativas se recalculan para mostrarlas en el informe:

```r
BSI_factors <- c("hyp", "agg", "dep", "atp", "aty", "wit")
BSI <- sum(factors[BSI_factors, "T-GC"])
BSI_mean <- round(BSI / 6)

asi_scales <- c("ada", "sos", "lea", "sus", "com")
asi <- sum(factors[asi_scales, "T-GC"])
asi_mean <- round(asi / 5)
```

1. Portada.
   Incluye nombre del instrumento, forma para profesores/tutores, datos identificativos, fecha, informante y logos.

3. Datos del usuario e informante.
   Profesor, tipo de profesor, tiempo que conoce al niño, observaciones sobre visión/audición y comentarios generales. Si no hay observaciones, mostrar que no se han proporcionado.

4. Descripción del BASC-3.
   Descripción general del sistema BASC-3 y de la forma TRS.

5. Resumen de índices de validez.
   Tabla con Índice F, Patrón de respuestas y Consistencia, mostrando clasificación y puntuación directa.

   ```r
   f_index <- counts
   if (f_index <= 1) {
     f_range <- "Aceptable"
   } else if (f_index <= 2) {
     f_range <- "Cautela"
   } else {
     f_range <- "Suma cautela"
   }

   response_pattern_index <- 0
   for (i in 1:(length(Score) - 1)) {
     response_pattern_index <- response_pattern_index + (Score[i + 1] != Score[i])
   }

   if (response_pattern_index <= 45) {
     response_pattern_range <- "Precaución-Bajo"
   } else if (response_pattern_index <= 123) {
     response_pattern_range <- "Aceptable"
   } else {
     response_pattern_range <- "Precaución-Alto"
   }
   ```

6. Perfil de puntuaciones T de escalas compuestas, clínicas y adaptativas.
   Gráfico de puntuaciones T. Para TRS-C debe contener compuestos, escalas clínicas/escolares y adaptativas.

7. Tabla de compuestos.
   Para cada compuesto: puntuación directa, T, percentil e intervalo de confianza del 90%. Incluir comparación entre compuestos y las medias T de BSI y Habilidades adaptativas.

8. Tabla de escalas clínicas, escolares y adaptativas.
   Para cada escala: puntuación directa, T, percentil, IC 90%, diferencia ipsativa, nivel de significación y frecuencia.

9. ítems de validez.
   Listar los ítems que contribuyen al Índice F, los elementos del patrón de respuesta y los pares de consistencia relevantes. Si el Índice F es aceptable y no hay contribuciones destacables, mostrar que la puntuación del Índice F es aceptable.

10. Perfil y tablas de escalas de contenido e índices clínicos.
    Incluir escalas de contenido y los índices clínicos con puntuación directa, T, percentil e IC 90%.

11. Resumen de índices de funcionamiento ejecutivo.
    Mostrar funcionamiento ejecutivo global, resolución de problemas, control atencional, control conductual y control emocional, con su clasificación.

    ```r
    psi <- sum_contenido_clinicos(c(17,18,39,55,58,86,108,143,148))
    aci <- sum_contenido_clinicos(c(1,14,21,53,64,88,107,152))
    bci <- sum_contenido_clinicos(c(30,33,40,93,103,126,154))
    eci <- sum_contenido_clinicos(c(6,29,51,69,75,111,142))
    efi <- sum_contenido_clinicos(c(
      1,6,14,17,18,21,29,30,33,39,40,51,53,55,58,64,
      69,75,86,88,93,103,107,108,111,126,142,143,148,152,154
    ))

    tabla_indices <- data.frame(
      `No elevado` = c(19, 15, 9,  7, 48),
      Elevado = c(24, 19, 13, 12, 61),
      `Muy elevado` = c(27, 24, 21, 21, 93),
      row.names = c("psi", "aci", "bci", "eci", "efi"),
      check.names = FALSE
    )
    ```

12. ítems críticos.
    Listar los críticos definidos en la ficha TRS-C y destacar aquellos con respuesta A veces, Frecuentemente o Casi siempre.

13. ítems por escala.
    Separar en escalas clínicas, adaptativas, de contenido, índices clínicos e índices de funcionamiento ejecutivo.

14. Respuestas a los ítems.
    Matriz final de respuestas, usando etiquetas de respuesta en español: Nunca, A veces, Frecuentemente, Casi siempre.

    ```r
    nsoa_map <- c(
      "0" = "Nunca",
      "1" = "A veces",
      "2" = "Frecuentemente",
      "3" = "Casi siempre"
    )

    text_Items <- unname(nsoa_map[as.character(Score)])
    ```

## Estado del resto de formas

La documentación general del BASC-3 se puede encontrar en Jira en un fichero llamado `BASC-3 Tot`.

En Concerto falta subir los ítems en español para todas las formas excepto TRS-C.

El código disponible para el resto de formas calcula solo escalas principales. Todavía no calcula escalas compuestas, escalas de contenido ni índices clínicos para esas formas.

También falta implementar el resto de informes. TRS-C es la forma más completa actualmente: incluye escalas principales, escalas compuestas, escalas de contenido, índices clínicos, índices de validez, índices de funcionamiento ejecutivo y salida de respuestas por ítem.

Nota final: la gestión de escalas inválidas por exceso de omisiones se ha incluido solo para las escalas base. No se ha incluido todavía la gestión de escalas inválidas para escalas compuestas, escalas de contenido, índices clínicos ni índices de funcionamiento ejecutivo.
