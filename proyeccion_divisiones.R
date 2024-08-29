library(forecast)
library(openxlsx)

# Archivo con los índices históricos de cada división a nivel nacional
ind <- read.csv("indices_divisiones.csv")

# Archivo con las ponderaciones de cada división
ponderaciones <- read.csv("ponderaciones.csv")$ponderacion

# El número de meses a predecir
m_pr <- 5

# Esta es una sequencia de meses para usar como encabezados
# La fecha de inicio deberá ser dinámica en el futuro.
fechas <- format(
  seq(
    as.Date("2023-12-01"),
    by = "month",
    length.out = 13
  ),
  "%Y-%m"
)

# Una lista de matrices donde guardar los pronósticos de las 13 divisiones
f_div <- vector(mode = "list", length = 14)

# La última matriz es para el IPC general. Inicialmente es 0.
f_div[[14]] <- diag(5)

for (i in 1:13) {
  # Seleccionamos los índices según la división
  ind_div <- ind[ ind$division == i, ]$indice
  # Se genera un pronóstico
  pr_div <- forecast(
    auto.arima(
      ts(
        ind_div,
        start = c(ind$anio[1], ind$mes[1]),
        frequency = 12
      ),
      seasonal = TRUE
    ),
    h = m_pr
  # Tomamos las filas que corresponden a upper, mean, y lower. La función
  # forecast las coloca en estas posiciones.
  )[c(5, 4, 6)]
  # Se reordenan en una matriz de mayor a menor
  f_div[[i]] <- rbind (
    t(pr_div$upper)[c(2, 1), ],
    pr_div$mean,
    t(pr_div$lower)
  )
  
  # Sumamos los resultados al IPC general
  f_div[[14]] <- f_div[[14]] + f_div[[i]] * ponderaciones[i]
  
  # Añadimos los índices actuales para completar una serie de 13.
  # La repetición es para obtener un arbol al gráficar.
  ind_div <- tail(ind_div, 13 - m_pr)
  f_div[[i]] <- cbind(
    t(matrix(rep(ind_div, times = 5), ncol = m_pr)),
    f_div[[i]]
  )
  # Se añaden los encabezados
  colnames(f_div[[i]]) <- fechas
}

# Convertimos la suma en un promedio
f_div[[14]] <- f_div[[14]] / 100

# Añadimos índices generales ya calculados
f_div[[14]] <- cbind(
  t(
    matrix(
      rep(
        tail(read.csv("Indices.csv")$indice, 13 - m_pr),
        times = 5
      ),
      ncol = m_pr
    )
  ),
  f_div[[14]]
)
# Se añaden los encabezados
colnames(f_div[[14]]) <- fechas

# Se nombra cada matriz según la región que representa
names(f_div) <- c(paste0("Región ", 1:13), "República")

# Se guardan los resultados
write.xlsx(f_div, "pronostico_divisiones.xlsx")