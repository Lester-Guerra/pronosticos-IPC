library(forecast)
library(openxlsx)

ind <- read.csv("indices_regionales.csv")
fecha_inicial <- c(ind[1, 1], ind[1, 2])
ind <- ind[ , 3:10]

# El número de meses a predecir
m_pr <- 5

# Archivo con los índices históricos a nivel república.
ipc <- tail(read.csv("Indices.csv"), 13 - m_pr)$indice

# La participación de cada región en la nueva base se calcula en el momento.
participacion <- solve (
  tail(ind, 8),
  ipc,
)

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

# Una lista de matrices donde guardar los pronósticos de las 8 regiones
f_reg <- vector(mode = "list", length = 9)

# La última matriz es para el IPC general. Inicialmente es 0.
f_reg[[9]] <- diag(5)

for (i in 1:8) {
  # Seleccionamos los índices según la división
  ind_reg <- ind[ , i ]
  # Se genera un pronóstico
  pr_reg <- forecast(
    auto.arima(
      ts(
        ind_reg,
        start = fecha_inicial,
        frequency = 12
      ),
      seasonal = TRUE
    ),
    h = m_pr
  # Estas filas que corresponden a upper, mean, y lower. La función forecast
  # siempre las coloca en estas posiciones.
  )[c(5, 4, 6)]
  # Se reordenan en una matriz de mayor a menor
  f_reg[[i]] <- rbind (
    t(pr_reg$upper)[c(2, 1), ],
    pr_reg$mean,
    t(pr_reg$lower)
  )

  # Sumamos los resultados al IPC general
  f_reg[[9]] <- f_reg[[9]] + f_reg[[i]] * participacion[i]
  
  # Añadimos los índices actuales para completar una serie de 13.
  ind_reg <- tail(ind_reg, 13 - m_pr)
  f_reg[[i]] <- rbind(
    cbind(
      matrix(NA, nrow = 5, ncol = 13 - m_pr - 1),
      rep(ind_reg[13 - m_pr], 5),
      f_reg[[i]]
    ),
    append(ind_reg, rep(NA, m_pr))
  )
  # Se añaden los encabezados
  colnames(f_reg[[i]]) <- fechas
}

# Se añaden los índices ya calculados
f_reg[[9]] <- rbind(
  cbind(
    matrix(NA, nrow = 5, ncol = 13 - m_pr - 1),
    rep(ipc[13 - m_pr], 5),
    f_reg[[9]]
  ),
  append(ipc, rep(NA, m_pr))
)

# Se añaden los encabezados
colnames(f_reg[[9]]) <- fechas

# Se nombra cada matriz según la región que representa
names(f_reg) <- c(paste0("Región ", 1:8), "República")

# Se guardan los resultados
write.xlsx(f_reg, "pronostico_regiones.xlsx")