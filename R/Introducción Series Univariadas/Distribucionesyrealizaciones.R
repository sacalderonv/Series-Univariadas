library(ggplot2)

#Caso estacionario e identica distribución----
# Datos de ejemplo
library(ggplot2)

# Datos basados en tu estructura
df_puntos <- data.frame(
  t = c(1, 2, 3, 6),
  realizacion = c(0.2, -0.7, 0.9, -0.2),
  label = c("x1", "x2", "x3", "xT") # Etiquetas para x1, x2, x3 y xT
)

mu <- 0
sigma <- 0.5

p <- ggplot(df_puntos, aes(x = t, y = realizacion)) +
  # 1. LÍNEA QUE UNE LAS REALIZACIONES (La serie de tiempo)
  geom_line(color = "navy", size = 0.8, alpha = 0.7) +
  
  # Línea de la media (mu)
  geom_hline(yintercept = mu, color = "blue", size = 1) +
  
  # Líneas verticales de referencia (t=1, 2, 3, T)
  geom_vline(aes(xintercept = t), color = "red", linetype = "dashed", alpha = 0.5)

# Añadir las densidades manualmente (Curvas verdes)
for(i in 1:nrow(df_puntos)) {
  temp_t <- df_puntos$t[i]
  
  # Generar curva normal (sigma^2)
  dist_df <- data.frame(
    y = seq(mu - 3*sigma, mu + 3*sigma, length.out = 100)
  )
  # Espejamos la curva hacia la izquierda del eje t como en el dibujo
  dist_df$x <- temp_t - dnorm(dist_df$y, mu, sigma) * 0.4
  
  p <- p + geom_path(data = dist_df, aes(x = x, y = y), color = "forestgreen", size = 0.7)
}

# Capas finales de puntos y etiquetas
p <- p + 
  geom_point(color = "navy", size = 3) +
  geom_text(aes(label = label), vjust = -1.2, hjust = -0.5, parse = FALSE) +
  # Eje X con el formato 1, 2, 3 ... T
  scale_x_continuous(breaks = c(1, 2, 3, 6), labels = c("1", "2", "3", "T")) +
  labs(title =expression(paste("Serie de Tiempo y Distribuciones ",mu," constante")),
       subtitle = "Trayectoria conectando realizaciones observadas",
       x = "Tiempo (t)", y = "X(t)") +
  theme_minimal()

print(p)

###Caso No estacionario tendencia determinística----


# Configuración de datos
t <- 1:6
mu_t <- 0.8 * t  # Tendencia de la media
set.seed(42)
realizaciones <- mu_t + rnorm(length(t), mean = 0, sd = 0.5)

df_base <- data.frame(t = t, mu = mu_t, x_t = realizaciones)

p <- ggplot(df_base, aes(x = t, y = x_t)) +
  # 1. CONEXIÓN DE REALIZACIONES: Ahora la línea azul une los puntos observados
  geom_line(color = "blue", size = 1, alpha = 0.8) + 
  
  # Líneas verticales de referencia (rojas y punteadas como en el dibujo)
  geom_vline(aes(xintercept = t), color = "red", linetype = "dashed", alpha = 0.3)

# 2. AÑADIR LAS CAMPANAS VERTICALES: Centradas en la media mu_t de cada punto
for(i in 1:nrow(df_base)) {
  curr_t <- df_base$t[i]
  curr_mu <- df_base$mu[i]
  
  # Generamos la curva de densidad normal
  dist_df <- data.frame(
    y = seq(curr_mu - 1.5, curr_mu + 1.5, length.out = 100)
  )
  # Espejamos la distribución hacia la izquierda del eje vertical [cite: 1]
  dist_df$x <- curr_t - dnorm(dist_df$y, curr_mu, 0.5) * 0.4
  
  p <- p + geom_path(data = dist_df, aes(x = x, y = y), color = "darkgreen")
}

# 3. CAPAS FINALES Y EJE T
p <- p + 
  geom_point(color = "navy", size = 3) +
  theme_minimal() +
  # Obligamos a que las etiquetas del eje X sean 1, 2, 3... T [cite: 11, 12, 13, 17]
  scale_x_continuous(breaks = t, 
                     labels = c(head(t, -1), "T")) + 
  labs(title = "Serie de Tiempo No Estacionaria", 
       subtitle = "Trayectoria conectando realizaciones con densidades verticales",
       x = "Tiempo (t)", 
       y = expression(X[t]))

print(p)
### Caminata Aletorio----



# Simulación
set.seed(42)
n <- 7
t <- 1:n
mu_t <- cumsum(rnorm(n, mean = 0, sd = 0.7))
x_t <- mu_t + rnorm(n, mean = 0, sd = 0.2)

df <- data.frame(t = t, mu = mu_t, x = x_t)

p <- ggplot(df, aes(x = t, y = x)) +
  # Conexión de las realizaciones
  geom_line(color = "navy", size = 1) +
  geom_point(color = "navy", size = 3)

# Añadir densidades verticales
for(i in 1:nrow(df)) {
  curr_mu <- df$mu[i]
  curr_t <- df$t[i]
  
  dens_df <- data.frame(
    y = seq(curr_mu - 2.5, curr_mu + 2.5, length.out = 100)
  )
  dens_df$x <- curr_t - dnorm(dens_df$y, curr_mu, 0.4) * 0.5
  
  p <- p + geom_path(data = dens_df, aes(x = x, y = y), color = "darkgreen", alpha = 0.6)
}

p <- p + 
  geom_vline(aes(xintercept = t), color = "red", linetype = "dashed", alpha = 0.3) +
  scale_x_continuous(breaks = 1:n, labels = c("1", "2", "3", "4", "5", "6", "T")) +
  labs(title = "Trayectoria de la Serie de Tiempo",
       subtitle = "La línea conecta las realizaciones x_t surgidas de cada distribución") +
  theme_minimal()

print(p)

###Cambia distribuciones y medias----




set.seed(10)
t_vec <- 1:4
mu_t <- cumsum(rnorm(4, 0, 1.5))

# Realizaciones manuales para asegurar visibilidad
x_t <- c(
  rnorm(1, mu_t[1], 0.4),
  rcauchy(1, mu_t[2], 0.2),
  runif(1, mu_t[3]-0.8, mu_t[3]+0.8),
  mu_t[4] + (rexp(1, 2) * sample(c(1, -1), 1)) # Simulación Laplace
)

df <- data.frame(t=t_vec, x=x_t, mu=mu_t)

p <- ggplot(df, aes(x=t, y=x)) +
  geom_line(color="navy", size=1) +
  geom_point(color="navy", size=3) +
  geom_vline(aes(xintercept=t), color="red", linetype="dashed", alpha=0.3)

# Dibujar densidades
for(i in 1:4) {
  curr_mu <- mu_t[i]
  curr_t <- t_vec[i]
  
  y_seq <- seq(curr_mu - 3, curr_mu + 3, length.out = 200)
  
  # Lógica de densidad por tipo
  if(i == 1) d <- dnorm(y_seq, curr_mu, 0.4)
  else if(i == 2) d <- dcauchy(y_seq, curr_mu, 0.2)
  else if(i == 3) d <- dunif(y_seq, curr_mu-1, curr_mu+1)
  else d <- exp(-abs(y_seq-curr_mu)/0.4) # Laplace simplificada
  
  dens_df <- data.frame(y = y_seq, x = curr_t - d * 0.4)
  p <- p + geom_path(data=dens_df, aes(x=x, y=y), color="darkgreen")
}

p + scale_x_continuous(breaks=1:4, labels=c("1", "2", "3", "T")) +
  theme_minimal() + labs(title="Serie de Tiempo: Heterocedasticidad y Cambio de Distribución")

