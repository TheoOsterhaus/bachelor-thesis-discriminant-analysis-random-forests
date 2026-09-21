
library(plot3D)


mu_vec_data01 <- c(2.5, 4)
cov_mat <- matrix(c(0.5, 0, 0, 0.5), nrow = 2)

z <- function(x1_val, x2_val) {
  # Parameter aus dem globalen Scope ziehen
  mu_current <- matrix(mu_vec_data01, ncol = 1)
  sigma_current <- cov_mat
  
  det_sigma_current <- det(sigma_current)
  if (det_sigma_current == 0) {
    return(0) # Rückgabe 0, wenn die Matrix singulär ist
  }
  inv_sigma_current <- solve(sigma_current)
  
  x_vec_mat <- matrix(c(x1_val, x2_val), ncol = 1) 
  
  exponent_term <- t(x_vec_mat - mu_current) %*% inv_sigma_current %*% (x_vec_mat - mu_current)
  
  pdf_value <- (1 / (2 * pi * sqrt(det_sigma_current))) * exp(-0.5 * exponent_term)
  return(as.numeric(pdf_value)) # Sicherstellen, dass ein Skalar zurückgegeben wird
}


# --- Erstellung des Gitters für die Plot-Achsen ---
# Die Bereiche der Achsen werden um die geschätzten Mittelwerte zentriert
# und decken ca. +/- 3 Standardabweichungen ab.
x1_range <- seq(mu1 - 3 * sqrt(sigma11), mu1 + 3 * sqrt(sigma11), length.out = 100)
x2_range <- seq(mu2 - 3 * sqrt(sigma22), mu2 + 3 * sqrt(sigma22), length.out = 100)

f <- outer(x1_range, x2_range, Vectorize(z))

# expand steuert die Streckung der Z-Achse
persp3D(x1_range, x2_range, f, 
        theta = 30, phi = 30, expand = 0.5, 
        xlab = "V1", ylab = "V2", zlab = "Dichte")





## ---- bivgaus01dis2
mu1<-mean(data02$V1)
mu2<-mean(data02$V2)#mean of X_2
sigma11<-var(data02$V1) #variance of X_1
sigma22<-var(data02$V2) #variance of X_2
sigma12<-cov(data02$V1, data02$V2) #covariance of X_1 and X_2
#plot
x1 <- seq(mu1-3, mu1+3, length= 500)
x2 <- seq(mu2-3, mu2+3, length= 500)
z <- function(x1,x2){ z <- exp(-(sigma22*(x1-mu1)^2+sigma11*(x2-mu2)^2-2*sigma12*(x1-mu1)*(x2-mu2))/(2*(sigma11*sigma22-sigma12^2)))/(2*pi*sqrt(sigma11*sigma22-sigma12^2)) }
f <- outer(x1,x2,z)
persp3D(x1, x2, f, theta = 30, phi = 30, expand = 0.5)



## ---- packages
suppressPackageStartupMessages({
  library(MASS)        # for mvrnorm, lda
  library(ggplot2)     # for plotting
  library(dplyr)       # for data manipulation
  library(tidyr)       # for pivot_longer
})

# Wir setzen hier EINEN Seed, um die GENERIERUNG der Hauptdatenbasis
# und die Reproduzierbarkeit der STARTPUNKTE für die Simulationen zu gewährleisten.
# Die Zufälligkeit von f-hat wird durch die wiederholte Ziehung von ZUFÄLLIGEN TRAININGSSETS
# IM LOOP demonstriert (hier wird KEIN set.seed() im Loop verwendet).
set.seed(123) 

# --- Globale Parameter für Datengenerierung ---
n <- 500 # Gesamtanzahl der Beobachtungen für simulierte Datensätze
large_data01_pool <- generate_data01(2000)


# --- Simulation der f-hat Variabilität ---
n_simulations <- 5 # Anzahl der verschiedenen f-hat Schätzungen, die wir plotten möchten

# Liste zum Speichern der Kontur-Geoms
boundary_layers <- list()

# Erstelle ein Gitter für die Vorhersagen der Entscheidungsgrenzen
# Die Bereiche basieren auf der gesamten data01_pool zur besseren Abdeckung
grid_df <- expand.grid(
  V1 = seq(min(large_data01_pool$V1) - 0.5, max(large_data01_pool$V1) + 0.5, length.out = 100),
  V2 = seq(min(large_data01_pool$V2) - 0.5, max(large_data01_pool$V2) + 0.5, length.out = 100)
)

cat("Starte Simulation zur Darstellung der f-hat Variabilität...\n")

for (i in 1:n_simulations) {
  # Ziehe eine neue ZUFÄLLIGE Trainingsstichprobe aus dem großen Pool
  # KEIN set.seed() hier, um tatsächliche Zufälligkeit zu gewährleisten
  train_idx <- sample(1:nrow(large_data01_pool), size = floor(nrow(large_data01_pool) * 0.3)) # z.B. 30% als Trainingsdaten
  sim_train_data <- large_data01_pool[train_idx, ]
  sim_train_data$class <- as.factor(sim_train_data$class)
  
  # Trainiere LDA auf den aktuellen Trainingsdaten
  lda_model_sim <- lda(class ~ V1 + V2, data = sim_train_data)
  
  # Vorhersage von Klassenzugehörigkeitswahrscheinlichkeiten auf dem Gitter
  pred_grid <- predict(lda_model_sim, newdata = grid_df, type = "response")
  grid_df$prob_B <- pred_grid$posterior[, "B"] # Wahrscheinlichkeit für Klasse B
  
  # Füge ein geom_contour Layer zur Liste hinzu
  # Hier wird die Konturlinie bei 0.5 Wahrscheinlichkeit geplottet (Entscheidungsgrenze)
  boundary_layers[[i]] <- geom_contour(
    data = grid_df,
    aes(x = V1, y = V2, z = prob_B),
    breaks = 0.5,
    color = "black",
    linetype = "dashed",
    alpha = 0.5 # Transparenz, um viele Linien sichtbar zu machen
  )
  
  if (i %% 10 == 0) {
    cat(paste("  Simulation", i, "abgeschlossen.\n"))
  }
}

# --- Plotten der f-hat Variabilität ---
cat("\nErstelle Plot der f-hat Variabilität...\n")

# Basis-Scatter-Plot der Originaldaten
# Wir verwenden hier eine kleine Stichprobe der originalen data01, um nicht zu viele Punkte zu haben
base_plot <- ggplot(sample_n(large_data01_pool, 500), aes(x = V1, y = V2, color = class)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "V1",
    y = "V2"
  ) +
  theme_minimal() +
  coord_fixed(
    xlim = range(large_data01_pool$V1) * 1.1,
    ylim = range(large_data01_pool$V2) * 1.1
  )


final_plot <- base_plot + boundary_layers
print(final_plot)


