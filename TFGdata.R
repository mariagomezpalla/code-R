library(ggplot2)
library(ggsignif)
library(dplyr)
#THROUGHPUT
TP_as <- c(1.9514, 1.5057, 2.0694, 1.2588, 1.682, 1.7041, 1.6456, 2.4476,
           1.674, 1.5285, 1.7388, 1.5792,1.3375,1.0342,1.7147,1.4645,1.3556,
           1.8108,1.1611,0.3538,1.6652, 1.7941,1.1873,1.8945,1.8945,1.794,
           1.5033,1.7261,1.8912, 1.8592)
TP_exo <- c(1.6475, 1.4932, 1.9262, 1.4941,1.7819, 1.4606, 1.4662, 2.1931,
            1.7445, 1.621, 1.2426, 1.6011, 1.5673, 1.1271, 1.801, 1.2769,1.0576,
            1.6908, 1.8272, 0.2796, 1.9351, 1.9006, 1.3471, 1.7545, 1.3148, 1.6736,
            1.8094, 1.6913, 1.9539, 2.0649)
TP_opo <- c(1.8905, 1.2019, 2.0426, 1.4881, 1.7245, 1.3741, 1.4499, 2.1118, 1.9516,
            1.569, 1.4112, 1.4343, 1.5578, 1.4011, 1.6755, 1.3428, 1.4736, 1.7083,
            1.2997, 0.3589, 1.7171, 1.6493, 2.0539, 1.7766, 1.4992, 1.7287, 1.6852,
            1.5163, 2.178, 1.5646)
datos <- data.frame(
  TP = c(TP_as, TP_exo, TP_opo),
  Grupo = rep(c("TP_as", "TP_exo", "TP_opo"), each = length(TP_as))
)


tp_color <- "skyblue"       
error_color <- "lightcoral" 
boxplot_theme <- list(      
  col = tp_color,
  ylab = "bits/s"
)
boxplot(TP_as, TP_exo, TP_opo,
        names = c("TP_as", "TP_exo", "TP_opo"),
        col = tp_color,
        main = "Boxplot of Throughput",
        ylab = "bits/s")



library(ez)

resultado <- ezANOVA(
  data = datos_repetidos,
  dv = TP,           
  wid = sujeto,     
  within = grupo     
)

tukey <- TukeyHSD(modelo_aov$lm)  


print(tukey)

plot(tukey)
#ERROR

library(ez)
error_as <- c(0.3, 0.4, 0.25, 0.4, 0.3, 0.3, 0.425, 0.275, 0.45, 0.475, 0.45, 0.225,
              0.575, 0.35, 0.325, 0.25, 0.45, 0.425, 0.525, 0.6, 0.4, 0.45, 0.375, 0.3,
              0.3, 0.375,0.325,0.45,0.325, 0.375)
error_exo <- c(0.35,0.35,0.4,0.425,0.175,0.4,0.35,0.25,0.425,0.425,0.625,0.35,0.35,
               0.4,0.4,0.275,0.45,0.475,0.3,0.875,0.275,0.375,0.325,0.325,0.3,0.35,0.22,
               0.475,0.225,0.275)
error_opo <- c(0.375,0.425,0.325,0.4,0.2,0.375,0.5,0.475,0.35,0.5,0.9,0.4,0.45,0.275,
               0.325,0.35,0.375,0.35,0.525,0.85,0.4,0.475,0.225,0.4,0.4,0.35,0.25,0.55
               ,0.3,0.45)

boxplot(error_as, error_exo, error_opo,
        names = c("Error_as", "Error_exo", "Error_opo"),
        col = error_color,
        main = "Boxplot of Errors",
        ylab = "error values ranged from 0-1")


data <- data.frame(subject = subject,
                   condition = condition,
                   error = c(error_as, error_exo, error_opo))


anova_result <- ezANOVA(data = data,
                        dv = .(error),   
                        wid = .(subject),  
                        within = .(condition),  
                        detailed = TRUE)


print(anova_result)


#TP_MAN
TP_as_m <- c(1.7041, 1.6456, 1.5285,1.7388, 1.3556,1.8108, 0.3538, 1.6652, 1.7941,
             1.1873, 1.8945, 1.8945, 1.794, 1.5033, 1.7261)
TP_exo_m <- c(1.4606, 1.4662, 1.621, 1.2426, 1.0576, 1.6908, 0.2796, 1.9351, 1.9006, 
              1.3471, 1.7545, 1.3148, 1.6736, 1.8094, 1.6913)
TP_opo_m <- c(1.3741, 1.4499, 1.569, 1.4112, 1.4736, 1.7083,0.3589, 1.7171, 1.6493,
              2.0539, 1.7766, 1.4992, 1.7287, 1.6852, 1.5163)
boxplot(TP_as_m, TP_exo_m, TP_opo_m,
        names = c("TP_as_m", "TP_exo_m", "TP_opo_m"),
        col = tp_color,
        main = "Boxplot of Throughput (m)",
        ylab = "bits/s")
library(car)
library(ez)


data <- data.frame(
  id = rep(1:15, times = 3), 
  condition = factor(rep(c("TP_as_m", "TP_exo_m", "TP_opo_m"), each = 15)),
  values = c(TP_as_m, TP_exo_m, TP_opo_m) 
)

anova_result <- ezANOVA(
  data = data,
  dv = .(values),
  wid = .(id),
  within = .(condition),
  detailed = TRUE
)


print(anova_result$Mauchly)


print(anova_result)

#TP_WOMAN
TP_as_w <- c(1.9514, 1.5057, 2.0694, 1.2588, 1.682, 2.4476, 1.674, 1.5792, 1.3375,
             1.0342, 1.7147, 1.4645,1.1611, 1.8912,1.8592)
TP_exo_w <- c(1.6475, 1.4932, 1.9262, 1.4941, 1.7819, 2.1931, 1.7445, 1.6011, 1.5673,
              1.1271, 1.801, 1.2769, 1.8272, 1.9539, 2.0649)
TP_opo_w <- c(1.8905, 1.2019, 2.0426, 1.4881, 1.7245, 2.1118, 1.9516, 1.4343,
              1.5578, 1.4011, 1.6755, 1.3428, 1.299, 2.178, 1.5646)

boxplot(TP_as_w, TP_exo_w, TP_opo_w,
        names = c("TP_as_f", "TP_exo_f", "TP_opo_f"),  
        col = tp_color,
        main = "Boxplot of Throughput (m)",
        ylab = "bits/s")

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
library(ez)
library(car)


data_w <- data.frame(
  id = rep(1:15, times = 3), 
  condition = factor(rep(c("TP_as_w", "TP_exo_w", "TP_opo_w"), each = 15)), 
  values = c(TP_as_w, TP_exo_w, TP_opo_w) 
)


anova_result_w <- ezANOVA(
  data = data_w,
  dv = .(values),
  wid = .(id),
  within = .(condition),
  detailed = TRUE
)


print(anova_result_w$ANOVA)

print(anova_result_w$Mauchly)

print(anova_result_w$`Sphericity Corrections`)

#ERROR_M
error_as_m <- c(0.3, 0.425, 0.475, 0.45, 0.45, 0.425, 0.6, 0.4, 0.45, 0.375, 0.3, 
                0.3, 0.375,0.325,0.45)
error_exo_m <- c(0.4, 0.35, 0.425, 0.625, 0.45, 0.475, 0.875,  0.275, 0.375, 0.325,  
                 0.325, 0.3, 0.35, 0.225, 0.475)
error_opo_m <- c(0.375, 0.5, 0.5, 0.9, 0.375, 0.35, 0.85, 0.4, 0.475, 0.225, 0.4,
                 0.4, 0.35, 0.25, 0.55)
boxplot(error_as_m, error_exo_m, error_opo_m,
        names = c("Error_as_m", "Error_exo_m", "Error_opo_m"),
        col = error_color,
        main = "Boxplot of Errors (m)",
        ylab = "error values ranged from 0-1")


grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
library(ez)
library(car)


data_error <- data.frame(
  id = rep(1:15, times = 3), 
  condition = factor(rep(c("Error_as_m", "Error_exo_m", "Error_opo_m"), each = 15)), 
  values = c(error_as_m, error_exo_m, error_opo_m) 
)


anova_result_error <- ezANOVA(
  data = data_error,
  dv = .(values),
  wid = .(id),
  within = .(condition),
  detailed = TRUE
)

print(anova_result_error$ANOVA)

print(anova_result_error$Mauchly)

print(anova_result_error$`Sphericity Corrections`)

#ERROR WOMAN
error_as_w <- c(0.3, 0.4, 0.25, 0.4, 0.3, 0.275, 0.45, 0.225, 0.575, 0.35, 0.325,
                0.25, 0.525, 0.325, 0.375)
error_exo_w <- c(0.35, 0.35, 0.4, 0.425, 0.175, 0.25, 0.425, 0.35, 0.35, 0.4, 0.4,
                 0.275, 0.3, 0.225, 0.275)
error_opo_w <- c(0.375, 0.425, 0.325, 0.4, 0.2, 0.475, 0.35, 0.4, 0.45, 0.275,
                 0.325, 0.35, 0.525, 0.3, 0.45)

boxplot(error_as_w, error_exo_w, error_opo_w,
        names = c("Error_as_f", "Error_exo_f", "Error_opo_f"),  # Cambiado de "_w" a "_m"
        col = error_color,
        main = "Boxplot of Errors (m)",
        ylab = "error values ranged from 0-1")

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
length(error_as_w)  # Debe devolver 15
length(error_exo_w)  # Debe devolver 15
length(error_opo_w)  # Debe devolver 15


data_error_w <- data.frame(
  id = rep(1:15, times = 3), 
  condition = factor(rep(c("Error_as_w", "Error_exo_w", "Error_opo_w"), each = 15)), 
  values = c(error_as_w, error_exo_w, error_opo_w) 
)


library(ez)
library(car)

anova_result_error_w <- ezANOVA(
  data = data_error_w,
  dv = .(values),
  wid = .(id),
  within = .(condition),
  detailed = TRUE
)

print(anova_result_error_w$ANOVA)

print(anova_result_error_w$Mauchly)

print(anova_result_error_w$`Sphericity Corrections`)

