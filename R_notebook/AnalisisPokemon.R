###############################################################################
#### 1. CARGA DE DATOS ########################################################
###############################################################################
m1 <- read.csv("pokemon_limpio.csv", header = TRUE)
names(m1)
str(m1)

unique(m1$type)
unique(m1$generation)

###############################################################################
#### 2. INSTALACIÓN DE PAQUETES ###############################################
###############################################################################
install.packages("dplyr")
install.packages("moments")
install.packages("ggplot2")
install.packages("MASS")
install.packages("AICcmodavg")
install.packages("broom")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("RColorBrewer")
install.packages("factoextra")
install.packages("qqplotr")



###############################################################################
#### 3. CARGA DE LIBRERÍAS ####################################################
###############################################################################

library(dplyr)
library(moments)
library(ggplot2)
library(MASS)
library(AICcmodavg)
library(broom)
library(ggpubr)
library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(ggpubr)
library(qqplotr)

###############################################################################
#### 4. CONTEO Y AGRUPAMIENTOS ################################################
###############################################################################
# Número total de registros
nrow(m1)

# Conteo por tipo
m1 %>% group_by(type) %>% summarise(n = n())

# Conteo por generación
m1 %>% group_by(generation) %>% summarise(n = n())



###############################################################################
#### 5. CORRELACIONES Y CORRELOGRAMAS #########################################
###############################################################################
# Prepara matriz con cuantitativas
m2 <- m1[, c("speed","hp","defense","attack")]

# Matriz de correlación
M <- cor(m2, method = "spearman", use = "pairwise.complete.obs")
round(M, 2)

# Función para p-values de cor.test
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(m2, method = "spearman", use = "pairwise.complete.obs")

# Correlograma básico
corrplot(M, type = "upper", order = "original",
         p.mat = p.mat, sig.level = 0.001, insig = "blank")

# Correlograma jerárquico con rectángulos
corrplot(M, order = "hclust", addrect = 2)

# Métodos alternativos
corrplot(M, method = "number", order = "AOE")
corrplot(M, method = "color",  order = "alphabet")
corrplot(M, method = "shade")

##### Aquí concluimos que vamos a usar speed y hp por su correlacion tan no correlacionada
##### NOTA debemos hacer las correlaciones para cada tipo y para cada generacion


################################################
########### 6.- Clustering para tipos
################################################


# 1. Calcular estadísticas
stats <- m1 %>%
  group_by(type) %>%
  summarise(
    median = median(speed),
    iqr = IQR(speed),
    q25 = quantile(speed, 0.25),
    q75 = quantile(speed, 0.75),
    skewness = moments::skewness(speed)
  )

# 2. Escalar y aplicar PAM
scaled_stats <- scale(stats[, -1])
rownames(scaled_stats) <- stats$type
pam_result <- pam(scaled_stats, k = 5)

#agregamos los stats
stats$cluster <- pam_result$clustering
unique(stats$cluster)
stats_df <- stats


# 3. Visualizar
fviz_cluster(pam_result, data = scaled_stats, repel = TRUE)

# 4. Exportar asignaciones
write.csv(data.frame(type = stats$type, cluster = pam_result$clustering),
          "clusters_tipos.csv")

# Crea la columna directamente usando match()
m1$cluster <- stats_df$cluster[ match(m1$type, stats_df$type) ]
# Sólo las combinaciones únicas
print( unique( m1[ , c("type","cluster")] ) )

#### 
names(m1)

#conteo por tipo_cluster:
m1 %>% group_by(cluster) %>% summarise(n = n())


###############################################################################
#### 7. BOXPLOTS (CRUDO Y TRANSFORMADO) ########################################
###############################################################################

### Graficas por tipo de Pokémon
# Speed crudo por type
ggplot(m1, aes(x = reorder(type, speed, median), y = speed)) +
  geom_boxplot() +
  ggtitle("Boxplot – speed (crudo)")

ggplot(m1, aes(x = reorder(cluster, speed, median), y = speed)) +
  geom_boxplot() +
  ggtitle("Boxplot – speed (crudo)")

# Hp crudo por type
ggplot(m1, aes(x = reorder(type, hp, median), y = hp)) +
  geom_boxplot() +
  ggtitle("Boxplot – hp (crudo)")

ggplot(m1, aes(x = reorder(cluster, hp, median), y = hp)) +
  geom_boxplot() +
  ggtitle("Boxplot – hp (crudo)")


#### Gráficos por  generación

# Speed crudo por generation
ggplot(m1, aes(x = reorder(generation, speed, median), y = speed)) +
  geom_boxplot() +
  ggtitle("Boxplot – speed (crudo)")

# Hp crudo por generation
ggplot(m1, aes(x = reorder(generation, hp, median), y = hp)) +
  geom_boxplot() +
  ggtitle("Boxplot – hp (crudo)")


###############################################################################
#### 8. ESTADÍSTICA DESCRIPTIVA POR GRUPO ######################################
###############################################################################
descrip <- function(x) {
  c(n = round(length(x),2),
    min = round(min(x),2),
    max = round(max(x),2),
    mean = round(mean(x),2),
    median = round(median(x),2),
    sd = round(sd(x),2),
    skewness = round(skewness(x),2))
}


##### aqui va la estadistica de speed y hp de todo el csv pero tambien pondremos
##### la prueba de normalizacion por logaritmo y ver que no son normales para poder descartarlo
descrip(m1$speed)

descrip(m1$hp)

####### aqui normalizamos para ver que falla

# Speed
m1$speed_log <- log(m1$speed + 1)

ggqqplot(m1$speed_log)
shapiro.test(m1$speed_log)

#Shapiro-Wilk normality test
#data:  m1$speed_log
#W = 0.95601, p-value < 2.2e-16

# Hp
m1$hp_log <- log(m1$hp + 1)
ggqqplot(m1$hp_log)
shapiro.test(m1$hp_log)

#Shapiro-Wilk normality test
#data:  m1$hp_log
#W = 0.94708, p-value < 2.2e-16

######

# Descriptiva por type
# descriptivos de speed por cluster
aggregate(m1$speed, by  = list(Cluster = m1$cluster), FUN = descrip)

# descriptivos de hp por cluster
aggregate(m1$hp, by  = list(Cluster = m1$cluster), FUN = descrip)

# Descriptiva por generation
aggregate(m1$speed, by = list(m1$generation), descrip)
aggregate(m1$hp,    by = list(m1$generation), descrip)


###############################################################################
#### 9. PRUEBAS DE NORMALIDAD POR GRUPO ########################################
###############################################################################
#––– Por TYPE –––#
# KS + Shapiro para speed
ks_speed_by_type <- m1 %>%
  group_by(cluster) %>%
  summarise(
    D       = ks.test(speed, pnorm, mean(speed), sd(speed))$statistic,
    p_value = ks.test(speed, pnorm, mean(speed), sd(speed))$p.value
  )
print(ks_speed_by_type)

sw_speed_by_type <- m1 %>%
  group_by(cluster) %>%
  summarise(
    W       = shapiro.test(speed)$statistic,
    p_value = shapiro.test(speed)$p.value
  )
print(sw_speed_by_type)

# KS + Shapiro para hp
ks_hp_by_type <- m1 %>%
  group_by(cluster) %>%
  summarise(
    D       = ks.test(hp, pnorm, mean(hp), sd(hp))$statistic,
    p_value = ks.test(hp, pnorm, mean(hp), sd(hp))$p.value
  )
print(ks_hp_by_type)

sw_hp_by_type <- m1 %>%
  group_by(cluster) %>%
  summarise(
    W       = shapiro.test(hp)$statistic,
    p_value = shapiro.test(hp)$p.value
  )
print(sw_hp_by_type)

### como los datoss no son normales
### pues ya vamos a usar los que queramos que sabemos que son normales
### y que ademas tenga algun sentido dentro de la logica de el juego
### asi pues usaremos electric, ground, fighting,fire,ice
## ahora viene la grafica de caja y bigotes de las 5:

# 1. Define los tipos que quieres
selected_types <- c("Electric", "Ground", "Fighting", "Fire", "Ice")

# 2. Calcula el orden de niveles por mediana ascendente de speed
ordered_levels <- m1 %>%
  filter(type %in% selected_types) %>%
  group_by(type) %>%
  summarise(med = median(speed, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(type)

# 3. Filtra y convierte 'type' en factor con ese orden
df_sel <- m1 %>%
  filter(type %in% selected_types) %>%
  mutate(type = factor(type, levels = ordered_levels))

# 4. Boxplot ordenado
ggplot(df_sel, aes(x = type, y = speed)) +
  geom_boxplot() +
  labs(
    title = "Boxplot de Speed por Tipo (Electric → Ice)",
    x     = "Tipo",
    y     = "Speed"
  ) +
  theme_minimal()

# 5. Q-Q plots con banda de confianza
ggplot(df_sel, aes(sample = speed)) +
  stat_qq_band(conf = 0.95, fill = "lightgray", alpha = 0.5) +
  stat_qq_line(color = "purple", size = 1) +
  stat_qq_point(size = 1.5) +
  facet_wrap(~ type, scales = "fixed") +
  labs(
    title = "Q-Q Plot Speed",
    x     = "Cuantiles Teóricos N(0,1)",
    y     = "Cuantiles Muestrales"
  ) +
  theme_minimal()
####### ahor para hp:

# 4. Boxplot ordenado
ggplot(df_sel, aes(x = type, y = hp)) +
  geom_boxplot() +
  labs(
    title = "Boxplot de hp por Tipo (Electric → Ice)",
    x     = "Tipo",
    y     = "hp"
  ) +
  theme_minimal()

# 5. Q-Q plots con banda de confianza
ggplot(df_sel, aes(sample = hp)) +
  stat_qq_band(conf = 0.95, fill = "lightgray", alpha = 0.5) +
  stat_qq_line(color = "purple", size = 1) +
  stat_qq_point(size = 1.5) +
  facet_wrap(~ type, scales = "fixed") +
  labs(
    title = "Q-Q Plot hp",
    x     = "Cuantiles Teóricos N(0,1)",
    y     = "Cuantiles Muestrales"
  ) +
  theme_minimal()

#Q-Q plot genreation:

m1$generation <- factor(m1$generation, levels = sort(unique(m1$generation)))


#### 5. Q-Q plots de hp con banda de confianza por generación ####
ggplot(m1, aes(sample = hp)) +
  stat_qq_band(conf = 0.95, fill = "lightgray", alpha = 0.5) +
  stat_qq_line(color = "purple", size = 1) +
  stat_qq_point(size = 1.5) +
  facet_wrap(~ generation, scales = "fixed") +
  labs(
    title = "Q-Q Plot de HP por Generación",
    x     = "Cuantiles Teóricos N(0,1)",
    y     = "Cuantiles Muestrales"
  ) +
  theme_minimal()
#### 5. Q-Q plots de speed con banda de confianza por generación ####
ggplot(m1, aes(sample = speed)) +
  stat_qq_band(conf = 0.95, fill = "lightgray", alpha = 0.5) +
  stat_qq_line(color = "purple", size = 1) +
  stat_qq_point(size = 1.5) +
  facet_wrap(~ generation, scales = "fixed") +
  labs(
    title = "Q-Q Plot de speed por Generación",
    x     = "Cuantiles Teóricos N(0,1)",
    y     = "Cuantiles Muestrales"
  ) +
  theme_minimal()



###############
# Estadisticas particulares
##################
tipos_fijos <- c("Fire", "Electric", "Ice", "Fighting", "Ground")
m1_sub     <- m1 %>% filter(type %in% tipos_fijos)

# Descriptiva directamente sobre el subset:
aggregate(m1_sub$speed,
          by = list(Tipo = m1_sub$type),
          descrip)
aggregate(m1_sub$hp,
          by = list(Tipo = m1_sub$type),
          descrip)

###############



#––– Por GENERATION –––#
# KS + Shapiro para speed
ks_speed_by_gen <- m1 %>%
  group_by(generation) %>%
  summarise(
    D       = ks.test(speed, pnorm, mean(speed), sd(speed))$statistic,
    p_value = ks.test(speed, pnorm, mean(speed), sd(speed))$p.value
  )
print(ks_speed_by_gen)

sw_speed_by_gen <- m1 %>%
  group_by(generation) %>%
  summarise(
    W       = shapiro.test(speed)$statistic,
    p_value = shapiro.test(speed)$p.value
  )
print(sw_speed_by_gen)

# KS + Shapiro para hp
ks_hp_by_gen <- m1 %>%
  group_by(generation) %>%
  summarise(
    D       = ks.test(hp, pnorm, mean(hp), sd(hp))$statistic,
    p_value = ks.test(hp, pnorm, mean(hp), sd(hp))$p.value
  )
print(ks_hp_by_gen)

sw_hp_by_gen <- m1 %>%
  group_by(generation) %>%
  summarise(
    W       = shapiro.test(hp)$statistic,
    p_value = shapiro.test(hp)$p.value
  )
print(sw_hp_by_gen)


###############################################################################
#### 9. TRANSFORMACIONES BOX-COX ##############################################
###############################################################################
# Optimiza λ para speed
bc_speed <- boxcox(lm(speed ~ 1, data = m1))
lambda_speed <- bc_speed$x[which.max(bc_speed$y)]
lambda_speed
m1$speed_bc <- (m1$speed^lambda_speed - 1) / lambda_speed

# Optimiza λ para hp
bc_hp <- boxcox(lm(hp ~ 1, data = m1))
lambda_hp <- bc_hp$x[which.max(bc_hp$y)]
lambda_hp
m1$hp_bc <- (m1$hp^lambda_hp - 1) / lambda_hp

# Q-Q y Shapiro sobre Box-Cox
qqnorm(m1$speed_bc); qqline(m1$speed_bc, col = "darkgreen", lwd = 2)
shapiro.test(m1$speed_bc)

qqnorm(m1$hp_bc); qqline(m1$hp_bc, col = "darkgreen", lwd = 2)
shapiro.test(m1$hp_bc)



###############################################################################
#### 11. PRUEBAS PARAMÉTRICAS DE VARIANZAS Y MEDIAS ############################
###############################################################################
# Creación de dos grupos de speed usando generation como ejemplo
speed_g1 <- m1[m1$generation %in% c(1,2,3), "speed"]
speed_g2 <- m1[m1$generation %in% c(4,5),   "speed"]

# Test de varianzas
var(speed_g1); var(speed_g2)
var.test(speed_g1, speed_g2, alternative = "two.sided", conf.level = 0.95)
boxplot(speed_g1, speed_g2, names = c("Gen 1-3","Gen 4-5"))

# T-test (igualdad de medias)
t.test(speed_g1, speed_g2, var.equal = TRUE, alternative = "two.sided")

# Repite para hp
hp_g1 <- m1[m1$generation %in% c(1,2,3), "hp"]
hp_g2 <- m1[m1$generation %in% c(4,5),   "hp"]
var.test(hp_g1, hp_g2)
boxplot(hp_g1, hp_g2, names = c("Gen 1-3","Gen 4-5"))
t.test(hp_g1, hp_g2, var.equal = TRUE)

###############################################################################
#### 12. ANÁLISIS DE VARIANZA (ANOVA) ##########################################
###############################################################################
# Speed ~ type
anova_speed_type <- aov(speed ~ type, data = m1)
summary(anova_speed_type)
plot(anova_speed_type)


# Speed ~ generation
anova_speed_gen <- aov(speed ~ generation, data = m1)
summary(anova_speed_gen)
plot(anova_speed_gen)

# Hp ~ type + generation + interacción
anova_hp_full <- aov(hp ~ type * generation, data = m1)
summary(anova_hp_full)
plot(anova_hp_full)

# Post-hoc Tukey
tuk_speed <- TukeyHSD(anova_speed_type)
tuk_speed
plot(tuk_speed, las = 1)

#########anovas"##################

# 1. Definir los tipos y generaciones de interés
sel_types <- c("Ground", "Ice", "Fighting", "Electric", "Fire")
sel_gens  <- c(8, 2, 4, 6)

# 2. Filtrar el data.frame original
m1_sel <- subset(m1,
                 type       %in% sel_types &
                   generation %in% sel_gens)

# 3. Asegurar que sean factores con el orden deseado
m1_sel$type       <- factor(m1_sel$type,       levels = sel_types)
m1_sel$generation <- factor(m1_sel$generation, levels = as.character(sel_gens))

# 4. Ajustar el ANOVA con interacción sobre el subset
anova_hp_sel <- aov(hp ~ type , data = m1_sel)

# 5. Calcular las comparaciones de Tukey
tuk_hp_sel   <- TukeyHSD(anova_hp_sel)

# 6. Graficar solo las comparaciones para este subset
plot(tuk_hp_sel, las = 1)


#########################################
######### Análisis Bivariado ############
#########################################

#### a)
#pares significativos:
# 1. Encuentra índices de p < 0.01 en la parte superior de la matriz
sig_idx <- which(p.mat < 0.01 & upper.tri(p.mat), arr.ind = TRUE)

# 2. Convierte a un data.frame legible
pares_signif <- data.frame(
  var1   = colnames(p.mat)[ sig_idx[, "col"] ],
  var2   = rownames(p.mat)[ sig_idx[, "row"] ],
  p_val  = p.mat[ sig_idx ]
)

print(pares_signif)


##### b)
#centroide
centroide <- colMeans(m2)        # m2 = df[, c("speed","hp","defense","attack")]
print(centroide)

#covarianza muestral
cov_mat <- cov(m2)               # covarianza entre todas las cuantitativas
print(cov_mat)
# o solamente entre speed y hp:
cov_speed_hp <- cov(m2$speed, m2$hp)
print(cov_speed_hp)

#coeficiente de pearson
pearson_speed_hp <- cor(m2$speed, m2$hp, method = "pearson")
print(pearson_speed_hp)

#### c) 
#regrecion lineal y significancia
fit <- lm(speed ~ hp, data = m1)
summary(fit)

coef(fit)  # c("(Intercept)" = a, "hp" = b)


###############################################################################
#### FIN DEL SCRIPT ###########################################################
###############################################################################

