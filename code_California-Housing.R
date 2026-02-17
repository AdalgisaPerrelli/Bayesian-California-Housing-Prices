#### PROGETTO BAYESIAN STATISTICAL MODELING
## Malvezzi Beatrice, Perrelli Adalgisa, Reina Francesca e Santero Nicole

# Librerie ----------------------------------------------------------------
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(maps)
library(ggcorrplot)
library(tidyr)
library(skimr)
library(caret)
library(randomForest)
library(rstan)
library(bayesplot)
library(mvtnorm)
library(truncnorm)
library(coda)
library(BayesLogit)
library(BoomSpikeSlab)
library(patchwork)


# Pre-processing ----------------------------------------------------------
data <- read.csv("housing.csv")

sapply(data, function(x) any(is.na(x)))

data1 <- na.omit(data)

# Creazione della mappa per rappresentare la variazione del costo mediano delle abitazioni rispetto alla vicinanza alla costa:
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california")
others     <- states %>% filter(ID != "california")
world <- st_as_sf(map("world", plot = FALSE, fill = TRUE))

ggplot() +
  geom_sf(data = world, fill = "lightblue", color = NA) +
  geom_sf(data = others, fill = "gray90", color = "gray70") +
  geom_sf(data = california, fill = "antiquewhite", color = "gray40") +
  geom_point(
    data = data1,
    aes(
      x = longitude,
      y = latitude,
      size = population,
      color = median_house_value
    ),
    alpha = 0.8
  ) +
  scale_color_gradientn(
    colors = c( "darkblue","blue", "cyan", "green", "yellow", "orange", "red", "darkred"),
    values = c(0, 0.15, 0.3, 0.5, 0.65, 0.8, 1),
    name = "Median house value (USD)",
    limits = c(14999, 500001),
    breaks = seq(0, 500000, 100000),
    guide = guide_colorbar(
      barwidth = 1.5,
      barheight = 12
    )
  ) +
  
  scale_size(range = c(0.3, 6),
             guide = "none") + 
  
  scale_size_continuous(
    name = "Population",
    range = c(0.3, 6)
  ) +
  
  coord_sf(
    xlim = c(-125, -113),
    ylim = c(32.3 , 42.5),
    expand = FALSE
  ) +
  
  ggtitle("Mappa California: popolazione e costo mediano delle abitazioni") +
  xlab("Longitude") +
  ylab("Latitude") +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA)
  )

# Analisi esplorative:
my_skim <- skim_with(base = sfl(), numeric = sfl(hist=NULL))
my_skim(data1)
table(data1$ocean_proximity)

data_num <- subset(data1, select = -ocean_proximity)
## Porto il dataset in formato "long":
data_long <- data_num %>% 
  pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore")

ggplot(data_long, aes(x = valore)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  facet_wrap(~ variabile, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuzione delle variabili")

ggplot(data_long, aes(x = valore)) +
  geom_density(fill = "green", alpha = 0.6) +
  facet_wrap(~ variabile, scales = "free") +
  theme_minimal() +
  labs(title = "Densità delle variabili")

data1$ocean_proximity <- factor(
  data1$ocean_proximity,
  levels = c("INLAND", "<1H OCEAN", "NEAR BAY", "NEAR OCEAN", "ISLAND")
)

colors_map <- c(
  "INLAND"     = "blue",
  "<1H OCEAN"  = "cyan",
  "NEAR BAY"   = "green",
  "NEAR OCEAN" = "yellow",
  "ISLAND"     = "orange"
)

ggplot(data1, aes(x = ocean_proximity, y = median_house_value, fill = ocean_proximity)) +
  geom_boxplot() +
  scale_fill_manual(values = colors_map) +
  labs(
    title = "Valore mediano delle abitazioni per vicinanza all'oceano",
    x = "Ocean Proximity",
    y = "Median House Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# Model selection tramite approccio Spike-and-slab:
dummies <- dummyVars(~ ocean_proximity, data = data)
df_onehot <- predict(dummies, newdata = data)
data_dummy <- cbind(data, df_onehot)
str(data_dummy)
data_dummy <- data_dummy[, -10]

model <- lm.spike(median_house_value ~ ., data = data_dummy, niter = 10000)
summary(model)

inclusion_probs <- summary(model)$coefficients[, "inc.prob"]
print(inclusion_probs)

# Accorpamento delle modalità di *ocean_proximity* e riduzione delle categorie da 5 a 3:
data <- data %>%
  mutate(ocean_proximity = case_when(
    ocean_proximity %in% c("ISLAND", "NEAR BAY", "NEAR OCEAN") ~ "ON THE COAST",
    TRUE ~ as.character(ocean_proximity)
  )) %>%
  mutate(ocean_proximity = factor(ocean_proximity))

table(data$ocean_proximity)

data1 <- na.omit(data)
table(data1$ocean_proximity)

# Verifica sulle correlazioni:
num_vars <- data1 %>%
  select(where(is.numeric))

cor_matrix <- cor(num_vars, use = "everything")
cor_matrix["median_house_value", ]

colors_map <- c("blue", "yellow", "red")

ggcorrplot(cor_matrix,
           hc.order = TRUE,        
           type = "lower",         
           lab = TRUE,             
           lab_size = 2.5,
           colors = colors_map,    
           outline.color = "white") +
  theme_minimal(base_size = 14) +
  labs(title = "Correlogramma",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Model selection tramite Random Forest:
set.seed(123)
cvCtrl <- trainControl(method = "cv", number = 10, search="grid", verboseIter = TRUE)
rfTune <- train(median_house_value ~ ., data = data1, method = "rf", tuneLength = 10, trControl = cvCtrl, ntree = 500)
rfTune

vimp = varImp(rfTune)

varImpData <- as.data.frame(varImp(rfTune)$importance)
varImpData$Variable <- rownames(varImpData)
ggplot(varImpData, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_hline(yintercept = 30, col = "red", linetype = "dashed", lwd = 1) +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme(axis.text.y = element_text(size = 12, color = "black"))

# Al termine della procedura di model selection decidiamo di eliminare la covariata *households*:
data_def <- data[, -7]

num_vars <- data_def %>%
  select(where(is.numeric))

cor_matrix <- cor(num_vars, use = "complete.obs")
cor_matrix["median_house_value", ]

colors_map <- c("blue", "yellow", "red")

ggcorrplot(cor_matrix,
           hc.order = TRUE,        
           type = "lower",         
           lab = TRUE,             
           lab_size = 2.5,
           colors = colors_map,    
           outline.color = "white") +
  theme_minimal(base_size = 14) +
  labs(title = "Correlogramma modello finale",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# e in seguito anche *total_bedrooms*:
data_def <- data_def[, -5]

# Esclusione degli outlier relativi a *total_rooms*:
boxplot(data_def$total_rooms)
out_rooms <- boxplot.stats(data_def$total_rooms)$out
which_rows_rooms <- which(data_def$total_rooms %in% c(out_rooms))
which_rows_rooms

which_rows_rooms[out_rooms>30000]

# Eliminiamo le righe che hanno total_rooms > 30000
data_def <- data_def[-c(6058, 9020, 9881, 10310, 12202, 12216, 13140), ]

# One-hot-encoding per *ocean_proximity*:
dummies <- dummyVars(~ ocean_proximity, data = data_def)
df_onehot <- predict(dummies, newdata = data_def)
head(df_onehot)
data_def <- cbind(data_def, df_onehot)
data_def <- data_def[, -8]

# Creazione della variabile *diag_coord*:
data_def$diag_coord <- data_def$longitude + data_def$latitude

num_vars <- data_def %>%
  select(where(is.numeric))

cor_matrix <- cor(num_vars, use = "everything")
cor_matrix["median_house_value", ]

colors_map <- c("blue", "yellow", "red")

ggcorrplot(cor_matrix,
           hc.order = TRUE,        
           type = "lower",         
           lab = TRUE,             
           lab_size = 2.5,
           colors = colors_map,    
           outline.color = "white") +
  theme_minimal(base_size = 14) +
  labs(title = "Correlogramma",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Trasformazione della variabile target su scala logaritmica e standardizzazione delle variabili quantitative:
data_def$median_house_value <- log(data_def$median_house_value)
hist(data_def$median_house_value)

preproc <- preProcess(data_def[, c(3, 4, 5, 6, 7, 11)], method = c("center", "scale"))
data_def <- predict(preproc, data_def)
summary(data_def)

# Verifica sulla distribuzione di *median_house_value*:
mu <- mean(data_def$median_house_value)
sigma <- sd(data_def$median_house_value)

hist(data_def$median_house_value, breaks=20, col="grey", freq=TRUE,
     main="Histogram of data_def$median_house_value",
     xlab="data_def$median_house_value")
x <- seq(min(data_def$median_house_value), max(data_def$median_house_value), length=100)
y_norm <- dnorm(x, mean=mu, sd=sigma) * length(data_def$median_house_value) * diff(hist(data_def$median_house_value, plot=FALSE)$breaks)[1]
lines(x, y_norm, col="blue", lwd=2)

qqnorm(data_def$median_house_value)
qqline(data_def$median_house_value, col="red")

# Modifica dei nomi associati alle categorie di *ocean_proximity*:
colnames(data_def)[colnames(data_def) == "ocean_proximity.<1H OCEAN"] <- "ocean_lt1h"
colnames(data_def)[colnames(data_def) == "ocean_proximity.INLAND"] <- "ocean_inland"
colnames(data_def)[colnames(data_def) == "ocean_proximity.ON THE COAST"] <- "on_the_coast"


set.seed(123)

# Suddivisione in train e test set:
n <- nrow(data_def)
train_indices <- sample(1:n, size = round(0.7 * n))
train_set <- data_def[train_indices, ]
test_set <- data_def[-train_indices, ]


# 1. Prior non informativa ---------------------------------------------------
vars <- c("housing_median_age", "total_rooms", "population", "median_income",
          "diag_coord", "ocean_lt1h", "ocean_inland", "on_the_coast")

ocean_cat <- factor(with(train_set, ifelse(ocean_lt1h == 1, "lt1h",
                                           ifelse(ocean_inland == 1, "inland", "oncoast"))))

dummies_ocean <- model.matrix(~ ocean_cat)[, -1]

numeric_vars <- c("housing_median_age", "total_rooms", "population",
                  "median_income", "diag_coord")

X_num <- as.matrix(train_set[, numeric_vars])
intercept <- rep(1, nrow(train_set))

X <- cbind(intercept, X_num, dummies_ocean)
cat("Dimensioni X:", dim(X), "\n")

check_linear_combinations <- function(X){
  combo_info <- findLinearCombos(X)
  if(!is.null(combo_info$remove)){
    cat("Variabili linearmente dipendenti trovate:\n")
    for(col_idx in combo_info$remove){
      col_name <- colnames(X)[col_idx]
      cat(paste0(" - ", col_name, " (colonna ", col_idx, ")\n"))
    }
  } else {
    cat("Nessuna collinearità perfetta trovata, matrice a rango pieno.\n")
  }
  return(combo_info)
}
combo_info <- check_linear_combinations(X)

if (!is.null(combo_info$remove)) {
  X_reduced <- X[, -combo_info$remove]
} else {
  X_reduced <- X
}

cat("Dimensioni X ridotta:", dim(X_reduced), "\n")
cat("Rango matrice X ridotta:", qr(X_reduced)$rank, "\n")


# Implementazione Gibbs
library(MASS)
library(reshape2)
gibbs_vague_prior <- function(y, X, niter){
  n <- nrow(X)
  p <- ncol(X)
  out_sigma2 <- numeric(niter)
  out_beta <- matrix(0, nrow = niter, ncol = p)
  
  beta_ML <- solve(t(X) %*% X) %*% t(X) %*% y
  Sigman <- solve(t(X) %*% X)
  
  an <- (n - p) / 2
  resid <- y - X %*% beta_ML
  bn <- 0.5 * t(resid) %*% resid
  
  for(i in 1:niter){
    out_sigma2[i] <- 1 / rgamma(1, shape = an, scale = 1 / bn)
    out_beta[i, ] <- mvrnorm(1, mu = beta_ML, Sigma = out_sigma2[i] * Sigman)
  }
  
  list(sigma2 = out_sigma2, beta = out_beta)
}

y <- train_set$median_house_value
results <- gibbs_vague_prior(y, X, 5000)


# Plot e diagnostiche
acf(results[[1]])
acf(results[[2]][,1])
acf(results[[2]][,2])
acf(results[[2]][,3])


hist(results[[1]], main="Istogramma sigma2", xlab="sigma2")

df_beta <- melt(results$beta)
colnames(df_beta) <- c("iteration", "parameter", "value")
df_beta$parameter <- factor(df_beta$parameter)

ggplot(df_beta) +
  geom_histogram(aes(x = value, fill = parameter), bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~parameter, scales = "free") +
  theme_bw() + ylab("Density")


summary(results[[1]])
summary(as.data.frame(results[[2]]))


# MCMC
beta_mcmc <- as.mcmc(results$beta)
sigma2_mcmc <- as.mcmc(results$sigma2)


summary(beta_mcmc)
summary(sigma2_mcmc)


beta_array <- array(NA, dim = c(nrow(beta_mcmc), 1, ncol(beta_mcmc)))
beta_array[, 1, ] <- beta_mcmc
param_names <- paste0("beta", 1:ncol(beta_mcmc))
dimnames(beta_array) <- list(
  NULL,
  NULL,
  param_names
)
mcmc_trace(beta_array, pars = param_names) +
  ggtitle("Traceplot beta") +
  theme_minimal()

plot(sigma2_mcmc, main = "Traceplot sigma2", col=2)


effectiveSize(beta_mcmc)
effectiveSize(sigma2_mcmc)


geweke.diag(beta_mcmc)
geweke.diag(sigma2_mcmc)




# Prediction sul test set
X_test_num <- as.matrix(test_set[, numeric_vars])
dummies_ocean_test <- model.matrix(~ factor(with(test_set, ifelse(ocean_lt1h == 1, "lt1h",
                                                                  ifelse(ocean_inland == 1, "inland", "oncoast")))))[, -1]

intercept_test <- rep(1, nrow(test_set))
X_test <- cbind(intercept_test, X_test_num, dummies_ocean_test)

n_iter <- length(results$sigma2)
n_test <- nrow(X_test)
y_pred_mat <- matrix(NA, nrow = n_iter, ncol = n_test)

for(i in 1:n_iter){
  beta_i <- results$beta[i, ]
  sigma2_i <- results$sigma2[i]
  mu_pred <- X_test %*% beta_i
  y_pred_mat[i, ] <- rnorm(n_test, mean= mu_pred, sd= sqrt(sigma2_i))
}

y_pred_mean <- apply(y_pred_mat, 2, mean)
y_pred_lower <- apply(y_pred_mat, 2, quantile, probs = 0.025)
y_pred_upper <- apply(y_pred_mat, 2, quantile, probs = 0.975)


rmse <- sqrt(mean((test_set$median_house_value - y_pred_mean)^2))
cat("RMSE :", rmse, "\n")
mae  <- mean(abs(test_set$median_house_value - y_pred_mean))
cat("MAE :", mae, "\n")
r2 <- 1 - sum((test_set$median_house_value - y_pred_mean)^2) / sum((test_set$median_house_value - mean(test_set$median_house_value))^2)
cat("R² :", r2, "\n")


abs_error <- abs(test_set$median_house_value - y_pred_mean)

df_pred <- data.frame(
  observed = test_set$median_house_value,
  predicted = y_pred_mean,
  lower = y_pred_lower,
  upper = y_pred_upper,
  abs_error = abs_error
)


ggplot(df_pred, aes(x = observed, y = predicted, color = abs_error)) +
  geom_point(alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(
    x = "Valori osservati",
    y = "Predizioni posteriori",
    color = "Errore ass."
  ) +
  theme_minimal()


# 2. Unit Information Prior --------------------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

vars <- c("housing_median_age", "total_rooms", "population", "median_income",
          "diag_coord", "ocean_lt1h", "ocean_inland", "on_the_coast")

ocean_cat <- factor(with(train_set, ifelse(ocean_lt1h == 1, "lt1h",
                                           ifelse(ocean_inland == 1, "inland", "oncoast"))))

dummies_ocean <- model.matrix(~ ocean_cat)[, -1]

numeric_vars <- c("housing_median_age", "total_rooms", "population",
                  "median_income", "diag_coord")

X_num <- as.matrix(train_set[, numeric_vars])
intercept <- rep(1, nrow(train_set))

X <- cbind(intercept, X_num, dummies_ocean)
y<-train_set$median_house_value

# OLS
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% y
resid <- y - X %*% beta_ols
s2_ols <- sum(resid^2) / (nrow(X) - ncol(X))

# Specificazione UIP
beta0 <- as.vector(beta_ols)
Sigma0 <- s2_ols * solve(t(X) %*% X)
v0 <- 5
a0 <- v0 / 2
b0 <- v0 * s2_ols / 2


data_list <- list(
  n = nrow(X),
  p = ncol(X),
  X = X,
  y = as.vector(y),
  beta0 = beta0,
  Sigma0 = Sigma0,
  a0 = a0,
  b0 = b0
)

fit <- stan(
  file = "regression_UIP.stan",
  data = data_list,
  chains = 4,
  iter = 20000,
  warmup = 10000,
  seed = 42,
  control = list(adapt_delta = 0.95)
)

print(fit, pars = c("beta", "sigma2"))

# Plot e diagnostiche
library(coda)
library(bayesplot)
library(posterior)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(gridExtra)


mcmc_list <- As.mcmc.list(fit)
posterior_samples <- as.data.frame(extract(fit))
draws <- as_draws_df(fit)    

posterior_arr <- as.array(fit)
beta_names_stan <- grep("^beta\\[", dimnames(posterior_arr)[[3]], value = TRUE)
beta_array <- posterior_arr[, , beta_names_stan]

beta_stan <- mcmc.list(
  lapply(1:dim(beta_array)[2], function(chain) {
    mcmc(beta_array[, chain, ])
  })
)
sigma2_stan <- as.mcmc(posterior_arr[,, "sigma2"])


summary(beta_stan)
summary(sigma2_stan)


geweke.diag(beta_stan)
geweke.diag(sigma2_stan)


effectiveSize(beta_stan)
effectiveSize(sigma2_stan)


autocorr.plot(mcmc_list, ask = FALSE)


df_sigma2 <- draws %>%
  dplyr::select(.iteration, .chain, sigma2) %>%
  mutate(iteration = as.integer(.iteration), chain = as.factor(.chain))

ggplot(df_sigma2, aes(x = iteration, y = sigma2, color = chain)) +
  geom_line(alpha = 0.8) +
  labs(title = "Traceplot sigma2",
       x = "Iterazione", y = expression(sigma^2)) +
  theme_minimal()


beta_names <- grep("^beta\\[", colnames(draws), value = TRUE)
df_beta_long <- draws %>%
  dplyr::select(.iteration, .chain, all_of(beta_names)) %>%
  pivot_longer(cols = all_of(beta_names),
               names_to = "parameter", values_to = "value") %>%
  mutate(iteration = as.integer(.iteration), chain = as.factor(.chain))

ggplot(df_beta_long, aes(x = iteration, y = value, color = chain)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~parameter, scales = "free_y") +
  labs(title = "Traceplot per parametri beta",
       x = "Iterazione", y = "Valore parametro") +
  theme_minimal()


ggplot(posterior_samples, aes(x = sigma2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", alpha = 0.7) +
  theme_bw() +
  labs(title = "Distribuzione a posteriori di sigma2", x = expression(sigma^2), y = "Densità")


beta_names <- grep("^beta\\.", colnames(posterior_samples), value = TRUE)
df_beta_hist <- melt(posterior_samples[, beta_names], variable.name = "parameter", value.name = "value")
df_beta_hist$iteration <- rep(1:nrow(posterior_samples), times = length(beta_names))

ggplot(df_beta_hist) +
  geom_histogram(aes(x = value, fill = parameter), bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~parameter, scales = "free") +
  theme_bw() +
  labs(title = "Distribuzione a posteriori dei parametri beta")




# Prediction sul test set
X_test_num <- as.matrix(test_set[, numeric_vars])
dummies_ocean_test <- model.matrix(~ factor(with(test_set, ifelse(ocean_lt1h == 1, "lt1h",
                                                                  ifelse(ocean_inland == 1, "inland", "oncoast")))))[, -1]

intercept_test <- rep(1, nrow(test_set))
X_test <- cbind(intercept_test, X_test_num, dummies_ocean_test)

beta_cols <- grep("^beta\\.", colnames(posterior_samples), value = TRUE)
sigma2_vec <- posterior_samples$sigma2
n_iter <- nrow(posterior_samples)
n_test <- nrow(X_test)

y_pred_mat_1 <- matrix(NA, nrow = n_iter, ncol = n_test)
for(i in 1:n_iter){
  beta_i <- as.numeric(posterior_samples[i, beta_cols])
  mu_pred_1 <- X_test %*% beta_i
  y_pred_mat_1[i, ] <- rnorm(n_test, mean = as.numeric(mu_pred_1), sd = sqrt(sigma2_vec[i]))
}

y_pred_mean_1 <- colMeans(y_pred_mat_1)
y_pred_lower_1 <- apply(y_pred_mat_1, 2, quantile, probs = 0.025)
y_pred_upper_1 <- apply(y_pred_mat_1, 2, quantile, probs = 0.975)


rmse <- sqrt(mean((test_set$median_house_value - y_pred_mean_1)^2))
cat("RMSE:", rmse, "\n")
mae <- mean(abs(test_set$median_house_value - y_pred_mean_1))
cat("MAE:", mae, "\n")
r2 <- 1 - sum((test_set$median_house_value - y_pred_mean_1)^2) / sum((test_set$median_house_value - mean(test_set$median_house_value))^2)
cat("R² :", r2, "\n")


abs_error <- abs(test_set$median_house_value - y_pred_mean_1)

df_pred <- data.frame(
  observed = test_set$median_house_value,
  predicted = y_pred_mean_1,
  lower = y_pred_lower_1,
  upper = y_pred_upper_1,
  abs_error = abs_error
)


ggplot(df_pred, aes(x = observed, y = predicted, color = abs_error)) +
  geom_point(alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(
    x = "Valori osservati",
    y = "Predizioni posteriori",
    color = "Errore ass."
  ) +
  theme_minimal()


# 3. Prior poco informativa con shrinkage ---------------------------------
#scelta a0 e b0
y_var <- var(train_set$median_house_value)  
m <- y_var          
v <- 0.1            

alpha <- 2 + (m^2 / v)
beta  <- m * (alpha - 1)

cat("Valori per la prior Inv-Gamma:\n")
cat("a0 (alpha) =", round(alpha, 3), "\n")
cat("b0 (beta)  =", round(beta, 3), "\n")

X_train <- model.matrix(
  median_house_value ~ diag_coord + housing_median_age +
    total_rooms + population + median_income +
    ocean_lt1h + ocean_inland,  
  data = train_set
)
y_train <- train_set$median_house_value

stan_data <- list(
  n = nrow(X_train),
  p = ncol(X_train),
  y = y_train,
  X = X_train,
  beta0 = rep(0, ncol(X_train)),
  a0 = 12.0,
  b0 = 11.0
)

fit_stan <- stan(
  file   = "gaussian_project.stan",
  data   = stan_data,
  chains = 4,
  iter   = 5000,
  warmup = 3000,
  seed   = 42,
  control = list(adapt_delta = 0.995, max_treedepth = 15)
)

print(fit_stan, pars = c("beta", "sigma2", "lambda"),
      probs = c(0.025, 0.5, 0.975))


#distribuzioni a posteriori beta 
posterior_arr <- as.data.frame(fit_stan)
beta_samples <- posterior_arr[, grep("^beta\\[", names(posterior_arr))]
beta_long <- beta_samples |>
  pivot_longer(cols = everything(),
               names_to = "Parametro",
               values_to = "Valore")
ggplot(beta_long) +
  geom_histogram( aes(x = Valore, fill = Parametro), bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~Parametro, scales = "free", ncol = 3) +
  labs(title = "Distribuzioni posteriori delle beta",
       x = "Valore campionato",
       y = "Frequenza")

ggplot(beta_long) +
  geom_histogram( aes(x = Valore, fill = Parametro), bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~Parametro, scales = "free", ncol = 3) +
  labs(x = "Valore campionato",
       y = "Frequenza")

#Traceplot beta
posterior <- as.array(fit_stan)
mcmc_trace(posterior,
           regex_pars = "^beta\\[") +
  ggtitle("Traceplot per i coefficienti beta")


# Traceplot + densità per sigma2
p_sigma2 <- mcmc_trace(posterior, pars = "sigma2") +
  theme_minimal()
d_sigma2 <- mcmc_dens_overlay(posterior, pars = "sigma2") +
  theme_minimal()
p_sigma2 | d_sigma2 

# Traceplot + densità per lambda
p_lambda <- mcmc_trace(posterior, pars = "lambda") +
  theme_minimal()
d_lambda <- mcmc_dens_overlay(posterior, pars = "lambda") +
  theme_minimal()
p_lambda | d_lambda  

#Autocorrelazione 
# n_betas <- sum(grepl("^beta\\[", dimnames(posterior)$parameters))
# sel_betas <- paste0("beta[", 1: n_betas, "]")
# mcmc_acf(posterior, pars = sel_betas) +
#   ggtitle("Autocorrelazione catene MCMC (beta)")


#summary
beta_array <- posterior[, , grep("^beta\\[", dimnames(posterior)[[3]])]
beta_stan <- mcmc.list(
  lapply(1:dim(beta_array)[2], function(chain) {
    mcmc(beta_array[, chain, ])
  })
)
sigma2_stan <- as.mcmc(posterior[,, "sigma2"])
lambda_stan <- as.mcmc(posterior[,, "lambda"])
summary(beta_stan)
summary(sigma2_stan)
summary(lambda_stan)
geweke.diag(beta_stan)
geweke.diag(sigma2_stan)
geweke.diag(lambda_stan)
effectiveSize(beta_stan)
effectiveSize(sigma2_stan)
effectiveSize(lambda_stan)

# Previsioni sul TEST
post <- rstan::extract(fit_stan)
beta_draws <- post$beta   
sigma2_draws <- post$sigma2
X_test <- model.matrix(
  median_house_value ~ diag_coord + housing_median_age +
    total_rooms + population + median_income +
    ocean_lt1h + ocean_inland ,
  data = test_set
)
y_test <- test_set$median_house_value
n_iter <- nrow(beta_draws)
n_test <- nrow(X_test)
y_pred_mat <- matrix(NA, nrow = n_iter, ncol = n_test)
for (i in 1:n_iter) {
  mu <- X_test %*% beta_draws[i, ]
  y_pred_mat[i, ] <- rnorm(n_test, mean = mu, sd = sqrt(sigma2_draws[i]))
}
y_pred_mean <- apply(y_pred_mat, 2, mean)
y_pred_low  <- apply(y_pred_mat, 2, quantile, 0.025)
y_pred_high <- apply(y_pred_mat, 2, quantile, 0.975)

mae <- mean(abs(y_test - y_pred_mean))
rmse <- sqrt(mean((y_test - y_pred_mean)^2))
r2 <- 1 - sum((y_test - y_pred_mean)^2) / sum((y_test - mean(y_test))^2)
cat("MAE =", round(mae, 6), "\n")
cat("RMSE =", round(rmse, 6), "\n")
cat("R^2 :", r2, "\n")
abs_error <- abs(y_test - y_pred_mean)
# Grafici
df_plot <- data.frame(
  real = y_test,
  pred = y_pred_mean,
  low = y_pred_low,
  high = y_pred_high,
  abs_error = abs_error
)


ggplot(df_plot, aes(x = real, y = pred, color = abs_error)) +
  geom_point(alpha = 0.7) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.1, alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red")+
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Valori ossevati", y = "Previsioni a posteriori", 
       color = "Errore ass.") +
  theme_minimal()


# 4. Mixed Model -------------------------------------------------------------
# Creazione della variabile *group* relativa alle categorie di *ocean_proximity*:
train_set$group <- factor(ifelse(train_set$ocean_lt1h == 1, "lt1h",
                                 ifelse(train_set$ocean_inland == 1, "inland", "coast")))
test_set$group <- factor(ifelse(test_set$ocean_lt1h == 1, "lt1h",
                                ifelse(test_set$ocean_inland == 1, "inland", "coast")))

# Matrice dei predittori fissi:
X_train <- cbind(1,
                 train_set$housing_median_age,
                 train_set$total_rooms,
                 train_set$population,
                 train_set$median_income,
                 train_set$diag_coord)

X_test <- cbind(1,
                test_set$housing_median_age,
                test_set$total_rooms,
                test_set$population,
                test_set$median_income,
                test_set$diag_coord)

covariate_all <- c("housing_median_age", "total_rooms", "population", "median_income", "diag_coord")

# Esclusione di *diag_coord* tra le covariate per le quali si vogliono generare random slopes:
covariate_random <- setdiff(covariate_all, "diag_coord")
# Formula per la matrice U con intercetta random e random slopes sui predittori scelti:
formula_U <- as.formula(paste("~ 0 + group +", paste(paste0("group:", covariate_random), collapse = " + ")))
# formula_U
## ~0 + group + group:housing_median_age + group:total_rooms + group:population + group:median_income

U_train <- model.matrix(formula_U, data = train_set)
U_test  <- model.matrix(formula_U, data = test_set)

# Variabile risposta:
y_train <- train_set$median_house_value
y_test  <- test_set$median_house_value

# Codifica gruppo numerico:
c_train <- as.numeric(train_set$group)
c_test  <- as.numeric(test_set$group)

# Implementazione Gibbs Sampling:
gibbs_LMM_random_slopes_chain <- function(y, X, U, c, niter, nburn,
                                          beta0, Sigma0, tau0, Psi0, a0, b0, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
  
  n <- length(y)
  p <- ncol(X)
  q <- ncol(U)
  
  n_save <- floor(niter / 20)
  out_beta <- matrix(0, nrow = n_save, ncol = p)
  out_gamma <- matrix(0, nrow = n_save, ncol = q)
  out_sigma2 <- rep(0, n_save)
  
  beta <- rep(0, p)
  gamma <- rep(0, q)
  sigma2 <- 1
  an <- a0 + n / 2
  
  for(i in 1:niter){
    Ugamma <- as.vector(U %*% gamma)
    resid <- y - X %*% beta - Ugamma
    bn <- b0 + 0.5 * sum(resid^2)
    sigma2 <- 1 / rgamma(1, shape = an, rate = bn)
    
    Sigman <- solve(solve(Sigma0) + t(X) %*% X / sigma2)
    betan <- Sigman %*% (solve(Sigma0) %*% beta0 + t(X) %*% (y - Ugamma) / sigma2)
    beta <- as.vector(MASS::mvrnorm(1, mu = betan, Sigma = Sigman))
    
    Sigman_gamma <- solve(solve(Psi0) + t(U) %*% U / sigma2)
    gamman <- Sigman_gamma %*% (solve(Psi0) %*% tau0 + t(U) %*% (y - X %*% beta) / sigma2)
    gamma <- as.vector(MASS::mvrnorm(1, mu = gamman, Sigma = Sigman_gamma))
    
    # Thinning:
    if (i %% 20 == 0) {
      out_beta[i/20, ] <- beta
      out_gamma[i/20, ] <- gamma
      out_sigma2[i/20] <- sigma2
    }
  }
  burnin_save <- nburn/20
  return(list(
    beta = out_beta[-(1:burnin_save), ],
    gamma = out_gamma[-(1:burnin_save), ],
    sigma2 = out_sigma2[-(1:burnin_save)]
  ))
}

run_multiple_chains <- function(y, X, U, c, niter, nburn,
                                beta0, Sigma0, tau0, Psi0, a0, b0, n_chains){
  chains_beta <- list()
  chains_gamma <- list()
  chains_sigma2 <- list()
  
  for(chain in 1:n_chains){
    cat("Running chain", chain, "\n")
    out <- gibbs_LMM_random_slopes_chain(y, X, U, c, niter, nburn,
                                         beta0, Sigma0, tau0, Psi0, a0, b0, seed = chain)
    
    chains_beta[[chain]] <- out$beta
    chains_gamma[[chain]] <- out$gamma
    chains_sigma2[[chain]] <- out$sigma2
  }
  
  return(list(
    beta = chains_beta,
    gamma = chains_gamma,
    sigma2 = chains_sigma2
  ))
}

# Parametri prior:
beta0 <- rep(0, ncol(X_train))
Sigma0 <- diag(1, ncol(X_train))
tau0 <- rep(0, ncol(U_train))
Psi0 <- diag(1, ncol(U_train))
a0 <- 3
b0 <- 2

out_LMM <- run_multiple_chains(
  y_train, X_train, U_train, c_train,
  niter = 200000, nburn = 50000,
  beta0, Sigma0, tau0, Psi0, a0, b0,
  n_chains = 4
)

# Combinazione delle catene:
beta_mat  <- do.call(rbind, out_LMM$beta)    
gamma_mat <- do.call(rbind, out_LMM$gamma)
beta_hat  <- colMeans(beta_mat)
gamma_hat <- colMeans(gamma_mat)


n_iter <- nrow(beta_mat)
n_test <- nrow(X_test)

sigma2_mat <- do.call(rbind, out_LMM$sigma2)

# Previsioni sul test set:
y_pred_mat <- matrix(NA_real_, nrow = n_iter, ncol = n_test)
for (i in 1:n_iter) {
  mu_pred <- as.vector(X_test %*% beta_mat[i, ] + U_test %*% gamma_mat[i, ])
  y_pred_mat[i, ] <- mu_pred
  y_pred_mat_previsione[i, ] <- rnorm(n_test, mean = mu_pred, sd = sqrt(sigma2_mat[i]))
}

y_pred_mean  <- colMeans(y_pred_mat)
y_pred_lower <- apply(y_pred_mat, 2, quantile, probs = 0.025)
y_pred_upper <- apply(y_pred_mat, 2, quantile, probs = 0.975)


y_previsione <- matrix(NA_real_, nrow = n_iter, ncol = n_test)
for (i in 1:n_iter) {
  mu_pred <- as.vector(X_test %*% beta_mat[i, ] + U_test %*% gamma_mat[i, ])
  y_previsione[i, ] <- rnorm(n_test, mean = mu_pred, sd = sqrt(sigma2_mat[i]))
}

y_previsione_mean  <- colMeans(y_previsione)
y_previsione_lower <- apply(y_previsione, 2, quantile, probs = 0.025)
y_previsione_upper <- apply(y_previsione, 2, quantile, probs = 0.975)

# Metriche di performance del modello:
rmse <- sqrt(mean((y_test - y_pred_mean)^2))
mae  <- mean(abs(y_test - y_pred_mean))
r2   <- 1 - sum((y_test - y_pred_mean)^2) / sum((y_test - mean(y_test))^2)

cat("RMSE:", rmse, "\n")
cat("MAE :", mae,  "\n")
cat("R²  :", r2,   "\n")

# Traceplot coefficienti beta
colnames(X_train) <- c("Intercept", "housing_median_age", "total_rooms",
                       "population", "median_income", "diag_coord")

par(mfrow = c(3, 2))
beta_names <- colnames(X_train)
beta_chains <- lapply(out_LMM$beta, as.mcmc)
for(i in seq_along(beta_chains)){
  colnames(beta_chains[[i]]) <- beta_names
}
beta_mcmc_list <- mcmc.list(beta_chains)
traceplot(beta_mcmc_list, smooth = TRUE, col = 1:4, type = "l")


# Traceplot coefficienti gamma
par(mfrow = c(5, 3))
gamma_names <- colnames(U_train)
gamma_chains <- lapply(out_LMM$gamma, as.mcmc)
for(i in seq_along(gamma_chains)){
  colnames(gamma_chains[[i]]) <- gamma_names
}
gamma_mcmc_list <- mcmc.list(gamma_chains)
traceplot(gamma_mcmc_list, smooth = TRUE, col = 1:4, type = "l")


# Summaries e diagnostiche:
beta_LMM  <- as.mcmc(beta_mat)
gamma_LMM <- as.mcmc(gamma_mat)

summary(beta_LMM)
summary(gamma_LMM)

geweke.diag(beta_LMM)
geweke.diag(gamma_LMM)

sigma2_chains <- lapply(out_LMM$sigma2, as.mcmc)
summary_per_chain <- lapply(sigma2_chains, summary)
geweke_per_chain <- lapply(sigma2_chains, geweke.diag)
ess_per_chain    <- sapply(sigma2_chains, effectiveSize)

list(
  Summary = summary_per_chain,
  Geweke  = geweke_per_chain,
  ESS     = ess_per_chain
)

# Traceplot e densità sigma2:
df_sigma2 <- do.call(rbind, lapply(seq_along(out_LMM$sigma2), function(i) {
  data.frame(iteration = seq_along(out_LMM$sigma2[[i]]),
             lambda = out_LMM$sigma2[[i]],
             chain = factor(i))
}))


p1 <- ggplot(df_sigma2, aes(x = iteration, y = lambda, color = chain)) +
  geom_line(alpha = 0.7) +
  labs(title = "Traceplot di sigma2 per catena", x = "Iterazione", y = expression(sigma^2)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))

p2 <- ggplot(df_sigma2, aes(x = lambda, color = chain)) +
  geom_density(size = 0.7) +
  labs(title = "Densità a posteriori sigma2", x = "sigma2", y = NULL) +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(linetype = 1, shape = NA, size = 1)))

(p1 | p2) + plot_layout(widths = c(1, 1))


# Distribuzioni coefficienti beta:
beta_names <- colnames(X_train)
df_beta <- do.call(rbind, lapply(out_LMM$beta, as.data.frame))
colnames(df_beta) <- beta_names

df_plot_beta <- df_beta %>%
  pivot_longer(cols = everything(), names_to = "label", values_to = "value") %>%
  mutate(label = factor(label, levels = beta_names))


ggplot(df_plot_beta, aes(x = value, fill = label)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.6, position = "identity", bins = 30) +
  theme_bw() +
  ylab("Count") +
  facet_wrap(~label, scales = "free") +
  theme(legend.position = "none")


ggplot(df_plot_beta, aes(x = label, y = value, fill = label)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  ylab(NULL) + xlab(NULL) +
  theme(legend.position = "none")

# Distribuzioni coefficienti gamma:
gamma_names <- colnames(U_train)
df_gamma <- as.data.frame(gamma_mat)
colnames(df_gamma) <- gamma_names

gamma_col_info <- data.frame(orig = gamma_names, stringsAsFactors = FALSE) %>%
  mutate(
    has_colon = grepl(":", orig),
    part1 = ifelse(has_colon, sub(":.*$", "", orig), orig),
    part2 = ifelse(has_colon, sub("^.*?:", "", orig), NA_character_),
    group = sub("^group", "", part1),
    covariate = ifelse(is.na(part2), "(Intercept)", part2)
  )

df_plot_gamma <- df_gamma %>%
  pivot_longer(cols = everything(), names_to = "orig", values_to = "value") %>%
  left_join(gamma_col_info, by = "orig")

df_plot_gamma$group     <- factor(df_plot_gamma$group, levels = c("coast","inland","lt1h"))
df_plot_gamma$covariate <- factor(df_plot_gamma$covariate,
                                  levels = c("(Intercept)","housing_median_age","total_rooms",
                                             "population","median_income"))


ggplot(df_plot_gamma, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~covariate, scales = "free") +
  theme_bw() +
  labs(x = "Coefficienti gamma", y = "Densità", fill = "Gruppo")


df_plot_gamma$covariate <- as.character(df_plot_gamma$covariate)
df_plot_gamma$covariate[is.na(df_plot_gamma$covariate)] <- "Intercept"
df_plot_gamma$covariate <- factor(df_plot_gamma$covariate)

ggplot(df_plot_gamma, aes(x = group, y = value, fill = group)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~covariate, scales = "free") +
  theme_bw() +
  labs(x = "Gruppo", y = "Coefficienti gamma")


# Previsioni vs osservazioni:
df_previsione <- data.frame(
  observed  = y_test,
  predicted = y_previsione_mean,
  lower     = y_previsione_lower,
  upper     = y_previsione_upper
) %>%
  mutate(abs_error = abs(predicted - observed))


ggplot(df_previsione, aes(x = observed, y = predicted, color = abs_error)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(
    title = "Previsioni a posteriori vs osservazioni con intervalli di credibilità",
    x = "Valori osservati",
    y = "Previsioni a posteriori",
    color = "Errore assoluto"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
dev.off()


# 5. BART -----------------------------------------------------------------
library(BART)

X_train <- train_set %>%
  select(diag_coord, housing_median_age, total_rooms, population, median_income,
         ocean_lt1h, ocean_inland, on_the_coast) %>%
  as.matrix()
y_train <- train_set$median_house_value

X_test <- test_set %>%
  select(diag_coord, housing_median_age, total_rooms, population, median_income,
         ocean_lt1h, ocean_inland, on_the_coast) %>%
  as.matrix()
y_test <- test_set$median_house_value
set.seed(42)
bart_fit <- wbart(x.train = X_train, y.train = y_train,
                  x.test = X_test, 
                  ntree   = 500,   
                  ndpost  = 5000,  
                  nskip   = 1000,
                  k=1.5)    
y_pred <- bart_fit$yhat.test.mean
mae_bart <- mean(abs(y_test - y_pred))
rmse_bart <- sqrt(mean((y_test - y_pred)^2))
r2_bart <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
cat("BART MAE:", round(mae_bart,6), " RMSE:", round(rmse_bart,6),"R2: ",round(r2_bart,6), "\n")
abs_error<- abs(y_test - y_pred)
df_plot <- data.frame(
  y_true = y_test,
  y_pred = y_pred,
  y_low = apply(bart_fit$yhat.test, 2, quantile, 0.025),
  y_high = apply(bart_fit$yhat.test, 2, quantile, 0.975),
  abs_error = abs_error
)

pdf("Nic2-valoriPvsO.pdf", width = 12, height = 8)
ggplot(df_plot, aes(x = y_true, y = y_pred, color = abs_error)) +
  geom_point(alpha = 0.7) +
  geom_errorbar(aes(ymin = y_low, ymax = y_high), width = 0.1, alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red")+
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Valori ossevati", y = "Previsioni a posteriori", 
       color = "Errore ass.") +
  theme_minimal()
dev.off()

#Mappa California
colonne_scelte <- test_set[,1:2]
nuovo_df <- data.frame(colonne_scelte, previsioni = y_pred)
summary(nuovo_df)

# dati sf per stati e mondo
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
california <- states %>% filter(ID == "california")
others     <- states %>% filter(ID != "california")
world <- st_as_sf(map("world", plot = FALSE, fill = TRUE))

# grafico con nuovo_df
ggplot() +
  geom_sf(data = world, fill = "lightblue", color = NA) +  # oceano
  geom_sf(data = others, fill = "gray90", color = "gray70") +  # altri stati
  geom_sf(data = california, fill = "antiquewhite", color = "gray40") +  # california
  geom_point(
    data = nuovo_df,
    aes(
      x = longitude,
      y = latitude,
      color = previsioni
    ),
    size = 3,      # dimensione fissa
    alpha = 0.8
  ) +
  scale_color_gradientn(
    colors = c("darkblue","blue","cyan","green","yellow","orange","red","darkred"),
    values = scales::rescale(c(min(nuovo_df$previsioni), 
                               quantile(nuovo_df$previsioni, 0.15),
                               quantile(nuovo_df$previsioni, 0.3),
                               quantile(nuovo_df$previsioni, 0.5),
                               quantile(nuovo_df$previsioni, 0.65),
                               quantile(nuovo_df$previsioni, 0.8),
                               max(nuovo_df$previsioni))),
    name = "Previsioni"
  ) +
  coord_sf(
    xlim = c(-125, -113),
    ylim = c(32.3 , 42.5),
    expand = FALSE
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA)
  )

# 6. Analisi spaziale: point referenced data ------------------------------
library(gstat)
library(sp)
library(automap)
library(corrplot)
library(gridExtra)
library(tigris)

# Shape file delle coontee della California
ca_counties <- counties(state = "CA", cb = TRUE, class = "sf")
points_sf <- st_as_sf(
  data_def, 
  coords = c("longitude", "latitude"), 
  crs = 4269  # sistema di riferimento WGS 84
)
points_joined <- st_join(points_sf, ca_counties["NAME"], left = TRUE)
ggplot() +
  geom_sf(data = ca_counties, fill = NA, color = "black") +
  geom_sf(data = points_joined, aes(color = 'red'), size = 0.5, alpha = 0.8) +
  guides(color = "none") +
  theme_minimal() 

punti_con_contea<- st_join(points_sf,ca_counties,join=st_within)
conteggio <- punti_con_contea %>%
  st_drop_geometry() %>%
  mutate(id = row_number())

conteggio <- punti_con_contea %>%
  count(NAME)

total_obs <- nrow(data_def)

# rimuoviamo l'ultima riga (osservazioni che non risultano all'interno della mappa)
conteggio <- conteggio %>%
  filter(!is.na(NAME))

# aggiungiamo colonna con la proporzione rispetto al totale
conteggio <- conteggio %>%
  mutate(prop = n / total_obs)
sum(conteggio$prop) # circa 1.15% dei dati risultano fuori dalla mappa

# calcoliamo numero campione iniziale proporzionale
conteggio <- conteggio %>%
  mutate(n_sample = ceiling(prop * 1000))
sum(conteggio$n_sample)

# campionamento stratificato proporzionale
set.seed(123)  # per riproducibilità
punti_con_contea <- punti_con_contea %>%
  st_drop_geometry() %>%
  mutate(id = row_number())

punti_con_contea <- punti_con_contea %>%
  left_join(conteggio %>% dplyr::select(NAME, n_sample), by = "NAME")

campione <- punti_con_contea %>%
  filter(!is.na(NAME)) %>%
  group_by(NAME) %>%
  group_modify(~ slice_sample(.x, n = unique(.x$n_sample))) %>%
  ungroup()

data_train_test <- data_def[campione$id, ]

dim(data_train_test) # 1015 osservazioni, 11 variabili

# mappe per confrontare il dataset originale con il campione
ggplot() +
  geom_point(data = data_def, aes(x = longitude, y = latitude, color = "Dati originali"), 
             alpha = 0.3) +
  geom_point(data = data_train_test, aes(x = longitude, y = latitude, 
                                         color = "Campione stratificato"), size = 1) +
  scale_color_manual(values = c("Dati originali" = "black", 
                                "Campione stratificato" = "red")) +
  labs(color = "Legenda") +
  theme_minimal()

ggplot() +
  geom_sf(data = ca_counties, 
          fill = "white", 
          color = "grey", 
          size = 0.3) +
  geom_point(data = data_def, 
             aes(x = longitude, y = latitude, color = "Dati originali"), 
             alpha = 0.4, 
             size = 1) +
  geom_point(data = data_train_test, 
             aes(x = longitude, y = latitude, color = "Campione stratificato"), 
             size = 1,
             alpha = 0.8) +
  scale_color_manual(values = c("Dati originali" = "#2C3E50",
                                "Campione stratificato" = "#E74C3C"))
labs(
  color = "Tipologia dati",
  x = "Longitudine",
  y = "Latitudine") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),
    #legend.margin = margin(0, 0, 0, 15),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    #plot.margin = margin(10, 10, 10, 10),
    text = element_text(family = "serif", color = "black")
  ) +
  # Coordinate fisse per mantenere le proporzioni
  coord_sf(expand = FALSE)

ks.test(data_train_test$median_house_value, data_def$median_house_value)

# suddivisione in train e test in proporzione 70/30
set.seed(123)
n <- nrow(data_train_test)
idx <- sample(seq_len(n), size = 0.7 * n)

train <- data_train_test[idx, ]
test  <- data_train_test[-idx, ]


# Impostazione della variabile risposta e delle covariate

y_fit <- train$median_house_value # log del prezzo, trasformazione già fatta

# costruisco X: intercetta + covariate numeriche + dummies ocean
X_fit <- cbind(
  intercept      = 1,
  total_rooms    = train$total_rooms,
  population     = train$population,
  median_income  = train$median_income,
  ocean_lt1h     = train$ocean_lt1h,
  ocean_inland   = train$ocean_inland,
  diag_coord     = train$diag_coord
)
X_fit <- as.matrix(X_fit)

S_fit_raw <- as.matrix(train[, c("longitude","latitude")])

# test
X_new <- cbind(
  intercept      = 1,
  total_rooms    = test$total_rooms,
  population     = test$population,
  median_income  = test$median_income,
  ocean_lt1h     = test$ocean_lt1h,
  ocean_inland   = test$ocean_inland,
  diag_coord     = test$diag_coord
)
X_new <- as.matrix(X_new)
S_new_raw <- as.matrix(test[, c("longitude","latitude")])

S_all_raw <- rbind(S_fit_raw, S_new_raw)
S_all_scaled <- scale(S_all_raw)
attr_center <- attr(S_all_scaled, "scaled:center")
attr_scale  <- attr(S_all_scaled, "scaled:scale")

S_fit <- S_all_scaled[1:nrow(S_fit_raw), ]
S_new <- S_all_scaled[(nrow(S_fit_raw)+1):nrow(S_all_scaled), ]


# Dimensioni e prior

n <- nrow(X_fit)
p <- ncol(X_fit)
M <- nrow(X_new)

# Prior per beta (coeff di regressione) --> normale multi
beta0   <- rep(0, p)
Lambda0 <- diag(4, p)

# Prior per varianze
asigma <- 3; bsigma <- 1.5   # IG per sigma2 (varianza spaziale)
atau   <- 3; btau   <- 1.5   # IG per tau2 (varianza nugget)

# Prior per phi 
aphi   <- 3.0; bphi   <- 2 # IG per phi (parametro di scala spaziale)

## ----------- Modello completo: covariate e componente spaziale ----------------

# lista dati
data_spat <- list(
  n = n, p = p, M = M,
  y = as.vector(y_fit),
  X = X_fit,
  S = S_fit,
  X_new = X_new,
  S_new = S_new,
  beta0 = beta0,
  Lambda0 = Lambda0,
  asigma = asigma, bsigma = bsigma,
  atau = atau, btau = btau,
  aphi = aphi, bphi = bphi
)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit <- stan(
  file = "point_referenced_multi.stan",
  data = data_spat,
  chains = 1,
  iter = 2000,
  warmup = 1000,
  seed = 123,
  refresh = 50,
  control = list(max_treedepth = 15, adapt_delta = 0.95)
)

print(fit, pars = c("sigma2","tau2","phi"))
print(fit, pars = c("beta"))

# Estrazione previsioni
draws_mean <- rstan::extract(fit, pars = "pred_mean")[[1]]  # [iter, M]
draws_var  <- rstan::extract(fit, pars = "pred_var")[[1]]   # [iter, M]

y_pred <- colMeans(draws_mean)
y_true <- test$median_house_value

# Metriche principali
mae <- mean(abs(y_pred - y_true))
rmse <- sqrt(mean((y_pred - y_true)^2))
r2 <- cor(y_pred, y_true)^2

cat("MAE:", round(mae, 4), "\n")
cat("RMSE:", round(rmse, 4), "\n") 
cat("R²:", round(r2, 4), "\n")

set.seed(123)
ppc_samples <- matrix(NA_real_, nrow = nrow(draws_mean), ncol = M)
for (i in 1:nrow(draws_mean)) {
  # varianza predittiva è su scala y (log-prezzo, nel tuo set up)
  ppc_samples[i, ] <- rnorm(M, mean = draws_mean[i, ], sd = sqrt(pmax(1e-12, draws_var[i, ])))
}

# Riassunti per ciascun punto
pred_mean_hat <- apply(draws_mean, 2, mean)
pred_sd_hat   <- sqrt(apply(draws_var,  2, mean))
pred_q025     <- apply(ppc_samples, 2, quantile, 0.025)
pred_q975     <- apply(ppc_samples, 2, quantile, 0.975)

pred_df <- data.frame(
  id = 1:M,
  pred_mean = pred_mean_hat,
  pred_sd   = pred_sd_hat,
  ppc_q025  = pred_q025,
  ppc_q975  = pred_q975,
  y_true    = test$median_house_value
)

# Esempio: confronto per il 20esimo test point
ggplot(data.frame(x = ppc_samples[,20]), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.3, col = 1, bins = 30) +
  geom_vline(xintercept = pred_df$y_true[20], col = "red", lwd = 0.8, lty = 2) +
  theme_bw() + xlab("Posterior predictive") + ylab("density")

# Diagnostica MCMC
param1 <- As.mcmc.list(fit, pars = c("sigma2","tau2","phi"))
param2 <- As.mcmc.list(fit, pars = c("beta"))
summary(param1); summary(param2)
geweke.diag(param1); geweke.diag(param2)

plot(param1)
par(mfrow=c(4,2), mar=c(3,3,3,2))
plot(param2[, paste0("beta[", 1:7, "]")])
par(mfrow = c(1,1))

## ------------------ Modello "nullo": solo covariate ---------------------------

data_null <- list(
  n = n,
  p = p,
  y = as.vector(y_fit),
  X = X_fit,
  M = M,
  X_new = X_new,
  beta0 = beta0,
  Lambda0 = Lambda0,
  atau = atau,
  btau = btau
)

fit_null <- stan(file = "point_referenced_null_multi.stan", 
                 data = data_null,
                 chains = 1, 
                 iter = 2000, 
                 warmup = 1000, 
                 seed = 123,
                 refresh = 50,
                 control = list(max_treedepth = 15, adapt_delta = 0.95))

print(fit_null, pars = c("beta"))

# Diagnostica MCMC
param_null <- As.mcmc.list(fit_null, pars = c("beta"))
summary(param_null)
geweke.diag(param_null);

# plot(param_null[, c("beta[1]","beta[2]","beta[3]")])
# plot(param_null[, c("beta[4]","beta[5]","beta[6]")])
par(mfrow=c(4,2), mar=c(3,3,3,2))
plot(param_null[, paste0("beta[", 1:7, "]")])
par(mfrow = c(1,1))

# Metriche
draws_mean_null <- rstan::extract(fit_null, pars = "pred_mean")[[1]]
y_pred_null <- colMeans(draws_mean_null)

mae_null <- mean(abs(y_pred_null - y_true))
rmse_null <- sqrt(mean((y_pred_null - y_true)^2))
r2_null <- cor(y_pred_null, y_true)^2

# Risultati
cat("MAE:", round(mae_null, 4), "\n")
cat("RMSE:", round(rmse_null, 4), "\n") 
cat("R²:", round(r2_null, 4), "\n")

# Bayes Factor

# M1: fit
# M2: fit_null

logsumexp <- function(x){
  M <- max(x)
  lse <- M + log(sum(exp(x - M)))
  return(lse)
}

# M1: modello spaziale
llik1 <- rstan::extract(fit, pars = "llik")[[1]] # log likelihood campionata
log_marginal_M1 <- log(length(llik1)) - logsumexp(-llik1) # log marginal likelihood

# M2: modello nullo
llik2 <- rstan::extract(fit_null, pars = "llik")[[1]]
log_marginal_M2 <- log(length(llik2)) - logsumexp(-llik2)


BF_21 <- exp(log_marginal_M1 - log_marginal_M2)
logBF_21 <- log_marginal_M1 - log_marginal_M2

BF_12 <- 1/BF_21
logBF_12 <- - logBF_12

cat("log marginal likelihood M1:", log_marginal_M1, "\n")
cat("log marginal likelihood M2:   ", log_marginal_M2, "\n")
cat("Bayes Factor (M1/M2):", BF_12, "\n")
cat("Log Bayes Factor (M1/M2):", logBF_12, "\n")

## ------------------------ Kriging Ordinario -----------------------------------

# stessa suddivisione del dataset iniziale (stesso seed)
set.seed(123)
n <- nrow(data_train_test)
idx <- sample(seq_len(n), size = 0.7 * n)

train <- data_train_test[idx, ]
test  <- data_train_test[-idx, ]

coordinates(train) <- ~ longitude + latitude
coordinates(test) <- ~ longitude + latitude

# impostazione del sistema di coordinate (WGS84)
proj4string(train) <- CRS("+proj=longlat +datum=WGS84")
proj4string(test) <- CRS("+proj=longlat +datum=WGS84")

# trasformazione a sistema metrico UTM Zone 11N (California)
train_utm <- spTransform(train, CRS("+proj=utm +zone=11 +datum=WGS84 +units=m"))
test_utm <- spTransform(test, CRS("+proj=utm +zone=11 +datum=WGS84 +units=m"))

# kriging ordinario (solo intercetta)
formula_ok <- median_house_value ~ 1

# variogramma empirico
vgm_emp <- variogram(formula_ok, train_utm, 
                     cutoff = 200000,  # 200 km
                     width = 15000)    # bins di 15 km

plot(vgm_emp, main = "Empirical Variogram - Ordinary Kriging")

vgm_fit <- autofitVariogram(formula_ok, train_utm,
                            model = c("Sph", "Exp", "Gau"),
                            miscFitOptions = list(merge.small.bins = TRUE))
plot(vgm_fit)
print(vgm_fit$var_model)


ok_pred <- krige(formula_ok, 
                 locations = train_utm,
                 newdata = test_utm,
                 model = vgm_fit$var_model,
                 nmin = 5,        # minimo 5 punti vicini
                 nmax = 15,       # massimo 15 punti vicini
                 maxdist = 150000) # raggio massimo 150km

predictions <- ok_pred$var1.pred
variances <- ok_pred$var1.var

# gestione NA
na_pred <- sum(is.na(predictions))
na_var <- sum(is.na(variances))

if(na_pred > 0) {
  cat("Trovate", na_pred, "predizioni NA - imputando con media\n")
  predictions[is.na(predictions)] <- mean(predictions, na.rm = TRUE)
}

if(na_var > 0) {
  cat("Trovate", na_var, "varianze NA/negative - imputando con media\n")
  variances[is.na(variances) | variances <= 0] <- mean(variances[variances > 0], na.rm = TRUE)
}

std_errors <- sqrt(pmax(variances, 0))

cat(length(predictions), "predizioni generate\n")

observed <- test_utm$median_house_value
predicted <- predictions

calculate_metrics <- function(observed, predicted) {
  residuals <- observed - predicted
  mae <- mean(abs(residuals))
  rmse <- sqrt(mean(residuals^2))
  r_squared <- cor(observed, predicted, use = "complete.obs")^2
  
  return(list(
    MAE = mae,
    RMSE = rmse,
    R2 = r_squared,
    n = length(observed),
    residuals = residuals
  ))
}

# calcolo metriche kriging
metrics_kriging <- calculate_metrics(observed, predicted)

cat("MAE: ", round(metrics_kriging$MAE, 4), "\n")
cat("RMSE:", round(metrics_kriging$RMSE, 4), "\n")
cat("R²:  ", round(metrics_kriging$R2, 4), "\n")
cat("N:   ", metrics_kriging$n, "\n")


## ------------------------- Kriging Universale ---------------------------------
formula_uk <- median_house_value ~ housing_median_age + total_rooms + population 
+ median_income + ocean_lt1h + ocean_inland + diag_coord

# variogramma
vgm_emp_uk <- variogram(formula_uk, train_utm, 
                        cutoff = 200000, 
                        width = 15000)

vgm_fit_uk <- autofitVariogram(formula_uk, train_utm,
                               model = c("Sph", "Exp", "Gau"))

# confronto variogrammi OK e UK
par(mfrow = c(1, 2))
plot(vgm_emp, main = "Variogramma, Kriging Ordinario")
plot(vgm_emp_uk, main = "Variogramma, Kriging Universale") 

uk_pred <- krige(formula_uk,
                 locations = train_utm,
                 newdata = test_utm, 
                 model = vgm_fit_uk$var_model,
                 nmin = 5,
                 nmax = 15,
                 maxdist = 150000)

predictions_uk <- uk_pred$var1.pred
variances_uk <- uk_pred$var1.var

# gestione NA
na_pred_uk <- sum(is.na(predictions_uk))
na_var_uk <- sum(is.na(variances_uk))

if(na_pred_uk > 0) {
  cat("Trovate", na_pred_uk, "predizioni NA - imputando\n")
  predictions_uk[is.na(predictions_uk)] <- mean(predictions_uk, na.rm = TRUE)
}

if(na_var_uk > 0) {
  cat("Trovate", na_var_uk, "varianze NA - imputando\n")
  variances_uk[is.na(variances_uk)] <- mean(variances_uk, na.rm = TRUE)
}

std_errors_uk <- sqrt(pmax(variances_uk, 0))


observed <- test_utm$median_house_value
metrics_uk <- calculate_metrics(observed, predictions_uk)

# metriche UK
cat("MAE:", round(metrics_uk$MAE, 4),"\n")
cat("RMSE:", round(metrics_uk$RMSE, 4),"\n")
cat("R²:", round(metrics_uk$R2, 4),"\n")


