## LIBRERIAS --------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(recipes)
library(plotly)
library(dplyr)
library(tidyr)
library(rsample)
library(glmnet)
library(tidymodels)
library(Matrix)
library(tidyr)
library(tune)
library(ROSE)  # Alternativa a SMOTE

## DATASET --------------------------------------------------------------------------------
file_path <- "C:\\Users\\paula\\MASTER EAE\\ESTADISTICA Y MINERIA\\TRABAJO\\fraud_oracle.csv"
data <- read.csv(file_path)

## PARTE 1 ESTUDIO EXPLORATORIO EDA --------------------------------------------------------------------------------
print("Estudio del dataset")
dim(data)           # Dimensiones
head(data)          # Primeras filas
glimpse(data)       # Estructura general
summary(data)       # Estadísticas básicas

print("Tipos de variables:")
categorical_vars <- names(data)[sapply(data, is.character)]
numerical_vars <- names(data)[sapply(data, is.numeric)]
print(numerical_vars)
print(categorical_vars)

# Distribuciones de las variables numéricas
data %>%
  select(all_of(numerical_vars)) %>%
  gather(variable, value) %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribución de Variables Numéricas", x = "Valor", y = "Frecuencia")

## VALORES FALTANTES E IGUALES A 0 ------------------------------
# Detección y manejo de valores faltantes, contando valores NA por columna
missing_values <- colSums(is.na(data))
missing_values[missing_values > 0]  # Mostrar solo columnas con NA
View(missing_values) 

# Detección de valores iguales a 0 por columna
print("Detección de valores iguales a 0:")
zero_values <- colSums(data == 0, na.rm = TRUE)
zero_values[zero_values > 0]  # Mostrar solo columnas con valores iguales a 0
View(zero_values) #Hay 0 en la columna AGE 320 

#Dado que es un valor minimo, decidimos eliminarlo
data <- data[data$Age != 0, ]
sum(data$Age == 0, na.rm = TRUE)

#Al eliminar los valores Age = 0, se eliminan los valores donde la poliza de seguros la tienen personas con 16-17 años LO CUAL SENTIDO PQ SON MENORES
zeros_AgeOfPolicyHolder <- sum(data$Age == 0, na.rm = TRUE)
cat("Número de ceros en la columna 'AgeOfPolicyHolder':", zeros_AgeOfPolicyHolder, "\n")

#Se quita una categoria entera, la cual no tenia sentido pq no hay polizas para niños de 16 a 17 años
age_counts <- table(data$AgeOfPolicyHolder)
print(age_counts)

## ELIMINACIÓN COLUMNAS INNECESARIAS  --------------------------------------------------------------------------------
print(colnames(data))

data_cleaned <- data %>% 
  select(-Month, -WeekOfMonth, -DayOfWeek, -DayOfWeekClaimed, -MonthClaimed, -WeekOfMonthClaimed, -PolicyNumber, -Age)

print(colnames(data_cleaned))

## ANALISIS VARIABLE TARGET ---------------------------------------------------------------------------
# Verificar la distribución de la variable target
fraud_distribution <- table(data_cleaned$FraudFound_P)
print(fraud_distribution) 
#vemos que la variable fraud esta desbalanceada

# Visualización gráfica con ggplot2
ggplot(data, aes(x = as.factor(FraudFound_P), fill = as.factor(FraudFound_P))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(
    title = "Distribución de la variable objetivo 'FraudFound_P'",
    x = "Fraude Encontrado (0: No, 1: Sí)",
    y = "Frecuencia",
    fill = "Fraude Encontrado"
  ) 

## VERIFICAR TIPOS DE DATOS ----------------------------------------------------------------------
print("Verificar tipos de datos antes de aplicar ROSE:")
glimpse(data_cleaned)

# Asegurar que las columnas categóricas sean factores
categorical_cols <- names(data_cleaned)[sapply(data_cleaned, is.character)]
data_cleaned[categorical_cols] <- lapply(data_cleaned[categorical_cols], as.factor)

# Asegurar que la variable objetivo sea un factor
data_cleaned$FraudFound_P <- as.factor(data_cleaned$FraudFound_P)

## APLICAR SMOTE CON ROSE -------------------------------------------------------------------------
set.seed(42)  # Para reproducibilidad

# Aplicar ROSE para balancear el dataset
data_smote <- ROSE(FraudFound_P ~ ., data = data_cleaned, seed = 42)$data

# Verificar la distribución de la variable objetivo después de ROSE
print("Distribución después de aplicar ROSE:")
fraud_distribution_smote <- table(data_smote$FraudFound_P)
print(fraud_distribution_smote)

ggplot(data_smote, aes(x = as.factor(FraudFound_P), fill = as.factor(FraudFound_P))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(
    title = "Distribución de la variable objetivo 'FraudFound_P' después de ROSE",
    x = "Fraude Encontrado (0: No, 1: Sí)",
    y = "Frecuencia",
    fill = "Fraude Encontrado"
  ) +
  theme_minimal()


## ANALISIS FEATURES  ---------------------------------------------------------------------------
#-----------------------1----------------------------------
# Verificar la distribucion de la variable Sex
# Fraud Detection by Sex
df_fraud <- data %>% filter(FraudFound_P == 1) # Filtrar el dataset para solo fraudes
# Calcular conteos y porcentajes para la variable Sex
fraud_counts_sex <- df_fraud %>% 
  count(Sex, name = "Count") %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)
print(fraud_counts_sex)

ggplot(data, aes(x = Sex, fill = Sex)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribución de la variable 'Sex'",
    x = "Sexo",
    y = "Frecuencia",
    fill = "Sexo"
  )

#-------------------------------2.1-------------
#Verificar la variable Age 
summary(data$Age)
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(
    title = "Distribución de la variable 'Age'",
    x = "Edad",
    y = "Frecuencia"
  )

#Fraude por edad
# Separar datos de fraude y no fraude
df_fraud <- data %>% filter(FraudFound_P == 1)
df_non_fraud <- data %>% filter(FraudFound_P == 0)

# Conteo por edad
df_counts_age <- data %>% 
  count(Age, name = "Total") %>% 
  arrange(Age)
df_counts_fraud <- df_fraud %>% 
  count(Age, name = "Fraud") %>% 
  arrange(Age)

# Calcular porcentajes de fraude por edad
df_percentages_fraud <- df_counts_age %>% 
  left_join(df_counts_fraud, by = "Age") %>% 
  mutate(Fraud = replace_na(Fraud, 0),
         Percentage = round((Fraud / Total) * 100, 2))

print(df_percentages_fraud)

# Gráfico de barras con ggplot2
ggplot(df_percentages_fraud, aes(x = Age, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.7) +
  geom_hline(yintercept = 6, color = "red", linetype = "dotdash", size = 1) + # Línea de referencia
  labs(
    title = "Detección de Fraude por Edad",
    x = "Edad",
    y = "Porcentaje"
  ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) + # Configuración de rango y ticks
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


#Variable Rating Resumen de la distribución peor no es una variable jmuy interesante
cat("Distribución de la variable 'DriverRating':\n")
print(table(data$DriverRating)) 
#su distribución es uniforme, lo que significa que cada categoría de DriverRating tiene aproximadamente la misma cantidad de datos (frecuencia similar).
#implica que la variable no muestra una relación obvia entre la calificación del conductor y la cantidad de accidentes o la variable objetivo (FraudFound_P
# Por lo que la vamos a convertir en dummies


#-----------------------3----------------------------------
# Fraud Detection by Age of Policy Holder CORREGIR


# Separar datos de fraude y no fraude
df_fraud <- data %>% filter(FraudFound_P == 1)
df_non_fraud <- data %>% filter(FraudFound_P == 0)

# Conteo por edad del titular de la póliza
df_counts_ageofpolicyholder <- data %>% 
  count(AgeOfPolicyHolder, name = "Total")
df_counts_fraud <- df_fraud %>% 
  count(AgeOfPolicyHolder, name = "Fraud")

# Calcular porcentajes de fraude por edad
df_percentages_fraud <- df_counts_ageofpolicyholder %>% 
  left_join(df_counts_fraud, by = "AgeOfPolicyHolder") %>% 
  mutate(Fraud = replace_na(Fraud, 0),
         Percentage = round((Fraud / Total) * 100, 2))

print(df_percentages_fraud)

# Gráfico de barras con ggplot2
ggplot(df_percentages_fraud, aes(x = AgeOfPolicyHolder, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.7) +
  geom_hline(yintercept = 6, color = "red", linetype = "dotdash", size = 1) + # Línea de referencia
  labs(
    title = "Detección de Fraude por Edad del Titular de la Póliza",
    x = "Edad del Titular de la Póliza",
    y = "Porcentaje"
  ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) + # Configuración de rango y ticks
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


#-----------------------4----------------------------------
#Fraud Detection by Vehicle Price
# Calcular conteos de VehiclePrice en el dataset completo y en fraudes
df_counts_vp <- data %>% 
  count(VehiclePrice, name = "Total") %>% 
  arrange(VehiclePrice)

df_counts_fraud3 <- df_fraud %>% 
  count(VehiclePrice, name = "Fraud") %>% 
  arrange(VehiclePrice)

# Combinar conteos y calcular porcentajes
df_percentages_fraud3 <- df_counts_vp %>% 
  left_join(df_counts_fraud3, by = "VehiclePrice") %>% 
  mutate(Fraud = replace_na(Fraud, 0),
         `Fraud %` = round((Fraud / Total) * 100, 2)) %>% 
  arrange(`Fraud %`)
df_percentages_fraud3

# Gráfico de barras horizontal con ggplot2
ggplot(df_percentages_fraud3, aes(x = `Fraud %`, y = VehiclePrice)) +
  geom_bar(stat = "identity", fill = "#9FF781", color = "black") +
  geom_vline(xintercept = 6, color = "darkgreen", linetype = "dotdash", size = 1) + # Línea de referencia
  labs(
    title = "Detección de Fraude por Precio del Vehículo",
    x = "Porcentaje",
    y = "Precio del Vehículo"
  ) +
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, 2)) + # Configuración de rango y ticks
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

#-----------------------4---------------------------------
#Fraud Detection by Vehicle Price Detención de fraude por marca
# Agrupación de datos y cálculo del porcentaje de fraude por marca
fraud_analysis <- data %>%
  group_by(Make) %>%
  summarise(
    Total_Cases = n(),
    Fraud_Cases = sum(FraudFound_P == 1, na.rm = TRUE),
    Fraud_Percent = round((Fraud_Cases / Total_Cases) * 100, 2)
  ) %>%
  arrange(desc(Fraud_Percent))  # Ordenar por mayor porcentaje de fraude

# Verificar el análisis
print(fraud_analysis)

# Visualización: Barra horizontal de porcentaje de fraude por marca
ggplot(fraud_analysis, aes(x = Fraud_Percent, y = reorder(Make, Fraud_Percent))) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(
    title = "Porcentaje de Fraude por Marca de Coche",
    x = "Porcentaje de Fraude",
    y = "Marca del Coche"
  ) +
  geom_vline(xintercept = 6, linetype = "dotdash", color = "gray", size = 1) +  # Línea de referencia al 6%
  theme_minimal()


#-----------------------5---------------------------------
#6 Fraud Detection by BasePolicy & PolicyType

# Agrupación de datos y cálculo del porcentaje de fraude por BasePolicy y PolicyType
fraud_analysis_policy <- data %>%
  group_by(BasePolicy, PolicyType) %>%
  summarise(
    Total_Cases = n(),
    Fraud_Cases = sum(FraudFound_P == 1, na.rm = TRUE),
    Fraud_Percent = round((Fraud_Cases / Total_Cases) * 100, 2)
  ) %>%
  arrange(desc(Fraud_Percent))  # Ordenar por mayor porcentaje de fraude

# Verificar el análisis
print(fraud_analysis_policy)

# Visualización: Heatmap del porcentaje de fraude por BasePolicy y PolicyType
ggplot(fraud_analysis_policy, aes(x = BasePolicy, y = PolicyType, fill = Fraud_Percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray") +
  labs(
    title = "Deteccion de fraude en funcion del poliza base y el tipo de poliza",
    x = "Poliza Base",
    y = "Tipo de poliza",
    fill = "Fraude (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#-----------------------6---------------------------------
# 7 Fraud Detection by Age of Vehicle

# Agrupación de datos y cálculo del porcentaje de fraude por Age of Vehicle
fraud_analysis_vehicle_age <- data %>%
  group_by(AgeOfVehicle) %>%
  summarise(
    Total_Cases = n(),
    Fraud_Cases = sum(FraudFound_P == 1, na.rm = TRUE),
    Fraud_Percent = round((Fraud_Cases / Total_Cases) * 100, 2)
  ) %>%
  arrange(desc(Fraud_Percent))  # Ordenar por mayor porcentaje de fraude

# Verificar el análisis
print(fraud_analysis_vehicle_age)

# Visualización: Barra horizontal del porcentaje de fraude por Age of Vehicle
ggplot(fraud_analysis_vehicle_age, aes(x = Fraud_Percent, y = reorder(AgeOfVehicle, Fraud_Percent))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Detección de fraude en funcion de la edad del vehículo",
    x = "Porcentaje de Fraude",
    y = "Edad del Vehículo"
  ) +
  geom_vline(xintercept = 6, linetype = "dotdash", color = "gray", size = 1) +  # Línea de referencia al 6%
  theme_minimal()




## PARTE 2 -------------------------------------------------------------------------------
## CREAR RECETA PARA PREPROCESAMIENTO Y FEATURE ENGINEERING -----------------------------------
receta <- recipe(FraudFound_P ~ ., data = data_smote) %>%
  
  # Paso 1: Convertir variables AccidentArea, Sex, Fault, PoliceReportFiled, WitnessPresent, AgentType en binarias
  step_mutate(
    AccidentArea = ifelse(AccidentArea == "Urban", 1, 0),
    Sex = ifelse(Sex == "Male", 1, 0),
    Fault = ifelse(Fault == "Policy Holder", 1, 0),
    PoliceReportFiled = ifelse(PoliceReportFiled == "Yes", 1, 0),
    WitnessPresent = ifelse(WitnessPresent == "Yes", 1, 0),
    AgentType = ifelse(AgentType == "Internal", 1, 0)
  ) %>%
  
  # Paso 2: Convertir variables ordinales con label encoding
  step_mutate(
    VehiclePrice = as.integer(factor(VehiclePrice, levels = c(
      "Less than 20,000", "20,000 to 30,000", "30,000 to 40,000", "40,000 to 50,000", "More than 50,000"
    ))),
    AgeOfVehicle = as.integer(factor(AgeOfVehicle, levels = c(
      "< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "> 7 years"
    ))),
    BasePolicy = as.integer(factor(BasePolicy, levels = c("Liability", "Collision", "Comprehensive")))
  ) %>%
  
  # Paso 3: One-hot encoding para todas las variables categóricas restantes
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  
  # Paso 4: Imputación de valores faltantes
  step_impute_median(all_numeric_predictors()) %>%  # Para variables numéricas
  step_impute_mode(all_nominal_predictors()) %>%   # Para variables categóricas
  
  # Paso 5: Remover cualquier columna con varianza cero
  step_zv(all_predictors())
  
## DIVIDIR EN CONJUNTOS DE ENTRENAMIENTO Y PRUEBA ---------------------------------------------
# Dividir el dataset preprocesado en entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
data_split <- initial_split(data_smote, prop = 0.7, strata = FraudFound_P)
train_data <- training(data_split)
test_data <- testing(data_split)

# Verificar distribuciones
cat("Distribución en el conjunto de entrenamiento:\n")
print(table(train_data$FraudFound_P))

cat("\nDistribución en el conjunto de prueba:\n")
print(table(test_data$FraudFound_P))

## PREPARAR Y APLICAR LA RECETA AL CONJUNTO DE ENTRENAMIENTO -----------------------------------
# Preparar y aplicar la receta al conjunto de entrenamiento
preprocessed_train_data <- prep(receta, training = train_data) %>%
  bake(new_data = NULL)  # Transformar train_data usando la receta


## PARTE 3: CREACION DE ALGORITMOS Y WORKFLOWS-------------------------------------------------------
## CREAR MODELOS ----------------------------------------------------------------------------
# Modelo Ridge
ridge_model <- logistic_reg(
  mode = "classification",
  penalty = tune(),  # Hiperparámetro a tunear
  mixture = 0        # Ridge (mixture = 0)
) %>%
  set_engine("glmnet")

# Modelo Lasso
lasso_model <- logistic_reg(
  mode = "classification",
  penalty = tune(),  # Hiperparámetro a tunear
  mixture = 1        # Lasso (mixture = 1)
) %>%
  set_engine("glmnet")

# Modelo Elastic Net
elastic_net_model <- logistic_reg(
  mode = "classification",
  penalty = tune(),  # Hiperparámetro a tunear
  mixture = tune()   # Elastic Net (mixture entre 0 y 1)
) %>%
  set_engine("glmnet")

## CREAR WORKFLOWS ----------------------------------------------------------------------------
# Crear un workflow para cada modelo, utilizando la receta y los conjuntos de entrenamiento
ridge_workflow <- workflow() %>%
  add_recipe(receta) %>%  # Usar la receta creada anteriormente
  add_model(ridge_model)

lasso_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(lasso_model)

elastic_net_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(elastic_net_model)

## CONFIGURAR GRIDS PARA TUNEADO -------------------------------------------------------------
# Grid de valores para los hiperparámetros

# Grid para Ridge (solo `penalty`)
ridge_grid <- grid_regular(
  penalty(range = c(-4, 0)),  # Valores de 10^-4 a 10^0
  levels = 10                # 10 niveles
)

# Grid para Lasso (solo `penalty`)
lasso_grid <- grid_regular(
  penalty(range = c(-4, 0)),  # Valores de 10^-4 a 10^0
  levels = 10                # 10 niveles
)

# Grid para Elastic Net (incluye `penalty` y `mixture`)
elastic_net_grid <- grid_regular(
  penalty(range = c(-4, 0)),  # Valores de 10^-4 a 10^0
  mixture(range = c(0, 1)),   # Valores entre 0 (Ridge) y 1 (Lasso)
  levels = 10                # 10 niveles por dimensión
)


## PARTE 4: VALIDACIÓN CRUZADA Y TUNEADO DE MODELOS ---------------------------------------------

# Configuración de la validación cruzada
set.seed(123)  # Para reproducibilidad
cv_folds <- vfold_cv(train_data, v = 5, strata = FraudFound_P)

# Verificación de los folds creados
cat("Validación cruzada (5 folds):\n")
print(cv_folds)

# Realización del tuneado para cada modelo
## Ridge Model
set.seed(123)
ridge_tuned_results <- tune_grid(
  ridge_workflow,
  resamples = cv_folds,
  grid = ridge_grid,
  metrics = metric_set(roc_auc, accuracy, recall, precision, f_meas)  # Métricas de evaluación
)

## Lasso Model
set.seed(123)
lasso_tuned_results <- tune_grid(
  lasso_workflow,
  resamples = cv_folds,
  grid = lasso_grid,
  metrics = metric_set(roc_auc, accuracy, recall, precision, f_meas)
)

## Elastic Net Model
set.seed(123)
elastic_net_tuned_results <- tune_grid(
  elastic_net_workflow,
  resamples = cv_folds,
  grid = elastic_net_grid,
  metrics = metric_set(roc_auc, accuracy, recall, precision, f_meas)
)

# Mostrar los mejores resultados de cada modelo
cat("\nMejores resultados para Ridge:\n")
show_best(ridge_tuned_results, metric = "roc_auc")

cat("\nMejores resultados para Lasso:\n")
show_best(lasso_tuned_results, metric = "roc_auc")

cat("\nMejores resultados para Elastic Net:\n")
show_best(elastic_net_tuned_results, metric = "roc_auc")

# Seleccionar los mejores hiperparámetros para cada modelo
best_params_ridge <- select_best(ridge_tuned_results, metric = "roc_auc")
best_params_lasso <- select_best(lasso_tuned_results, metric = "roc_auc")
best_params_elastic_net <- select_best(elastic_net_tuned_results, metric = "roc_auc")

## PARTE 5: NUEVA VALIDACIÓN CRUZADA -----------------------------------------------------------

# Nueva validación cruzada con otra partición
set.seed(456)  # Cambiar la semilla
new_cv_folds <- vfold_cv(train_data, v = 5, strata = FraudFound_P)

# Función para evaluar modelos en nueva validación cruzada
evaluar_modelo_cv <- function(workflow, folds, metricas) {
  resultado_cv <- fit_resamples(
    workflow,
    resamples = folds,
    metrics = metricas
  )
  collect_metrics(resultado_cv)
}

# Evaluación de cada modelo
cat("\nEvaluación del modelo Ridge con nueva validación cruzada:\n")
ridge_new_cv_metrics <- evaluar_modelo_cv(finalize_workflow(ridge_workflow, best_params_ridge),
                                          new_cv_folds,
                                          metric_set(roc_auc, accuracy, recall, precision, f_meas))
print(ridge_new_cv_metrics)

cat("\nEvaluación del modelo Lasso con nueva validación cruzada:\n")
lasso_new_cv_metrics <- evaluar_modelo_cv(finalize_workflow(lasso_workflow, best_params_lasso),
                                          new_cv_folds,
                                          metric_set(roc_auc, accuracy, recall, precision, f_meas))
print(lasso_new_cv_metrics)

cat("\nEvaluación del modelo Elastic Net con nueva validación cruzada:\n")
elastic_net_new_cv_metrics <- evaluar_modelo_cv(finalize_workflow(elastic_net_workflow, best_params_elastic_net),
                                                new_cv_folds,
                                                metric_set(roc_auc, accuracy, recall, precision, f_meas))
print(elastic_net_new_cv_metrics)

## PARTE 6: SELECCIÓN DEL MEJOR MODELO Y EVALUACIÓN FINAL --------------------------------------

# Seleccionar el modelo con el mejor roc_auc
mejor_modelo <- list(
  Ridge = ridge_new_cv_metrics,
  Lasso = lasso_new_cv_metrics,
  ElasticNet = elastic_net_new_cv_metrics
) %>%
  purrr::map_dfr(~.x %>% filter(.metric == "roc_auc"), .id = "Modelo") %>%
  arrange(desc(mean))

cat("\nEl mejor modelo es:\n")
print(mejor_modelo[1, ])

# Ajustar el mejor modelo en todo el conjunto de entrenamiento
if (mejor_modelo$Modelo[1] == "Ridge") {
  final_workflow <- finalize_workflow(ridge_workflow, best_params_ridge)
} else if (mejor_modelo$Modelo[1] == "Lasso") {
  final_workflow <- finalize_workflow(lasso_workflow, best_params_lasso)
} else {
  final_workflow <- finalize_workflow(elastic_net_workflow, best_params_elastic_net)
}

# Ajustar el modelo final en el conjunto completo de entrenamiento
set.seed(123)
final_fit <- last_fit(final_workflow, split = data_split)

# Métricas finales en el conjunto de prueba
final_metrics <- collect_metrics(final_fit)
cat("\nMétricas finales en el conjunto de prueba:\n")
print(final_metrics)

## PARTE 7: PREDICCIÓN Y MÉTRICAS SOBRE EL CONJUNTO DE PRUEBA ----------------------------------

# Realizar predicciones sobre el conjunto de prueba
test_predictions <- collect_predictions(final_fit)

# Mostrar las primeras filas de las predicciones
cat("\nPredicciones en el conjunto de prueba:\n")
print(head(test_predictions))

# Calcular métricas de clasificación sobre el conjunto de prueba
test_metrics <- test_predictions %>%
  metrics(truth = FraudFound_P, estimate = .pred_class, .pred_1) %>%
  filter(.metric %in% c("accuracy", "precision", "recall", "f_meas", "roc_auc"))

cat("\nMétricas de clasificación para el conjunto de prueba:\n")
print(test_metrics)

# Crear matriz de confusión
confusion_matrix <- test_predictions %>%
  conf_mat(truth = FraudFound_P, estimate = .pred_class)

cat("\nMatriz de confusión:\n")
print(confusion_matrix)

# Visualización de la matriz de confusión
autoplot(confusion_matrix, type = "heatmap") +
  labs(
    title = "Matriz de Confusión",
    x = "Predicción",
    y = "Real"
  ) +
  theme_minimal()