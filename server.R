
library(readxl)
library(tidyverse)
library(KernSmooth)
library(strucchange)
library(ggplot2)
library(DT)
library(lubridate)
library(markdown)
library(tinytex)
library(broom)
library(forecast)

shinyServer(function(input, output) {
  
  #### Funciones
  
  ## Suavizado simple
  
  single_smooth <- function(data, alpha, h = 1) {
    # Inicializar el nivel con el primer valor de los datos
    level <- data[1]
    smoothed <- c(level)  # Almacena los valores suavizados (fiteados)
    
    # Iterar sobre los datos para calcular el suavizado
    for (i in 2:length(data)) {
      level <- (1 - alpha) * level + alpha * data[i]
      smoothed <- c(smoothed, level)  # Guardar el nuevo valor suavizado
    }
    
    # Pronóstico para los siguientes h períodos (simplemente el último nivel)
    forecasted <- rep(level, h)
    
    # Retornar los valores fiteados y pronosticados
    return(list(fitted = smoothed, forecasted = forecasted))
  }
  
  
  ### Suavizado doble
  holt_smooth <- function(data, alpha, beta, h = 1) {
    n <- length(data)
    
    # Inicialización del nivel y la tendencia
    level <- data[1]
    trend <- data[2] - data[1]
    
    # Inicializar fitted con los valores originales en las dos primeras posiciones
    fitted <- rep(NA, n)
    fitted[1:2] <- data[1:2]
    
    # Ajustar a partir del tercer valor
    for (i in 3:n) {
      fitted[i] <- level + trend
      
      # Actualizar nivel y tendencia
      new_level <- alpha * data[i] + (1 - alpha) * (level + trend)
      new_trend <- beta * (new_level - level) + (1 - beta) * trend
      
      level <- new_level
      trend <- new_trend
    }
    
    # Pronóstico h pasos adelante
    forecasted <- level + trend * (1:h)
    
    return(list(fitted = fitted, forecasted = forecasted))
  }
  
  
  
  
  ### ERROR
  
  # Función de Error MASE
  MASE <- function(data, smoothed) {
    n <- length(data)
    naive <- sum(abs(data[-1] - data[-n])) / (n - 1)
    
    # Evitar división por cero en series constantes
    if (naive == 0) {
      naive <- 1
    }
    
    # Calcular el error MASE
    MASE <- sum(abs(data - smoothed)) / (naive * n)
    return(MASE)
  }
  
  
  
  
  # Definir MASE
  MASE <- function(actual, forecast) {
    n <- length(actual)
    scale <- mean(abs(diff(actual)), na.rm = TRUE)
    error <- mean(abs(actual - forecast), na.rm = TRUE)
    return(error / scale)
  }
  
  # Optimización de alpha y beta
  
  optimize_MASE <- function(data, N_alpha = 100, N_beta = 100) {
    alpha_vals <- seq(0.01, 0.99, length.out = N_alpha)
    beta_vals <- seq(0.01, 0.99, length.out = N_beta)
    
    best_alpha <- NA
    best_beta <- NA
    best_error <- Inf
    
    for (a in alpha_vals) {
      for (b in beta_vals) {
        result <- holt_smooth(data, a, b)
        
        # Evitar los primeros valores si la inicialización es inestable
        error <- MASE(data[3:length(data)], result$fitted[3:length(data)])
        
        if (!is.na(error) && error < best_error) {
          best_alpha <- a
          best_beta <- b
          best_error <- error
        }
      }
    }
    
    return(list(best_alpha = best_alpha, best_beta = best_beta, best_error = best_error))
  }
  
  
  calculate_confidence_intervals <- function(x, fitted_values, residuals, confidence_level = 0.95) {
    n <- length(residuals)  # Número de observaciones
    p <- 2  # Número de parámetros (intercepto y pendiente)
    df <- n - p  # Grados de libertad
    
    # Calcular la varianza de los residuos
    sigma_sq <- sum(residuals^2) / df
    
    # Calcular el error estándar de la predicción
    x_mean <- mean(x)
    Sxx <- sum((x - x_mean)^2)
    se_pred <- sqrt(sigma_sq * (1/n + (x - x_mean)^2 / Sxx))
    
    # Calcular el valor crítico de la distribución t
    t_value <- qt(1 - (1 - confidence_level) / 2, df = df)
    
    # Calcular los intervalos de confianza
    lower <- fitted_values - t_value * se_pred
    upper <- fitted_values + t_value * se_pred
    
    return(data.frame(lower = lower, upper = upper))
  }
  
  ### intervalo para los puntos
  
  calculate_prediction_intervals <- function(forecasted_values, residuals, h, confidence_level = 0.95) {
    n <- length(residuals)
    df <- n - 2  # nivel y tendencia estimados
    
    # Varianza de residuos
    sigma_sq <- sum(residuals^2, na.rm = TRUE) / df
    
    # Valor crítico de t
    t_value <- qt(1 - (1 - confidence_level)/2, df = df)
    
    # Error estándar para cada paso de predicción: sqrt(sigma^2 * (1 + h^2))
    steps_ahead <- 1:h
    SE_pred <- sqrt(sigma_sq * (1 + steps_ahead^2))
    
    # Intervalos para cada punto pronosticado
    lower <- forecasted_values - t_value * SE_pred
    upper <- forecasted_values + t_value * SE_pred
    
    return(data.frame(step = steps_ahead, forecast = forecasted_values,
                      lower = lower, upper = upper))
  }
  
  
  ## intervalo para la linea
  
  calculo_confianza_holt <- function(fitted_values, residuals, confidence_level = 0.95) {
    n <- length(residuals)
    df <- n - 2  # 2 parámetros: nivel y tendencia
    
    # Calcular sigma² (varianza residual)
    sigma_sq <- sum(residuals^2, na.rm = TRUE) / df
    SE_fitted <- sqrt(sigma_sq)  # Error estándar común para los fitted
    
    # Valor crítico de la t de Student
    t_value <- qt(1 - (1 - confidence_level) / 2, df = df)
    
    # Calcular IC para cada valor ajustado
    lower <- fitted_values - t_value * SE_fitted
    upper <- fitted_values + t_value * SE_fitted
    
    return(data.frame(lower = lower, upper = upper))
  }
  
  
  
  ## output para cargar poblacion
  
  output$cargar_poblacion <- renderUI(
    if (input$pregunta == "SI") {
      fileInput('file5', 'Cargar la población para el período de la proyección',
                accept = c(".xlsx"))
    }
  )
   
  
  ## output para cargar brecha
  
  output$cargar_brecha <- renderUI(
    if (input$pregunta_brecha == "SI") {
      numericInput('brecha', 'Brecha estimada (%)',
                   min = 0, max = 99.99, step = 1,value = 1)
    }
  )
  
  
  ## output para cargar reducir brecha
  
  output$cargar_reduccion <- renderUI(
    if (input$pregunta_reduccion == "SI") {
      fluidPage(
      numericInput('reduccion', 'Reducción de la brecha estimada (%)',
                   min = 0, max = 99.99, step = 1,value = 1),
      tags$h5(tags$b("Período de la reducción:")),
      fluidRow(column(6,numericInput('inicio_reduccion', 'Año inicial',
                                     min = 1900, max = 2100, step = 1,value = year(Sys.Date())+1)),
               column(6,numericInput('final_reduccion', 'Año final',
                                     min = 1900, max = 2100, step = 1,value = year(Sys.Date())+4)))
      )
    }
  )
  
  df_pdf <- reactiveVal()
  
  df <- reactive({
    file <- input$tabla1
    df_pob <- read_excel(file$datapath) %>%
    mutate(across(c(AÑO, POBLACION, CASOS, TASA), as.numeric))
    df_pdf(data.frame(df_pob))
    return(df_pob)
  }
  )
  
  pob_proyec <- reactive({
    req(input$file5)
    file <- input$file5
    read_excel(file$datapath)
  }
  )
  
  texto_final <- reactiveVal()
  
  figura2 <- reactive({
    isolate({
      
      req(input$lugar)
      req(input$inicio)
      req(input$final)
      req(input$submitbutton)
      
      if (input$pregunta == "SI") {
        
        req(input$file5)
      }
      
      
      if (input$pregunta_brecha == "SI") {
        
        req(input$brecha)
      }
      
      
      if (input$pregunta_reduccion == "SI") {
        
        req(input$reduccion,input$inicio_reduccion,input$final_reduccion)
      }
      
      df <- df()
      data <- data.frame(AÑO = df$AÑO, TASA = df$TASA)
      
      ####
      
      anio_pred_inicio_orig <- input$inicio
      anio_pred_final_orig <- input$final
      not_report_inicial <- input$brecha/100
      not_report_reduccion <- input$reduccion
      not_report_final <- not_report_inicial*not_report_reduccion/100
      year_inicio_report <- input$inicio_reduccion
      year_fin_report <- input$final_reduccion
      
      ####
      
      ## PROYECCION
      
      ## PASO 1: IDENTIFICAR SI HAY UNA TENDENCIA CONTINUA O HAY PUNTOS DE QUIEBRE
      
      # Lista para almacenar los valores de BIC
      
      bic_values <- data.frame(Punto_Quiebre = integer(), BIC = numeric())
      
      # Iterar sobre cada año como punto de quiebre
      
      for (punto_quiebre in df$AÑO) {
        # Crear una variable indicadora para el punto de quiebre
        df$indicator <- ifelse(df$AÑO >= punto_quiebre, 1, 0)
        
        # Crear una variable interactiva para el modelo segmentado
        df$interaction <- (df$AÑO - punto_quiebre) * df$indicator
        
        # Ajustar el modelo segmentado
        modelo_segmentado <- lm(TASA ~ AÑO + indicator + interaction, data = df)
        
        # Calcular el BIC
        bic_value <- BIC(modelo_segmentado)
        
        # Guardar los resultados en el data frame
        bic_values <- rbind(bic_values, data.frame(Punto_Quiebre = punto_quiebre, BIC = bic_value))
      }
      
      posicion_quiebre <- which.min(bic_values$BIC)
      
      año_quiebre <- bic_values$Punto_Quiebre[which.min(bic_values$BIC)]  
      
      
      # Prueba de Chow para punto de quiebre conocido (x=50)
      
      #Chow <- sctest(df$TASA ~ df$AÑO, data = df, type = "Chow", point = posicion_quiebre)
      
      
      resultado_chow <- tryCatch({
        chow_test <- sctest(df$TASA ~ df$AÑO, data = df, type = "Chow", point = posicion_quiebre)
        # Si no hay error, devuelve el p-value
        chow_test$p.value
      }, error = function(e) {
        # Si hay error, devuelve un valor que indique fallo (por ejemplo, NA o un valor específico)
        return(NA)
      })
      
      
      if (is.na(resultado_chow) || resultado_chow >= 0.05) {
        
        
        #if (Chow$p.value >= 0.05) {
        
        # Cargar y aplicar suavizado con Kernel gaussiano
        
        h_optimo <- bw.nrd0(df$TASA)
        
        
        # Suavizado con kernel gaussiano
        
        suavizado <- locpoly(
          x = df$AÑO,
          y = df$TASA,
          bandwidth = h_optimo,  # Ajusta este valor (controla el suavizado)
          gridsize = length(df$TASA),
          degree = 1  # Para suavizado, no ajuste polinomial
        )
        
        # Valores suavizados
        
        #datos_suavizados <- suavizado$y
        
        
        ## APLICACION DE SUAVIZADO EXPONENCIAL (SIMPLE O DOBLE -HOLT- SEGÚN SI TIENE O NO TENDENCIA)
        
        BB <- optimize_MASE(suavizado$y)
        
        ## VERIFICAR SI HAY TENDENCIA O NO PARA DEFINIR EL ALFA Y EL BETA
        
        ajuste <- lm(df$TASA ~ df$`AÑO`)
        
        resultado_ajuste <- summary(ajuste)
        
        if (resultado_ajuste$coefficients[2,4] >= 0.05) {
          
          Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
          
        } else {
          
          Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
          
        }
        
        
        ## CALCULO DE LOS RESIDUOS ENTRE LOS DATOS SUAVIZADOS Y EL SUAVIZADO EXPONENCIAL
        
        residuals2 <- suavizado$y- Período2$fitted
        
        
        
        ## HACER LA PROYECCION
        
        ## DEFINICION DEL PERIODO PARA LA PROYECCION 2025-2029
        
        anio_pred_orig <- anio_pred_inicio_orig:anio_pred_final_orig
        
        if (resultado_ajuste$coefficients[2,4] >= 0.05) {
          
          Período2_2 <- holt(df$TASA,h = length(anio_pred_orig),
                             alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
          
        } else {
          
          Período2_2 <- holt(df$TASA,h = length(anio_pred_orig),
                             alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
          
        }
        
        ## HACER LA PROYECCION USANDO LA FUNCION DE CONFIANZA HOLT
        
        ci2_2_pred<- calculo_confianza_holt(Período2_2$mean, Período2$residuals, confidence_level = 0.95)
        
        
        ## CREACION DEL DATA FRAME
        
        ### AGREGADO BRECHA Y SUBREGISTRO
        
        if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "SI") {
          
          periodo <- length(c(year_inicio_report:year_fin_report))
          
          if(not_report_inicial <= 0 || not_report_final <= 0) {
            stop("Los valores inicial y final deben ser positivos")
          }
          
          rate_report <- -log(not_report_final/not_report_inicial)/periodo
          
          i <- 0:periodo
          
          faqtor <- 1 + (not_report_inicial-(not_report_inicial*exp(-rate_report*i)))
          
        } else if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "NO") {
          
          faqtor <- 1
          
        } else {
          
          faqtor <- 1
          
        }
        
        
        ci2_2_pred_2 <- calculate_prediction_intervals(Período2_2$mean,Período2$residuals,
                                                       as.numeric(length(anio_pred_orig)),confidence_level = 0.95)
        
        
        confianza_futura_orig <- data.frame(AÑO = anio_pred_orig, 
                                            tasa_proyectada = Período2_2$mean*faqtor,
                                            LI_orig = ci2_2_pred$lower*faqtor,
                                            LS_orig = ci2_2_pred$upper*faqtor,
                                            LI_puntos = ci2_2_pred_2$lower*faqtor,
                                            LS_puntos = ci2_2_pred_2$upper*faqtor)
        
        
        ### GRAFICO PROYECCION SIN CAMBIOS
        
        limit_y_max <- if (max(df$TASA) > max(confianza_futura_orig$LS_puntos)) {
          
          max(df$TASA) } else {
            
            max(confianza_futura_orig$LS_puntos) }
        
        
        grafico <- ggplot(df, aes(x = AÑO)) +
          geom_line(aes(y = TASA), color = "blue", linewidth = 1, linetype = "solid") +
          geom_line(data = confianza_futura_orig, aes(x = AÑO, y = tasa_proyectada),
                    color = "darkblue", size = 1, linetype = "dashed") +
          geom_ribbon(data = subset(confianza_futura_orig, AÑO <= 2029), 
                      aes(ymin = LI_orig, ymax = LS_orig), fill = "lightblue", alpha = 0.3) +
          geom_ribbon(data = subset(confianza_futura_orig, AÑO <= 2029), 
                      aes(ymin = LI_puntos, ymax = LS_puntos), fill = "gold", alpha = 0.3) +
          labs(title = paste0("Tendencia de la tasa de notificación de tuberculosis en ", input$lugar,", ",min(df$AÑO), "-", max(df$AÑO),",\n",
                              "proyección ",min(confianza_futura_orig$AÑO), "-", max(confianza_futura_orig$AÑO),", y región de confianza del 95% de la proyección"),
               x = "Año",
               y = "Tasa") +
          scale_x_continuous(breaks = seq(min(df$AÑO, na.rm = TRUE), max(confianza_futura_orig$AÑO, na.rm = TRUE), by = 1)) +
          scale_y_continuous(limits = c(0, limit_y_max*1.1)) + 
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                axis.text = element_text(size = 14, face = "bold"),
                axis.title = element_text(size = 14, face = "bold"))
        
      } else {
        
        
        ### CALCULOS A PARTIR DEL PUNTO DE QUIEBRE
        
        df_2 <- df %>% 
          dplyr::filter(AÑO >= año_quiebre)
        
        h_optimo <- bw.nrd0(df_2$TASA)
        
        
        # Suavizado con kernel gaussiano
        
        suavizado <- locpoly(
          x = df_2$AÑO,
          y = df_2$TASA,
          bandwidth = h_optimo,  # Ajusta este valor (controla el suavizado)
          gridsize = length(df_2$TASA),
          degree = 1  # Para suavizado, no ajuste polinomial
        )
        
        # Valores suavizados
        
        #datos_suavizados <- suavizado$y
        
        
        ## APLICACION DE SUAVIZADO EXPONENCIAL (SIMPLE O DOBLE -HOLT- SEGÚN SI TIENE O NO TENDENCIA)
        
        BB <- optimize_MASE(suavizado$y)
        
        ajuste <- lm(df_2$TASA ~ df_2$`AÑO`)
        
        resultado_ajuste <- summary(ajuste)
        
        if (resultado_ajuste$coefficients[2,4] >= 0.05) {
          
          Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
          
        } else {
          
          Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
          
        }
        
        
        ## CALCULO DE LOS RESIDUOS ENTRE LOS DATOS SUAVIZADOS Y EL SUAVIZADO EXPONENCIAL
        
        residuals2 <- suavizado$y - Período2$fitted
        
        
        
        ## HACER LA PROYECCION
        
        ## DEFINICION DEL PERIODO PARA LA PROYECCION 2025-2029
        
        anio_pred_orig <- anio_pred_inicio_orig:anio_pred_final_orig
        
        if (resultado_ajuste$coefficients[2,4] >= 0.05) {
          
          Período2_2 <- holt(df_2$TASA,h = length(anio_pred_orig),
                             alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
          
        } else {
          
          Período2_2 <- holt(df_2$TASA,h = length(anio_pred_orig),
                             alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
          
        }
        
        ci2_2_pred<- calculo_confianza_holt(Período2_2$mean, Período2$residuals, confidence_level = 0.95)
        
        
        ### AGREGADO BRECHA Y SUBREGISTRO
        
        if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "SI") {
          
          periodo <- length(c(year_inicio_report:year_fin_report))
          
          if(not_report_inicial <= 0 || not_report_final <= 0) {
            stop("Los valores inicial y final deben ser positivos")
          }
          
          rate_report <- -log(not_report_final/not_report_inicial)/periodo
          
          i <- 0:periodo
          
          faqtor <- 1 + (not_report_inicial-(not_report_inicial*exp(-rate_report*i)))
          
        } else if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "NO") {
          
          faqtor <- 1
          
        } else {
          
          faqtor <- 1
          
        }
        
        ci2_2_pred_2 <- calculate_prediction_intervals(Período2_2$mean,Período2$residuals,
                                                       as.numeric(length(anio_pred_orig)),confidence_level = 0.95)
        
        
        confianza_futura_orig <- data.frame(AÑO = anio_pred_orig, 
                                            tasa_proyectada = Período2_2$mean*faqtor,
                                            LI_orig = ci2_2_pred$lower*faqtor,
                                            LS_orig = ci2_2_pred$upper*faqtor,
                                            LI_puntos = ci2_2_pred_2$lower*faqtor,
                                            LS_puntos = ci2_2_pred_2$upper*faqtor)
        
        
        ### GRAFICO PROYECCION SIN CAMBIOS
        
        limit_y_max <- if (max(df$TASA) > max(confianza_futura_orig$LS_puntos)) {
          
          max(df$TASA) } else {
            
            max(confianza_futura_orig$LS_puntos) }
        
        
        grafico <- ggplot(df, aes(x = AÑO)) +
          geom_line(aes(y = TASA), color = "blue", linewidth = 1, linetype = "solid") +
          geom_line(data = confianza_futura_orig, aes(x = AÑO, y = tasa_proyectada),
                    color = "darkblue", size = 1, linetype = "dashed") +
          geom_ribbon(data = subset(confianza_futura_orig, AÑO <= 2029), 
                      aes(ymin = LI_orig, ymax = LS_orig), fill = "lightblue", alpha = 0.8) +
          geom_ribbon(data = subset(confianza_futura_orig, AÑO <= 2029), 
                      aes(ymin = LI_puntos, ymax = LS_puntos), fill = "gold", alpha = 0.3) +
          labs(title = paste0("Tendencia de la tasa de notificación de tuberculosis en ", input$lugar,", ",min(df$AÑO), "-", max(df$AÑO),",\n",
                              "proyección ",min(confianza_futura_orig$AÑO), "-", max(confianza_futura_orig$AÑO),", y región de confianza del 95% de la proyección"),
               x = "Año",
               y = "Tasa") +
          scale_x_continuous(breaks = seq(min(df$AÑO, na.rm = TRUE), max(confianza_futura_orig$AÑO, na.rm = TRUE), by = 1)) +
          scale_y_continuous(limits = c(0, limit_y_max*1.1)) + 
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                axis.text = element_text(size = 14, face = "bold"),
                axis.title = element_text(size = 14, face = "bold"))
        
        
      } # cierre else
      
      ### textos para reporte en pdf
      
      periodo <- paste0(min(df$AÑO),"-",max(df$AÑO))
      lugar <- input$lugar
      
      # Crear boxes con valores en negrita
      
      periodo <- paste0(min(df$AÑO), "-", max(df$AÑO))
      lugar <- input$lugar
      
      Box_1 <- paste0("Durante el período **", periodo, "** la tuberculosis en **", lugar,
                      "** pasó de una tasa de **", round(first(df$TASA), 2),
                      "** por 100000 habitantes a una tasa de **", round(last(df$TASA), 2),
                      "** por 100000 habitantes.")
      
      # Box 2
      if (is.na(resultado_chow) || resultado_chow >= 0.05) {
        VAP_1 <- lm(log(df$TASA) ~ df$`AÑO`)
        n_1 = length(df$TASA[complete.cases(df$TASA)])
        tcritico_1 = qt(0.975, n_1 - 2)
        beta_1 = c(summary(VAP_1)$coefficients[2, 1],
                   summary(VAP_1)$coefficients[2, 1] - tcritico_1 * summary(VAP_1)$coefficients[2, 2],
                   summary(VAP_1)$coefficients[2, 1] + tcritico_1 * summary(VAP_1)$coefficients[2, 2])
        VAP_1_valores <- round(((-1 + exp(beta_1)) * 100), 2)
        VAP_1_VC = VAP_1_valores[1]
        VAP_1_LI = VAP_1_valores[2]
        VAP_1_LS = VAP_1_valores[3]
        p_VAP_1 <- formatC(glance(VAP_1)$p.value, format = "f", digits = 4)
        
        Box_2 <- paste0("No se identificó ningún punto de quiebre durante el período **", periodo,
                        "**; y, la tasa tuvo una variación anual promedio de **", VAP_1_VC,
                        "%**; IC 95% **", VAP_1_LI, "%**; **", VAP_1_LS, "%**; p = **", p_VAP_1, "**.")
      } else {
        data <- df %>% dplyr::filter(AÑO >= año_quiebre)
        VAP_1 <- lm(log(data$TASA) ~ data$`AÑO`)
        n_1 = length(data$TASA[complete.cases(data$TASA)])
        tcritico_1 = qt(0.975, n_1 - 2)
        beta_1 = c(summary(VAP_1)$coefficients[2, 1],
                   summary(VAP_1)$coefficients[2, 1] - tcritico_1 * summary(VAP_1)$coefficients[2, 2],
                   summary(VAP_1)$coefficients[2, 1] + tcritico_1 * summary(VAP_1)$coefficients[2, 2])
        VAP_1_valores <- round(((-1 + exp(beta_1)) * 100), 2)
        VAP_1_VC = VAP_1_valores[1]
        VAP_1_LI = VAP_1_valores[2]
        VAP_1_LS = VAP_1_valores[3]
        p_VAP_1 <- formatC(glance(VAP_1)$p.value, format = "f", digits = 4)
        periodo <- paste0(min(data$AÑO), "-", max(data$AÑO))
        
        Box_2 <- paste0("Durante el período **", periodo, "** se identificó un punto de quiebre en **",
                        año_quiebre, "**: p = **", round(chow_test$p.value, 4),
                        "**; y, la tasa tuvo una variación anual promedio de **", VAP_1_VC,
                        "%**; IC 95% **", VAP_1_LI, "%**; **", VAP_1_LS, "%**; p = **", p_VAP_1,
                        "** durante el período **", min(data$AÑO), "-", max(data$AÑO), "**.")
      }
      
      Box_3 <- paste0("Para la proyección del período **", anio_pred_inicio_orig, "-",
                      anio_pred_final_orig, "** se utilizó el método de Holt sobre los datos observados para el período **",
                      periodo, "**; considerando un valor alfa de **", round(BB$best_alpha, 2),
                      "** y un valor beta de **", round(BB$best_beta, 2),
                      "**, calculados utilizando el Error Absoluto Medio Escalonado (MASE, por su sigla en inglés).")
      
      Box_4 <- paste0("Para calcular la región de confianza de la proyección para el período **",
                      anio_pred_inicio_orig, "-", anio_pred_final_orig,
                      "** se realizó en primer lugar una regresión no paramétrica con kernel gaussiano de los datos observados para el período **",
                      periodo, "**; y se obtuvo una serie de datos suavizados.")
      
      Box_5 <- paste0("En segundo lugar, se utilizó el método de Holt sobre esos datos suavizados; considerando un valor alfa de **",
                      round(BB$best_alpha, 2), "** y un valor beta de **", round(BB$best_beta, 2),
                      "**; y se calculó una nueva serie que se utilizó para calcular los residuos. La variabilidad de los residuos se utilizó para calcular la región de confianza de los datos proyectados.")
      
      Box_6 <- paste0("En la figura siguiente se muestra la tendencia de la notificación de casos de tuberculosis en **",
                      lugar, "** para el período **", periodo,
                      "**, la proyección de los valores observados para el período **",
                      anio_pred_inicio_orig, "-", anio_pred_final_orig,
                      "** y la región de confianza del 95% de la proyección.")
      
      Box_7 <- paste0("En la tabla siguiente se muestran los valores de la tasa proyectada para el período **",
                      anio_pred_inicio_orig, "-", anio_pred_final_orig,
                      "**, el límite inferior y superior de la región de confianza del 95% para la tasa; y los casos estimados para esas tasas a partir de la proyección de la población.")
      
      # Unir todos los textos en párrafos
      texto_final(paste(Box_1, Box_2, Box_3, Box_4, Box_5, Box_6, Box_7, sep = "\n\n"))
      
      
      ### fin textos para pdf
      
      return(grafico)
      
    }) # cierre isolate
    
  }) # cierre reactive figura
  
  figura1 <- reactive({
    
    df <- df()
    
    ggplot(df, aes(x = AÑO)) +
      geom_line(aes(y = TASA), color = "blue", linewidth = 1, linetype = "solid") +
      labs(title = paste0("Tendencia de la tasa de notificación de tuberculosis en ", input$lugar,", ",min(df$AÑO), "-", max(df$AÑO),",\n",
                          " y línea de mejor ajuste."),
           x = "Año",
           y = "Tasa") +
      scale_x_continuous(breaks = seq(min(df$AÑO, na.rm = TRUE), max(df$AÑO, na.rm = TRUE), by = 1)) +
      scale_y_continuous(limits = c(0, max(df$TASA)*1.2)) + 
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"))
    
  })
  
  observeEvent(input$submitbutton, {
    
  output$figura <- renderPlot({
      figura2()
    })
  })
  
  #### FIGURA 1 REPORTE PDF
  
  output$reporte_pdf <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Generate first plot (figura1)
      p1 <- figura1()
      plot_file1 <- tempfile(fileext = ".png")
      ggsave(plot_file1, plot = p1, width = 8, height = 5, dpi = 300)
      plot_file1 <- normalizePath(plot_file1, winslash = "/")
      
      # Generate second plot (figura2)
      p2 <- figura2()
      plot_file2 <- tempfile(fileext = ".png")
      ggsave(plot_file2, plot = p2, width = 8, height = 5, dpi = 300)
      plot_file2 <- normalizePath(plot_file2, winslash = "/")
      req(input$lugar)
      lugar <- input$lugar
      
      # Render report with both plots
      result <- tryCatch({
        rmarkdown::render(
          input = "report_template.Rmd",
          output_file = tempfile(fileext = ".pdf"),
          params = list(
            plot_file1 = plot_file1,
            plot_file2 = plot_file2,
            summary_text = texto_final(),
            lugar = lugar,
            tabla1_reporte = df(),
            tabla2_reporte = tabla2_reporte()
          ),
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        print(paste("Error al renderizar Rmd:", e$message))
        NULL
      })
      
      if (is.null(result) || !file.exists(result)) {
        stop("Error: El archivo PDF no se generó correctamente.")
      }
      
      file.copy(from = result, to = file, overwrite = TRUE)
    }
  )
  
  tabla2_reporte <- reactiveVal()
  
  observeEvent(input$submitbutton, {
    
  output$tabla <- renderDT({
    
    isolate({
    
    req(input$lugar)
    req(input$inicio)
    req(input$final)
    req(input$submitbutton)
    
    if (input$pregunta == "SI") {
      
      req(input$file5)
    }
    
    
    if (input$pregunta_brecha == "SI") {
      
      req(input$brecha)
    }
    
    
    if (input$pregunta_reduccion == "SI") {
      
      req(input$reduccion,input$inicio_reduccion,input$final_reduccion)
    }
    
    df <- df()
    data <- data.frame(AÑO = df$AÑO, TASA = df$TASA)
    
    ####
    anio_pred_inicio_orig <- input$inicio
    anio_pred_final_orig <- input$final
    not_report_inicial <- input$brecha/100
    not_report_reduccion <- input$reduccion
    not_report_final <- not_report_inicial*not_report_reduccion/100
    year_inicio_report <- input$inicio_reduccion
    year_fin_report <- input$final_reduccion
    ####
    
    ## PROYECCION
    
    ## PASO 1: IDENTIFICAR SI HAY UNA TENDENCIA CONTINUA O HAY PUNTOS DE QUIEBRE
    
    # Lista para almacenar los valores de BIC
    
    bic_values <- data.frame(Punto_Quiebre = integer(), BIC = numeric())
    
    # Iterar sobre cada año como punto de quiebre
    
    for (punto_quiebre in df$AÑO) {
      # Crear una variable indicadora para el punto de quiebre
      df$indicator <- ifelse(df$AÑO >= punto_quiebre, 1, 0)
      
      # Crear una variable interactiva para el modelo segmentado
      df$interaction <- (df$AÑO - punto_quiebre) * df$indicator
      
      # Ajustar el modelo segmentado
      modelo_segmentado <- lm(TASA ~ AÑO + indicator + interaction, data = df)
      
      # Calcular el BIC
      bic_value <- BIC(modelo_segmentado)
      
      # Guardar los resultados en el data frame
      bic_values <- rbind(bic_values, data.frame(Punto_Quiebre = punto_quiebre, BIC = bic_value))
    }
    
    posicion_quiebre <- which.min(bic_values$BIC)
    
    año_quiebre <- bic_values$Punto_Quiebre[which.min(bic_values$BIC)]  
    
    # Prueba de Chow para punto de quiebre conocido (x=50)
    #Chow <- sctest(df$TASA ~ df$AÑO, data = df, type = "Chow", point = posicion_quiebre)
    
    resultado_chow <- tryCatch({
      chow_test <- sctest(df$TASA ~ df$AÑO, data = df, type = "Chow", point = posicion_quiebre)
      # Si no hay error, devuelve el p-value
      chow_test$p.value
    }, error = function(e) {
      # Si hay error, devuelve un valor que indique fallo (por ejemplo, NA o un valor específico)
      return(NA)
    })
    
    
    if (is.na(resultado_chow) || resultado_chow >= 0.05) {
    
    #if (Chow$p.value >= 0.05) {
      
      # Cargar y aplicar suavizado con Kernel gaussiano
      
      h_optimo <- dpill(df$TASA,df$AÑO)
      
      # Suavizado con kernel gaussiano
      
      suavizado <- locpoly(
        x = df$AÑO,
        y = df$TASA,
        bandwidth = h_optimo,  # Ajusta este valor (controla el suavizado)
        gridsize = length(df$TASA),
        degree = 1  # Para suavizado, no ajuste polinomial
      )
      
      # Valores suavizados
      
      #datos_suavizados <- suavizado$y
      
      ## APLICACION DE SUAVIZADO EXPONENCIAL (SIMPLE O DOBLE -HOLT- SEGÚN SI TIENE O NO TENDENCIA)
      
      BB <- optimize_MASE(suavizado$y)
      
      ## VERIFICAR SI HAY TENDENCIA O NO PARA DEFINIR EL ALFA Y EL BETA
      
      ajuste <- lm(df$TASA ~ df$`AÑO`)
      
      resultado_ajuste <- summary(ajuste)
      
      if (resultado_ajuste$coefficients[2,4] >= 0.05) {
        
        Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
        
      } else {
        
        Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
        
      }
      
      ## CALCULO DE LOS RESIDUOS ENTRE LOS DATOS SUAVIZADOS Y EL SUAVIZADO EXPONENCIAL
      
      residuals2 <- suavizado$y - Período2$fitted
      
      ## HACER LA PROYECCION
      
      ## DEFINICION DEL PERIODO PARA LA PROYECCION 2025-2029
      
      anio_pred_orig <- anio_pred_inicio_orig:anio_pred_final_orig
      
      if (resultado_ajuste$coefficients[2,4] >= 0.05) {
        
        Período2_2 <- holt(df$TASA,h = length(anio_pred_orig),
                           alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
        
      } else {
        
        Período2_2 <- holt(df$TASA,h = length(anio_pred_orig),
                           alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
        
      }
      
      
      ## HACER LA PROYECCION USANDO LA FUNCION DE CONFIANZA HOLT
      
      ci2_2_pred<- calculo_confianza_holt(Período2_2$mean, Período2$residuals, confidence_level = 0.95)
      
      
      ## CREACION DEL DATA FRAME
      
      ### AGREGADO BRECHA Y SUBREGISTRO
      
      if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "SI") {
        
        periodo <- length(c(year_inicio_report:year_fin_report))
        
        if(not_report_inicial <= 0 || not_report_final <= 0) {
          stop("Los valores inicial y final deben ser positivos")
        }
        
        rate_report <- -log(not_report_final/not_report_inicial)/periodo
        
        i <- 0:periodo
        
        faqtor <- 1 + (not_report_inicial-(not_report_inicial*exp(-rate_report*i)))
        
      } else if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "NO") {
        
        faqtor <- 1
        
      } else {
        
        faqtor <- 1
        
      }
      
      
      ci2_2_pred_2 <- calculate_prediction_intervals(Período2_2$mean,Período2$residuals,
                                                     as.numeric(length(anio_pred_orig)),confidence_level = 0.95)
      
      
      confianza_futura_orig <- data.frame(AÑO = anio_pred_orig, 
                                          tasa_proyectada = Período2_2$mean*faqtor,
                                          LI_orig = ci2_2_pred$lower*faqtor,
                                          LS_orig = ci2_2_pred$upper*faqtor,
                                          LI_puntos = ci2_2_pred_2$lower*faqtor,
                                          LS_puntos = ci2_2_pred_2$upper*faqtor)
      
    } else {
      
      
      ### CALCULOS A PARTIR DEL PUNTO DE QUIEBRE
      
      df_2 <- df %>% 
        dplyr::filter(AÑO >= año_quiebre)
      
      h_optimo <- bw.nrd0(df_2$TASA)
      
      
      # Suavizado con kernel gaussiano
      
      suavizado <- locpoly(
        x = df_2$AÑO,
        y = df_2$TASA,
        bandwidth = h_optimo,  # Ajusta este valor (controla el suavizado)
        gridsize = length(df_2$TASA),
        degree = 1  # Para suavizado, no ajuste polinomial
      )
      
      
      ## APLICACION DE SUAVIZADO EXPONENCIAL (SIMPLE O DOBLE -HOLT- SEGÚN SI TIENE O NO TENDENCIA)
      
      BB <- optimize_MASE(suavizado$y)
      
      ## VERIFICAR SI HAY TENDENCIA O NO PARA DEFINIR EL ALFA Y EL BETA
      
      ajuste <- lm(df_2$TASA ~ df_2$`AÑO`)
      
      resultado_ajuste <- summary(ajuste)
      
      if (resultado_ajuste$coefficients[2,4] >= 0.05) {
        
        Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
        
      } else {
        
        Período2 <- holt(suavizado$y,h = 5,alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
        
      }
      
      
      ## CALCULO DE LOS RESIDUOS ENTRE LOS DATOS SUAVIZADOS Y EL SUAVIZADO EXPONENCIAL
      
      residuals2 <- suavizado$y- Período2$fitted
      
      
      
      ## HACER LA PROYECCION
      
      ## DEFINICION DEL PERIODO PARA LA PROYECCION 2025-2029
      
      anio_pred_orig <- anio_pred_inicio_orig:anio_pred_final_orig
      
      if (resultado_ajuste$coefficients[2,4] >= 0.05) {
        
        Período2_2 <- holt(df_2$TASA,h = length(anio_pred_orig),
                           alpha = BB$best_alpha,beta = 0.001,optim = FALSE)
        
      } else {
        
        Período2_2 <- holt(df_2$TASA,h = length(anio_pred_orig),
                           alpha = BB$best_alpha,beta = BB$best_beta,optim = FALSE)
        
      }
      
      ## HACER LA PROYECCION USANDO LA FUNCION DE CONFIANZA HOLT
      
      ci2_2_pred<- calculo_confianza_holt(Período2_2$mean, Período2$residuals, confidence_level = 0.95)
      
      
      ### PROYECCION DE LOS DATOS ORIGINALES SIN KERNEL GAUSSIANO
      
      # BB <- optimize_MASE(data$TASA)
      # Período2_2 <- holt_smooth(data$TASA, BB$best_alpha, BB$best_beta, length(anio_pred_orig))
      # ci2_2_pred <- calculo_confianza_holt(Período2_2$forecasted,
      #                                       residuals2,0.95)
      
      ### AGREGADO BRECHA Y SUBREGISTRO
      
      if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "SI") {
        
        periodo <- length(c(year_inicio_report:year_fin_report))
        
        if(not_report_inicial <= 0 || not_report_final <= 0) {
          stop("Los valores inicial y final deben ser positivos")
        }
        
        rate_report <- -log(not_report_final/not_report_inicial)/periodo
        
        i <- 0:periodo
        
        faqtor <- 1 + (not_report_inicial-(not_report_inicial*exp(-rate_report*i)))
        
      } else if (input$pregunta_brecha == "SI" & input$pregunta_reduccion == "NO") {
        
        faqtor <- 1
        
      } else {
        
        faqtor <- 1
        
      }
      
      ci2_2_pred_2 <- calculate_prediction_intervals(Período2_2$mean,Período2$residuals,
                                                     as.numeric(length(anio_pred_orig)),confidence_level = 0.95)
      
      confianza_futura_orig <- data.frame(AÑO = anio_pred_orig, 
                                          tasa_proyectada = as.numeric(Período2_2$mean)*faqtor,
                                          LI_orig = ci2_2_pred$lower*faqtor,
                                          LS_orig = ci2_2_pred$upper*faqtor,
                                          LI_puntos = ci2_2_pred_2$lower*faqtor,
                                          LS_puntos = ci2_2_pred_2$upper*faqtor)
      
      
    }
    
    ### DEFINICION DE LA POBLACIÓN A USAR PARA LA PROYECCION
    
    if (input$pregunta == "NO") {
      
      data_pob <- df %>% 
        dplyr::filter(`AÑO` >= min(df$AÑO) & `AÑO` <= max(df$AÑO))
      
      h_optimo_pob <- bw.nrd0(data_pob$POBLACION)
      
      suavizado_pob <- locpoly(
        x = as.numeric(data_pob$AÑO),
        y = data_pob$POBLACION,
        bandwidth = h_optimo_pob,  # Ajusta este valor (controla el suavizado)
        gridsize = length(data_pob$POBLACION),
        degree = 1  # Para suavizado, no ajuste polinomial
      )
      
      datos_suavizados_pob <- suavizado_pob$y
      
      BB_pob <- optimize_MASE(datos_suavizados_pob)
      
      
      Período2_pob <- holt_smooth(datos_suavizados_pob,
                                  BB_pob$best_alpha,
                                  BB_pob$best_beta,
                                  length(c(anio_pred_inicio_orig:anio_pred_final_orig)))
      
      data_pob <- data.frame(AÑO = c(anio_pred_inicio_orig:anio_pred_final_orig), 
                             POBLACION = round(Período2_pob$forecasted,0))
      
      
    } else {
      
      data_pob <- pob_proyec() %>% 
        dplyr::filter(`AÑO` >= anio_pred_inicio_orig & `AÑO` <= anio_pred_final_orig)
      
    }
    
    ### CONFECCION DE LA TABLA DE DATOS DE LA PROYECCION
    
    datos <- data.frame(AÑO = data_pob$AÑO,
                        POBLACION = round(data_pob$POBLACION,0),
                        `TASA PROYECTADA` = round(as.numeric(Período2_2$mean)*faqtor,2),
                        `LÍMITE INFERIOR TASA` = round(ci2_2_pred$lower*faqtor,2),
                        `LÍMITE SUPERIOR TASA` = round(ci2_2_pred$upper*faqtor,2),
                        `CASOS PROYECTADOS` = round(as.numeric(Período2_2$mean)*faqtor*data_pob$POBLACION/100000,0),
                        `LÍMITE INFERIOR CASOS` = round(ci2_2_pred$lower*faqtor*data_pob$POBLACION/100000,0),
                        `LÍMITE SUPERIOR CASOS` = round(ci2_2_pred$upper*faqtor*data_pob$POBLACION/100000,0),
                        `LÍMITE INFERIOR TASA (puntos)` = round(ci2_2_pred_2$lower*faqtor,2),
                        `LÍMITE SUPERIOR TASA (puntos)` = round(ci2_2_pred_2$upper*faqtor,2),
                        `LÍMITE INFERIOR CASOS (puntos)` = round(ci2_2_pred_2$lower*faqtor*data_pob$POBLACION/100000,0),
                        `LÍMITE SUPERIOR CASOS (puntos)` = round(ci2_2_pred_2$upper*faqtor*data_pob$POBLACION/100000,0))
    
    datos_transpuesto <- datos %>%
      pivot_longer(
        cols = -AÑO,
        names_to = "Variable",
        values_to = "Valor"
      ) %>%
      pivot_wider(
        names_from = AÑO,
        values_from = Valor
      )
    
    datos_transpuesto$Variable[datos_transpuesto$Variable == "TASA.PROYECTADA"] <- "Tasa proyectada"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.INFERIOR.TASA"] <- "Límite inferior tasa (recta)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.SUPERIOR.TASA"] <- "Límite superior tasa (recta)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "CASOS.PROYECTADOS"] <- "Casos proyectados (recta)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.INFERIOR.CASOS"] <- "Límite inferior casos (recta)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.SUPERIOR.CASOS"] <- "Límite superior casos (recta)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.INFERIOR.TASA..puntos."] <- "Límite inferior tasa (puntos)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.SUPERIOR.TASA..puntos."] <- "Límite superior tasa (puntos)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.INFERIOR.CASOS..puntos."] <- "Límite inferior casos (puntos)"
    datos_transpuesto$Variable[datos_transpuesto$Variable == "LÍMITE.SUPERIOR.CASOS..puntos."] <- "Límite superior casos (puntos)"
    
    tabla2_reporte(datos_transpuesto)
    
    datatable(datos_transpuesto,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',  # B = Buttons, r = processing, t = table, i = info, p = pagination
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                pageLength = 11
              ),
              class = 'stripe hover cell-border order-column',
              style = 'bootstrap'
    ) %>%
      DT::formatStyle(
        columns = names(datos_transpuesto),
        backgroundColor = '#FFFFFF',
        color = '#000000'
      )
     
    })
  
  })
  
  })
  
  })
