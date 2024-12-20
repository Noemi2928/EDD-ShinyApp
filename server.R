#LIBRERIAS
library(shiny)
library(readxl)
library(corrplot)
library(car)
library(lmtest)
library(broom)
library(tseries)
library(nortest)
library(caret)
library(plotly)

vinos <- read_xls("vinos.xls")
vida <- read.csv("vida.csv", sep = ",", dec = ".")
vida <- vida[,-c(19,20)]
names(vida)<-c("PS", "R","AA","MI","MI5","MA","CA","HB", "S","BMI", "P","D","VIH",
                "PBI","PP","D10","D5","ESC","EV")
desc_vida <- readLines("desc_vida.txt")
desc_vinos <- readLines("desc_vinos.txt")

agregar_na <- function(dataset, n) {
  fila_na <- as.data.frame(matrix(NA, nrow = n, ncol = ncol(dataset)))
  colnames(fila_na) <- colnames(dataset)
  return(rbind(dataset, fila_na))
}
vinos <- agregar_na(vinos, 3)
vida <- agregar_na(vida, 5)

# SERVIDOR
function(input, output, session) {
  
  ### INFO ####
  observeEvent(input$show, {
    showModal(
      modalDialog(
        title = "Información de la aplicación",
        "Esta aplicación fue desarrollada por Milagros Benites y Nicolas Leone
      en el marco de la materia Explotación de datos de la Universidad Nacional
      del Oeste. Merlo, Buenos Aires, 2024",
        footer = modalButton("Cerrar"),
        size = c("m", "s", "l", "xl"),
        easyClose = TRUE,
        fade = TRUE
    ))
  })
  
  vals <- reactiveValues(plt1=NULL) 
  
  ###DATOS####
  datasetInput <- reactive({
    switch(input$dataset,
           "Vinos" = vinos,
           "Esperanza de vida" = vida)
  })
  
  datasetDescription <- reactive({
    switch(input$dataset,
           "Vinos" = desc_vinos,
           "Esperanza de vida" = desc_vida)
  })
  
  filtered_data <- reactiveVal(isolate(datasetInput()))
  
  observeEvent(input$dataset, {filtered_data(datasetInput())})
  
  datasetCuantisInput <- reactive({
    data <- filtered_data()
    if(input$dataset == "Vinos")
    {data <- data[, -1]} else {data <- data[,-c(1,2,3)]}
    return(data)
  })
  
  output$description <- renderText(paste(datasetDescription(), collapse = "\n"))
  
    output$str <- renderPrint(str(filtered_data()))
    output$summary <- renderPrint(summary(filtered_data()))
    output$dynamic <- DT::renderDT(datasetInput(), options = list(pageLength = 8))
    
    ###ANALISIS EXPLORATORIO####
    
    output$sel_var_aed <- renderUI(radioButtons("var_aed", "Seleccione una variable:", choices = colnames(datasetInput())))
    
    output$ae_plot <- renderPlot(boxplot(filtered_data()[input$var_aed],  col = "#C70039",xlab=input$var_aed))
    
    output$nulls <- renderPrint({
      if(!any(is.na(filtered_data()))){
        "No hay datos nulos."
      }else{sapply(filtered_data(), function(x) sum(is.na(x)))}})
    
    output$ae_correlations <- renderPlot({
      validate(need(all(!is.na(filtered_data())),"Por favor , elimine los valores nulos del dataset"))
      corrplot.mixed(cor(datasetCuantisInput()), lower = 'number', 
                     upper = 'circle', tl.col="black")
      })
    
    observeEvent(input$no_nulls, {
      dataset <- filtered_data()
      dataset <- dataset[!is.na(dataset[[input$var_aed]]), ]
      filtered_data(dataset)
    })
    
    output$range_slider <- renderUI({
      validate(need(all(!is.na(filtered_data())),"Por favor , elimine los valores nulos del dataset"))
      min_range <- floor(min(filtered_data()[input$var_aed]))
      max_range <- ceiling(max(filtered_data()[input$var_aed]))
      sliderInput( 
        "range", "Seleccione el rango de la variable:", 
        min = min_range, max = max_range, value = c(min_range+0.25,max_range-0.5)
      )
    })
    
    observeEvent(input$delete_out, {
      dataset <- filtered_data()
      dataset <- dataset[dataset[[input$var_aed]] >= input$range[1] & dataset[[input$var_aed]] <= input$range[2], ]
      filtered_data(dataset)
    })
    
    output$var_summ <- renderPrint(summary(filtered_data()[input$var_aed]))
    
    ###REGRESION LINEAL MULTIPLE####
    
    observe({
      updateSelectInput(session, "columns", choices = colnames(datasetCuantisInput()))
      updateCheckboxGroupInput(session, "indep_vars", choices = colnames(datasetCuantisInput()))
    })
    
    partitioned_data <- reactive({
      # Crear el conjunto train - test
      set.seed(123)
      parte <- createDataPartition(datasetCuantisInput()[[input$columns]], p=0.8, list=FALSE)
      train<- datasetCuantisInput()[parte,]
      test<- datasetCuantisInput()[-parte,]
      
      list(train = train, test = test)
    })
    
    output$correlations <- renderPlot(
      {
        validate(need(all(!is.na(filtered_data())),"Por favor , elimine los valores nulos del dataset"))
      corrplot(cor(datasetCuantisInput()), method="circle",tl.col="black",tl.cex=0.8)})
    
    model_reactive <- reactive({
      #req(input$columns, input$indep_vars)  Asegura que ambos inputs esten seleccionados
      formula <- as.formula(paste(input$columns, "~", paste(input$indep_vars, collapse = "+")))
      lm(formula, data = partitioned_data()$train)
    })
    
    output$regression_summary <- renderPrint({
      if (length(input$indep_vars) > 0) {
        summary(model_reactive())
      } else {
        "Por favor, seleccione al menos una variable regresora."
      }
    })
    
    output$media <- renderPlot({
      if (length(input$indep_vars) > 0) {
        plot(model_reactive(),1)
      }})
    
    output$homog <- renderPrint({
      if (length(input$indep_vars) > 0) {
        ncvTest(model_reactive())
      } else {
        "Por favor, seleccione al menos una variable regresora."
      }})
    
    output$darwin_w <- renderPrint({
      if (length(input$indep_vars) > 0) {
        dwtest(model_reactive())
      } else {
        "Por favor, seleccione al menos una variable regresora."
      }})
    
    testSelection <- reactive({
      switch(input$normalityTest,
             "Shapiro" = shapiro.test(model_reactive()$residuals),
             "Kolmogorov-Smirnov" = lillie.test(residuals(model_reactive())),
             "Jaque Bera" = jarque.bera.test(residuals(model_reactive()))
      )
    })
    
    output$normality <- renderPrint({
      if (length(input$indep_vars) > 0) {
        testSelection()
      } else {
        "Por favor, seleccione al menos una variable regresora."
      }})
    
    output$norm_plot <- renderPlot({
      if (length(input$indep_vars) > 0)
      {plot(model_reactive(), 2)}})
    
    output$vif <- renderPrint({
      if (length(input$indep_vars) > 0) {
        vif(model_reactive())
      } else {
        "Por favor, seleccione al menos una variable regresora."
      }})
    
    output$residuos <- renderPlot({
      if (length(input$indep_vars) > 0)
      {boxplot(residuals(model_reactive()),col="#C70039",horizontal=T,
               main="Boxplot de residuos del modelo")}
    })
    
    output$cook_test <- renderPrint({
      model.diag.metrics <- augment(model_reactive())
      if (length(which(model.diag.metrics$.cooksd>input$slider)) > 0){
        which(model.diag.metrics$.cooksd>input$slider)
      } else { "No hay puntos que puedan considerarse influyentes."}
    })
    
    output$cook_plot <- renderPlot({
      if (length(input$indep_vars) > 0)
        {plot(model_reactive(), 4)}})
    
    output$predi_inputs <- renderUI({
      numerics <- lapply(input$indep_vars, function(var_p){
        numericInput(paste0("num_",var_p), var_p, value = min(datasetCuantisInput()[var_p]),
                     max = ceiling(max(datasetCuantisInput()[var_p])),
                     min = floor(min(datasetCuantisInput()[var_p])))
      })
      fluidPage(
        fluidRow(
          lapply(seq_along(numerics), function(i){
            column(4, numerics[[i]])
          })
        ),
        actionButton("user_predi", "Predecir")
      )
    })
    
    observeEvent(input$user_predi, {
      req(input$indep_vars)
      vars_to_pred <- sapply(input$indep_vars, function(var){
        input[[paste0("num_",var)]]
      })
      pred_data <- as.data.frame(t(vars_to_pred))
      colnames(pred_data) <- input$indep_vars
      
      output$predi <- renderText({
        pred_value <- predict(isolate(model_reactive()), pred_data)
        paste("El valor de ",input$columns, "predicho es de ", pred_value)
      })
      
    })
    
    output$kindness <- renderPlot({
      if (length(input$indep_vars) > 0)
      {predi <- predict(model_reactive(),partitioned_data()$test)
      plot(partitioned_data()$test[[input$columns]],predi)
      abline(a=0,b=1)}
    })
    
    output$rmse <- renderText({
      if (length(input$indep_vars) > 0)
      {predi <- predict(model_reactive(),partitioned_data()$test)
      MSE<-mean((partitioned_data()$test[[input$columns]]-predi)^2) 
      RMSE <- sqrt(MSE)
      vals$plt1 <- RMSE
      paste("El valor de RMSE del ajuste es:", RMSE)
      }else {
        "Por favor, seleccione al menos una variable regresora."
      }
    })
}