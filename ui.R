#LIBRERIAS
library(shiny)
library(bslib)

#INTERFAZ DE USUARIO
page_navbar(
  title = "EDD",
  bg = "#C70039",
  inverse = TRUE,
  nav_panel(title = "Datos", 
            {page_sidebar(
              sidebar = sidebar(
                h2("Datos"),
                
                #Eleccion del dataset
                selectInput( 
                  inputId = "dataset",
                  label = "Seleccione un dataset:",
                  choices = c("Vinos", "Esperanza de vida")
                )
              ),
              
              navset_card_underline(
                nav_panel("Tabla de datos", DT::DTOutput("dynamic")),
                nav_panel("Explicacion", verbatimTextOutput("description"))
                #textOutput("value") - renderText
              )
            )}
            ),
  nav_panel(title = "Analisis Exploratorio", 
            page_sidebar(
              sidebar = sidebar(h2("Analisis Exploratorio"),
                                uiOutput("sel_var_aed")),
              page_fillable(
                
                navset_card_underline(
                  nav_panel("General", fluidPage(
                    h3("Estructura de los datos"),
                    verbatimTextOutput("str"),
                    h3("Resumen de los datos"),
                    verbatimTextOutput("summary")
                  )),
                  nav_panel("Plots", 
                    fluidRow(
                      column(5, 
                             card(card_header("Graficos"), plotOutput("ae_plot"))
                      ),
                      column(7, 
                             card(card_header("Existen datos nulos?"), 
                                  card_body(verbatimTextOutput("nulls"),
                                            actionButton("no_nulls", "Eliminar nulos"))),
                             fluidRow(
                               column(6,
                                      card(card_header("Eliminar outliers"),
                                           card_body(
                                             uiOutput("range_slider"),
                                             actionButton("delete_out", "Eliminar")
                                           ))),
                               column(6,card(card_header("Summary"), card_body(verbatimTextOutput("var_summ"))))
                             )
                      ))
                            ),
                  nav_panel("Correlaciones", plotOutput("ae_correlations"))
                )
              )
            )
            ),
  nav_panel(title = "Regresion Lineal Multiple", 
            page_sidebar(
              sidebar = sidebar(h2("Regresion Lineal Multiple"),
                
                # Selector para variable dependiente
                selectInput("columns", "Selecciona la variable dependiente:",
                            choices = NULL, selected = NULL),
                
                # Selector para variables regresoras (checkboxes)
                checkboxGroupInput("indep_vars", "Selecciona las variables regresoras:",
                                   choices = NULL, selected = NULL),
                ),
              navset_card_underline(
                nav_panel("Resumen", 
                          fluidRow(
                            column(7, 
                                   h3("Modelo Ajustado"),
                                   verbatimTextOutput("regression_summary")
                            ),
                            column(5, 
                                   h3("Correlaciones"),
                                   plotOutput("correlations")
                            ))
                          ),
                nav_panel("Media de errores y homogeneidad", fluidPage(
                  p("Para que la media de errores sea cero la recta roja del grafico
                  debe aproximarse lo maximo posible a la linea punteada.", br(),"
                  Si la media de los errores no es cero, podria indicar que el 
                  modelo esta sesgado o que no esta capturando correctamente la 
                    relacion entre las variables."),
                  plotOutput("media"),
                  p("El analisis de la homogeneidad en las varianzas de los
                  errores puede realizarse tanto de manera grafica como a traves de
                    pruebas. Para ello, se establecen las siguientes hipotesis:",br(),
                    "H0 : el modelo es homogeneo.",br(),
                    "H1 : el modelo no es homogeneo."),
                  verbatimTextOutput("homog"),
                  p("Si los residuos no se acumulan en ninguno de los extremos del grafico 
                  sino que se distribuyen de forma balanceada y, ademas, el p-valor del test de 
                    Breusch Pagan NO es pequenio entonces no se rechaza la hipotesis nula. En caso contrario, se 
                    afirma que existe heterocedasticidad en las varianzas de los residuos.")
                )),
                nav_panel("Independencia", fluidPage(
                  p("Para observar la correlacion de los errores se utiliza el test de 
                    Durbin Watson y las hipotesis con las cuales se analiza son las 
                    siguientes:", br(),"H0: no hay autocorrelacion.",br(),"H1: existe autocorrelacion."),
                  verbatimTextOutput("darwin_w"),
                  p("Si el p-valor es cercano a 1 y el coeficiente DW se aproxima a 2 entonces no es posible 
                    rechazar la hiposis nula, por la cual se corrobora que los errores son independientes.")
                )),
                nav_panel("Normalidad", fluidPage(
                  p("La normalidad de los residuos se analiza a traves de pruebas y de manera grafica. Para 
                    ello, se utilizan las siguientes hipotesis:", br(),"H0: el modelo tiene una distribucion 
                    normal de errores.",br(),"H1: el modelo no tiene una distribucion normal de errores."),
                  plotOutput("norm_plot"),
                  p("Si la mayoria de residuos se aproximan a la diagonal entonces podria decirse que los 
                    errores se distribuyen normalmente. Para corroborarlo, el p-valor obtenido en el test de 
                    normalidad escogido NO debe ser pequenio."),
                  fluidRow(
                    column(4, 
                           selectInput( 
                             inputId = "normalityTest",
                             label = "Seleccione una prueba de normalidad:",
                             choices = c("Shapiro", "Kolmogorov-Smirnov","Jaque Bera"))),
                    column(8, 
                           verbatimTextOutput("normality")))
                )),
                nav_panel("Multicolinealidad", fluidPage(
                  p("Cuando existen problemas de multicolinealidad, las estimaciones de los coeficientes 
                    de las variables independientes pueden volverse imprecisas y volatiles, lo que 
                    hace que las predicciones y la interpretacion de los resultados sean menos confiables."),
                  p("Para evitarlo, el estadistico VIF evaluado en el modelo debe ser menor a 10 para todas las 
                  variables regresoras. Si esto se cumple entonces es posible afirmar que elmodelo ajustado NO 
                  presenta problemas de multicolinealidad"),
                  verbatimTextOutput("vif"))),
                nav_panel("Influyentes", fluidPage(
                  h2("Analisis de outliers"),
                  plotOutput("residuos"),
                  h2("Puntos influyentes"),
                  fluidRow(
                    column(6, sliderInput("slider", "Seleccione el valor de corte:", 
                                          min = 0.01, max = 1, value = 0.5, width = '100%')),
                    column(6, verbatimTextOutput("cook_test"))
                  ),
                  plotOutput("cook_plot")
                )),
                nav_panel("Predicciones", fluidPage(
                  h3("Datos a predecir"),
                  p("Ingrese los valores que desee para cada variable."),
                  uiOutput("predi_inputs"), br(),
                  textOutput("predi"), br(),
                  h3("Bondad del ajuste"),
                  textOutput("rmse"), br(),
                  plotOutput("kindness")
                  )),
              ))
            )
  ,nav_spacer(),nav_item(
    actionButton("show","",  icon = icon("info-circle"))
  ) 
  
)