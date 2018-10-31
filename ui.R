library(shiny)
library(shinydashboard)
library(shinythemes)

shinyUI(navbarPage(
  "Credit scoring using Machine Learning",
  tabPanel("Analysis",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               fileInput(
                 "dataset_file",
                 label = "Select dataset",
                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
               ),
               
               actionButton(
                 "analyze",
                 "Analyze",
                 icon("refresh"),
                 style = "color: #fff; background-color:
                 #337ab7; border-color: #2e6da4"
               ),
               
               uiOutput("dwnload")
             ),
             
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel(
                   "PROFILING",
                   fluidRow(
                     column(
                       width = 8,
                       h4("Structure"),
                       verbatimTextOutput("str", placeholder = TRUE),
                       h4("Summary"),
                       verbatimTextOutput("summary", placeholder = TRUE)
                     ),
                     column(width = 4,
                            # h4("Financing Ratio"),
                            plotOutput("fratio"),
                            # h4("Savings Capacity"),
                            plotOutput("savcap"))
                   ),
                   h4("Profiling"),
                   plotOutput("means_plot", width = "100%", height = "500px"),
                   h4(" "),
                   plotOutput("prop_plot", width = "100%", height = "1200px")
                 ),
                 
                 
                 
                 tabPanel("DECISION TREES",
                          fluidRow(
                            column(
                              9,
                              plotOutput("rpart_tree_plot", width = "100%", height = "700px")
                            ),
                            column(3,
                                   h4("Classification error"),
                                   htmlOutput("err_rate"))
                          )),
                 
                 
                 
                 tabPanel(
                   "LOGISTIC",
                   br(),
                   h4("Prediction"),
                   fileInput(
                     "usr_inp",
                     label = "Select test data",
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                   ),

                   fluidRow(column(
                     1, actionButton(inputId = "calc", label = "calculate")
                   ),

                   column(
                     11, verbatimTextOutput("user_ip_result", placeholder = TRUE)
                   )),
                   br(),
                   fluidRow(
                     column(
                       7,
                       h4("Logistic model summary"),
                       verbatimTextOutput("log_summary_txt", placeholder = TRUE)
                     ),
                     column(
                       5,
                       h4("Confusion matrix"),
                       verbatimTextOutput("con_matrix", placeholder = TRUE),
                       h4("Error rate"),
                       verbatimTextOutput("error_rate", placeholder = TRUE),
                       
                       h4("Importance of variables in the model"),
                       verbatimTextOutput("anova", placeholder = TRUE)
                     )
                   )
                 ),
                 
                 tabPanel("LDA",
                          
                          fluidRow(
                            column(
                              8,
                              h4("Coefficients of linear discriminants"),
                              verbatimTextOutput("lda_coeff", placeholder = TRUE)
                            ),
                            column(
                              4,
                              h4("Confusion matrix"),
                              verbatimTextOutput("lda_con_mat"),
                              h4("Error rate"),
                              verbatimTextOutput("lda_err", placeholder = TRUE),
                              plotOutput("lda_plot")
                            )
                            
                          )),
                 
                 tabPanel("RANDOM FOREST",
                          fluidRow(
                            column(
                              7,
                              h4("Importance of variables"),
                              verbatimTextOutput("rand_varimp", placeholder = TRUE),
                              plotOutput("rand_varimp_plot")
                            ),
                            column(
                              5,
                              h4("Confusion matrix"),
                              verbatimTextOutput("rand_con_mat", placeholder = TRUE),
                              h4("Error rate"),
                              verbatimTextOutput("rand_error", placeholder = TRUE),
                              plotOutput("rand_rf50_plot")
                            )
                          )),
                 
                 
                 
                 tabPanel("XGBoost",
                          fluidRow(
                            column(
                              6,
                              h4("Confusion matrix and statistics"),
                              verbatimTextOutput("xg_stats", placeholder = TRUE)
                            ),
                            column(6,
                                   h4("Variable importance"),
                                   plotOutput("xg_plot"))
                          )),
                 
                 
                 
                 tabPanel("SUMMARY",
                          fluidRow(
                            column(
                              3,
                              h3("Logistic"),
                              br(),
                              h4("Confusion matrix"),
                              verbatimTextOutput("sum_log_conf", placeholder = TRUE),
                              br(),
                              h4("Accuracy"),
                              verbatimTextOutput("sum_log_err", placeholder = TRUE)
                            ),
                            column(
                              3,
                              h3("LDA"),
                              br(),
                              h4("Confusion matrix"),
                              verbatimTextOutput("sum_lda_conf", placeholder = TRUE),
                              br(),
                              h4("Accuracy"),
                              verbatimTextOutput("sum_lda_err", placeholder = TRUE)
                            ),
                            column(
                              3,
                              h3("Random Forest"),
                              br(),
                              h4("Confusion matrix"),
                              verbatimTextOutput("sum_rand_conf", placeholder = TRUE), br(),
                              h4("Accuracy"),
                              verbatimTextOutput("sum_rand_err", placeholder = TRUE)
                            ),
                            column(
                              3,
                              h3("XGBoost"),
                              br(),
                              h4("Confusion matrix"),
                              verbatimTextOutput("sum_xg_conf", placeholder = TRUE), br(),
                              h4("Accuracy"),
                              verbatimTextOutput("sum_xg_err", placeholder = TRUE)
                            )
                          ))
                 
                 
                 
                 
                 
               )
             )
           ))
))