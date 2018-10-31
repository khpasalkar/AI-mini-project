library(shiny)
library(MASS)
library(ROCR)
require(ggplot2)
require(rpart)
require(FactoMineR)

require(xgboost)
require(data.table)

shinyServer(function(input, output) {

  ## Input dataset
  cr_dataset <- eventReactive(input$analyze, {
    infile <- input$dataset_file
    rdd <<- read.csv(infile$datapath,
                     header = TRUE,
                     stringsAsFactors = FALSE)
  })
  
  ## Processed dataset
  processing <- reactive({
    cr_dataset()
    isolate({
      source("processing.R")
    })
    
    dd <<- mt
    ddr <<- read.csv("CleanCreditScoring.csv",
               header = TRUE,
               stringsAsFactors = TRUE)
    
    return(dd)
  })
  
  ################### summary tab ###############
  
  output$sum_log_conf <- renderPrint({
    logistic_an()
    logistic_conf
  })
  
  output$sum_log_err <- renderPrint({
    logistic_an()
    100 - logistic_err
  })
  
  output$sum_lda_conf <- renderPrint({
    lda()
    lda_conf
  })
  
  
  output$sum_lda_err <- renderPrint({
    lda()
    100 - lda_err
  })
  
  
  output$sum_rand_conf <- renderPrint({
    random()
    rand_conf
  })
  
  
  output$sum_rand_err <- renderPrint({
    random()
    100 - rand_err
  })
  
  
  output$sum_xg_conf <- renderPrint({
    xgbst()
    xgboost_conf
  })
  
  output$sum_xg_err <- renderPrint({
    xgbst()
    100 - xgboost_error
  })
  
  
  
  ################## profiling tab ############
  
  profiling <- reactive({
    cr_dataset()
    
    isolate({
      withProgress(message = 'Processing and Profiling...', value = 1, {
        source("profiling.R")
      })
      
    })
    
  })
  
  
  output$fratio <- renderPlot({
    x = processing()
    hist(x$Finrat, col = "gray85", main = "Financing Ratio")
  })
  
  output$savcap <- renderPlot({
    x = processing()
    hist(x$Savings, col = "gray85", main = "Savings Capacity")
  })
  
  output$summary <- renderPrint({
    summary(cr_dataset())
  })
  
  output$str <- renderPrint({
    str(cr_dataset())
  })
  
  
  output$means_plot <- renderPlot({
    profiling()
    
    par(mfrow = c(3, 4), mar = c(3, 3, 3, 3))
    for (i in 1:ncon)
    {
      barplot(
        tapply(var.cont[, i], dd$Status, mean),
        main = paste("Means by", names(pval.cont)[i]),
        cex.main = 0.9,
        border = NA,
        col = c("steelblue", "skyblue")
      )
      abline(h = mean(var.cont[, i]), col = "gray40")
      legend(0,
             mean(var.cont[, i]),
             "global mean",
             bty = "n",
             text.col = "gray20")
    }
  })
  
  output$prop_plot <- renderPlot({
    profiling()
    
    par(mfrow = c(5, 3))
    n = nrow(dd)
    for (i in 1:ncat)
    {
      rowprof <- WhoGetsWhatCat(var.cat[, i], dd$Status)$rowpf
      marg <- table(var.cat[, i]) / n
      plot(
        marg,
        type = "l",
        ylim = c(0, 0.6),
        main = paste("Prop. of pos & neg by", names(pval.cat)[i])
      )
      lines(rowprof[1,], col = "blue")
      lines(rowprof[2,], col = "red")
      #legend("topright", c("pos","neg"), col=c("blue","red"), lty=1)
    }
    
  })
  
  ################## decision trees ###########
  
  dec_tree <- reactive({
    cr_dataset()
    
    isolate({
      withProgress(message = 'Processing...', value = 1, {
        source("dtrees.R")
      })
    })
  })
  
  output$rpart_tree_plot <- renderPlot({
    dec_tree()
    
    par(mar = c(1, 1, 2, 0.5))
    plot(
      ct3,
      margin = 0.05,
      compress = TRUE,
      main = "Decision Tree",
      uniform = TRUE
    )
    text(ct3,
         use.n = TRUE,
         all = TRUE,
         cex = 0.7)
  })
  
  output$err_rate <- renderUI({
    dec_tree()
    str1 <-
      paste("<b><h4>Error rate in the learning sample:</h4></b> ",
            100 * sum(diag(table(
              dd$Status[learn], ct3.learnp
            ))) / nlearn,
            sep = " ")
    
    str2 <-
      paste("<b><h4>Error rate in the testing sample:</h4></b> ",
            100 * sum(diag(table(
              dd$Status[-learn], ct3.testp
            ))) / ntest,
            sep = " ")
    HTML(paste(str1, str2, sep = "<br/>"))
  })
  
  ################## Logistic #################
  
  logistic_an <- reactive({
    cr_dataset()
    
    isolate({
      withProgress(message = 'Processing...', value = 1, {
        source("logistic.R")
      })
      
    })
    
  })
  
  output$log_summary_txt <- renderPrint({
    logistic_an()
    summary(gl1)
  })
  
  output$anova <- renderPrint({
    logistic_an()
    anova(gl1)
  })
  
  output$con_matrix <- renderPrint({
    logistic_an()
    table(dd$Status[-learn], glftpred)
  })
  
  output$error_rate <- renderPrint({
    logistic_an()
    error_rate.test
  })
  
  output$conc_curve <- renderPlot({
    logistic_an()
    
    plot(
      ac_pos_test,
      ac_tot,
      type = "l",
      lwd = 2,
      main = "Concentration Curve"
    )
    lines(ac_tot, ac_tot, col = "gray70", lwd = 2)
    
  })
  
  output$roc_curve <- renderPlot({
    logistic_an()
    
    plot(
      ac_pos_test,
      ac_neg_test,
      type = "l",
      lwd = 2,
      main = "ROC Curve",
      col = "blue"
    )
    lines(ac_neg_test, ac_neg_test, col = "grey70", lwd = 2)
    
    
  })
  
  userip <- eventReactive(input$calc, {
    infile2 <- input$usr_inp
    tdd = read.csv(infile2$datapath,
                   header = TRUE,
                   stringsAsFactors = FALSE)
  })
  
  
  ### prediction
  
  output$user_ip_result <- renderPrint({
    tdd2 = userip()
    
    glft = predict(glf, newdata = tdd2)
    pt = 1 / (1 + exp(-glft))
    pt = 1 - pt
    
    # vector of predicted values
    glftpred1 = rep(NA, length(pt))
    glftpred1[pt < 0.5] = "good"
    glftpred1[pt >= 0.5] = "bad"
    print(glftpred1)
  })
  
  ################## LDA ##################
  
  lda <- reactive({
    cr_dataset()
    
    
    isolate({
      withProgress(message = 'LDA...', value = 2, {
        source("lda.R")
      })
      
    })
  })
  
  output$lda_coeff <- renderPrint({
    lda()
    ldafit
  })
  
  output$lda_plot <- renderPlot({
    lda()
    plot(ldafit)
  })
  
  output$lda_con_mat <- renderPrint({
    lda()
    lda_conf
  })
  
  output$lda_err <- renderPrint({
    lda()
    lda_err
  })
  
  ################## Random Forest ##################
  
  random <- reactive({
    cr_dataset()
    isolate({
      withProgress(message = 'Random Forest...', value = 1, {
        source("random_forest.R")
      })
    })
  })
  
  output$rand_varimp <- renderPrint({
    random()
    importance(rf50)
  })
  
  output$rand_varimp_plot <- renderPlot({
    random()
    varImpPlot(rf50,  main = "", cex = 0.8)
  })
  
  output$rand_con_mat <- renderPrint({
    random()
    table(Test50_rf_pred, test50$Status)
  })
  
  output$rand_rf50_plot <- renderPlot({
    random()
    plot(rf50, main = "")
  })
  
  output$rand_error <- renderPrint({
    random()
    rand_err
  })
  
  ################## XGBoost ##################
  
  xgbst <- reactive({
    cr_dataset()
    
    isolate({
      withProgress(message = 'XGBoost...', value = 1, {
        source("XGBoost.R")
      })
    })
  })
  
  output$xg_stats <- renderPrint({
    xgbst()
    
    #confusionMatrix(xgbpred, ts_label)
    confusionMatrix(
      factor(xgbpred, levels = 1:2223),
      factor(ts_label, levels = 1:2223)
    )
    
    
  })
  
  output$xg_plot <- renderPlot({
    xgbst()
    xgb.plot.importance(importance_matrix = mat[1:15])
  })
  
  ################## sidebar ##################
  
  output$dwnload <- renderUI({
    conditionalPanel(
      !is.null(cr_dataset()),
      br(),
      br(),
      strong("Download clean data"),
      br(),
      downloadButton("dwn_btn", label = "Download")
    )
  })
  
  output$dwn_btn <- downloadHandler(
    filename = function() {
      paste("CleanCreditScoring.csv")
    },
    content = function(file) {
      write.csv(processing(), file, row.names = FALSE)
    }
  )
})
