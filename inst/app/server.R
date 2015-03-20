source('pkg_check.R')

library(knitr)     # dynamic reports
library(lsmeans)   # analysis intrepretation
library(effects)   # for plotting
library(agricolae) # for datasets 
library(shiny)     # the web app javascript engine
library(shinyAce)  # for displaying R code - pull from github
library(lattice)   # for plotting
library(gridExtra) # for arranging plots
library(shinyBS)   # http://spark.rstudio.com/johnharrison/shinyBS-Demo/
# devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")

shinyServer(function(input, output, session){

  output$debug <- renderText({fmla()})
  
##### server side element for reading in data ####################################
  datp <- reactive({
    if((is.null(input$data_file) || length(input$data_file)==0) && !input$use_sample_data){return(NULL)
    } else {
      if(length(input$data_file) > 0 && !input$use_sample_data){
        dat <- call('read.csv', 
                    file=input$data_file$datapath)
      } else {
#         dat <- parse(text=input$sample_data_buttons)
        dat <- as.name(input$sample_data_buttons)
      }
    }
    return(dat)
  })

##### assign data to "dat" by evaluating the call to "read.csv" or loading the data
  dat <- reactive({
      eval(call('data', input$sample_data_buttons, 
                package='agricolae',
                envir = environment()))
    dat <- eval(datp(), envir=environment())
    if(input$use_sample_data){ dat$A <- NULL }
    return(dat)
    })


##### create the code needed to read in data ##########
  read.expr <- reactive({
    if(!input$use_sample_data){
      return(deparse(datp()))
    } else {
      l <- 'library(agricolae)  # load "agricolae" package for the sample data'
      l <- paste0(l, '\ndata("',input$sample_data_buttons,  '")')
      l <- paste0(l, '\n', 'my_data <- ', input$sample_data_buttons)
    }
    })

##### display the data on the "upload data" tab ################
  output$data_table <- renderDataTable({dat()})
  

##### UI element for selecting the desired analysis ############  
  output$select_analysis <- renderUI({
    if(is.null(dat())){
      h4('please upload data first')
    } else {
    selectInput('analysis', 
                'Select your statistical model',
                choices=c('t-test'='t.test', 
                          'ANOVA'='aov',
                          'Linear or Generalized Linear Model'='glm',
                          'Randomized Complete Block Design'='rcbd'),
                selected=NULL)
    }
  })
 
##### UI element for selecting DV ##################################
  output$select_dv <- renderUI({
    if(length(input$analysis)==0){return(NULL)}
    selectInput('dv',
                'Select your dependent variable',
                choices=names(dat()),
                selected=NULL)
  })
  
##### UI element for selecting IV #################################

output$select_block <- renderUI({
    if(length(input$dv)==0 || input$analysis !='rcbd'){return(NULL)}
      selectInput('block', 
                  'Select your block variable',
                  choices=names(dat2())[!names(dat2()) %in% input$dv],
                  multiple=FALSE,
                  selected=NULL)
  })

output$select_treatment <- renderUI({
  if(length(input$dv)==0 || input$analysis != 'rcbd'){return(NULL)}
  selectInput('treatment',
              'Select your treatment variable',
              choices=names(dat2())[!names(dat2()) %in% c(input$dv, input$block)],
              multiple=FALSE, 
              selected=NULL)
})

output$select_iv <- renderUI({
  if(length(input$dv)==0 || input$analysis=='rcbd'){return(NULL)}
  selectInput('iv', 
              switch(input$analysis, 
                     aov='Select your independent variable(s)',
                     t.test='Select your grouping variable',
                     glm='Select your independent variables(s)',
                     rcbd='Select your treatment variable'),
              choices=names(dat2())[!names(dat2()) %in% input$dv],
              multiple=ifelse(input$analysis %in% c('t.test', 'rcbd'), FALSE, TRUE))
})

ivs <- reactive({
  if(length(input$dv) == 1){
    if(input$analysis=='rcbd'){
      return(c(input$block, input$treatment)) 
    } else if(input$analysis %in% c('aov', 't.test', 'glm')){
      return(input$iv)
    }
  } else {
    return(NULL)
  }
})




##### UI element for selecting the DV type ##########################
  output$select_dv_type <- renderUI({
    if(length(input$dv)==0 || input$analysis != 'glm'){return(NULL)}
    selectInput('dv_type',
                'What type of data is your dependent variable?',
                choices=c('continuous'='gaussian',
                          'dichotomous (binary)'='binomial',
                          'count' = 'poisson'),
                selected='gaussian')
  })

##### select variable types #####################################

output$var_types_select <- renderUI({
  
  if(is.null(dat())){
    return(NULL)
  }
  
  class.recode <- c('character'='factor', 
                    'factor'='factor',
                    'logical'='factor',
                    'numeric'='numeric',
                    'integer'='numeric')
  btns <- list()
  
  for(i in 1:ncol(dat())){
    clss <- class(dat()[,i])
    clss <- class.recode[clss]
    
    btns[[i]] <- radioButtons(inputId=paste0(names(dat())[i], '_recode'),
                             label=names(dat())[i],
                             choices=c('Numeric'='numeric', 'Grouping'='factor'),
                             selected=clss, 
                             inline=TRUE)
  } 
  return(btns)
})

######## convert variables to their respective types ############

dat2 <- reactive({
  dat2 <- dat() 
  for(i in 1:ncol(dat2)){
    var_type <- input[[paste0(names(dat2)[i], '_recode')]]
    dat2[,i] <- eval(call(paste0('as.', var_type),
                          dat2[,i]))
  }
  return(dat2)
})



##### select the interactions  ################################
# TukeyHSD or multcomp::glht
output$select_interactions <- renderUI({
  
  if(input$analysis=='t.test'){
    return(h4('Interactions not available for t-tests'))
  } 
  if(input$analysis=='rcbd'){
    return(h4('Interactions not allowed in RCBD. To run a more flexible ANOVA, please select "ANOVA" under tab 2: Type of analysis'))
  }
  
  ch <- expand.grid(var1=ivs(), var2=ivs())
  ch <- ch[which(ch$var1 != ch$var2), ]
  ch <- apply(ch, 2, c)
  ch2 <- list()
  for(i in 1:nrow(ch)){
    ch2[[i]] <- ch[i, ]
  }
  for(i in seq_along(ch2)){
    ch2[[i]] <- sort(ch2[[i]])
  }
  ch2 <- data.frame(ch2)
  names(ch2) <- paste('var', 1:ncol(ch2))
  ch2 <- as.data.frame(t(ch2))
  ch2$int <- paste(ch2$var2, ch2$var1, sep='.___.___.')
  ch2 <- unlist(subset(ch2, !duplicated(int), select='int'))
  ch2 <- unname(gsub('.___.___.', ' & ', ch2, fixed=TRUE))
  names(ch2) <- ch2
  ch2 <- gsub('&','*', ch2) 
  
  if(input$analysis == 't.test'){return(NULL)}
  selectInput('interaction_input', 
                     label=NULL,
                     choices=ch2,
              multiple=TRUE)
})




##### create the formula for analysis ##########################
  fmla <- reactive({
    int <- paste0(input$interaction_input, 
                  collapse=' + ')
    int <- ifelse(is.null(input$interaction_input) | input$analysis=='rcbd', NA, int)
    main <- paste0(ivs(), collapse=' + ')
    eff <- paste_na(main, int, sep=' + ')
    
    paste0(input$dv, ' ~ ', eff)
    })
  
##### run the analysis, assign to reactive object "fit" ##############
  fitp <- reactive({
    input$run_analysis
    isolate({
      if(!is.character(input$analysis)){
        return(NULL)
      } 
      fit <- call(input$analysis, 
                  formula=as.formula(fmla()),
                  family=input$dv_type,
                  data=as.name('my_data'))
      fit <- strip.args(fit)
    })
    return(fit)
    })

fit <- reactive({
  input$run_analysis
    isolate({
      my_data <- dat2()
      x <- eval(fitp())
    })
  return(x)
  })

fit.expr <- reactive({
  input$run_analysis
  isolate({
    x <- deparse(fitp(), width.cutoff=500L)
  })
  return(x)
})

##### the fit summary for analysis ###################################
### print anova table for all linear models
### the interpretaion tab will show post-hoc tests
  output$fit_summary <- renderPrint({
    analysis <- isolate(input$analysis)
    input$run_analysis
    if(analysis=='t.test'){
      sum <- isolate(fit())
    } else {
      sum <- isolate(anova(fit(), 
                           test=ifelse((input$analysis=='glm' & 
                                          input$dv_type=='gaussian') |
                                          input$analysis=='aov',
                                       'F', 'LRT')))
    }
    return(sum)
  })

##### UI element for fit_summary object ####################################
  output$fit_output <- renderUI({
    if(is.null(input$run_analysis) || input$run_analysis==0){return(NULL)}
      verbatimTextOutput('fit_summary')
  })
  
##### UI element - the "run analysis" button ################################
  output$action_button <- renderUI({
    actionButton('run_analysis', 'Run analysis')
  })

##### model check #############################################################

pois <- renderPrint({
  if(is.pois(dat2()[input$dv])){
    h3('Your dependent variable may be poisson distributed. Consider running a generalized linear model and selecting "count" Where asked "What type of data is your dependent variable?" See the help tab on data analysis for more information')
  }
})


  
##### the code for reading in the data ###################################
  read_code <- reactive({
    if(length(input$data_file)==0 && !input$use_sample_data){return(NULL)}
    if(!input$use_sample_data){
    filestr <- gsub('(?<=file \\= ").*(?="\\))', 
                    input$data_file$name, perl=TRUE,
                    read.expr())
    filestr <- paste0('my_data <- ', filestr)
    } else {
      filestr <- read.expr()
    }
    return(filestr)
  })
 
##### UI update for the reading in code display ##########################
  observe({
    updateAceEditor(session, 'code_used_read', value=read_code(),
                    readOnly=TRUE, wordWrap=TRUE)
  })
  
##### return the code used to run the model ##############################
 mcode <- reactive({
   if(is.null(input$analysis) || input$run_analysis==0){return(NULL)}
    mcode <- paste0('model.fit <- ', fit.expr())
   
   mcode <- paste(mcode, sep='', collapse='')
   mcode <- gsub('(\\, )', ',\n\t\t\t', mcode)
   
   
   analysis.name <- c('aov'='ANOVA', 'glm'='(generalized) linear model',
                      't.test'='t-test')[input$analysis]
   
   factor.ind <- which(sapply(dat2(), is.factor))
    if(length(factor.ind)>0){
      factor.name <- names(dat2()[factor.ind])
      
      fcode <- paste0('my_data$', factor.name, ' <- as.factor(my_data$', 
                      factor.name, ')', collapse='\n')
      mcode <- paste0('# convert categorical variables to factors\n', 
                      fcode, '\n\n# run ', analysis.name,'\n',
                      mcode)
    }
   return(mcode)
  })
  
##### update the editor to display code used ##############################
  observe({
    input$run_analysis
    updateAceEditor(session, 'code_used_model', value=isolate(mcode()),
                    readOnly=TRUE, wordWrap=TRUE)
  })
  
##### generate plots from the effects package ###############################
 ef <- reactive({
    input$update_plots
    if(input$analysis=='t.test'){
      return(NULL)
    }
    #browser()
    coeff.ind <- row.names(anova(fit()))[which(!row.names(anova((fit()))) %in% c('NULL', 'Residuals'))]
   ef <- vector('list', length(coeff.ind))
   names(ef) <- coeff.ind
   assign('my_data', dat2(), env=.GlobalEnv)
   assign('.fit', fit(), env=.GlobalEnv)
   for(i in names(ef)){
     ef[[i]] <- effect(i, .fit)
   }
   remove('.fit', env=.GlobalEnv)
   remove('my_data', envir=.GlobalEnv)
   return(ef)
 })
 
##### server element - generate the plots ######################################
 output$effect_plots <- renderPlot({
   if(input$analysis=='t.test'){
     return(NULL)
   }
   ps <- lapply(ef(), plot, ci.style='bars', multiline=TRUE, 
                colors=rep('black', length(ef())))
   for(i in seq_along(ps)){
     class(ps[[i]]) <- 'trellis'
   }
   p <- do.call(arrangeGrob, 
                c(ps, ncol=2))
   return(p)
 })
 
##### UI element - print the plots ##########################################
 output$plots <- renderUI({
   if(is.null(fit())){
     return(NULL)
   }
   if(input$analysis=='t.test'){
     return(h4('Effect plots not available with t-tests'))
   }
   h <- (length(ef()) + 1) %/% 2
   plotOutput('effect_plots', 
              height=h * 450)
 })
  

##### Histograms #############################################################
output$histograms <- renderPlot({
  varinds <- which(sapply(dat2(), is.numeric2))
  par(mfrow=c((length(varinds) + 1) %/% 2, 2))
  for(i in names(dat2())[varinds]){
    hist(dat2()[,i], main=i, xlab='')
  }
  })


output$hists <- renderUI({
  if(is.null(dat2())){return(NULL)}
  h <- (length(which(sapply(dat2(), is.numeric2))) + 1) %/% 2
  plotOutput('histograms',
             height=h *450)
})

##### Download report #####################################################
output$download_report <- downloadHandler(
  filename = function() {
    paste0(input$analysis, '_analysis_report_', Sys.Date(),'.html')
  },
  content = function(file) {
    src <- normalizePath('report.Rmd')
    
    file.copy(src, 'report.Rmd')
    
    out <- knit2html('report.Rmd', 
                     output=paste0(input$analysis, '_analysis_report_', Sys.Date(),'.html'))
    file.rename(out, file)
  }
)


 
##### stop the session after it's over #########################################
  session$onSessionEnded(function() { 
    stopApp()
  })
  
})