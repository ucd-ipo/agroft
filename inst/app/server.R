library(shiny)

# This package has bindings for some cool twitter bootstrap UI stuff that shiny
# doesn't include. Includes the modals, collapse panels, and tool tips.
library(shinyBS)

# for displaying R code
library(shinyAce)

# for reading in excel files,
# It is a package that has no Java dependencies (only c++), so once
# binaries are on CRAN, anyone can install it (no JRE required!) and the app can
# have functionality to read in excel files.
#library(readxl)

library(agricolae) # for sample datasets and LSD.Test()

library(car)  # for leveneTest()

library(Rmisc)  # for summarySE()

library(ggplot2)  # for ggplot(), etc.

library(nlme) #for split plot designs

# for loading dynamic reports. I don't use rmarkdown because that requires that
# pandoc be installed which is a whole different ballgame. knitr doesn't require
# dependencies like that
library(knitr)

shinyServer( function(input, output, session) {
  
  #############################################################################
  # Load Data Tab
  #############################################################################
  
  output$debug <- renderText({
    
  })
  
  GetLoadCall <- reactive({
    # Returns one of three things:
    # 1. call containing the read.csv unevaluated function call
    # 2. name containing the name of the agricolae data set
    # 3. NULL
    # if no file has been uploaded or a file has been uploaded but it is of
    # length zero (i.e. corrupt or weird filetype) and the user hasn't selected
    # use sample data, then... make the reactive expression "GetLoadCall" NULL,
    # otherwise, do read in data things
    if ((is.null(input$data_file) || length(input$data_file) == 0) &&
        !input$use_sample_data) {
      return(NULL)
    } else {
      # if a data file has been uploaded and the user doesn't want to use sample
      # data...
      if (length(input$data_file) > 0 && !input$use_sample_data) {
        # uncomment once readxl gets on CRAN
        #         readcall <- switch(file_ext(input$data_file$name),
        #                            'csv'='read.csv',
        #                            'xls'='read_excel',
        #                            'xlsx'='read_excel')
        # make the call "read.csv". It is set up like this so that when the
        # readxl is enabled, we can change the call depeding on the file tpe
        # uploaded
        # # this is sep us as a call rather than just the function so it is easy
        # to print to the example code section of the app via deparse()
        readcall <- 'read.csv'
        load.call <- call(readcall, file=input$data_file$datapath)
      } else {
        # if there is no data selected and the user wants to use sample data dat
        # should be the name of the dataset to use
        load.call <- as.name(input$sample_data_buttons)
      }
    }
    return(load.call)
  })
  
  LoadData <- reactive({
    # Returns a data.frame with the appropriate data set or NULL.
    
    # TODO : This should only run if GetLoadCall returns a name.
    # TODO : This should probably be in the GetLoadCall function.
    # The following loads the selected agricolae data set into the workspace,
    # regardless if the user selects "Use sample data instead". The name of the
    # data.frame is the same as input$sample_data_buttons.
    eval(call('data', 
              input$sample_data_buttons, 
              package='agricolae',
              envir=environment()))
    
    # set "data" to the GetLoadCall object
    # remeber that GetLoadCall is either NULL, the call to read.csv, or the name of
    # the dataset the user wants.
    # if we eval NULL, data is set to NULL, if we eval read.csv, we get the
    # data the user submitted. If we eval the name of the data the user
    # wanted, that data is assigned to data (because we read it into the
    # workspace via the call to 'data' above)
    data <- eval(GetLoadCall(), envir=environment())
    
    return(data)
    
  })
  
  GetSimpleLoadExpr <- reactive({
    # Returns a character containing the code an R user would use to load the
    # data.
    
    # if they aren't using sample data, deparse the call to get the character
    # represntation of the call, otherwise, construct it from scratch
    if (!input$use_sample_data) {
      return(deparse(GetLoadCall()))
    } else {
      l <- "# load the agricolae package for the sample data"
      l <- paste0(l, "\nlibrary('agricolae')")
      l <- paste0(l, "\ndata('",input$sample_data_buttons,  "')")
      l <- paste0(l, '\nmy.data <- ', input$sample_data_buttons)
    }
  })
  
  ReadCode <- reactive({
    if (length(input$data_file) == 0 && !input$use_sample_data) {
      return(NULL)
    }
    if (!input$use_sample_data) {
      filestr <- gsub('(?<=file \\= ").*(?="\\))',
                      input$data_file$name, perl=TRUE,
                      GetSimpleLoadExpr())
      filestr <- paste0('my.data <- ', filestr)
    } else {
      filestr <- GetSimpleLoadExpr()
    }
    return(filestr)
  })
  
  observe({
    # Updates the load code editor.
    updateAceEditor(session, 'code_used_read', value=ReadCode(), readOnly=TRUE)
  })
  
  output$data_table <- renderDataTable({LoadData()})
  
  #############################################################################
  ##### Analysis Tab #####
  #############################################################################
  
  #---------------------------------------------------------------------------#
  # Functions used to run the analyses.
  #---------------------------------------------------------------------------#
  
  ConvertData <- reactive({
    # convert variables to their respective types
    raw.data <- LoadData()
    col.names <- names(raw.data)
    for(i in 1:ncol(raw.data)){
      # each input ID was apended "_recode" so for each column of the raw.data take
      # that value and recode it to the correct value based on the input id.
      var_type <- input[[paste0(col.names[i], '_recode')]]
      # since the input value will be either numeric or factor, you can paste
      # "as." in front of it to create the function call we'll use. then convert
      # that variable type and return the raw.data with the converted variables. Call
      # it raw.data so it doensn't get mixed up with LoadData()
      raw.data[,i] <- eval(call(paste0('as.', var_type), raw.data[,i]))
    }
    return(raw.data)
  })
  
  ComputeExponent <- reactive({
    # Returns the exponent numeric to be used in the power transformation.
    if (input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
      form = paste(input$dependent.variable, '~',
                   input$independent.variable.one)
    } else {
      form = paste(input$dependent.variable, '~',
                   input$independent.variable.one, '+',
                   input$independent.variable.two)
    }
    mean.data <- aggregate(as.formula(form), data = ConvertData(),
                           function(x) c(logmean = log10(mean(x)),
                                         logvar = log10(var(x))))
    power.fit <- lm(logvar ~ logmean,
                    data = as.data.frame(mean.data[[input$dependent.variable]]))
    power <- 1 - summary(power.fit)$coefficients[2, 1] / 2
    return(power)
  })
  
  TransformedDepVarColName <- function() {
    # Returns the transformed dependent variable name.
    dep.var <- input$dependent.variable
    choices = c('None' = dep.var,
                'Power' = paste0(dep.var, '.pow'),
                'Logarithmic' = paste0(dep.var, '.log10'),
                'Square Root' = paste0(dep.var, '.sqrt'))
    return(choices[[input$transformation]])
  }
  
  AddTransformationColumns <- reactive({
    # Returns the converted data frame with three new columns for the three
    # transformations.
    data <- ConvertData()
    dep.var <- input$dependent.variable
    trans.dep.var <- TransformedDepVarColName()
    dep.var.col <- data[[dep.var]]
    if (input$transformation == 'Power') {
      data[[trans.dep.var]] <- dep.var.col^ComputeExponent()
    } else if (input$transformation == 'Logarithmic') {
      data[[trans.dep.var]] <- log10(dep.var.col)
    } else if (input$transformation == 'Square Root') {
      data[[trans.dep.var]] <- sqrt(dep.var.col)
    }
    return(data)
  })
  
  GenerateFormula <- reactive({
    left.side <- paste(TransformedDepVarColName(), '~')
    if (input$exp.design %in% c('LR', 'CRD1')) {
      right.side <- input$independent.variable.one
    } else if (input$exp.design == 'CRD2') {
      right.side <- paste0(input$independent.variable.one, ' + ',
                           input$independent.variable.two, ' + ',
                           input$independent.variable.one, ':',
                           input$independent.variable.two)
    } else if (input$exp.design == 'RCBD1') {
      right.side <- paste0(input$independent.variable.one, ' + ',
                           input$independent.variable.blk)
    } else if (input$exp.design == 'RCBD2') {
      right.side <- paste0(input$independent.variable.one, ' + ',
                           input$independent.variable.two, ' + ',
                           input$independent.variable.one, ':',
                           input$independent.variable.two, ' + ',
                           input$independent.variable.blk)
    } else if (input$exp.design %in%  c('SPRCBD', 'SPCRD')) {
      right.side <- paste0(input$independent.variable.one, ' * ',
                           input$independent.variable.two)
    }
    form <- paste(left.side, right.side)
    return(form)
  })
  
  GenerateRandomEffFormula <- reactive({
    if(input$exp.design %in%  c('SPRCBD', 'SPCRD')){
      f <- paste0('~ 1|', input$independent.variable.blk, '/', input$independent.variable.one)
      return(f)
    }
  })
  
  GetFitCall <- reactive({
    # Returns the call used to run the analysis.
    
    # The following line forces this reactive expression to take a dependency on
    # the "Run Analysis" button. Thus this will run anytime it is clicked.
    input$run_analysis
    
    # isolate prevents this reactive expression from depending on any of the
    # variables inside the isolated expression.
    isolate({
      if (input$exp.design %in%  c('SPRCBD', 'SPCRD')){
        fit <- call('lme',
                    fixed   = as.formula(GenerateFormula()),
                    random  = as.formula(GenerateRandomEffFormula()),
                    data    = as.name('my.data'))
      } else {
        fit <- call('aov',
                    formula = as.formula(GenerateFormula()),
                    data = as.name('my.data'))
      }
    })
    
    return(fit)
    
  })
  
  EvalFit <- reactive({
    # Returns the fit model.
    
    # Run every time the "Run Analysis" button is pressed.
    input$run_analysis
    
    isolate({
      my.data <- AddTransformationColumns()
      model.fit <- eval(GetFitCall())
    })
    
    return(model.fit)
    
  })
  
  GetFitExpr <- reactive({
    # Returns the char of the expression used to evaluate the fit.
    
    # Run every time the "Run Analysis" button is pressed.
    input$run_analysis
    
    isolate({
      x <- deparse(GetFitCall(), width.cutoff=500L)
    })
    
    return(x)
    
  })
  
  ModelFitWithoutError <- reactive({
    # Returns the model fit from formulas with the Error() term removed.
    input$run_analysis
    isolate(exp.design <- input$exp.design)
    if (exp.design %in% c('SPCRD', 'SPRCBD')) {
      my.data <- AddTransformationColumns()
      model.fit <- aov(formula = as.formula(GenerateFormula()),
                       data = my.data)
    } else {
      model.fit <- EvalFit()
    }
    return(model.fit)
  })
  
  GenerateIndividualFormulas <- reactive({
    # Returns single variate formulas.
    dep.var <- TransformedDepVarColName()
    if (input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
      f <- paste0(dep.var, ' ~ ',
                  input$independent.variable.one)
      l <- list()
      l[[f]] <- as.formula(f)
      return(l)
    } else {
      f1 <- paste0(dep.var, ' ~ ',
                   input$independent.variable.one)
      f2 <- paste0(dep.var, ' ~ ',
                   input$independent.variable.two)
      # should levenes test be used for block variable in split plot designs?
      l <- list()
      l[[f1]] <- as.formula(f1)
      l[[f2]] <- as.formula(f2)
      return(l)
    }
  })
  
  GenerateTukeyFormula <- reactive({
    dep.var <- TransformedDepVarColName()
    return(paste0(GenerateFormula(),
                  ' + ', dep.var, '.pred.sq'))
  })
  
  GenerateAnalysisCode <- reactive({
    # Returns the R code a user would type to run the analysis.
    if (input$run_analysis==0) {
      return(NULL)
    } else {
      # analysisCode for converting columns to factors
      factor.idx <- which(sapply(ConvertData(), is.factor))
      if (length(factor.idx) > 0) {
        factor.names <- names(ConvertData()[factor.idx])
        analysisCode <- paste0('my.data$', factor.names, ' <- as.factor(my.data$',
                               factor.names, ')', collapse='\n')
        analysisCode <- paste0('# convert categorical variables to factors\n', analysisCode)
      }
      
      # analysisCode for the transformation
      dep.var <- input$dependent.variable
      if (input$transformation == 'Power') {
        analysisCode <- paste0(analysisCode, '\n\n# transform the dependent variable\n')
        if (input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
          analysisCode <- paste0(analysisCode, 'mean.data <- aggregate(', dep.var, ' ~ ',
                                 input$independent.variable.one)
        } else {
          analysisCode <- paste0(analysisCode, 'mean.data <- aggregate(', dep.var, ' ~ ',
                                 input$independent.variable.one, ' + ',
                                 input$independent.variable.two)
        }
        analysisCode <- paste0(analysisCode,
                               ', data = my.data, function(x) ',
                               'c(logmean=log10(mean(x)), logvar=log10(var(x))))\n',
                               'power.fit <- lm(logvar ~ logmean, ',
                               'data = as.data.frame(mean.data$', dep.var, '))\n',
                               'power <- 1 - summary(power.fit)$coefficients[2, 1] / 2\n',
                               'my.data$', dep.var, '.pow <- my.data$', dep.var,
                               '^power')
      } 
      if (input$transformation == 'Logarithmic') {
        analysisCode <- paste0(analysisCode, '\n\n# transform the dependent variable\nmy.data$',
                               input$dependent.variable, '.log10 <- log10(my.data$',
                               input$dependent.variable, ')')
      } 
      if (input$transformation == 'Square Root') {
        analysisCode <- paste0(analysisCode, '\n\n# transform the dependent variable\nmy.data$',
                               input$dependent.variable, '.sqrt <- sqrt(my.data$',
                               input$dependent.variable, ')')
      }
      
      # analysisCode for the model fit and summary
      if (input$exp.design %in% c('SPRCBD', 'SPCRD')){
        summaryExpr <- 'anova'
      } else {
        summaryExpr <- 'summary'
      }
      
      analysisCode <- paste0(analysisCode, '\n\n# fit the model\n')
      analysisCode <- paste0(analysisCode, 'model.fit <- ', GetFitExpr())
      analysisCode <- paste0(analysisCode, sprintf('\n\n# print summary table\n%s(model.fit)', summaryExpr))
      
      # analysisCode for the assumptions tests
      if (!input$exp.design %in% c('SPCRD', 'SPRCBD')) {
        analysisCode <- paste0(analysisCode,
                               '\n\n# assumptions tests\nshapiro.test(residuals(model.fit))')
      }
      
      if (input$exp.design != 'LR') {
        formulas <- GenerateIndividualFormulas()
        levene.calls <- paste0('leveneTest(', formulas, ', data = my.data)',
                               collapse = '\n')
        analysisCode <- paste0(analysisCode, "\n\n# Levene's Test\nlibrary('car')\n", levene.calls)
      }
      
      trans.dep.var <- TransformedDepVarColName()
      if (!input$exp.design %in% c('LR', 'CRD1', 'CRD2')) {
        # TODO : I'm not sure this is the correct thing to do for split plot
        # Tukey tests.
        #         if (input$exp.design %in% c('SPCRD', 'SPRCBD')) {
        #           fit.name <- 'model.fit.no.error'
        #           fit.line <- paste0(fit.name, ' <- aov(',
        #                              GenerateFormula(), ', data = my.data)\n')
        #         } else {
        fit.name <- 'model.fit'
        fit.line <- ''
        #         }
        analysisCode <- paste0(analysisCode, "\n\n# Tukey's Test for Nonadditivity\n", fit.line,
                               "my.data$", trans.dep.var,
                               ".pred.sq <- predict(", fit.name, ")^2\n",
                               "tukey.one.df.fit <- lm(formula = ",
                               GenerateTukeyFormula(),
                               ", data = my.data)\nanova(tukey.one.df.fit)")
      }
      # browser()
      return(analysisCode)
    }
  })
  
  observe({
    # Updates the analysis code in the editor.
    GenerateAnalysisCode()
    updateAceEditor(session, 'code_used_model',
                    value=isolate(GenerateAnalysisCode()), readOnly=TRUE)
  })
  
  # TODO : It could be useful to break this up into each plot and utilize this
  # code for actual evaluation later on to deduplicate the code.
  MakePlotAnalysisCode <- reactive({
    if (input$exp.design %in% c('SPCRD', 'SPRCBD')) {
      analysisCode <- paste0("# Residuals vs. Fitted\nplot(model.fit.no.error, which = 1)")
      analysisCode <- paste0(analysisCode, "\n\n# Kernel Density Plot",
                             "\nplot(density(residuals(model.fit.no.error)))")
    } else {
      analysisCode <- paste0("# Residuals vs. Fitted\nplot(model.fit, which = 1)")
      analysisCode <- paste0(analysisCode, "\n\n# Kernel Density Plot\nplot(density(residuals(model.fit)))")
    }
    
    if (input$exp.design == 'LR') {
      analysisCode <- paste0(analysisCode, "\n\n# Best Fit Line\nplot(formula = ",
                             GenerateFormula(), ", data = my.data)\nabline(model.fit)")
    } else {
      dep.var <- TransformedDepVarColName()
      ind.var.one <- input$independent.variable.one
      ind.var.two <- input$independent.variable.two
      f1 <- paste0(dep.var, ' ~ ', ind.var.one)
      main <- paste0("Effect of ", ind.var.one, " on ",
                     dep.var)
      analysisCode <- paste0(analysisCode, "\n\n# Effects Box Plots\n",
                             "boxplot(", f1, ", data = my.data, main = '", main,
                             "', xlab = '", ind.var.one,
                             "', ylab = '", dep.var,  "')")
      if (!input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
        f2 <- paste0(dep.var, ' ~ ', ind.var.two)
        main <- paste0("Effect of ", ind.var.two, " on ",
                       dep.var)
        analysisCode <- paste0(analysisCode, "\nboxplot(", f2, ", data = my.data, main = '",
                               main, "', xlab = '", ind.var.two,
                               "', ylab = '", dep.var,  "')")
      }
    }
    
    if (!input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
      analysisCode <- paste0(analysisCode, "\n# Interaction Plots\n",
                             "interaction.plot(my.data$", ind.var.one, ", my.data$",
                             ind.var.two, ", my.data$", dep.var, ", xlab = '",
                             ind.var.one, "', trace.label = '", ind.var.two, "', ylab = '",
                             dep.var, "')")
      analysisCode <- paste0(analysisCode, "\ninteraction.plot(my.data$", ind.var.two,
                             ", my.data$", ind.var.one, ", my.data$", dep.var, ", xlab = '",
                             ind.var.two, "', trace.label = '", ind.var.one,
                             "', ylab = '", dep.var, "')")
    }
    
    return(analysisCode)
  })
  
  #---------------------------------------------------------------------------#
  # UI elements for the analysis tab side panel.
  #---------------------------------------------------------------------------#
  
  output$selectDesign <- renderUI({
    # Renders a dropdown for selecting the type of experimental design.
    if(is.null(LoadData())){
      h4('Please upload or select data first.')
    } else {
      choices <- c('Two Continous Variables' = 'LR',
                   'Completely Randomized Design (CRD) with One Treatment' = 'CRD1',
                   'Completely Randomized Design (CRD) with Two Treatments' = 'CRD2',
                   'Randomized Complete Block Design (RCBD) with One Treatment' = 'RCBD1',
                   'Randomized Complete Block Design (RCBD) with Two Treatments' = 'RCBD2',
                   'Split-Plot Completely Randomized Design' = 'SPCRD',
                   'Split-Plot Randomized Complete Block Design' = 'SPRCBD')

      selectInput('exp.design',
                  'Select Your Experimental Design',
                  choices = choices,
                  selected = NULL)
      
      # TODO : When this is selected it should clear all of the analysis related
      # input variables so nothing lingers from previous analyses, e.g.
      # independent.variable.two.
      
    }
  })
  
  output$selectDependent <- renderUI({
    # Renders a dropdown for selecting the dependent variable from the loaded
    # data.
    if (is.null(input$exp.design)) {
      h4('Please select an experimental design first.')
      input$dependent.variable = NULL
    } else {
      selectInput('dependent.variable',
                  'Select a dependent variable:',
                  choices = names(LoadData()),
                  selected = NULL)
    }
  })
  
  output$selectIndependent <- renderUI({
    # Renders a number of dropdowns for selecting in independent variables.
    # TODO : This should check that the variable type panel has been run,
    # otherwise `ConvertData()` will fail.
    if (is.null(input$dependent.variable)) {
      return(NULL)
    } else {
      all.col.names <- names(ConvertData())
      choices = all.col.names[!(all.col.names %in% input$dependent.variable)]
      if (input$exp.design == 'LR') {
        selectInput('independent.variable.one',
                    'Select a single continous independent variable:',
                    choices = choices,
                    selected = NULL)
      } else if (input$exp.design == 'CRD1') {
        selectInput('independent.variable.one',
                    'Select a single independent factor variable:',
                    choices = choices,
                    selected = NULL)
      } else if (input$exp.design == 'CRD2') {
        input1 <- selectInput('independent.variable.one',
                              'Select the first independent factor variable:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.two',
                              'Select the second independent factor variable:',
                              choices = choices,
                              selected = NULL)
        return(list(input1, input2))
      } else if (input$exp.design == 'RCBD1') {
        input1 <- selectInput('independent.variable.one',
                              'Select the first independent factor variable:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.blk',
                              'Select the blocking factor variable:',
                              choices = choices,
                              selected = NULL)
        return(list(input1, input2))
      } else if (input$exp.design == 'RCBD2') {
        input1 <- selectInput('independent.variable.one',
                              'Select the first independent factor variable:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.two',
                              'Select the second independent factor variable:',
                              choices = choices,
                              selected = NULL)
        input3 <- selectInput('independent.variable.blk',
                              'Select the blocking factor variable:',
                              choices = choices,
                              selected = NULL)
        return(list(input1, input2, input3))
      } else if (input$exp.design == 'SPCRD') {
        input1 <- selectInput('independent.variable.one',
                              'Select the main plot treatment:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.two',
                              'Select the sub plot treatment:',
                              choices = choices,
                              selected = NULL)
        input3 <- selectInput('independent.variable.blk',
                              'Select the repetition:',
                              choices = choices,
                              selected = NULL)
        return(list(input1, input2, input3))
      } else if (input$exp.design == 'SPRCBD') {
        input1 <- selectInput('independent.variable.one',
                              'Select the main plot treatment:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.two',
                              'Select the sub plot treatment:',
                              choices = choices,
                              selected = NULL)
        input3 <- selectInput('independent.variable.blk',
                              'Select the blocking factor variable:',
                              choices = choices,
                              selected = NULL)
        return(list(input1, input2, input3))
      }
    }
  })
  
  output$var_types_select <- renderUI({
    # Renders a series of radio buttons for selecting a type for each varible:
    # numeric or factor.
    
    raw.data <- LoadData()
    raw.data.col.names <- names(raw.data)
    
    if (is.null(raw.data)) {
      return(NULL)
    }
    
    class.recode <- c('character'='factor',
                      'factor'='factor',
                      'logical'='factor',
                      'numeric'='numeric',
                      'integer'='numeric')
    btns <- list()
    # a loop to create radio buttons for each variable in the data
    for(i in 1:ncol(raw.data)){
      clss <- class(raw.data[,i])
      clss <- class.recode[clss]
      
      btns[[i]] <- radioButtons(inputId=paste0(raw.data.col.names[i], '_recode'),
                                label=raw.data.col.names[i],
                                choices=c('Numeric'='numeric', 'Grouping'='factor'),
                                selected=clss,
                                inline=TRUE)
    }
    return(btns)
  })
  
  #---------------------------------------------------------------------------#
  # UI elements for the analysis tab main panel.
  #---------------------------------------------------------------------------#
  
  output$formula <- renderText({
    if(is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      if (!input$exp.design %in% c('SPRCBD', 'SPCRD')) {
        GenerateFormula()
      } else {
        paste('fixed  = ', GenerateFormula(), '\nrandom = ', GenerateRandomEffFormula())
      }
    }
  })
  
  output$fit.summary.text <- renderPrint({
    isolate({
      input$run_analysis
      fit.summary <- anova(EvalFit())
    })
    return(fit.summary)
  })
  
  output$fit.summary <- renderUI({
    if(is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      list(h2('Model Fit Summary'),
           if (input$exp.design != 'LR') { h3('ANOVA Table') } else{ NULL },
           verbatimTextOutput('fit.summary.text'))
    }
  })
  
  output$exponent <- renderUI({
    # Renders a paragraph tag with the computed exponent value.
    if (input$transformation == 'Power') {
      header <- h2('Exponent from Power Transformation')
      text <- p(as.character(ComputeExponent()))
      return(list(header, text))
    } else {
      return(NULL)
    }
  })
  
  output$shapiro.wilk.results.text <- renderPrint({
    input$run_analysis
    # NOTE : We don't do the Shapiro-Wilk test for the split plot designs
    # because it isn't straight forward to implement.
    if (!input$exp.design %in% c('SPCRD', 'SPRCBD')) {
      isolate({fit <- EvalFit()})
      return(shapiro.test(residuals(fit)))
    } else {
      return(cat(paste0("Shapiro-Wilk Normality Test is not performed because ",
                        "it is not straightforward for split-plot designs.")))
    }
  })
  
  output$shapiro.wilk.results <- renderUI({
    if(is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      list(h2('Shapiro-Wilk Normality Test Results'),
           verbatimTextOutput('shapiro.wilk.results.text'))
    }
  })
  
  output$levene.results.text <- renderPrint({
    input$run_analysis
    isolate({
      formulas <- GenerateIndividualFormulas()
      my.data <- AddTransformationColumns()
    })
    return(lapply(formulas, leveneTest, data = my.data))
  })
  
  output$levene.results <- renderUI({
    if(is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      if (input$exp.design != 'LR') {
        list(h2("Levene's Test for Homogeneity of Variance"),
             verbatimTextOutput('levene.results.text'))
      } else {
        return(NULL)
      }
    }
  })
  
  output$tukey.results.text <- renderPrint({
    input$run_analysis
    isolate({
      # TODO : Check to make sure this is what I'm supposed to use for the split
      # plot results.
      fit <- ModelFitWithoutError()
      my.data <- AddTransformationColumns()
      dep.var <- TransformedDepVarColName()
      my.data[[paste0(dep.var, '.pred.sq')]] <- predict(fit)^2
      f <- GenerateTukeyFormula()
      tukey.one.df.fit <- lm(formula = as.formula(f), data = my.data)
    })
    return(anova(tukey.one.df.fit))
  })
  
  output$tukey.results <- renderUI({
    if(is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      if (!input$exp.design %in% c('LR', 'CRD1', 'CRD2')) {
        isolate({
          dep.var <- TransformedDepVarColName()
          pred.var <- paste0(dep.var, '.pred.sq')
        })
        list(h2("Tukey's Test for Nonadditivity"),
             p(strong(paste0("Attention: Refer only to the '", pred.var, "' ",
                             "row in this table, ignore all other rows."))),
             verbatimTextOutput('tukey.results.text'))
      } else {
        return(NULL)
      }
    }
  })
  
  output$plot.residuals.vs.fitted <- renderPlot({
    input$run_analysis
    model.fit <- ModelFitWithoutError()
    plot(model.fit, which = 1)
  })
  
  output$residuals.vs.fitted.plot <- renderUI({
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      list(h2('Residuals vs Fitted'),
           plotOutput('plot.residuals.vs.fitted'))
    }
  })
  
  output$plot.kernel.density <- renderPlot({
    input$run_analysis
    model.fit <- ModelFitWithoutError()
    plot(density(residuals(model.fit)))
  })
  
  output$kernel.density.plot <- renderUI({
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      list(h2('Kernel Density of the Residuals'),
           plotOutput('plot.kernel.density'))
    }
  })
  
  output$plot.best.fit <- renderPlot({
    input$run_analysis
    f <- paste0(input$dependent.variable, ' ~ ',
                input$independent.variable.one)
    my.data <- AddTransformationColumns()
    plot(formula = as.formula(f), data = my.data)
    model.fit <- ModelFitWithoutError()
    abline(model.fit)
  })
  
  output$best.fit.plot <- renderUI({
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      if (input$exp.design == 'LR') {
        list(h2('Best Fit'),
             plotOutput('plot.best.fit'))
      } else {
        return(NULL)
      }
    }
  })
  
  output$plot.boxplot.one <- renderPlot({
    input$run_analysis
    dep.var <- TransformedDepVarColName()
    if (input$exp.design != 'LR') {
      my.data <- AddTransformationColumns()
      f1 <- paste0(dep.var, ' ~ ',
                   input$independent.variable.one)
      boxplot(as.formula(f1), data = my.data,
              main = paste0("Effect of ", input$independent.variable.one,
                            " on ", dep.var),
              xlab = input$independent.variable.one,
              ylab = dep.var)
    }
  })
  
  output$plot.boxplot.two <- renderPlot({
    input$run_analysis
    dep.var <- TransformedDepVarColName()
    if (!input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
      my.data <- AddTransformationColumns()
      f2 <- paste0(dep.var, ' ~ ',
                   input$independent.variable.two)
      boxplot(as.formula(f2), data = my.data,
              main = paste0("Effect of ", input$independent.variable.two,
                            " on ", dep.var),
              xlab = input$independent.variable.two,
              ylab = dep.var)
    }
  })
  
  output$boxplot.plot <- renderUI({
    input$run_analysis
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      if (input$exp.design != 'LR') {
        if (!input$exp.design %in% c('CRD1', 'RCBD1')) {
          elements <- list(h2('Effects Box Plots'),
                           plotOutput('plot.boxplot.one'),
                           plotOutput('plot.boxplot.two'))
        } else {
          elements <- list(h2('Effects Box Plots'),
                           plotOutput('plot.boxplot.one'))
        }
        return(elements)
      } else {
        return(NULL)
      }
    }
  })
  
  output$plot.interaction.one <- renderPlot({
    input$run_analysis
    isolate({
      if (!input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
        dep.var <- TransformedDepVarColName()
        ind.var.one <- input$independent.variable.one
        ind.var.two <- input$independent.variable.two
        my.data <- AddTransformationColumns()
        interaction.plot(my.data[[ind.var.one]], my.data[[ind.var.two]],
                         my.data[[dep.var]], xlab = ind.var.one, trace.label =
                           ind.var.two, ylab = dep.var)
      }
    })
  })
  
  output$plot.interaction.two <- renderPlot({
    input$run_analysis
    isolate({
      if (!input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
        dep.var <- TransformedDepVarColName()
        ind.var.one <- input$independent.variable.one
        ind.var.two <- input$independent.variable.two
        my.data <- AddTransformationColumns()
        interaction.plot(my.data[[ind.var.two]], my.data[[ind.var.one]],
                         my.data[[dep.var]], xlab = ind.var.two, trace.label =
                           ind.var.one, ylab = dep.var)
      }
    })
  })
  
  output$interaction.plot <- renderUI({
    input$run_analysis
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      return(NULL)
    } else {
      if (!input$exp.design %in% c('LR', 'CRD1', 'RCBD1')) {
        return(list(h2('Interaction Plots'),
                    plotOutput('plot.interaction.one'),
                    plotOutput('plot.interaction.two')))
      } else {
        return(NULL)
      }
    }
  })
  
  #############################################################################
  # Post hoc tab
  #############################################################################
  
  MakePostHocPlot <- function(data, fit, dep.var, ind.var) {
    lsd.results <- LSD.test(fit, ind.var)
    summary.stats <- summarySE(data = data, dep.var, groupvars = ind.var)
    if (length(ind.var) == 2) {
      summary.stats$trt <- apply(summary.stats[ , ind.var], 1, paste,
                                 collapse = ":")
      x.label = paste(ind.var, collapse = ":")
    } else {
      summary.stats$trt <- summary.stats[[ind.var]]
      x.label = ind.var
    }
    merged.table <- merge(summary.stats, lsd.results$groups, by = "trt")
    ggplot(merged.table, aes(x = trt, y = means, ymin = 0,
                             ymax = 1.35 * max(means))) +
      geom_bar(stat = "identity", fill = "gray50", colour = "black", width = 0.7) +
      geom_errorbar(aes(ymax = means + se, ymin = means - se), width = 0.0,
                    size = 0.5, color = "black") +
      geom_text(aes(label = M, y = means + se / 1.8, vjust = -2.5)) +
      labs(x = x.label, y = dep.var) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "grey80"),
            plot.title = element_text(size = rel(1.5),
                                      face = "bold", vjust = 1.5),
            axis.title = element_text(face = "bold"),
            axis.title.y = element_text(vjust= 1.8),
            axis.title.x = element_text(vjust= -0.5),
            panel.border = element_rect(colour = "black"),
            text = element_text(size = 20))
  }
  
  # TODO : Clean up this insane nested if statement! Sorry...
  output$lsd.results <- renderUI({
    input$run_post_hoc_analysis
    if (is.null(input$run_post_hoc_analysis) || input$run_post_hoc_analysis == 0) {
      return(NULL)
    } else {
      isolate({
        exp.design <- input$exp.design
        dep.var <- TransformedDepVarColName()
        ind.var.one <- input$independent.variable.one
        if (exp.design %in% c('CRD1', 'RCBD1')) {
          ind.var.two <- NULL
        } else {
          ind.var.two <- input$independent.variable.two
        }
        ind.vars <- c(ind.var.one, ind.var.two)
        my.data <- AddTransformationColumns()
        fit <- EvalFit()
      })
      alpha <- 0.05
      if (input$exp.design == 'LR') {
        return(p('Post hoc tests are not run for simple linear regression.'))
      } else if (exp.design %in% c('CRD1', 'RCBD1')) {
        p.value <- summary(fit)[[1]]$'Pr(>F)'[1]
        if (p.value < alpha) {
          output$lsd.results.text <- renderPrint({
            LSD.test(fit, ind.vars, console = TRUE)
          })
          output$lsd.bar.plot <- renderPlot({
            MakePostHocPlot(my.data, fit, dep.var, ind.vars)
          })
          return(list(p(paste0(ind.vars, ' is significant (alpha=0.05).')),
                      verbatimTextOutput('lsd.results.text'),
                      plotOutput('lsd.bar.plot')))
        } else {
          return(p(paste0(ind.vars, ' is not significant.')))
        }
      } else if (input$exp.design %in% c('CRD2', 'RCBD2')) {
        var.one.p.value <- summary(fit)[[1]]$'Pr(>F)'[1]
        var.two.p.value <- summary(fit)[[1]]$'Pr(>F)'[2]
        if (exp.design == 'CRD2') {
          idx <- 3
        } else {
          idx <- 4
        }
        if (interaction.p.value < .05) {
          text <- paste0("The interaction, ", paste(ind.vars, collapse = ":"),
                         ", is significant (alpha = 0.05).")
          if (var.one.p.value < alpha && var.two.p.value < alpha){
            lsd.vars <- ind.vars
            text <- paste0(text, " Both factors are significant.")
          } else if (var.one.p.value < alpha) {
            text <- paste0(text, " Only ", ind.var.one, " is significant.")
            lsd.vars <- ind.var.one
          } else if (var.two.p.value < alpha) {
            text <- paste0(text, " Only ", ind.var.two, " is significant.")
            lsd.vars <- ind.var.two
          } else {
            text <- paste0(text, " Neither factor is significant.")
            return(p(text))
          }
          output$lsd.results.text <- renderPrint({
            LSD.test(fit, lsd.vars, console = TRUE)
          })
          output$lsd.bar.plot <- renderPlot({
            MakePostHocPlot(my.data, fit, dep.var, lsd.vars)
          })
          return(list(p(text),
                      verbatimTextOutput('lsd.results.text'),
                      plotOutput('lsd.bar.plot')))
        } else {
          # TODO : Implement what happens here.
          return(p(paste0('The interaction is not significant and the post ',
                          'hoc analyses for this scenario are not ',
                          'implemented.')))
        }
      } else if (input$exp.design %in% c('SPCRD', 'SPRCBD')) {
        # NOTE : This always seems to be [2] for both formulas.
          interaction.p.value <- anova(fit)[4, 'p-value']
        if (interaction.p.value < .05) {
          stuff <- list()
          for (ivars in list(ind.vars, rev(ind.vars))) {
            f <- paste0(dep.var, ' ~ ', ivars[2])
            if (exp.design %in%  c('SPRCBD', 'SPCRD')) {
              f <- paste0(f, ' + ', input$independent.variable.blk)
            }
            for (level in levels(my.data[[ivars[1]]])) {
              sub.data <- my.data[my.data[[ivars[1]]] == level, ]
              sub.model.fit <- aov(as.formula(f), sub.data)
              sub.p.value <- summary(sub.model.fit)[[1]][["Pr(>F)"]][1]
              output.name <- paste0('lsd.results.text.', ivars[1], '.', level)
              stuff[[paste0(output.name, '.heading')]] <-
                h4(paste0('Subset of ', ivars[1], ':', level))
              if (sub.p.value < alpha) {
                stuff[[output.name]] <- pre(
                  paste(capture.output(LSD.test(sub.model.fit, ivars[2], console
                                                = TRUE)), collapse = "\n"))
                # TODO : These plots work except that the plots created in the
                # first pass of this loop (for ivars...) are overwritten by
                # those from the second pass. This also happened when I was
                # using renderPrint/verbatimTextOutput for the text output and I
                # couldn't debug it. It seems like the output variable is being
                # overwritten or that the reactiveness of the functions does
                # something weird.
                #output[[paste0(output.name, '.plot')]] <- renderPlot({
                #MakePostHocPlot(sub.data, sub.model.fit, dep.var, ivars[2])
                #})
                #stuff[[paste0(output.name, '.plot')]] <-
                #plotOutput(paste0(output.name, '.plot'))
              } else {
                stuff[[output.name]] <- pre(paste0(ivars[2],
                                                   ' effect not significant, thus no LSD is performed.\n'))
              }
            }
          }
          #TODO : Compare between subplot levels across main plot levels
          return(stuff)
        } else {  # interaction is not significant
          isolate({fit.without <- ModelFitWithoutError()})
          text <- paste0("The interaction, ", paste(ind.vars, collapse = ":"),
                         ", is not significant (alpha = 0.05).")
          main.plot.p.value <- anova(fit)[ind.var.one, 'p-value']
          sub.plot.p.value <- anova(fit)[ind.var.two, 'p-value']
          if (main.plot.p.value < alpha) {
            text <- paste0(text, " ", ind.var.one, " is significant.")
            output$lsd.results.text.one <- renderPrint({
              LSD.test(fit.without, ind.var.one, console = TRUE)
            })
            output$lsd.bar.plot.one <- renderPlot({
              MakePostHocPlot(my.data, fit.without, dep.var, ind.var.one)
            })
          } else if (sub.plot.p.value < alpha) {
            text <- paste0(text, " ", ind.var.two, " is significant.")
            output$lsd.results.text.two <- renderPrint({
              LSD.test(fit.without, ind.var.two, console = TRUE)
            })
            output$lsd.bar.plot.two <- renderPlot({
              MakePostHocPlot(my.data, fit.without, dep.var, ind.var.two)
            })
          } else {
            text <- paste0(text,
                           " Neither the main plot or sub plot is significant.")
            return(p(text))
          }
          return(list(p(text),
                      verbatimTextOutput('lsd.results.text.one'),
                      plotOutput('lsd.bar.plot.one'),
                      verbatimTextOutput('lsd.results.text.two'),
                      plotOutput('lsd.bar.plot.two')))
        }
      }
    }
  })
  
  #############################################################################
  # Report tab
  #############################################################################
  
  output$download_report_button <- renderUI({
    if (input$run_analysis == 0){
      list(bsButton('download_report0_button',
               "Download Report"),
      bsModal('download_report0_modal',
              trigger='download_report0_button',
              title='Download report error',
              h5('You must run an analysis before downloading your report.')))
    } else {
      downloadButton('download_report', 'Download Report')
    }
  })
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      input$file.name
    },
    content = function(file) {
      template <- paste(readLines('report-template.Rmd'), collapse='\n')
      filled.template <- gsub('replace_with_data_code', ReadCode(), template)
      filled.template <- gsub('replace_with_analysis_code',
                              GenerateAnalysisCode(), filled.template)
      filled.template <- gsub('replace_with_analysis_plot_code',
                              MakePlotAnalysisCode(), filled.template)
      writeLines(filled.template, 'report.Rmd')
      src <- normalizePath('report.Rmd')
      file.copy(filled.template, 'report.Rmd')
      out <- knit2html('report.Rmd', output=input$file.name)
      file.copy(out, file)
    }
  )
  
  
  session$onSessionEnded(function() { 
    stopApp()
  })
  
})
