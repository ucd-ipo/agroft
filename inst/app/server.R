suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinyAce)) # for displaying R code
suppressPackageStartupMessages(library(car))  # for leveneTest() and Anova()
suppressPackageStartupMessages(library(nlme)) #for split plot designs
suppressPackageStartupMessages(library(knitr)) # for loading dynamic reports.
suppressPackageStartupMessages(library(lsmeans))
#library(readxl) # for reading in excel files (not yet implimented)

shinyServer( function(input, output, session) {
  
  #############################################################################
  # Load Data Tab #####
  #############################################################################
  
  output$debug <- renderText({
    lsd.displaycode()
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
      # if a data file has been uploaded and the user doesn't want to use sample
      # data...
      if (length(input$data_file) > 0 && !input$use_sample_data) {
        # uncomment once readxl gets on CRAN 
        # NOTE: readxl has issues reading files without the .xls/.xlsx file extention
        #       and shiny makes the uploaded file a temp file with no extansion
        #       so there needs to be a bit of file copying and renaming done in the background 
        #       to get around that issue. tl;dr, just uncommenting below won't work. 
        #         readcall <- switch(file_ext(input$data_file$name),
        #                            'csv'='read.csv',
        #                            'xls'='read_excel',
        #                            'xlsx'='read_excel')
        # make the call "read.csv". It is set up like this so that when the
        # readxl is enabled, we can change the call depeding on the file tpe
        # uploaded
        # # this is set up as a call rather than just the function so it is easy
        # to print to the example code section of the app via deparse()
        readcall <- 'read.csv'
        load.call <- call(readcall, file=input$data_file$datapath)
      } else {
        # if there is no data selected and the user wants to use sample data dat
        # should be the name of the dataset to use
        load.call <- as.name(input$sample_data_buttons)
      }
    # }
    return(load.call)
  })
  
  LoadData <- reactive({
    # Returns a data.frame with the appropriate data set or NULL.
    
    # The following loads the selected agricolae data set into the workspace,
    # regardless if the user selects "Use sample data instead". The name of the
    # data.frame is the same as input$sample_data_buttons.
    eval(call('data', 
              input$sample_data_buttons, 
              package='agroft',
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
      l <- paste0("\ndata('",input$sample_data_buttons,  "', package='agroft')")
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
  
  output$data_table <- renderDataTable({
    if(!input$use_sample_data & length(input$data_file) == 0){
      return(NULL)
    }
    LoadData()})
  
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
    if (exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
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
  
  AddTransformationColumns <- reactive({
    # Returns the converted data frame with a new variable housing the transformed data
    data <- ConvertData()
    dep.var <- input$dependent.variable
    dep.var.col <- data[[dep.var]]
      data[[paste0(dep.var, '.pow')]] <- dep.var.col^ComputeExponent()
      data[[paste0(dep.var, '.log10')]] <- log10(dep.var.col)
      data[[paste0(dep.var, '.sqrt')]] <- sqrt(dep.var.col)
    return(data)
  })
 
  GenerateFormula <- function(transformation){
    left.side <- switch(transformation,
                      NoTfm = input$dependent.variable,
                      PwrTfm = paste0(input$dependent.variable, '.pow'),
                      LogTfm = paste0(input$dependent.variable, '.log10'),
                      SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
    if (exp.design()[['exp.design']] == 'CRD1') {
      right.side <- input$independent.variable.one
    } 
    if (exp.design()[['exp.design']] == 'CRD2') {
      right.side <- paste0(input$independent.variable.one, ' * ',
                           input$independent.variable.two)
    }
    if (exp.design()[['exp.design']] == 'RCBD1') {
      if(exp.design()[['is_multisite']]){
        right.side <- paste0(input$independent.variable.one)
      } else {
        right.side <- paste0(input$independent.variable.one, ' + ',
                             input$independent.variable.blk)
      }
    } 
    if (exp.design()[['exp.design']] == 'RCBD2') {
      right.side <- paste0(input$independent.variable.one, ' * ',
                           input$independent.variable.two, ' + ',
                           input$independent.variable.blk)
    } 
    if (exp.design()[['exp.design']] == 'SPRCBD') {
      right.side <- paste0(input$independent.variable.one, ' * ',
                           input$independent.variable.two)
    } 
    if (exp.design()[['exp.design']] == 'SPCRD'){
      right.side <- paste0(input$independent.variable.one, ' * ',
                           input$independent.variable.two)
    }
    form <- paste(left.side, right.side, sep=' ~ ')
    return(form)
  } 

  GenerateRandomEffFormula <- reactive({
    if(exp.design()[['exp.design']] %in% c('SPRCBD', 'SPCRD')){
      f <- paste0('~ 1|', input$independent.variable.blk, '/', input$independent.variable.one)
    } 
    if (exp.design()[['is_multisite']] & exp.design()[['exp.design']] %in% c('RCBD1', 'RCBD2', 'SPRCBD')){
      f <- paste0('~ 1|', input$independent.variable.site, '/', input$independent.variable.blk)
    }
    if (exp.design()[['is_multisite']] & ! exp.design()[['exp.design']] %in% c('RCBD1', 'RCBD2', 'SPRCBD')){
      f <- paste0('~ 1|', input$independent.variable.site)
    }
    return(f)
  })
  
  GetFitCall <- function(transformation){
      if (exp.design()[['exp.design']] %in%  c('SPRCBD', 'SPCRD') | exp.design()[['is_multisite']]){
        fit <- call('lme',
                    fixed   = as.formula(GenerateFormula(transformation)),
                    random  = as.formula(GenerateRandomEffFormula()),
                    data    = as.name('my.data'))
      } else {
        fit <- call('aov',
                    formula = as.formula(GenerateFormula(transformation)),
                    data = as.name('my.data'))
      }
    return(fit)
  }


EvalFit <- function(transformation){
    my.data <- AddTransformationColumns()
    model.fit <- eval(GetFitCall(transformation), envir = my.data)
  return(model.fit)
}

  
  GetFitExpr <- reactive({
    input$view_anova_table
    # isolate({
      x <- deparse(GetFitCall(input$transformation), width.cutoff=500L)
    # })
    return(x)
  })

  
  ModelFitWithoutError <- function(transformation){
    exp.design <- exp.design()[['exp.design']]
    my.data <- AddTransformationColumns()
    if (exp.design %in% c('SPCRD', 'SPRCBD') | exp.design()[['is_multisite']]) {
      model.fit <- aov(formula = as.formula(GenerateFormula(transformation)),
                       data = my.data)
    } else {
      model.fit <- EvalFit(transformation)
    }
    return(model.fit)
  }
  
  
  GenerateIndividualFormulas <- function(transformation){
    dep.var <- switch(transformation,
                      'NoTfm' = input$dependent.variable,
                      'PwrTfm' = paste0(input$dependent.variable, '.pow'),
                      'LogTfm' = paste0(input$dependent.variable, '.log10'),
                      'SqrtTfm' = paste0(input$dependent.variable, '.sqrt'))
    if (exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
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
  }
 
  GenerateTukeyFormula <- function(transformation){
    dep.var <- switch(transformation,
                      NoTfm = input$dependent.variable,
                      PwrTfm = paste0(input$dependent.variable, '.pow'),
                      LogTfm = paste0(input$dependent.variable, '.log10'),
                      SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
    return(paste0(GenerateFormula(transformation),
                  ' + ', dep.var, '.pred.sq'))
  }
  
  
  
  GenerateAnalysisCodeFactors <- reactive({
#     if (input$run_analysis == 0) {
#       return(NULL)
#     } else {
      # analysisCode for converting columns to factors
      factor.idx <- which(sapply(ConvertData(), is.factor))
      if (length(factor.idx) > 0) {
        factor.names <- names(ConvertData()[factor.idx])
        analysisCode <-
          paste0(
            'my.data$', factor.names, ' <- as.factor(my.data$',
            factor.names, ')', collapse = '\n'
          )
        analysisCode <-
          paste0('# convert categorical variables to factors\n', analysisCode)
      }
      return(analysisCode)
    # }
  })
      
### analysisCode for the transformations ###
  
  GenerateAnalysisCodePwr <- reactive({
      dep.var <- paste0(input$dependent.variable, '.pow')
        analysisCode <- '\n# transform the dependent variable\n'
        if (exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
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
        return(analysisCode)
  })
      
      
  GenerateAnalysisCodeLog <- reactive({
 
        analysisCode <- paste0('\n# transform the dependent variable\nmy.data$',
                               input$dependent.variable, '.log10 <- log10(my.data$',
                               input$dependent.variable, ')')
        return(analysisCode)
      }) 
  
  
  GenerateAnalysisCodeSqrt <- reactive({
        analysisCode <- paste0('\n# transform the dependent variable\nmy.data$',
                               input$dependent.variable, '.sqrt <- sqrt(my.data$',
                               input$dependent.variable, ')')
        return(analysisCode)
      })
      
  
    GenerateAnalysisCodeANOVA <- reactive({
      analysisCode <- paste0('# fit the model\n')
      analysisCode <- paste0(analysisCode, 'model.fit <- ', GetFitExpr())
      analysisCode <- paste0(analysisCode, '\n\n# print summary table\nlibrary(car)\nAnova(model.fit, type = 3, singular.ok=TRUE)')
      return(analysisCode)
    })
    
    GenerateAnalysisCodeANOVA2 <- function(transformation){
      pkg <- if (exp.design()[['exp.design']] %in% c('SPCRD', 'SPRCBD') || 
                 exp.design()[['is_multisite']]) 'library(nlme)\n' else ''
      paste0('\n# fit the model\n', 
             pkg,
             'model.fit <- ', deparse(GetFitCall(transformation), width.cutoff=500L))
    }
    
    
      
    GenerateAnalysisAsump <- function(transformation){
      # analysisCode for the assumptions tests
      if (!(exp.design()[['exp.design']] %in% c('SPCRD', 'SPRCBD') || exp.design()[['is_multisite']])) {
        analysisCode <- '\n\n# assumptions tests\nshapiro.test(residuals(model.fit))'
      } else {
        analysisCode <- ''
      }

      formulas <- GenerateIndividualFormulas(transformation)
      levene.calls <- paste0('leveneTest(', formulas, ', data = my.data)',
                               collapse = '\n')
      analysisCode <- paste0(analysisCode, "\n\n# Levene's Test\nlibrary(car)\n", levene.calls)

      
      # trans.dep.var <- TransformedDepVarColName()
      if (exp.design()[['exp.design']] %in% c('RCBD1', 'RCBD2')) {
        fit.name <- 'model.fit'
        fit.line <- ''
        analysisCode <- paste0("\n\n# Tukey's Test for Nonadditivity\n", fit.line,
                               "my.data$", '%s', # %s = the transformed variable name
                               ".pred.sq <- predict(", fit.name, ")^2\n",
                               "tukey.one.df.fit <- lm(formula = ",
                               GenerateTukeyFormula(transformation),
                               ", data = my.data)\nAnova(tukey.one.df.fit, type = 3, singular.ok=TRUE)")
        dep.var <- switch(transformation,
                          NoTfm = input$dependent.variable,
                          PwrTfm = paste0(input$dependent.variable, '.pow'),
                          LogTfm = paste0(input$dependent.variable, '.log10'),
                          SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
        analysisCode <- sprintf(analysisCode, dep.var)
      }
      return(analysisCode)
    }
  
    GenerateAnalysisCode <- reactive({
      AnalysisCode <- GenerateAnalysisCodeFactors()
      AnalysisCodeAsump <- list(GenerateAnalysisAsump('NoTfm'), 
                                GenerateAnalysisAsump('PwrTfm'), 
                                GenerateAnalysisAsump('LogTfm'), 
                                GenerateAnalysisAsump('SqrtTfm'))
      TransformCode <- list(NoTfm = paste(AnalysisCode, 
                                          GenerateAnalysisCodeANOVA2('NoTfm'),
                                          AnalysisCodeAsump[[1]], 
                                          sep='\n'),
                              PwrTfm = paste(AnalysisCode, 
                                             GenerateAnalysisCodePwr(),
                                             GenerateAnalysisCodeANOVA2('PwrTfm'),
                                             AnalysisCodeAsump[[2]],
                                             sep='\n'),
                              LogTfm = paste(AnalysisCode, 
                                             GenerateAnalysisCodeLog(),
                                             GenerateAnalysisCodeANOVA2('LogTfm'),
                                             AnalysisCodeAsump[[3]],
                                             sep='\n'),
                              SqrtTfm = paste(AnalysisCode, 
                                              GenerateAnalysisCodeSqrt(),
                                              GenerateAnalysisCodeANOVA2('SqrtTfm'),
                                              AnalysisCodeAsump[[4]],
                                              sep='\n'))
      return(TransformCode)
    })
    
  
    observe({
      input$view_anova_table
      isolate({
      tryCatch({
          updateAceEditor(
            session, 'code_used_anova',
            value = GenerateAnalysisCodeANOVA(), readOnly = TRUE
          )
        },
        error = function(e) {
          return(updateAceEditor(
            session, 'code_used_anova',
            value = '# code used to run ANOVA', readOnly = TRUE
          ))}
        )
      })
    })
    
    
    observe({
      input$run_analysis
      isolate({
      tryCatch({
        updateAceEditor(session, 'no_code_used_model',
                        value = GenerateAnalysisCode()[['NoTfm']], readOnly = TRUE)
      },
      error = function(e) {
        return(NULL)
      })
      tryCatch({
        updateAceEditor(session, 'pwr_code_used_model',
                        value = GenerateAnalysisCode()[['PwrTfm']], readOnly = TRUE)
      },
      error = function(e) {
        return(NULL)
      })
      tryCatch({
        updateAceEditor(session, 'log_code_used_model',
                        value = GenerateAnalysisCode()[['LogTfm']], readOnly = TRUE)
      },
      error = function(e) {
        return(NULL)
      })
      tryCatch({
        updateAceEditor(session, 'sqrt_code_used_model',
                        value = GenerateAnalysisCode()[['SqrtTfm']], readOnly = TRUE)
      },
      error = function(e) {
        return(NULL)
      })
      })
    })
  
  # TODO : It could be useful to break this up into each plot and utilize this
  # code for actual evaluation later on to deduplicate the code.
  MakePlotAnalysisCode <- function(transformation){
    
      analysisCode <- paste0("# Residuals vs. Fitted\nplot(model.fit, which = 1)")
      analysisCode <- paste0(analysisCode, "\n\n# Kernel Density Plot\nplot(density(residuals(model.fit)))")
    
      dep.var <- switch(transformation,
                        NoTfm = input$dependent.variable,
                        PwrTfm = paste0(input$dependent.variable, '.pow'),
                        LogTfm = paste0(input$dependent.variable, '.log10'),
                        SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
      ind.var.one <- input$independent.variable.one
      ind.var.two <- input$independent.variable.two
      f1 <- paste0(dep.var, ' ~ ', ind.var.one)
      main <- paste0("Effect of ", ind.var.one, " on ",
                     dep.var)
      analysisCode <- paste0(analysisCode, "\n\n# Effects Box Plots\n",
                             "boxplot(", f1, ", data = my.data, main = '", main,
                             "', xlab = '", ind.var.one,
                             "', ylab = '", dep.var,  "')")
      if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
        f2 <- paste0(dep.var, ' ~ ', ind.var.two)
        main <- paste0("Effect of ", ind.var.two, " on ",
                       dep.var)
        analysisCode <- paste0(analysisCode, "\nboxplot(", f2, ", data = my.data, main = '",
                               main, "', xlab = '", ind.var.two,
                               "', ylab = '", dep.var,  "')")
      }
    
    if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
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
  }
  
  #---------------------------------------------------------------------------#
  # UI elements for the analysis tab side panel.
  #---------------------------------------------------------------------------#
  
  output$selectDesign <- renderUI({
    # Renders a dropdown for selecting the type of experimental design.
    if(is.null(LoadData())){
      h4('Please upload or select data first.')
    } else {
      
      alys_type <- radioButtons('analysis_type', 
                                'Select your experimental design', 
                                c('Randomized Complete Block Design' = 'RCBD',
                                  'Completly Random Design' = 'CRD'))
      
      n_iv <- conditionalPanel('input.analysis_subtype != "SP"', 
                               radioButtons('n_ivs', "Select the number of factors in your experiment", 
                                            choices = 1:2, 
                                            inline = TRUE))

      n_iv_msg2 <- conditionalPanel('input.analysis_subtype == "SP"',
                                    h4('You must use two factors in split-plot design')) 
      
      alys_subtype <- conditionalPanel('input.analysis_type == "RCBD" || input.analysis_type == "CRD"', 
                                       radioButtons('analysis_subtype', 'Select Analysis Subtype',
                                   c('Not Applicable' = 'NA',
                                     'Split-plot Design' = 'SP',
                                     'Multisite experiment' = 'multisite')))
      
      return(list(alys_type, alys_subtype, n_iv, n_iv_msg2))
      
    }
  })
  
  exp.design <- reactive({
    if(is.null(input$analysis_type) && is.null(input$n_ivs) && is.null(input$analysis_subtype)){
      return(NULL)
    }
    subtype <- ifelse(input$analysis_subtype %in% c('multisite', 'NA'), 
                      '', input$analysis_subtype)
    is_multisite <- input$analysis_subtype == 'multisite'
    n_iv <- ifelse(input$analysis_subtype == 'SP', '', input$n_ivs)
    dsng <- paste0(subtype, input$analysis_type, n_iv)
    return(list(exp.design = dsng, 
                is_multisite = is_multisite))
  })
  
  output$selectDependent <- renderUI({
    # Renders a dropdown for selecting the dependent variable from the loaded
    # data.
    if (is.null(exp.design()[['exp.design']])) {
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
      input1 <- input2 <- input3 <- input4 <- NULL
      all.col.names <- names(ConvertData())
      choices = all.col.names[!(all.col.names %in% input$dependent.variable)]
      if (exp.design()[['exp.design']] == 'CRD1') {
        input1 <- selectInput('independent.variable.one',
                    'Select a single independent factor variable:',
                    choices = choices,
                    selected = NULL)
      } else if (exp.design()[['exp.design']] == 'CRD2') {
        input1 <- selectInput('independent.variable.one',
                              'Select the first independent factor variable:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.two',
                              'Select the second independent factor variable:',
                              choices = choices,
                              selected = NULL)
      } else if (exp.design()[['exp.design']] == 'RCBD1') {
        input1 <- selectInput('independent.variable.one',
                              'Select the first independent factor variable:',
                              choices = choices,
                              selected = NULL)
        input2 <- selectInput('independent.variable.blk',
                              'Select the blocking factor variable:',
                              choices = choices,
                              selected = NULL)
      } else if (exp.design()[['exp.design']] == 'RCBD2') {
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
      } else if (exp.design()[['exp.design']] == 'SPCRD') {
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
      } else if (exp.design()[['exp.design']] == 'SPRCBD') {
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
        }
      if (exp.design()[['is_multisite']]){
        input4 <- selectInput('independent.variable.site',
                              'Select the site variable',
                              choices = choices, 
                              selected = NULL)
      }
      return(list(input1, input2, input3, input4))
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
#     if(is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
      if (!exp.design()[['exp.design']] %in% c('SPRCBD', 'SPCRD')) {
        GenerateFormula(input$transformation)
      } else {
        paste('fixed  = ', GenerateFormula(input$transformation), '\nrandom = ', GenerateRandomEffFormula())
      }
    # }
  })
  
  output$fitSummaryText <- renderPrint({
        assign('my.data', AddTransformationColumns(), envir=.GlobalEnv)
        fit.summary <- Anova(EvalFit(input$transformation), type='III', singular.ok = TRUE)
    return(fit.summary)
  })
  
  output$fit.summary <- renderUI({
#     if(is.null(input$run_analysis) || input$run_analysis == 0 || input$view_anova_table == 0) {
#       return(NULL)
#     } else {
      input$view_anova_table
      isolate({
      list(h2('Model Fit Summary'),
           verbatimTextOutput('fitSummaryText'))
      })
    # }
  })
  
  output$exponent <- renderUI({
    # Renders a paragraph tag with the computed exponent value.
      header <- h2('Exponent from Power Transformation')
      text <- h4(as.character(round(ComputeExponent(), 5)))
      return(list(header, text))
  })
  
    shapiro.wilk.results.text <- function(transformation){
    
    # NOTE : We don't do the Shapiro-Wilk test for the split plot designs
    # because it isn't straight forward to implement.
    if (!exp.design()[['exp.design']] %in% c('SPCRD', 'SPRCBD')) {
      fit <- EvalFit(transformation)
      res <- shapiro.test(residuals(fit))
    } else {
      res <- paste0("Shapiro-Wilk Normality Test is not performed because ",
                        "it is not straightforward for split-plot designs.")
    }
      return(res)
  }
  
  output$no_shapiro.wilk.results.text <- renderPrint({
    # input$run_analysis
    # isolate({
      # h3('Shapiro-Wilk Normality Test Results')
      shapiro.wilk.results.text('NoTfm')
    # })
  })
  output$pwr_shapiro.wilk.results.text <- renderPrint({
    input$run_analysis
    # isolate({
    shapiro.wilk.results.text('PwrTfm')
    # })
  })
  output$log_shapiro.wilk.results.text <- renderPrint({
    input$run_analysis
    # isolate({
    shapiro.wilk.results.text('LogTfm')
    # })
  })
  output$sqrt_shapiro.wilk.results.text <- renderPrint({
    input$run_analysis
    # isolate({
    shapiro.wilk.results.text('SqrtTfm')
    # })
  })

  output$no_levene.results.text <- renderPrint({
    formulas <- GenerateIndividualFormulas(transformation = 'NoTfm')
    assign('my.data', AddTransformationColumns(), envir = .GlobalEnv)
    return(lapply(formulas, leveneTest, data = my.data))
  })
  
  output$pwr_levene.results.text <- renderPrint({
    formulas <- GenerateIndividualFormulas(transformation = 'PwrTfm')
    assign('my.data', AddTransformationColumns(), envir = .GlobalEnv)
    return(lapply(formulas, leveneTest, data = my.data))
  })
  output$log_levene.results.text <- renderPrint({
    formulas <- GenerateIndividualFormulas(transformation = 'LogTfm')
    assign('my.data', AddTransformationColumns(), envir = .GlobalEnv)
    return(lapply(formulas, leveneTest, data = my.data))
  })
  output$sqrt_levene.results.text <- renderPrint({
    formulas <- GenerateIndividualFormulas(transformation = 'SqrtTfm')
    assign('my.data', AddTransformationColumns(), envir = .GlobalEnv)
    return(lapply(formulas, leveneTest, data = my.data))
  })
 
  
  tukey.results.text <- function(transformation){
      # TODO : Check to make sure this is what I'm supposed to use for the split
      # plot results.
      fit <- ModelFitWithoutError(transformation)
      my.data <- AddTransformationColumns()
      
      dep.var <- switch(transformation,
                        NoTfm = input$dependent.variable,
                        PwrTfm = paste0(input$dependent.variable, '.pow'),
                        LogTfm = paste0(input$dependent.variable, '.log10'),
                        SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
      
      my.data[[paste0(dep.var, '.pred.sq')]] <- predict(fit)^2
      f <- GenerateTukeyFormula(transformation)
      tukey.one.df.fit <- lm(formula = as.formula(f), data = my.data)
    return(Anova(tukey.one.df.fit, type = 3, singular.ok = TRUE))
  }
  
  output$no_tukey.results.text <- renderPrint({
    # input$run_analysis
    # isolate({
    tukey.results.text('NoTfm')
    # })
  })
  output$pwr_tukey.results.text <- renderPrint({
    input$run_analysis
    # isolate({
    tukey.results.text('PwrTfm')
    # })
  })
  output$log_tukey.results.text <- renderPrint({
    input$run_analysis
    # isolate({
    tukey.results.text('LogTfm')
    # })
  })
  output$sqrt_tukey.results.text <- renderPrint({
    input$run_analysis
    # isolate({
    tukey.results.text('SqrtTfm')
    # })
  })

  tukey.results <- function(transformation){
      if (exp.design()[['exp.design']] %in% c('CRD2', 'RCBD2')) {
          dep.var <- switch(transformation,
                            NoTfm = input$dependent.variable,
                            PwrTfm = paste0(input$dependent.variable, '.pow'),
                            LogTfm = paste0(input$dependent.variable, '.log10'),
                            SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
          pred.var <- paste0(dep.var, '.pred.sq')
          
          res <- switch(transformation,
                        NoTfm = 'no_tukey.results.text',
                        PwrTfm = 'pwr_tukey.results.text',
                        LogTfm = 'log_tukey.results.text',
                        SqrtTfm = 'sqrt_tukey.results.text')
          
        list(h2("Tukey's Test for Nonadditivity"),
             p(strong(paste0("Attention: Refer only to the '", pred.var, "' ",
                             "row in this table, ignore all other rows."))),
             verbatimTextOutput(res))
      } else {
        return(NULL)
      }
    }
  
  
  output$no_tukey.results <- renderUI({
#     if(is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
    tukey.results('NoTfm')
    # }
  })
  output$pwr_tukey.results <- renderUI({
#     if(is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
    tukey.results('PwrTfm')
    # }
  })
  output$log_tukey.results <- renderUI({
#     if(is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
    tukey.results('LogTfm')
    # }
  })
  output$sqrt_tukey.results <- renderUI({
#     if(is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
    tukey.results('SqrtTfm')
    # }
  })
  
  plot.residuals.vs.fitted <- function(transformation){
    model.fit <- ModelFitWithoutError(transformation)
    
    scatter.smooth(fitted.values(model.fit), 
                   residuals(model.fit),
                   main = 'Residuals vs Predicted', 
                   xlab = 'Predicted', ylab = 'Residuals',
                   lpars = list(col='red'))
    abline(h=0, lty=2, col='gray')
  }
  
  output$no_plot.residuals.vs.fitted <- renderPlot({
    plot.residuals.vs.fitted('NoTfm')
  })
  output$pwr_plot.residuals.vs.fitted <- renderPlot({
    plot.residuals.vs.fitted('PwrTfm')
  })
  output$log_plot.residuals.vs.fitted <- renderPlot({
    plot.residuals.vs.fitted('LogTfm')
  })
  output$sqrt_plot.residuals.vs.fitted <- renderPlot({
    plot.residuals.vs.fitted('SqrtTfm')
  })
  
  
  residuals.vs.fitted.plot <- function(transformation){
#     if (is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
      res <- switch(transformation,
                    NoTfm = 'no_plot.residuals.vs.fitted',
                    PwrTfm = 'pwr_plot.residuals.vs.fitted',
                    LogTfm = 'log_plot.residuals.vs.fitted',
                    SqrtTfm = 'sqrt_plot.residuals.vs.fitted')
      list(h2('Residuals vs Fitted'),
           plotOutput(res))
    # }
  }
  
  output$no_residuals.vs.fitted.plot <- renderUI({
    residuals.vs.fitted.plot('NoTfm')
  })
  output$pwr_residuals.vs.fitted.plot <- renderUI({
    residuals.vs.fitted.plot('PwrTfm')
  })
  output$log_residuals.vs.fitted.plot <- renderUI({
    residuals.vs.fitted.plot('LogTfm')
  })
  output$sqrt_residuals.vs.fitted.plot <- renderUI({
    residuals.vs.fitted.plot('SqrtTfm')
  })
  
  plot.kernel.density <- function(transformation){
    model.fit <- ModelFitWithoutError(transformation)
    plot(density(residuals(model.fit)))
  }
  
  output$no_plot.kernel.density <- renderPlot({
    plot.kernel.density('NoTfm')
  })
  output$pwr_plot.kernel.density <- renderPlot({
    plot.kernel.density('PwrTfm')
  })
  output$log_plot.kernel.density <- renderPlot({
    plot.kernel.density('LogTfm')
  })
  output$sqrt_plot.kernel.density <- renderPlot({
    plot.kernel.density('SqrtTfm')
  })
  
  kernel.density.plot <- function(transformation){
#     if (is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
      res <- switch(transformation,
                    NoTfm = 'no_plot.kernel.density',
                    PwrTfm = 'pwr_plot.kernel.density',
                    LogTfm = 'log_plot.kernel.density',
                    SqrtTfm = 'sqrt_plot.kernel.density')
      list(h2('Kernel Density of the Residuals'),
           plotOutput(res))
    # }
  }
  
  output$no_kernel.density.plot <- renderUI({
    kernel.density.plot('NoTfm')
  })
  output$pwr_kernel.density.plot <- renderUI({
    kernel.density.plot('PwrTfm')
  })
  output$log_kernel.density.plot <- renderUI({
    kernel.density.plot('LogTfm')
  })
  output$sqrt_kernel.density.plot <- renderUI({
    kernel.density.plot('SqrtTfm')
  })  

  
  plot.boxplot.one <- function(transformation){
    dep.var <- switch(transformation,
                      NoTfm = input$dependent.variable,
                      PwrTfm = paste0(input$dependent.variable, '.pow'),
                      LogTfm = paste0(input$dependent.variable, '.log10'),
                      SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
    

      my.data <- AddTransformationColumns()
      f1 <- paste0(dep.var, ' ~ ',
                   input$independent.variable.one)
      boxplot(as.formula(f1), data = my.data,
              main = paste0("Effect of ", input$independent.variable.one,
                            " on ", dep.var),
              xlab = input$independent.variable.one,
              ylab = dep.var)

  }
  
  plot.boxplot.two <- function(transformation){
    dep.var <- switch(transformation,
                      NoTfm = input$dependent.variable,
                      PwrTfm = paste0(input$dependent.variable, '.pow'),
                      LogTfm = paste0(input$dependent.variable, '.log10'),
                      SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
    if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
      my.data <- AddTransformationColumns()
      f2 <- paste0(dep.var, ' ~ ',
                   input$independent.variable.two)
      boxplot(as.formula(f2), data = my.data,
              main = paste0("Effect of ", input$independent.variable.two,
                            " on ", dep.var),
              xlab = input$independent.variable.two,
              ylab = dep.var)
    }
  }
  
  output$no_plot.boxplot.one <- renderPlot({
    plot.boxplot.one('NoTfm')
  })
  output$pwr_plot.boxplot.one <- renderPlot({
    plot.boxplot.one('PwrTfm')
  })
  output$log_plot.boxplot.one <- renderPlot({
    plot.boxplot.one('LogTfm')
  })
  output$sqrt_plot.boxplot.one <- renderPlot({
    plot.boxplot.one('SqrtTfm')
  })
  
  output$no_plot.boxplot.two <- renderPlot({
    plot.boxplot.two('NoTfm')
  })
  output$pwr_plot.boxplot.two <- renderPlot({
    plot.boxplot.two('PwrTfm')
  })
  output$log_plot.boxplot.two <- renderPlot({
    plot.boxplot.two('LogTfm')
  })
  output$sqrt_plot.boxplot.two <- renderPlot({
    plot.boxplot.two('SqrtTfm')
  })
  
  boxplot.plot <- function(transformation){
    input$run_analysis
    bpe <- switch(transformation,
                     NoTfm = 'no',
                     PwrTfm = 'pwr',
                     LogTfm = 'log',
                     SqrtTfm = 'sqrt')
    bp1 <- paste0(bpe, '_plot.boxplot.one')
    bp2 <- paste0(bpe, '_plot.boxplot.two')
    
#     if (is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
        if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
          elements <- list(h2('Effects Box Plots'),
                           plotOutput(bp1),
                           plotOutput(bp2))
        } else {
          elements <- list(h2('Effects Box Plots'),
                           plotOutput(bp1))
        }
        return(elements)

    # }
  }

  output$no_boxplot.plot <- renderUI({
    boxplot.plot('NoTfm') 
  })
  output$pwr_boxplot.plot <- renderUI({
    boxplot.plot('PwrTfm') 
  })
  output$log_boxplot.plot <- renderUI({
    boxplot.plot('LogTfm') 
  })
  output$sqrt_boxplot.plot <- renderUI({
    boxplot.plot('SqrtTfm') 
  })
  
  output$plot.interaction.one <- renderPlot({
    input$view_anova_table
    # isolate({
      if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
        dep.var <- switch(input$transformation,
               NoTfm = input$dependent.variable,
               PwrTfm = paste0(input$dependent.variable, '.pow'),
               LogTfm = paste0(input$dependent.variable, '.log10'),
               SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
        ind.var.one <- input$independent.variable.one
        ind.var.two <- input$independent.variable.two
        my.data <- AddTransformationColumns()
        interaction.plot(my.data[[ind.var.one]], my.data[[ind.var.two]],
                         my.data[[dep.var]], xlab = ind.var.one, trace.label =
                           ind.var.two, ylab = dep.var)
      }
    # })
  })
  
  output$plot.interaction.two <- renderPlot({
    input$view_anova_table
    # isolate({
      if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
        dep.var <-  switch(input$transformation,
                           NoTfm = input$dependent.variable,
                           PwrTfm = paste0(input$dependent.variable, '.pow'),
                           LogTfm = paste0(input$dependent.variable, '.log10'),
                           SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
        ind.var.one <- input$independent.variable.one
        ind.var.two <- input$independent.variable.two
        my.data <- AddTransformationColumns()
        interaction.plot(my.data[[ind.var.two]], my.data[[ind.var.one]],
                         my.data[[dep.var]], xlab = ind.var.two, trace.label =
                           ind.var.one, ylab = dep.var)
      }
    # })
  })
  
  output$interaction.plot <- renderUI({
    input$run_analysis
#     if (is.null(input$run_analysis) || input$run_analysis == 0) {
#       return(NULL)
#     } else {
      if (!exp.design()[['exp.design']] %in% c('CRD1', 'RCBD1')) {
        return(list(h2('Interaction Plots'),
                    plotOutput('plot.interaction.one'),
                    plotOutput('plot.interaction.two')))
      } else {
        return(NULL)
      }
    # }
  })
  
  
  #############################################################################
  # Post hoc tab
  #############################################################################
  
  ##### code to run post-hoc tests #####
  
  lsd.code <- reactive({
    if (is.null(input$view_anova_table) || input$view_anova_table == 0) {
      return(NULL)
    } else {
      isolate({
        exp.design <- exp.design()[['exp.design']]
        dep.var <-  switch(input$transformation,
                           NoTfm = input$dependent.variable,
                           PwrTfm = paste0(input$dependent.variable, '.pow'),
                           LogTfm = paste0(input$dependent.variable, '.log10'),
                           SqrtTfm = paste0(input$dependent.variable, '.sqrt'))
        ind.var.one <- input$independent.variable.one
        if (exp.design %in% c('CRD1', 'RCBD1')) {
          ind.var.two <- NULL
        } else {
          ind.var.two <- input$independent.variable.two
        }
        ind.vars <- c(ind.var.one, ind.var.two)
        my.data <- AddTransformationColumns()
        model.fit <- EvalFit(input$transformation)
      })
      probcol <- ifelse(exp.design()[['is_multisite']] | 
                          exp.design()[['exp.design']] %in% c('SPRCBD', 'SPCRD'), 
                        'Pr(>Chisq)', 'Pr(>F)')
      alpha <- 0.05
      if (exp.design %in% c('CRD1', 'RCBD1')) {
        p.value <- Anova(model.fit, type = 3, singular.ok=TRUE)[2, probcol]
        if (p.value < alpha) {
          f <- as.formula(paste0('~ ', ind.vars))

            lsd.results.text <- quote(
              cld(lsmeans(model.fit, f), Letters=letters)
            )

          lsd.bar.plot <- quote(
            MakePostHocPlot(model.fit, dep.var, ind.vars)
          )
          return(list(text=paste0(ind.vars, ' is significant'),
                      res = lsd.results.text, 
                      f = f,
                      plt = lsd.bar.plot,
                      model.fit=model.fit, dep.var=dep.var, ind.vars=ind.vars))
        } else {
          return(list(text=paste0(ind.vars, ' is not significant.')))
        }
      } else if (exp.design()[['exp.design']] %in% c('CRD2', 'RCBD2')) {
        assign('my.data', AddTransformationColumns(), envir=.GlobalEnv)
        var.one.p.value <- Anova(model.fit, type = 3, 
                                 singular.ok=TRUE)[input$independent.variable.one, probcol]
        var.two.p.value <- Anova(model.fit, type = 3, 
                                 singular.ok=TRUE)[input$independent.variable.two, probcol]
        interact.index <- grep(':', row.names(Anova(model.fit, type=3, singular.ok=TRUE)))
        interaction.p.value <- Anova(model.fit, type='III', singular.ok=TRUE)[interact.index, probcol]
        if (interaction.p.value < .05) {
          text <- paste0("The interaction effect ", paste(ind.vars, collapse = ":"),
                         ", is significant (alpha = 0.05).")
          lsd.vars <- ind.vars
          if (var.one.p.value < alpha && var.two.p.value < alpha){
            text <- paste0(text, " Both main effects are significant.")
          } else if (var.one.p.value < alpha) {
            text <- paste0(text, " Only the ", ind.var.one, " main effect is significant.")
          } else if (var.two.p.value < alpha) {
            text <- paste0(text, " Only the ", ind.var.two, " main effect is significant.")
          } else {
            text <- paste0(text, " Neither factor main effect is significant.")
            return(list(text=text))
          }
          f <- as.formula(paste0('~ ', paste0(ind.vars, collapse = ' + ')))

            lsd.results.text <- quote(
              cld(lsmeans(model.fit, f), Letters=letters)
            )
            
          lsd.bar.plot <- quote(
            MakePostHocPlot(model.fit, dep.var, lsd.vars)
          )
          return(list(text=text,
                      res=lsd.results.text, f=f,
                      plt=lsd.bar.plot,
                      model.fit=model.fit, dep.var=dep.var, lsd.vars=lsd.vars))
        } else if (var.one.p.value < .05 & var.two.p.value >= .05) {
          lsd.vars <- ind.var.one
          f <- as.formula(paste0('~ ', paste0(ind.vars, collapse = ' + ')))

            lsd.results.text <- quote(
              cld(lsmeans(model.fit, f), Letters=letters)
            )

          lsd.bar.plot <- quote(
            MakePostHocPlot(model.fit, dep.var, lsd.vars)
          )
          text <- paste('Only the ', ind.var.one, ' main effect is significant.')
          return(list(text=text,
                      res=lsd.results.text,
                      plt=lsd.bar.plot, 
                      model.fit=model.fit, dep.var=dep.var, 
                      ind.var.one = ind.var.one))
          
          } else if (var.one.p.value >= .05 & var.two.p.value < .05) {
            lsd.vars <- ind.var.two
            f <- as.formula(paste0('~ ', paste0(ind.vars, collapse = ' + ')))

              lsd.results.text <- quote(
                cld(lsmeans(model.fit, f), Letters=letters)
              )

            lsd.bar.plot <- quote(
              MakePostHocPlot(model.fit, dep.var, lsd.vars)
            )
            text <- paste('Only the ', ind.var.two, ' main effect is significant')
            return(list(text=text,
                        res=lsd.results.text,
                        plt=lsd.bar.plot, 
                        model.fit=model.fit, dep.var=dep.var, 
                        ind.var.two = ind.var.two))
            
          } else {
          # TODO : Implement what happens here.
          return(list(text=paste0('The interaction is not significant and the post ',
                                  'hoc analyses for this scenario are not ',
                                  'implemented.')))
        }
      } else if (exp.design()[['exp.design']] %in% c('SPCRD', 'SPRCBD')){
        assign('my.data', AddTransformationColumns(), envir=.GlobalEnv)
        var.one.p.value <- Anova(model.fit, type = 3, 
                                 singular.ok=TRUE)[input$independent.variable.one, probcol]
        var.two.p.value <- Anova(model.fit, type = 3, 
                                 singular.ok=TRUE)[input$independent.variable.two, probcol]
        interact.index <- grep(':', row.names(Anova(model.fit, type=3, singular.ok=TRUE)))
        interaction.p.value <- Anova(model.fit, type='III', singular.ok=TRUE)[interact.index, probcol]
        if (interaction.p.value < .05) {
          text <- paste0("The interaction effect", paste(ind.vars, collapse = ":"),
                         ", is significant (alpha = 0.05).")
          if (var.one.p.value < alpha && var.two.p.value < alpha){
            lsd.vars <- ind.vars
            text <- paste0(text, " Both factors are significant.")
          } else if (var.one.p.value < alpha) {
            text <- paste0(text, " Only the ", ind.var.one, " main effect is significant.")
            lsd.vars <- ind.var.one
          } else if (var.two.p.value < alpha) {
            text <- paste0(text, " Only the ", ind.var.two, " main effect is significant.")
            lsd.vars <- ind.var.two
          } else {
            text <- paste0(text, " Neither main effect is significant.")
            return(list(text=text))
          }
          f <- as.formula(paste0('~ ', paste0(lsd.vars, collapse = ' + ')))
          lsd.results.text <- quote(
            cld(lsmeans(model.fit, f), Letters=letters)
          )
          lsd.bar.plot <- quote(
            MakePostHocPlot(model.fit, dep.var, lsd.vars)
          )
          return(list(text = text, 
                      res = lsd.results.text, 
                      plt = lsd.bar.plot,
                      f=f, model.fit=model.fit, dep.var=dep.var, lsd.vars=lsd.vars))
        } else if (var.one.p.value < .05 & var.two.p.value >= .05) {
          lsd.vars <- ind.var.one
          text <- paste0('Only the ', lsd.vars, ' main effect is significant. ')
          f <- as.formula(paste0('~ ', lsd.vars))
          lsd.results.text <- quote(
            cld(lsmeans(model.fit, f), Letters=letters)
          )
          lsd.bar.plot <- quote(
            MakePostHocPlot(model.fit, dep.var, lsd.vars)
          )
          return(list(text = text, 
                      res = lsd.results.text, 
                      plt = lsd.bar.plot,
                      f=f, model.fit=model.fit, dep.var=dep.var, lsd.vars=lsd.vars))
          
        } else if (var.one.p.value >= .05 & var.two.p.value < .05) {
          lsd.vars <- ind.var.two
          text <- paste0('Only the ', lsd.vars, ' main effect is significant.')
          f <- as.formula(paste0('~ ', lsd.vars))
          lsd.results.text <- quote(
            cld(lsmeans(model.fit, f), Letters=letters)
          )
          lsd.bar.plot <- quote(
            MakePostHocPlot(model.fit, dep.var, lsd.vars)
          )
          return(list(text = text, 
                      res = lsd.results.text, 
                      plt = lsd.bar.plot,
                      f=f, model.fit=model.fit, dep.var=dep.var, lsd.vars=lsd.vars))
        } else {
          return(list(text=paste0('No effects are significant and post ',
                                  'hoc analyses for this scenario are not ',
                                  'implemented.')))
        }
      }
    }
  })
  
  output$lsd.results <- renderUI({
    if (input$view_anova_table == 0){
      return(NULL)
    }
    input$view_anova_table
    isolate({
      modelSpecificationMsg <- NULL
      obj <- lsd.code()
      if(!is.null(obj$text)){
        txt <- obj$text
      }
      if(!is.null(obj$res)){
        output$lsd.results.text <- renderPrint(eval(obj$res, envir = obj))
        if (any(eval(obj$res, envir = obj)$df == 0)) {
          BlockWarning <- TRUE
        } else {
          BlockWarning <- FALSE
        }
      }
      if(!is.null(obj$plt)){
        output$lsd.bar.plot <- renderPlot(eval(obj$plt, envir = obj))
      }
      if (exists('BlockWarning') && BlockWarning){
        plt <- h4(paste('WARNING: Your model may be overspecified.',
                        'Try using a Split-plot CRD model rather than a Split-plot RCBD'))
      } else {
        plt <- plotOutput('lsd.bar.plot')
      }
    })
    return(list(p(txt),
                verbatimTextOutput('lsd.results.text'),
                plt))
  })
  
  lsd.displaycode <- reactive({
    obj <- lsd.code()
    if (!is.null(obj$res)){
      txt <- deparse(obj$res)
      txt <- gsub('f(?=\\)\\, Letters)', deparse(obj$f), txt, perl=TRUE)
      txt <- paste0('library(lsmeans)\n', txt)
      cat(txt)
      return(txt)
    }
  })
  
  observe({
    input$view_anova_table
    isolate({
      tryCatch({
      updateAceEditor(session, 
                      editorId='code_used_posthoc',
                      value = lsd.displaycode(),
                      readOnly = TRUE)
      }, error=function(e){NULL})
    })
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
      filled.template <- sub('replace_with_data_code', ReadCode(), template)
      filled.template <- sub('replace_with_analysis_code',
                              GenerateAnalysisCode()[[input$transformation]], filled.template)
      filled.template <- sub('replace_with_analysis_plot_code',
                              MakePlotAnalysisCode(input$transformation), filled.template)
      filled.template <- sub('replace_with_post_hoc_code', 
                             lsd.displaycode(), filled.template)
      writeLines(filled.template, 'report.Rmd')
      src <- normalizePath('report.Rmd')
      file.copy(filled.template, 'report.Rmd')
      out <- knit2html('report.Rmd', output=input$file.name)
      file.copy(out, file)
    }
  )
  
  #############################################################################
  # About tab 
  #############################################################################
  
  output$downloadSlides1 <- downloadHandler(
    filename = function(){"Workshop Slides Day 1.pdf"},
    content  = function(file) file.copy('www/Stats Tutorial Day 1.pptx', 
                                        file, overwrite = FALSE)
  )
  
  output$downloadSlides2 <- downloadHandler(
    filename = function(){"Workshop Slides Day 2.pdf"},
    content  = function(file) file.copy('www/Stats Tutorial Day 2.pptx', 
                                        file, overwrite = FALSE)
  )
  
  #############################################################################
  ##### UI elements                                                       #####
  #############################################################################
  output$logTrnsfrmTab <- renderUI({
    if(input$run_analysis == 0){
      return(NULL)
    } else {
    if(min(AddTransformationColumns()[,input$dependent.variable], na.rm=TRUE)>=1){
      return(list(
    bsTooltip(
      'code_used_modelLogTfm',
      'Click for more information',
      placement = 'top',
      trigger = 'hover'
    ),
    bsPopover(
      'code_used_modelLogTfm',
      title = 'Analysis R code',
      content = help.text$analysis.code.explanation,
      placement = 'bottom',
      trigger = 'click'
    ),
    conditionalPanel(
      'input.run_analysis > 0',
      uiOutput('log_residuals.vs.fitted.plot'),
      uiOutput('log_kernel.density.plot'),
      uiOutput('log_boxplot.plot'),
      h2('Shapiro-Wilk Normality Test Results'),
      verbatimTextOutput('log_shapiro.wilk.results.text'),
      h2('Levene\'s Test for Homogeneity of Variance'),
      verbatimTextOutput('log_levene.results.text'),
      uiOutput('log_tukey.results')
    )
  ))
    } else {
      return(tabPanel(
        'Logarithmic transformation',
        h2('Your dependent variable has values less than 1. So that the log transformation is done correctly, please make sure all values in your dependent variable are greater than one by multiplying and/or adding a constant to that variable')))
    }
    }
  })
  
  output$transformation.panel <- renderUI({
    if(input$run_analysis == 0){
      return(NULL)
    }
    if(min(AddTransformationColumns()[,input$dependent.variable], na.rm=TRUE)>=1){
      choices <- c('None' = 'NoTfm', 
                  'Power' = 'PwrTfm', 
                  'Logarithmic' = 'LogTfm', 
                  'Square Root' = 'SqrtTfm')
    } else {
      choices <-  c('None' = 'NoTfm', 
                    'Power' = 'PwrTfm', 
                    'Square Root' = 'SqrtTfm')
    }
    radioButtons('transformation',
                 'Select a transformation for the dependent variable:',
                 choices = choices,
                 selected = 'NoTfm',
                 inline=TRUE)
  })

  
  
  #############################################################################
  # end 
  #############################################################################

  session$onSessionEnded(function() { 
    cat('\n\n\nThank you for using Agroft.\nYou can start another Agroft session by typing "launch()" at the R prompt (>) and pressing enter.\n\n')
    stopApp()
  })
  
})
