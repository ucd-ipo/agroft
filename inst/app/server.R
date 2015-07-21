# this sources the file that checks whether all the packages are installed. It
# might be a good idea to edit this source file (located in the app working
# directory, "AIP/inst/app") so that it checks the required version is installed
# too. Required versions can be found in the AIP package DESCRIPTION file
source('pkg_check.R')

# for loading dynamic reports. I don't use rmarkdown because that requires that
# pandoc be installed which is a whole different ballgame. knitr doesn't require
# dependencies like that
library(knitr)

# analysis intrepretation functions like glht and lsmeans will be useful for
# intrepriting interactions
library(lsmeans)

# for creating the "eff" object to pass to plot_effects (in the app working
# directory, sourced in global.R)
library(effects)

library(agricolae) # for datasets

library(shiny) # the main web app javascript engine

# for displaying R code - pull from github until the correct version is on CRAN
library(shinyAce)

library(lattice) # for plotting, the main dependency of plot_effects function

library(gridExtra) # for arranging plots in the plots/Effect plots tab

# for reading in excel files, uncomment once on CRAN. For now it is on github
# only. It is a package that has no Java dependencies (only c++), so once
# binaries are on CRAN, anyone can install it (no JRE required!) and the app can
# have functionality to read in excel files.
#library(readxl)

# http://spark.rstudio.com/johnharrison/shinyBS-Demo/, vers. 0.5 should be on
# CRAN soon, that is the version we need. Once it is up there, change the code
# in initialize_AIP() to pull from CRAN instead. This package has bindings for
# some cool twitter bootstrap UI stuff that shiny doesn't include. Includes the
# modals, collapse panels, and tool tips.
library(shinyBS)

# install github packages via:
# devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
# devtools::install_github("trestletech/shinyAce")

shinyServer(function(input, output, session){

  # for debugging. Uncomment the verbatimTextOutput under main panel in the UI
  # script to see the contents of this. Useful for seeing what the objects you
  # are crating actually look like rather than what you think/want them to look
  # like.
  output$debug <- renderText({GenerateFormula()})

##### server side element for reading in data ####################################
  GetLoadCall <- reactive({
    # Returns one of three things:
    # 1. language:call containing the read.csv unevaluated function call
    # 2. symbol:name containing the name of the agricolae data set
    # 3. NULL
    # if no file has been uploaded or a file has been uploaded but it is of
    # length zero (i.e. corrupt or weird filetype) and the user hasn't selected
    # use sample data, then... make the reactive expression "GetLoadCall" NULL,
    # otherwise, do read in data things
    if((is.null(input$data_file) || length(input$data_file)==0) &&
       !input$use_sample_data) {
      return(NULL)
    } else {
      # if a data file has been uploaded and the user doesn't want to use sample
      # data...
      if(length(input$data_file) > 0 && !input$use_sample_data){
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

##### Assign the data frame to the variable "data" by evaluating the call to
##### load the data.

  LoadData <- reactive({
    # Returns a list:data.frame with the appropriate data set or NULL.

    # TODO : This should only run if GetLoadCall returns a name:symbol.
    # TODO : This should probably be in the GetLoadCall function.
    # The following loads the selected agricolae data set into the workspace,
    # regardless if the user selects "Use sample data instead". The name of the
    # data.frame is the same as input$sample_data_buttons.
    eval(call('data', input$sample_data_buttons, package='agricolae',
              envir=environment()))

    # set "dat" to the GetLoadCall object
    # remeber that GetLoadCall is either NULL, the call to read.csv, or the name of
    # the dataset the user wants.
    # if we eval NULL, data is set to NULL, if we eval read.csv, we get the
    # data the user submitted. If we eval the name of the data the user
    # wanted, that data is assigned to data (because we read it into the
    # workspace via the call to 'data' above)
    data <- eval(GetLoadCall(), envir=environment())

    return(data)

    })

##### Generate the code used to load the data ##########

  GetSimpleLoadExpr <- reactive({
    # Returns a char containing the code an R user would use to load the data.

    # if they aren't using sample data, deparse the call to get the character
    # represntation of the call, otherwise, construct it from scratch
    if(!input$use_sample_data){
      return(deparse(GetLoadCall()))
    } else {
      l <- 'library(agricolae)  # load "agricolae" package for the sample data'
      l <- paste0(l, '\ndata("',input$sample_data_buttons,  '")')
      l <- paste0(l, '\n', 'my_data <- ', input$sample_data_buttons)
    }
    })

##### display the data on the "upload data" tab ################
  output$data_table <- renderDataTable({LoadData()})

##### UI element for selecting the desired analysis ############
  output$select_analysis <- renderUI({
    # if the data reactive expression is NULL, tell them up upload their data,
    # otherwise give them options for tests to run
    if(is.null(LoadData())){
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

##### UI element for selecting the dependent variable ############
  output$select_dv <- renderUI({
    if(length(input$analysis)==0){return(NULL)}
    # don't return dv selection if they haven't selected an analysis
    # otherwise, select a DV
    # choices are all the variables in LoadData()
    selectInput('dv',
                'Select your dependent variable',
                choices=names(LoadData()),
                selected=NULL)
  })

##### UI element for selecting the independent variable(s) #########

# if they are running a RCBD, ask them for their block variable
output$select_block <- renderUI({
    if(length(input$dv)==0 || input$analysis !='rcbd'){return(NULL)}
      selectInput('block',
                  'Select your block variable',
                  # remove the DV from the options
                  choices=names(ConvertData())[!names(ConvertData()) %in% input$dv],
                  multiple=FALSE, # only one block variable allowed
                  selected=NULL)
  })

# if they are running RCBD, ask them for treatment variable
output$select_treatment <- renderUI({
  if(length(input$dv)==0 || input$analysis != 'rcbd'){return(NULL)}
  selectInput('treatment',
              'Select your treatment variable',
              # remove DV and block from their options
              choices=names(ConvertData())[!names(ConvertData()) %in% c(input$dv, input$block)],
              # only one treatment variable allowed, other IVs can be added with
              # the input$iv input
              multiple=FALSE,
              selected=NULL)
})

# select the IV
output$select_iv <- renderUI({
  if(length(input$dv)==0){return(NULL)}
  # change the input text depending on input$analysis
  # rcbd is set to "Select any other independent variables" because you can
  # already select the block and treatment in the inputs specific to those
  # variables
  selectInput('iv',
              switch(input$analysis,
                     aov='Select your independent variable(s)',
                     t.test='Select your grouping variable',
                     glm='Select your independent variables(s)',
                     rcbd='Select any other independent variables'),
              choices=names(ConvertData())[!names(ConvertData()) %in% c(input$dv,
                                                          input$treatment,
                                                          input$block)],
              # only allow multiple if you aren't using a t-test
              multiple=input$analysis != 't.test')
})

# list of IVs being used for use in constructing the formula if you have a DV
ivs <- reactive({
  if(length(input$dv) == 1){
    if(input$analysis=='rcbd'){
      return(c(input$block, input$treatment, ifelse(is.null(input$iv), NA, input$iv)))
    } else if(input$analysis %in% c('aov', 't.test', 'glm')){
      return(input$iv)
    }
  } else {
    return(NULL)
  }
})

##### UI element for selecting the DV type ##########################
# if they are using a GLM, select the dist family to be put in the family
# argument of glm
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
# prompt the user to select the class of each variable. The default selected
# class is the one that R read the data in as.
  output$var_types_select <- renderUI({

    raw.data <- LoadData()
    raw.data.col.names <- names(raw.data)

    if(is.null(raw.data)){
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

######## convert variables to their respective types ############

  ConvertData <- reactive({
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

##### select the interactions  ################################
# TukeyHSD or multcomp::glht
  output$select_interactions <- renderUI({

    if(input$analysis=='t.test'){
      return(h4('Interactions not available for t-tests'))
    }
    # this is kind of crazy. It creates every possible combination of IVs and then
    # gets all the uniqe combos
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
    # I made this weird separator so it doesn't conflict with any other separators
    # that might be in their variable names. Later it gets substituted out and
    # replaced with "&" for the dropdown box
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
# This uses the paste_na function from global.R to create the formula, joining
# all the main and interaction effects
# dv : dependent variable
# invs: independent variables
  GenerateFormula <- reactive({
    int <- paste0(input$interaction_input, collapse=' + ')
    int <- ifelse(is.null(input$interaction_input), NA, int)
    main <- paste0(ivs(), collapse=' + ')
    eff <- paste_na(main, int, sep=' + ')

    paste0(input$dv, ' ~ ', eff)
    })

##### run the analysis, assign to reactive object "fit" ##############

  GetFitCall <- reactive({
    # Returns the call used to run the analysis.

    # The following line forces this reactive expression to take a dependency on
    # the "Run Analysis" button. Thus this will run anytime it is clicked.
    input$run_analysis

    # isolate prevents this reactive expression from depending on any of the
    # variables inside the isolated expression.
    isolate({

      if(!is.character(input$analysis)){
        return(NULL)
      }

      fit <- call(input$analysis,
                  formula=as.formula(GenerateFormula()),
                  family=input$dv_type,
                  data=as.name('my_data'))

      # santizes the call (from global.R)
      fit <- strip.args(fit)

    })

    return(fit)

    })

  EvalFit <- reactive({
    # Returns the fit model.

    # Run every time the "Run Analysis" button is pressed.
    input$run_analysis

    isolate({
      my_data <- ConvertData()
      x <- eval(GetFitCall())
    })

    return(x)

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

##### the fit summary for analysis ###################################
### print anova table for all linear models
# TODO : This code should be shown to the user, otherwise the table output in
# the app doesn't match the code shown above it.
  output$fit_summary <- renderPrint({

    analysis <- isolate(input$analysis)

    # Run every time the "Run Analysis" button is pressed.
    input$run_analysis

    if(analysis=='t.test'){
      fit_summary <- isolate(EvalFit())
    } else {
      isolate({
        # See:
        # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.glm.html
        # for an explanation of the `test` argument.
        tst = ifelse((input$analysis=='glm' & input$dv_type=='gaussian') |
                     input$analysis=='aov', 'F', 'LRT')
        fit_summary <- anova(EvalFit(), test=tst)
      })
    }

    return(fit_summary)

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

# this hasn't been implimented in the UI yet
output$pois <- renderPrint({
  if(is.pois(ConvertData()[input$dv])){
    h3(paste('Your dependent variable may be poisson distributed. Consider ',
             'running a generalized linear model and selecting "count" where ',
             'asked "What type of data is your dependent variable?" See the ',
             'help tab on data analysis for more information.', sep=''))
  }
})

##### the code for reading in the data ###################################
  read_code <- reactive({
    if(length(input$data_file)==0 && !input$use_sample_data){return(NULL)}
    if(!input$use_sample_data){
    filestr <- gsub('(?<=file \\= ").*(?="\\))',
                    input$data_file$name, perl=TRUE,
                    GetSimpleLoadExpr())
    filestr <- paste0('my_data <- ', filestr)
    } else {
      filestr <- GetSimpleLoadExpr()
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
    mcode <- paste0('model.fit <- ', GetFitExpr())

   mcode <- paste(mcode, sep='', collapse='')
   mcode <- gsub('(\\, )', ',\n\t\t\t', mcode)

   # recode the analysis name
   analysis.name <- c('aov'='ANOVA', 'glm'='(generalized) linear model',
                      't.test'='t-test')[input$analysis]

   factor.ind <- which(sapply(ConvertData(), is.factor))
    if(length(factor.ind)>0){
      factor.name <- names(ConvertData()[factor.ind])

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
    coeff.ind <- row.names(anova(EvalFit()))[which(!row.names(anova((EvalFit()))) %in% c('NULL', 'Residuals'))]
   ef <- vector('list', length(coeff.ind))
   names(ef) <- coeff.ind
   assign('my_data', ConvertData(), env=.GlobalEnv)
   assign('.fit', EvalFit(), env=.GlobalEnv)
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
   # right now it uses the default plot method (plot.eff), but the
   # plot_effects.R is a somewhat more flexible version of plot.eff that I wrote
   # and can be used if needed. It isn't documented but I can add documentation
   # for that function eventually. It should probably be moved into the AIP/R
   # directory so it is a function exported in the AIP package rather than
   # sourcing the function from the app wd.
   # actually, plot_effects might not be needed because John Fox recently edited
   # the function in effects to fix the main issue I had a problem with. So the
   # newest version should be fine.
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
   if(is.null(EvalFit())){
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
  varinds <- which(sapply(ConvertData(), is.numeric2))
  par(mfrow=c((length(varinds) + 1) %/% 2, 2))
  for(i in names(ConvertData())[varinds]){
    hist(ConvertData()[,i], main=i, xlab='')
  }
  })

output$hists <- renderUI({
  if(is.null(ConvertData())){return(NULL)}
  h <- (length(which(sapply(ConvertData(), is.numeric2))) + 1) %/% 2
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

})
