library(shiny)
library(shinyBS)
library(shinyAce)

###############################################################################
# Load Data Tab
###############################################################################

load.data.editor <- aceEditor('code_used_read',
                              value='# code to read in your data',
                              mode='r',
                              readOnly=TRUE,
                              height='100px')

load.data.side.panel <- sidebarPanel(
  h4('Upload your CSV file by pressing "Load data" below'),
  h5(paste('Your data should appear to the right. If ',
           'this data is correct, please move to tab ',
           '2: "Data analysis"', sep='')),
  fileInput('data_file', 'Load data'),
  h6('Code used to read in data:'),
  load.data.editor,
  checkboxInput('use_sample_data', 'Use sample data instead'),
  conditionalPanel(
    condition="input.use_sample_data == true",
    radioButtons('sample_data_buttons',
                 'Select your sample data',
                 choices=c('CRD 1 IV'='milk_crd1',
                           'RCBD 1 IV'='wheat_rcbd1',
                           'CRD 2 IVs'='clone_crd2',
                           'RCBD 2 IVs'='clone_rcbd2',
                           'SP-RCBD 2 IVs' = 'oats_sprcbd',
                           'Multisite RCBD 2 IVs' = 'multisite_RCBD', 
                           'Transformation Data 1' = 'crd_transformed1', 
                           'Transformation Data 2' = 'crd_transformed2',
                           'Transformation Data 3' = 'crd_transformed3')
    )),
  bsTooltip('code_used_read',
            title='click for more information',
            placement = 'top',
            trigger='hover'),
  bsPopover('code_used_read',
            title='Load data R code',
            content=help.text$load.data.explanation,
            placement='top',
            trigger='click')
)

load.data.tab <- tabPanel('1. Load data',
                          tags$style(type="text/css", "body {padding-top: 55px;}"),
                          sidebarLayout(
                            load.data.side.panel,
                            mainPanel(
                              h4('Loaded Data'),
                              dataTableOutput('data_table')
                            )
                          )
)

###############################################################################
# Design and Diagnostics tab
###############################################################################

# In this panel the user selects 1 of 9 experimental design options from a
# dropdown list. The choice is stored in input$exp.design. Data must be loaded
# first for the panel to function.
experimental.design.panel <-
  bsCollapsePanel(
    '1. Experimental Design',
    h5('Choose an experimental design that matches your data.'),
    uiOutput('selectDesign'),
    bsButton('exp_design_info_button',
             "Experimental Design Information"),
    bsModal('exp_design_info_content',
            trigger='exp_design_info_button',
            title='Information On Experimental Design Types',
            h5(help.text$exp.design.types)
    )
  )

# The user can adjust whether variables are continuous or factors.
# TODO : Maybe it should go below the dependent and independent variable
# selections so you only have to choose the variable type for the variables used
# in the analysis.
variable.type.panel <-
  bsCollapsePanel(
    '2. Variable types',
    h5('Indicate your variable types below'),
    h6(paste('We have made guesses at the variable types ',
             'in your data, but change the variable types ',
             'below if they are incorrect.', sep='')),
    uiOutput('var_types_select'),
    bsButton('variable_type_button',
             'Information on variable types'),
    bsModal('var_type_info',
            title='Variable type info',
            trigger='variable_type_button',
            h5(help.text$var.type.info))
  )

# In this panel, the user selects one of the columns from their data set to be
# the dependent variable. The choice is stored in input$dependent.variable. The
# experimental design must be chosen first.
dependent.panel <-
  bsCollapsePanel(
    "3. Dependent Variable",
    uiOutput('selectDependent'),
    bsButton('dependent_info_button',
             "Dependent Variable Information"),
    bsModal('dependent_info_content',
            trigger='dependent_info_button',
            title='Information on Dependent Variable Choice',
            h5(help.text$dependent.variable.info)
    )
  )

# In this panel the user selects the independent variables. Depending on which
# experimental design is chosen, different selection boxes appear.
independent.panel <-
  bsCollapsePanel(
    '4. Independent variables',
    uiOutput('selectIndependent'),
    bsButton('select_iv_info',
             'Independent variable info'),
    bsModal('iv_info_content',
            title='Information on independent variables',
            trigger='select_iv_info',
            h5(help.text$ind.var.explanation))
  )

# In this panel, the user can select a type of transformation to apply to the
# dependent variable.
# transformation.panel <-
#     radioButtons('transformation',
#                 'Select a transformation for the dependent variable:',
#                 choices = c('None' = 'NoTfm', 
#                             'Power' = 'PwrTfm', 
#                             'Logarithmic' = 'LogTfm', 
#                             'Square Root' = 'SqrtTfm'),
#                 selected = 'NoTfm',
#                 inline=TRUE)

analysis.editorNoTfm <- aceEditor('no_code_used_model', value='# code to run analysis',
                             mode='r', readOnly=TRUE, height='200px')
analysis.editorPwrTfm <- aceEditor('pwr_code_used_model', value='# code to run analysis',
                             mode='r', readOnly=TRUE, height='200px')
analysis.editorLogTfm <- aceEditor('log_code_used_model', value='# code to run analysis',
                             mode='r', readOnly=TRUE, height='200px')
analysis.editorSqrtTfm <- aceEditor('sqrt_code_used_model', value='# code to run analysis',
                             mode='r', readOnly=TRUE, height='200px')
analysis.editorANOVA <- aceEditor('code_used_anova', 
                                  value = '# code used to run ANOVA',
                                  mode = 'r', 
                                  readOnly = TRUE, 
                                  height = '75px')



###########################
##### transformations #####
###########################

### None ###

noTrnsfrmTab <- tabPanel(
  'No transformation',
  analysis.editorNoTfm,
  bsTooltip(
    'code_used_modelNoTfm',
    'Click for more information',
    placement = 'top',
    trigger = 'hover'
  ),
  bsPopover(
    'code_used_modelNoTfm',
    title = 'Analysis R code',
    content = help.text$analysis.code.explanation,
    placement = 'bottom',
    trigger = 'click'
  ),
  conditionalPanel(
    'input.run_analysis > 0',
    uiOutput('no_residuals.vs.fitted.plot'),
    uiOutput('no_kernel.density.plot'),
    uiOutput('no_best.fit.plot'),
    uiOutput('no_boxplot.plot'),
    h2('Shapiro-Wilk Normality Test Results'),
    verbatimTextOutput('no_shapiro.wilk.results.text'),
    h2('Levene\'s Test for Homogeneity of Variance'),
    verbatimTextOutput('no_levene.results.text'),
    # h2("Tukey's Test for Nonadditivity"),
    uiOutput('no_tukey.results')
  )
)

### Power ###

pwrTrnsfrmTab <- tabPanel(
  'Power transformation',
  analysis.editorPwrTfm,
  bsTooltip(
    'code_used_modelPwrTfm',
    'Click for more information',
    placement = 'top',
    trigger = 'hover'
  ),
  bsPopover(
    'code_used_modelPwrTfm',
    title = 'Analysis R code',
    content = help.text$analysis.code.explanation,
    placement = 'bottom',
    trigger = 'click'
  ),
  conditionalPanel(
    'input.run_analysis > 0',
  uiOutput('exponent'),
  uiOutput('pwr_residuals.vs.fitted.plot'),
  uiOutput('pwr_kernel.density.plot'),
  uiOutput('pwr_best.fit.plot'),
  uiOutput('pwr_boxplot.plot'),
  h2('Shapiro-Wilk Normality Test Results'),
  verbatimTextOutput('pwr_shapiro.wilk.results.text'),
  h2('Levene\'s Test for Homogeneity of Variance'),
  verbatimTextOutput('pwr_levene.results.text'),
  uiOutput('pwr_tukey.results')
))


### Log ###
# moved to server side so if data are nevative, warning pops up instead of other stuff


### Square Root ###
sqrtTrnsfrmTab <- tabPanel(
  'Square root transformation',
  analysis.editorSqrtTfm,
  bsTooltip(
    'code_used_modelSqrtTfm',
    'Click for more information',
    placement = 'top',
    trigger = 'hover'
  ),
  bsPopover(
    'code_used_modelSqrtTfm',
    title = 'Analysis R code',
    content = help.text$analysis.code.explanation,
    placement = 'bottom',
    trigger = 'click'
  ),
  conditionalPanel(
    'input.run_analysis > 0',
  uiOutput('sqrt_residuals.vs.fitted.plot'),
  uiOutput('sqrt_kernel.density.plot'),
  uiOutput('sqrt_best.fit.plot'),
  uiOutput('sqrt_boxplot.plot'),
  h2('Shapiro-Wilk Normality Test Results'),
  verbatimTextOutput('sqrt_shapiro.wilk.results.text'),
  h2('Levene\'s Test for Homogeneity of Variance'),
  verbatimTextOutput('sqrt_levene.results.text'),
  uiOutput('sqrt_tukey.results')
  )
)

data.analysis.tab <-
  tabPanel(
    '2. Model Design and Diagnostics',
    sidebarLayout(
      sidebarPanel(
        bsCollapse(
          multiple = FALSE,
          id = 'main_collapse_panel',
          experimental.design.panel,
          variable.type.panel,
          dependent.panel,
          independent.panel
        ),
        actionButton('run_analysis', 'Check Assumptions')
      ),
      mainPanel(
        uiOutput('transformation.panel'),
        # verbatimTextOutput('debug'),
      tabsetPanel(
        noTrnsfrmTab,
        pwrTrnsfrmTab,
        tabPanel(
          'Logarithmic transformation',
          analysis.editorLogTfm,
          uiOutput('logTrnsfrmTab')),
        sqrtTrnsfrmTab
      )
      )
    )
  )

###############################################################################
# Results and Post-hoc Tests Tab
###############################################################################
analysis.editor.posthoc <- aceEditor('code_used_posthoc',
                                     value = '# code used to run post-hoc tests',
                                     mode = 'r',
                                     readOnly = TRUE, 
                                     height = '50px')


posthoc.tab <-
  tabPanel('3. Results and Post-hoc tests',
           sidebarLayout(
             sidebarPanel(
               actionButton('view_anova_table',
                                       'Run Data Analysis'),
                          conditionalPanel('input.view_anova_table != 0', 
                                           h2('Model Formula'),
                                           verbatimTextOutput('formula')
                          )),
             mainPanel(tabsetPanel(
               tabPanel('Model Fit Summary',
                conditionalPanel('input.view_anova_table > 0',
                                 analysis.editorANOVA,
                       uiOutput('fit.summary'),
                       bsTooltip('fit.summary',
                                 'Click for more information',
                                 placement = 'top',
                                 trigger = 'hover'),
                       bsPopover('fit.summary',
                                 title = 'Standard output',
                                 help.text$fit.explanation,
                                 placement = 'left',
                                 trigger = 'click'),
                       uiOutput('interaction.plot'))),
                tabPanel('Post-hoc Tests',        
               conditionalPanel('input.view_anova_table > 0',
                       h3('Post hoc tests and figures'),
                       analysis.editor.posthoc
                       ),
                       uiOutput('lsd.results'))))
           )
  )

###############################################################################
# Report Tab
###############################################################################

report.tab <-
  tabPanel('4. Report',
           verticalLayout(
             textInput('file.name', "File name:", "analysis.html"),
             uiOutput('download_report_button')
           )
  )

###############################################################################
# About Tab
###############################################################################

about.tab <-
  tabPanel('About',
           verticalLayout(
             p(help.text$about),
             h1('Funding'),
             p(help.text$funding),
             img(src = "usaid-logo-600.png", width = "300px"),
             h1('Authors'),
             tags$ul(tags$li('Ian K. Kyle'), tags$li('Jason K. Moore'),
                     tags$li('Maegen B. Simmonds')),
             h1('Disclaimer'),
             p(help.text$disclaimer),
             h1('License'),
             p(help.text$license, tags$a('http://github.com/ucd-ipo/aip-analysis')),
             downloadButton('downloadSlides1', label = 'Download Workshop Day 1 Slides'),
             downloadButton('downloadSlides2', label = 'Download Workshop Day 2 Slides')
           )
  )

###############################################################################
# Main User Interface
###############################################################################
shinyUI(
  navbarPage(
    title = 'Agricultural Field Trial Statistics Package',
    windowTitle = 'Agricultural Field Trial Statistics Package',
    position = 'fixed-top',
    load.data.tab,
    data.analysis.tab,
    posthoc.tab,
    report.tab,
    about.tab
  )
)
