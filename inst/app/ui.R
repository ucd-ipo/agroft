source('pkg_check.R')

library(shiny)
library(shinyAce)
library(shinyBS)
library(yaml)

# The shinyBS popovers can't handle line returns in the strings so a special
# handler is needed.
str.handler <- function(x) { gsub("[\r\n]", "", x) }
help.text <- yaml.load_file('help-text.yaml', handlers = list(str = str.handler))

# Load Data Tab
load.data.editor <- aceEditor('code_used_read',
                              value='# code to read in your data',
                              mode='r',
                              readOnly=TRUE,
                              wordWrap=TRUE,
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
                                         choices=c('Split plot design'='plots',
                                                   'Data of corn'='corn',
                                                   'Data of cotton'='cotton',
                                                   'Data of sweetpotato yield'='sweetpotato')
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
                              #verbatimTextOutput('debug'),
                              dataTableOutput('data_table')
                              )
                            )
                          )

# Data Analysis Tab
var.type.collapse <- bsCollapsePanel(
                       '1. Variable types',
                       id='variable_type_panel',
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

anal.type.collapse <- bsCollapsePanel(
                        '2. Type of analysis',
                        id='select_analysis_panel',
                        uiOutput('select_analysis'),
                        bsButton('analysis_info_button',
                                 "Analysis information"),
                        bsModal('analysis_info_content',
                                trigger='analysis_info_button',
                                title='Information on analysis types',
                                h5(help.text$t.test.explanation), br(),
                                h5(help.text$anova.explanation), br(),
                                h5(help.text$lm.explanation), br(),
                                h5(help.text$rcbd.explanation)
                                )
                        )

dep.var.collapse <- bsCollapsePanel(
                      "3. Dependent variable",
                      id='select_variables_panel',
                      uiOutput('select_dv'),
                      uiOutput('select_dv_type'),
                      conditionalPanel("input.analysis=='t.test'",
                                       bsButton('ttest_dv_info_button',
                                                'More info')),
                      conditionalPanel("input.analysis=='aov'",
                                       bsButton('aov_dv_info_button',
                                                'More info')),
                      conditionalPanel("input.analysis=='glm'",
                                       bsButton('glm_dv_info_button',
                                                'More info')),
                      conditionalPanel("input.analysis=='rcbd'",
                                       bsButton('rcbd_dv_info_button',
                                                'More info')),
                      bsModal('glm_dv_info_content',
                              title='Dependent variable information',
                              trigger='glm_dv_info_button',
                              h5(help.text$continuous.explanation), br(),
                              h5(help.text$dichotomous.explanation), br(),
                              h5(help.text$count.explanation)
                              ),
                      bsModal('ttest_dv_info_content',
                              title='Dependent variable information',
                              trigger='ttest_dv_info_button',
                              h5(help.text$t.test.dv)),
                      bsModal('aov_dv_info_content',
                              title='Dependent variable information',
                              trigger='aov_dv_info_button',
                              h5(help.text$anova.dv)),
                      bsModal('rcbd_dv_info_content',
                              title='Dependent variable information',
                              trigger='rcbd_dv_info_button',
                              h5(help.text$rcbd.dv))
                       )

ind.var.collapse <- bsCollapsePanel(
                      '4. Independent variables',
                      id='iv_info_panel',
                      uiOutput('select_block'),
                      uiOutput('select_treatment'),
                      uiOutput('select_iv'),
                      bsButton('select_iv_info',
                               'Independent variable info'),
                      bsModal('iv_info_content',
                              title='Information on independent variables',
                              trigger='select_iv_info',
                              h5(help.text$ind.var.explanation))
                      )

interactions.collapse <- bsCollapsePanel('5. Interactions',
                                         uiOutput('select_interactions'))

model.check.collapse <- bsCollapsePanel('6. Model check')

data.anal.editor <- aceEditor('code_used_model', value='# code to run analysis',
                              mode='r', wordWrap=TRUE, readOnly=TRUE,
                              height='150px')

data.analysis.tab <- tabPanel('2. Data analysis',
                              sidebarLayout(
                                sidebarPanel(
                                  bsCollapse(
                                    multiple=FALSE,
                                    open='variable_type_panel',
                                    id='main_collapse_panel',
                                    var.type.collapse,
                                    anal.type.collapse,
                                    dep.var.collapse,
                                    ind.var.collapse,
                                    interactions.collapse,
                                    model.check.collapse
                                    ),
                                  uiOutput('action_button')),
                                  mainPanel(
                                    data.anal.editor,
                                    bsTooltip('code_used_model',
                                              'Click for more information',
                                              placement='top',
                                              trigger='hover'),
                                    bsPopover('code_used_model',
                                              title='Analysis R code',
                                              content=help.text$analysis.code.explanation,
                                              placement='bottom',
                                              trigger='click'),
                                    uiOutput('fit_output'),
                                    bsTooltip('fit_output',
                                              'Click for more information',
                                              placement='top',
                                              trigger='hover'),
                                    bsPopover('fit_output',
                                              title='Standard output',
                                              help.text$fit.explanation,
                                              placement='left',
                                              trigger='click'))
                                  )
                              )

# Post-hoc Tests Tab
posthoc.tab <- tabPanel('3. Post-hoc tests',
                        sidebarLayout(
                          sidebarPanel(h4('Specify post hoc tests')),
                          mainPanel(h3('Post-hoc test results'))
                          )
                        )

# Plots Tabs
plot.tabs <- navbarMenu('4. Plots',
                        tabPanel('Histograms', uiOutput('hists')),
                        tabPanel('Effect plots',
                                 actionButton('update.plots', 'Update plots'),
                                 uiOutput('plots'))
                        )

# Report Tab
report.tab <- tabPanel('5. Report',
                       sidebarLayout(
                         sidebarPanel(downloadButton('download_report')),
                         mainPanel()
                         )
                       )

# Help Tab
help.tab <- navbarMenu('Help',
                       tabPanel('1. Load data'),
                       tabPanel('2. Data analysis'),
                       tabPanel('3. Post-hoc tests'),
                       tabPanel('4. Plots'),
                       tabPanel('5. Report'))

# UI
shinyUI(
  navbarPage(
    title='AIP Analysis Interface',
    windowTitle='AIP Analysis Interface',
    position='fixed-top',
    load.data.tab,
    data.analysis.tab,
    posthoc.tab,
    plot.tabs,
    report.tab,
    help.tab
    )
  )
