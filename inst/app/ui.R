source('pkg_check.R')

library(shiny)
library(shinyAce)
library(shinyBS)

# Load Data Tab
load.data.editor <- aceEditor('code_used_read',
                              value='# code to read in your data',
                              mode='r',
                              readOnly=TRUE,
                              wordWrap=TRUE,
                              height='100px')

editor.popover.content <- paste('The R code needed to read in your data will ',
                                'appear here. If you have selected "use sample ',
                                'data instead," you will be given the option ',
                                'of selecting from a number of datasets ',
                                'available to R users. These datasets provide ',
                                'an easy way to explore different data analysis',
                                'techniques.',
                                sep='')

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
                                    content=editor.popover.content,
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
text <- paste('Numeric variables are any variables that are measured on a ',
              'continuous scale. Yield is a common numeric variable. Grouping ',
              'variables are variables with different levels. These levels do ',
              'not convey information on a continuous scale in the way that ',
              'numeric variables do. Block, plot, variety, and treatment are ',
              'common grouping variables.', sep='')

text.0 <- paste('t-tests are for testing the difference between the means of ',
                'two groups. A t-test requires a continuous response or ',
                'dependent variable, such as yield or N content', sep='')

text.1 <- paste('ANOVAs are for testing differences between more than two ',
                'groups. A one-way ANOVA examines the influence of one ',
                'categorical, independent variable on one continuous ',
                'dependent variable, such as yield. A two-way ANOVA examines ',
                'the influence of two categorical independent variables on a ',
                'dependent variable. With a two-way ANOVA, it is possible to ',
                'assess the main effect of each independent variable and also ',
                'if there is an interaction between them. If each level of ',
                'each independent variable is tested against each level of ',
                'the other factor, then this is called a factorial ANOVA',
                sep="")

text.2 <- paste('Linear models and generalized linear models allow you to run ',
                'a regression. Linear models are useful if your dependent ',
                'variable is continuous. An example would be understanding ',
                'the relationship between pest incidence and yield. ',
                'Generalized linear models are useful for using different ',
                'distributions. To find out more, select "linear or ',
                'generalized linear model" from the Select Analysis tab and ',
                'click "more info" under the dropdown box asking "What type ',
                'of data is your dependent variable?', sep='')

text.3 <- paste('The analysis of a Randomized Complete Block Designs (RCBD) ',
                'is done using an ANOVA. Generally only two independent ',
                'variables are used in a RCBD, block and treatment. If you ',
                'have used a RCBD but want to include more than just block ',
                'and treatment in your analysis, please select ANOVA instead.',
                sep='')

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
                               h5(text))
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
                                h5(text.0), br(),
                                h5(text.1), br(),
                                h5(text.2), br(),
                                h5(text.3)
                                )
                        )

text.4 <- paste('If your dependent variable (DV) is continuous, choose ',
                '"continuous" from the drop-down box above. This will run a ',
                'regression. A common dependent variable in regression with a ',
                'continuous variable is yield.', sep='')

text.5 <- paste('If your DV has only two levels, 1 or 0, choose "dichotomous ',
                '(binary)". This will run a logistic regression with the ',
                'assumption that your DV follows a binomial distribution. ',
                'This is not a common selection.', sep='')

text.6 <- paste('If your DV is a count variable, select "count" to run a ',
                'poisson regression. Disease count in crops is a common ',
                '"count" variable', sep='')

text.7 <- paste('Your dependent variable in a t-test is the continuous ',
                'variable you have measured. A common dependent variable ',
                'is yield.', sep='')

text.8 <- paste('Your dependent variable in an ANOVA is the continuous ',
                'variable you have measured. A common dependent variable ',
                'is yield.', sep='')

text.9 <- paste('When analyzing a randomized complete block design (RCBD), ',
                'your block variable is the variable contaning information ',
                'on what block each treatment was applied to. The treatment ',
                'variable is the variable that contains information on the ',
                'treatments you have applied in your experiment.', sep='')

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
                              h5(text.4), br(),
                              h5(text.5), br(),
                              h5(text.6)
                              ),
                      bsModal('ttest_dv_info_content',
                              title='Dependent variable information',
                              trigger='ttest_dv_info_button',
                              h5(text.7)),
                      bsModal('aov_dv_info_content',
                              title='Dependent variable information',
                              trigger='aov_dv_info_button',
                              h5(text.8)),
                      bsModal('rcbd_dv_info_content',
                              title='Dependent variable information',
                              trigger='rcbd_dv_info_button',
                              h5(text.9))
                       )

text.10 <- paste('Independent variables are those that are manipulated in ',
                 'order to cause change in the dependent variable. Some ',
                 'common independent variables are field, block, and ',
                 'treatment, but independent variables vary across ',
                 'experiments.', sep='')

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
                              h5(text.10))
                      )

interactions.collapse <- bsCollapsePanel('5. Interactions',
                                         uiOutput('select_interactions'))

model.check.collapse <- bsCollapsePanel('6. Model check')

data.anal.editor <- aceEditor('code_used_model', value='# code to run analysis',
                              mode='r', wordWrap=TRUE, readOnly=TRUE,
                              height='150px')

content.0 <- paste('This is the code needed to run your analysis. If you have ',
                   'specified that your data contains categorical variables ',
                   '(called "factors" in R), first they will be converted to ',
                   'this variable type via the function "as.factor." Only ',
                   'after R recognizes every variable in the form that you ',
                   'intended, is it advised to run your analyses. In R, the ',
                   'first argument when running a t-test, ANOVA, or a linear ',
                   'model is the formula. This formula allows you to specify ',
                   'your dependent and independent variables, including any ',
                   'interactions between your independent variables', sep='')

content.1 <- paste('When you clicked "Run analysis," your model was assigned ',
                   'to an object called "model.fit." If you type just ',
                   '"model.fit" into R, it will display the information it ',
                   'connects to this object. It is very easy to create and ',
                   'delete objects in R to assist with your analyses. It is ',
                   'possible to name objects anything that helps you remember ',
                   'the information the object contains, for example ',
                   '"cotton.T.Test." If you designate that the object ',
                   '"cotton.T.Test" is a t-test for your selected variables, ',
                   'then it is as simple as typing "cotton.T.Test" into the R ',
                   'console to view the results. If you are running an ANOVA ',
                   'or GLM analysis, first assign your desired test to an ',
                   'object such as "model.fit" and then print the test ',
                   'results using a function anova(). This looks like ',
                   'anova(model.fit).', sep='')

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
                                              content=content.0,
                                              placement='bottom',
                                              trigger='click'),
                                    uiOutput('fit_output'),
                                    bsTooltip('fit_output',
                                              'Click for more information',
                                              placement='top',
                                              trigger='hover'),
                                    bsPopover('fit_output',
                                              title='Standard output',
                                              content.1,
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
