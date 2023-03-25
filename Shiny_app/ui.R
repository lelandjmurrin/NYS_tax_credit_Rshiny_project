library(shinydashboard)

dashboardPage(
  dashboardHeader(title='NYS Article 9A Tax Credit Analysis'),
  dashboardSidebar(
    sidebarUserPanel("Leland Murrin",
                     image = 'NYCDSA.png'),
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("database"),
               menuSubItem('Background', tabName = 'background'),
               menuSubItem('Sample of Data', tabName = 'sample'),
               menuSubItem('Correlations', tabName = 'correlations'),
               menuSubItem('Distributions', tabName = 'distributions'),
               selectizeInput('dataset',
                              'Dataset',
                              choices = c('income', 'industry')), #dataset selection 
               selectizeInput('facetwrap',
                              'Facetwraps',
                              choices = c('Number of Taxpayers', 'Mean Amount of Credit')), #display facetwrap histograms
               selectizeInput('creditname',
                              'Credit Name',
                              choices = c()) #write out data as a table
               ),
      menuItem("Transform", tabName = "transform", icon = icon("cog"),
               menuSubItem('Methods', tabName = 'methods'),
               menuSubItem('Results', tabName = 'results')
               ),
      menuItem("Regression", tabName = "regression", icon = icon("line-chart"),
               menuSubItem('BoxCox', tabName = 'boxcox'),
               menuSubItem('Stepwise BIC', tabName = 'stepwise_bic'),
               menuSubItem('Regularization', tabName = 'regularization'),
               menuSubItem('Best Model', tabName = 'best_model')
               ),
      menuItem("Discussion", tabName = "discussion", icon = icon("refresh"),
               menuSubItem('Conclusions', tabName = 'conclusions'),
               menuSubItem('Future Work', tabName = 'future_work')
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'background',
              htmlOutput('background')
              ),
      tabItem(tabName = 'sample',
              dataTableOutput('sample')
              ),
      tabItem(tabName = 'correlations',
              fluidRow(
                column(12, box(h2("Correlations Plots"), width = NULL, plotOutput("correlations",  height = 600)))
                #column(7, plotOutput("delay"))
              )),
      tabItem(tabName = 'distributions',
              fluidRow(
                box(h2("Number of Taxpayers"), width = NULL, plotOutput("distributions.taxpayer",  height = 600)),
                box(h2("Amount of Credit per Taxpayer"), width = NULL, plotOutput("distributions.credit",  height = 600))
                #column(7, plotOutput("delay"))
              )),
      tabItem(tabName = 'data', dataTableOutput('table'))
    )
  )
)