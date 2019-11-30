#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(plotly)
library(DT)


dashboardPage(
  dashboardHeader(title = "Expense Tracker"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accounts", tabName = "accounts", icon = icon("table")),
      menuItem("Trends", tabName = "trends", icon = icon("line-chart")),
      menuItem("Expense Details", tabName = "expense_details", icon = icon("bar-chart-o")),
      menuItem("Expense Overview", tabName = "expense_overview", icon = icon("bar-chart-o"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Accounts
      tabItem(tabName = "accounts",
              fluidRow(
                accountUI("bmo"),
                accountUI("mastercard"),
                accountUI("cashback_mastercard"),
                accountUI("tangerine"),
                accountUI("tangerine_tfsa"),
                accountUI("tangerine_cc"),
                accountUI("cash"),
                accountUI("koho"),
                accountUI("capital_one"),
                accountUI("celi")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "trends",
              fluidRow(
                plotOutput("income_expense_plot0"),
                br(),
                plotOutput("income_expense_plot"),
                br(),
                DT::dataTableOutput("income_expense_table")
              )
      ),
      
      tabItem(tabName = "expense_details",
              fluidRow(
                column(
                  6, 
                  fluidRow(
                    uiOutput("year_start"),
                    uiOutput("year_end")
                  )
                ),
                column(
                  6,
                  uiOutput("category"),
                  uiOutput("subcategory")
                )
    
              ),
              
              fluidRow(
                fluidRow(
                  #valueBoxOutput("expense_details_total"),
                  #valueBoxOutput("expense_details_perc"),
                  plotOutput("expense_details_plot")
                )
              )
      ),
      
      tabItem(tabName = "expense_overview",
              fluidRow(
                column(6, uiOutput("year_start_overview")),
                column(6, uiOutput("year_end_overview"))
              ),
              fluidRow(
                column(6, uiOutput("month_start_overview")),
                column(6, uiOutput("month_end_overview"))
              ),
              fluidRow(
                column(12, 
                       radioButtons("period", 
                                    "Period", 
                                    choiceNames=c("Total", "Per 4 weeks", "Per month"),
                                    choiceValues=c("total", "4weeks", "monthly"),
                                    inline=TRUE))
              ),
              fluidRow(
                fluidRow(
                  valueBoxOutput("income_monthly"),
                  valueBoxOutput("expense_monthly"),
                  valueBoxOutput("profit_monthly")
                ),
                plotOutput("expense_distr_plot", height="600px"),
                plotOutput("expense_distr_plot2", height="400px")
              )
      )
    )
  )
)