#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(scales) 
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)
library(scales)
library(plotly)
library(DT)
library(lubridate)


start_date <- as.Date("2018-08-12", "%Y-%m-%d")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  raw_data <- read_data()
  
  accounts <-
    c(
      BMO = 17809.33,
      Tangerine = 260.60,
      TangerineTFSA = 0,
      TangerineCC = 0,
      Cash = 55 - 80 - 20,
      Mastercard = 0,
      CELI = 0,
      MAD = 0,
      Koho = 0,
      CapitalOne = 0,
      Loans = 0,
      MastercardBlack = 0
    )
  
  callModule(account, "bmo", accounts, "BMO", raw_data, start_date, "aqua")
  callModule(account, "mastercard", accounts, "Mastercard", raw_data, start_date, "orange")
  callModule(account, "cashback_mastercard", accounts, "MastercardBlack", raw_data, start_date, "black")
  callModule(account, "tangerine", accounts, "Tangerine", raw_data, start_date, "lime")
  callModule(account, "tangerine_tfsa", accounts, "TangerineTFSA", raw_data, start_date, "purple")
  callModule(account, "tangerine_cc", accounts, "TangerineCC", raw_data, start_date, "maroon")
  callModule(account, "cash", accounts, "Cash", raw_data, start_date, "blue")
  callModule(account, "mastercard", accounts, "Mastercard", raw_data, start_date, "orange")
  callModule(account, "koho", accounts, "Koho", raw_data, start_date, "green")
  callModule(account, "capital_one", accounts, "CapitalOne", raw_data, start_date, "navy")
  callModule(account, "celi", accounts, "CELI", raw_data, start_date, "yellow")
  
  ####
  
  output$year_start <- renderUI({
    selectInput(
      inputId = "year_start",
      label = "Start year",
      choices = seq(min(raw_data$trx_year), max(raw_data$trx_year)),
      selected = min(raw_data$trx_year)
    )
  })
  
  output$year_start_overview <- renderUI({
    selectInput(
      inputId = "year_start_overview",
      label = "Start year",
      choices = seq(min(raw_data$trx_year), max(raw_data$trx_year)),
      selected = min(raw_data$trx_year)
    )
  })
  
  output$month_start_overview <- renderUI({
    selectInput(
      inputId = "month_start_overview",
      label = "Start month",
      choices = seq(1, 12),
      selected = 1
    )
  })
  
  
  output$year_end <- renderUI({
    req(input$year_start)
    
    selectInput(
      inputId = "year_end",
      label = "End year",
      choices = seq(min(raw_data$trx_year), max(raw_data$trx_year)),
      selected = max(max(raw_data$trx_year), input$year_start)
    )
  })
  
  output$year_end_overview <- renderUI({
    req(input$year_start_overview)
    
    selectInput(
      inputId = "year_end_overview",
      label = "End year",
      choices = seq(min(raw_data$trx_year), max(raw_data$trx_year)),
      selected = max(max(raw_data$trx_year), input$year_start_overview)
    )
  })
  
  output$month_end_overview <- renderUI({
    selectInput(
      inputId = "month_end_overview",
      label = "End month",
      choices = seq(1, 12),
      selected = 12
    )
  })
  
  
  income_expense_data <- 
    raw_data %>%
    filter(is_income | is_expense)
  
  income_expense_profit <- 
    income_expense_data %>%
    mutate(amount = ifelse(type == "Expense", -amount, amount),
           type = "Profit")
  
  income_expense_data <-
    bind_rows(
      income_expense_data,
      income_expense_profit
    )
  
  
  output$income_expense_plot0 <- renderPlot({
    
    ggplot(
      data = 
        income_expense_data %>%
        mutate(year_month = as.Date(ISOdate(year(date), month(date), 1))) %>%
        group_by(year_month, type) %>%
        summarise(amount = sum(amount)),
      aes(x = year_month, y = amount)
    ) + 
      geom_line(aes(color = type, group=type), size = 1) +
      geom_point(aes(color = type, group=type), size = 2) + 
      #geom_bar(aes(fill = type), position="fill", stat="identity") +
      #geom_step(aes(color = type, group=type), size = 1) +
      #geom_point(aes(color = type, group=type), size = 5, shape=22) +
      geom_hline(yintercept=0) + 
      scale_x_date(date_breaks = "3 months",
                   date_minor_breaks = "1 month", 
                   labels = date_format("%Y-%m"),
                   limits = c(as.Date("2016-06-01"), NA)) +  
      theme_bw()
  })
  
  
  output$income_expense_plot <- renderPlot({
  
    ggplot(
      data = 
        income_expense_data %>%
        group_by(type) %>%
        mutate(amount = cumsum(amount)),
      aes(x = date, y = amount)
    ) + 
      geom_line(aes(color = type), size = 1) + 
      scale_x_date(date_breaks = "3 months",
                   date_minor_breaks = "1 month",
                   labels = date_format("%Y-%m"),
                   limits = c(as.Date("2016-06-01"), NA)) + 
      theme_bw()
  })
  
  
  output$income_expense_table <- DT::renderDataTable({
    
    income_expense_data %>%
      group_by(trx_year, trx_month, type) %>%
      summarise(
        amount = sum(amount)
      ) %>%
      spread(type, amount) %>%
      ungroup() %>%
      mutate(
        Profit = Income - Expense,
        `Cum Expense` = cumsum(Expense),
        `Cum Income` = cumsum(Income),
        `Cum Profit` = cumsum(Profit),
        Income = dollar(round(Income)),
        Expense = dollar(round(Expense)),
        Profit = dollar(round(Profit)),
        `Cum Income` = dollar(round(`Cum Income`)),
        `Cum Expense` = dollar(round(`Cum Expense`)),
        `Cum Profit` = dollar(round(`Cum Profit`))
      ) 
  }, filter = 'top', options=list(style = 'bootstrap', autoWidth = TRUE))
  
  
  
  output$category <- renderUI({
    selectInput(
      inputId = "category",
      label = "Category",
      choices = sort(c("All", unique(raw_data %>% filter(is_expense) %>% .$category))),
      selected = "All"
    )
  })
  
  
  data_expense_details <- reactive({
    req(input$category)
    
    data_year <-
      raw_data %>%
      filter(
        trx_year >= input$year_start,
        trx_year <= input$year_end
      )
    
    if(input$category == "All"){
      data_year %>% filter(is_expense)
    }else{
      data_year %>% filter(is_expense, category == input$category)
    }
  })
  
  
  
  output$subcategory <- renderUI({
    selectInput(
      inputId =  "subcategory",
      label = "Sub-Category",
      choices = sort(c("All", unique(data_expense_details()$to))),
      selected = "All"
    )
  })
  
  
  data_expense_details2 <- reactive({
    req(input$subcategory)
    
    if(input$subcategory == "All"){
      data_expense_details()
    }else{
      data_expense_details() %>% filter(to == input$subcategory)
    }
  })
  
  
  data_expense_monthly <- reactive({

    data_expense_details2() %>%
      group_by(
        trx_year, trx_month
      ) %>%
      summarise(
        amount = sum(amount)
      )
  })
  
  output$expense_details_plot <- renderPlot({
    
    req(input$category)
    req(input$subcategory)
    
    if(input$year_start == 2016){
      limit_min <- as.Date(ISOdate(2016, 5, 1))
    }else{
      limit_min <- as.Date(ISOdate(as.numeric(input$year_start)-1, 12, 1))
    }
    
    ggplot(
      data = data_expense_monthly(),
      aes(x = as.Date(ISOdate(trx_year, trx_month, 1)), y = amount)
    ) +
      geom_bar(stat = "identity", fill = "coral1") + 
      geom_text(aes(label = round(amount)), size = 4, vjust = -0.25) + 
      scale_x_date(name = "Month",
                   date_breaks = "3 months",
                   labels = date_format("%Y-%m"),
                   limits = c(limit_min, NA)) 
  })
  
  
  data_income <- reactive({
    req(input$year_start_overview)
    req(input$year_end_overview)
    req(input$month_start_overview)
    req(input$month_end_overview)
    
    year_start <- as.numeric(input$year_start_overview)
    year_end <- as.numeric(input$year_end_overview)
    month_start <- as.numeric(input$month_start_overview)
    month_end <- as.numeric(input$month_end_overview)
    
    data_year <-
      raw_data %>%
      filter(
        ((trx_year == year_start) & (trx_month >= month_start)) | 
          trx_year > year_start,
        ((trx_year == input$year_end_overview) & (trx_month <= month_end)) | 
          trx_year < year_end,
        is_income
      )
    
    if(input$period %in% c("4weeks", "monthly")){
      
      start <- ymd(min(paste0(
        data_year$trx_year, 
        sprintf("%02d", data_year$trx_month), 
        "01")))
      
      end <- ymd(max(paste0(
        data_year$trx_year, 
        sprintf("%02d", data_year$trx_month), 
        "01")))
      day(end) <- days_in_month(end)
      end <- end + 1
      
      if(input$period == "4weeks"){
        interv <- time_length(interval(start, end), "week") / 4
      }else{
        interv <- time_length(interval(start, end), "month")
      }
      data_year$amount <- data_year$amount / interv 
    }
    
    data_year
  })
  
  
  data_expense_distr <- reactive({
    req(input$year_start_overview)
    req(input$year_end_overview)
    req(input$month_start_overview)
    req(input$month_end_overview)
    
    year_start <- as.numeric(input$year_start_overview)
    year_end <- as.numeric(input$year_end_overview)
    month_start <- as.numeric(input$month_start_overview)
    month_end <- as.numeric(input$month_end_overview)
    
    data_year <-
      raw_data %>%
      filter(
        ((trx_year == year_start) & (trx_month >= month_start)) | 
          trx_year > year_start,
        ((trx_year == year_end) & (trx_month <= month_end)) | 
          trx_year < year_end,
        is_expense
      )
    
    if(input$period %in% c("4weeks", "monthly")){
      
      start <- ymd(min(paste0(
                        data_year$trx_year, 
                        sprintf("%02d", data_year$trx_month), 
                        "01")))
      
      end <- ymd(max(paste0(
                      data_year$trx_year, 
                      sprintf("%02d", data_year$trx_month), 
                      "01")))
      day(end) <- days_in_month(end)
      end <- end + 1
      
      if(input$period == "4weeks"){
        interv <- time_length(interval(start, end), "week") / 4
      }else{
        interv <- time_length(interval(start, end), "month")
      }
      data_year$amount <- data_year$amount / interv
    }
    
    data_year
  })
  
  
  output$expense_distr_plot <- renderPlot({
    req(input$year_start_overview)
    req(input$year_end_overview)
    req(input$month_start_overview)
    req(input$month_end_overview)
    
    #date_int <- interval(min(data_expense_distr()$date), max(data_expense_distr()$date))
    #cat(ceiling(time_length(date_int, "month")))
    
    plot_data <- data_expense_distr() %>%
                group_by(category) %>%
                summarise(amount = abs(sum(amount)))
    
    ggplot(data = plot_data,
           aes(x=category, y=amount, fill=category)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(y = amount, label = round(amount)), vjust=-0.5) + 
      ggtitle("Expense distribution", subtitle = "Per 4 week periods") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) #+
      #scale_y_continuous(breaks=seq(from=0, to=max(plot_data$amount), by=100))
  })
  
  
  output$expense_distr_plot2 <- renderPlot({
    req(input$year_start_overview)
    req(input$year_end_overview)
    req(input$month_start_overview)
    req(input$month_end_overview)
    
    #date_int <- interval(min(data_expense_distr()$date), max(data_expense_distr()$date))
    
    #cat(ceiling(time_length(date_int, "month")))
    
    plot_data =data_expense_distr() %>%
      group_by(category) %>%
      summarise(amount = abs(sum(amount))) %>%
      mutate(amount = amount / sum(amount))
    
    ggplot(data = plot_data,
           aes(x=category, y=amount, fill=category)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(y = amount, label = scales::percent(amount)), vjust=-0.5) + 
      ggtitle("Expense distribution", subtitle = "Per 4 week periods") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) +
      scale_y_continuous(breaks=seq(from=0, to=max(plot_data$amount), by=100))
  })
  
  
  # output$expense_details_total <- renderValueBox({
  #   plot_data <- data_expense_distr()
  #   
  #   valueBox(
  #     value = dollar(round(sum(plot_data$amount))),
  #     subtitle = "Total",
  #     color = "light-blue"
  #   )
  # })
  
  
  output$expense_details_perc <- renderValueBox({
    valueBox(
      value = percent((sum(data_expense_monthly()$amount) / sum(data_income()$amount))),
      subtitle = "% of Income",
      color = "green"
    )
  })
  
  
  output$expense_monthly <- renderValueBox({
    plot_data <- data_expense_distr()
    
    valueBox(
      value = dollar(round(sum(plot_data$amount))),
      subtitle = "Expense",
      color = "orange"
    )
  })
  
  output$income_monthly <- renderValueBox({
    plot_data <- data_income()
    
    valueBox(
      value = dollar(round(sum(plot_data$amount))),
      subtitle = "Income",
      color = "green"
    )
  })
  
  output$profit_monthly <- renderValueBox({
    plot_data1 <- data_income()
    plot_data2 <- data_expense_distr()
    
    valueBox(
      value = dollar(round(sum(plot_data1$amount) - sum(plot_data2$amount))),
      subtitle = "Profit",
      color = "blue"
    )
  })
  
  
  #### Update accounts ----
  
  # for(account in isolate(names(accounts))){
  #   
  #   cat(account)
  #   cat('\n')
  # 
  #   output[[account]] <- 
  #     renderInfoBox({
  #       
  #       data_account <- 
  #         data %>%
  #         filter(
  #           date >= start_date,
  #           from == account | to == account
  #         ) %>%
  #         mutate(
  #           amount = ifelse(account == from, -amount, amount)
  #         )
  #       
  #       delta <- sum(data_account$amount)
  #       accounts[[account]] <- accounts[[account]] + delta
  #       
  #       infoBox(
  #         account,
  #         accounts[[account]]
  #       )
  #     })
  # }
  
})
