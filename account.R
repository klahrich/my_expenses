library(dplyr)


accountUI <- function(id){
  
  ns <- NS(id)
  
  infoBoxOutput(
    ns("account")
  )
  
}


account <- function(input, output, session, accounts, account, data, start_date, color){
  
  data_account <- 
    data %>%
    filter(
      date >= start_date,
      from == account | to == account
    ) %>%
    mutate(
      amount = ifelse(account == from, -amount, amount)
    )
  
  delta_account <- sum(data_account$amount)
  
  accounts[[account]] <- accounts[[account]] + delta_account
  
  output$account <- 
    renderValueBox({
      valueBox(
        value = dollar(accounts[[account]]),
        subtitle = account,
        color = color
      )
    })
  
}