#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

# mortgage logic
calculate_monthly_payment <- function(mortgage_cost, mortgage_rate_monthly, total_months) {
  mortgage_cost * 
    (mortgage_rate_monthly * (1 + mortgage_rate_monthly) ^ total_months) / 
    ((1 + mortgage_rate_monthly) ^ total_months - 1)
}

calculate_mortgage <- function(mortgage_cost, mortgage_rate_monthly, monthly_payment, total_months) {
  principal <- mortgage_cost
  cumulative_interest <- 0
  
  principal_out <- vector(mode = "double", length = total_months)
  interest_out <- vector(mode = "double", length = total_months)
  
  for(i in 1:total_months) {
    I <- mortgage_rate_monthly * principal
    new_principal <- principal + I - monthly_payment
    principal_out[i] <- new_principal
    interest_out[i] <- I
    principal <- new_principal
  }
  
  tibble::tibble(
    month_number = 1:total_months,
    remaining_principal = principal_out,
    monthly_interest = interest_out
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("To buy or rent?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput(
          "rent",
          "How much would you be paying in rent + utilities?",
          min = 0,
          max = 3000,
          value = 1500,
          step = 25
        ),
        sliderInput(
          "rent_appreciation",
          "What is the approximate yearly increase in rent (%)?",
          min = 0,
          max = 10,
          value = 2, 
          step = 0.1
        ),
         sliderInput(
           "price",
           "How much are you paying for the house?",
           min = 50000,
           max = 500000,
           value = 150000,
           step = 10000
        ),
        sliderInput(
          "mortgage_rate",
          "What is the mortgage interest rate?",
          min = 1,
          max = 10,
          value = 4,
          step = 0.1
        ),
        sliderInput(
          "mortgage_length",
          "What is the amortization period of the morgage?",
          min = 10,
          max = 30,
          value = 25
        ),
        sliderInput(
          "down_payment",
          "What is the down payment on the mortage?",
          min = 5000,
          max = 50000,
          value = 10000,
          step = 100
        ),
        sliderInput(
          "buy_fixed",
          "What are the fixed costs associated with buying the house (deed transfer, legal, inspection, etc.)?",
          min = 0, 
          max = 20000,
          value = 8000
        ),
        sliderInput(
          "utilities",
          "What is the estimated monthly utility cost?",
          min = 0,
          max = 2000,
          value = 300,
          step = 10
        ),
        sliderInput(
          "maintenance",
          "What is the estimated monthly maintenance cost?",
          min = 0,
          max = 2000,
          value = 400,
          step = 10
        ),
        sliderInput(
          "property_tax_rate",
          "What is the property tax rate?",
          min = 0,
          max = 20,
          value = 2.5,
          step = 0.1
        ),
        sliderInput(
          "appreciation",
          "What is the anticipated rate of property appreciation?",
          min = -10,
          max = 10,
          value = 2,
          step = 0.1
        ),
        sliderInput(
          "realator_cost_rate",
          "What percent will the real-estate agent take when you well the house?",
          min = 0,
          max = 10,
          value = 7,
          step = 0.5
        )
      ),
      
      mainPanel(
         plotlyOutput("dataPlot", height = "600px"),
         tableOutput("monthly"),
         htmlOutput("summary")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  calcs <- reactive({
    mortgage_cost <- input$price - input$down_payment
    mortgage_rate <- input$mortgage_rate / 100
    mortgage_rate_monthly <- mortgage_rate / 12
    total_months <- 12 * input$mortgage_length
    
    monthly_payment <- calculate_monthly_payment(
      mortgage_cost = mortgage_cost,
      mortgage_rate_monthly = mortgage_rate_monthly,
      total_months = total_months
    )
    
    mortgage_calcs <- calculate_mortgage(
      mortgage_cost = mortgage_cost,
      mortgage_rate_monthly = mortgage_rate_monthly,
      monthly_payment = monthly_payment,
      total_months = total_months
    )
    
    df <- tibble(
      # time variables
      month_number = seq_len(total_months),
      years = month_number / 12,
      year_number = ((month_number - 1) %/% 12) + 1,
      month_in_year = month_number - ((year_number - 1) * 12),
      
      # calculate rent
      monthly_rent = input$rent * (1 + input$rent_appreciation / 100) ^ years,
      cumulative_rent = cumsum(monthly_rent),
      
      # house buying costs
      cumulative_buying_costs = input$buy_fixed,
      
      # calculate mortagage info
      monthly_payment = monthly_payment,
      monthly_interest = mortgage_calcs$monthly_interest,
      cumulative_principal_remaining = mortgage_calcs$remaining_principal,
      cumulative_equity = mortgage_cost - cumulative_principal_remaining + input$down_payment,
      cumulative_payment = cumsum(monthly_payment),
      cumulative_interest = cumsum(monthly_interest),
      
      # calculate home info
      house_value = input$price * (1 + input$appreciation / 100) ^ years,
      cumulative_appreciation = house_value - input$price,
      cumulative_depreceation = -cumulative_appreciation,
      house_value_yearly = input$price * (1 + input$appreciation / 100) ^ year_number,
      monthly_utilities = input$utilities * (1 + input$rent_appreciation / 100) ^ years,
      cumulative_utilities = cumsum(monthly_utilities),
      monthly_property_tax = house_value_yearly * input$property_tax_rate / 100 / 12,
      cumulative_property_tax = cumsum(monthly_property_tax),
      monthly_maintenance = input$maintenance * (1 + input$rent_appreciation / 100) ^ years,
      cumulative_maintenance = cumsum(monthly_maintenance),
      
      # theoretical house sale costs
      cumulative_selling_costs = house_value * input$realator_cost_rate / 100,
      
      # totals
      cumulative_rent_cost = cumulative_rent,
      cumulative_home_cost = cumulative_depreceation +
        cumulative_buying_costs + cumulative_selling_costs +
        cumulative_interest + cumulative_utilities +
        cumulative_maintenance + cumulative_property_tax
    )

    # return df    
    df
  })
  
   output$dataPlot <- renderPlotly({
     df <- calcs()
     
     df_cumulative_expenses <- df %>%
       select(years,
              cumulative_depreceation,
              cumulative_buying_costs, cumulative_selling_costs,
              cumulative_interest, cumulative_utilities,
              cumulative_maintenance, cumulative_property_tax) %>%
       gather(-years, key = expense_type, value = value, factor_key = TRUE) %>%
       group_by(years) %>%
       arrange(expense_type) %>%
       mutate(ymax = cumsum(value), ymin = lag(ymax, default = 0)) %>%
       ungroup() %>%
       arrange(years, expense_type) %>%
       mutate(expense_type = fct_relabel(expense_type, . %>% 
                                           str_remove("^cumulative_") %>% 
                                           str_replace_all("_", " ") %>%
                                           str_to_title()))
     
     ggplot(df, aes(years)) +
       geom_ribbon(aes(fill = expense_type, ymin = ymin, ymax = ymax), 
                   data = df_cumulative_expenses) +
       geom_line(aes(y = cumulative_rent)) +
       scale_y_continuous(
         labels = function(x) paste("   ", as.character(x / 1000))
       ) +
       labs(y = "Cumulative cost (x $1,000)", x = "Time (years)",
            fill = "Expense Type")
   })
   
   output$monthly <- renderTable({
     d <- event_data("plotly_hover")
     df <- calcs()
     
     if (is.null(d)) {
       row <- df %>% slice(1)
     } else {
       row <- df %>%
         filter(years >= d$x) %>%
         slice(1)
     }
     
     row %>%
       select(years, starts_with("monthly")) %>%
       rename_all(. %>% 
                    str_remove("^monthly_") %>% 
                    str_replace_all("_", " ") %>%
                    str_to_title()) %>%
       mutate_all(~format(round(.), trim = TRUE, big.mark = ","))
   })
   
   output$summary <- renderText({
     
     df <- calcs()
     buying_is_cheaper <- df$cumulative_home_cost < df$cumulative_rent_cost
     
     if(any(buying_is_cheaper)) {
       years <- min(df$years[buying_is_cheaper]) %>%
         ceiling()
       cost <- df %>% 
         filter(buying_is_cheaper) %>% 
         pull(cumulative_rent_cost) %>% 
         min() %>%
         round() %>%
         format(big.mark = ",", trim = TRUE)
       
       message <- paste(
         "<p>You will save money live in this house for at least <b>", years, " years</b>, 
         after you spent <b>$", cost, "</b> that you will never get back.</p>",
         sep = ""
       )
     } else {
       message <- "<p>Buying this house will never be cheaper than renting that apartment.</p>"
     }
     
     
      paste(
        "<h3>Summary</h3>",
        message,
        sep = "\n"
      )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

