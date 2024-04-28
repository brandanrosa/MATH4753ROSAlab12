library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),

  # Application title
  titlePanel("Power Calc"),

  sidebarLayout(
    sidebarPanel(
      numericInput("mu0",
                   "mu_0:",
                   value = 10),

      sliderInput("sigma",
                  "sigma:",
                  min = 0.5,
                  max = 25,
                  value = 2),

      sliderInput("n",
                  "sample size n:",
                  min = 1,
                  max = 20,
                  value = 10,
                  step = 1),

      sliderInput("delta",
                  "delta:",
                  min = 1,
                  max = 10,
                  value = 2,
                  step = 0.5),

      sliderInput("alpha",
                  "alpha level:",
                  min = 0,
                  max = 1,
                  value = 0.05,
                  step = 0.01),
    ),

    mainPanel(
      plotOutput("power",
                 click = "plot_click"),
      verbatimTextOutput("data")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$power <- renderPlot({

    reactive({
      n <- input$n
      moe <- qnorm(1-input$alpha/2)*input$sigma/sqrt(input$n)
      L <- input$mu0 - moe
      U <- input$mu0 + moe

      mu <- input$mu0 + input$delta
      zL <- (L-mu) / (input$sigma/sqrt(input$n))
      zU <- (U-mu) / (input$sigma/sqrt(input$n))

      beta <- pnorm(zU) - pnorm(zL)
      power <- 1 - beta

      df <- data.frame(power, beta, n)
    })

    # Power Plot
    x <- 0:30
    plotpwr <- pnorm(q = sqrt(x)-qnorm(1-input$alpha/2, 0 , 1), mean = 0, sd = 1)
    plot(x,
         plotpwr,
         type = "l",
         main = paste0("Power Calc: delta = ", input$delta, ", sd = ", input$sigma, ", alpha = ", input$alpha, sep=""),
         ylab = "Power",
         xlab = "n",
         col = "hotpink",
         lwd = 3
    )
  })

  output$data <- renderPrint({
    req(input$plot_click)
    x <- input$plot_click$x
    y <- round(input$plot_click$y, 4)
    cat("Power = ", y, " n = ", x, sep="")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
