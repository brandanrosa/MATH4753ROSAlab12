y <- c(4.5052, 5.4089, 4.8848, 5.0478, 6.9697, 4.4436, 3.8956, 5.9755, 4.6879, 6.1052)
iter <- 10000
n <- length(y)
sam <- sample(x = y, size = n*iter, replace = TRUE)
mat <- matrix(sam, nrow = n, ncol = iter)

loglam <- function(x) {
  n1 <- length(x)
  (-n1 * mean(x)^2) / 2
}

meany <- function(x) {
  mean(x)
}

###########################################################################
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Distributions of log(lamda) and Ybar"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha",
                  "alpha level:",
                  min = 0,
                  max = 1,
                  value = 0.05,
                  step = 0.01)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("loglam"),
      plotOutput("ybar")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$loglam <- renderPlot({

    xstat1 <- apply(X = mat, MARGIN = 2, FUN = "loglam")
    h1 <- hist(xstat1, plot = FALSE)
    r1 <- h1$density/max(h1$density)

    hist1 <- hist(x = xstat1,
                  freq = FALSE,
                  col = rgb(r1, r1^2, 0),
                  xlab = "log(lambda)",
                  main = "Sampling Distribution of log(lambda)",
    )
    hist1

    loglamcut <- quantile(xstat1, probs = input$alpha)
    abline(v = loglamcut, col = 'blue', lwd=2)

  })

  output$ybar <- renderPlot({

    xstat2 <- apply(X = mat, MARGIN = 2, FUN = "meany")
    h2 <- hist(xstat2, plot = FALSE)
    r2 <- h2$density/max(h2$density)

    hist2 <- hist(x = xstat2,
                  freq = FALSE,
                  col = rgb(r2, r2^2, 0),
                  xlab = "Ybar",
                  main = "Sampling Distribution of Ybar",
    )
    hist2

    ybarcut <- quantile(x = xstat2, probs = 1 - input$alpha)
    abline(v = ybarcut, col = 'blue', lwd=2)

  })
}

# Run the application
shinyApp(ui = ui, server = server)
