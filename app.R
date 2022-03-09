library(shiny)
library(tidyverse)
library(shinyChakraSlider)
theme_set(theme_bw())

#### UI ####

ui <- fluidPage(
  tabPanel(
    "Distribution Gallery",
    sidebarLayout(
      sidebarPanel(
        tags$style(".well {background-color: #EEFEF2;}"),
        selectInput("dist", "Distribution",
                    choices = c("Normal", "Beta", "Gamma", "t"),
                    selected = "Normal"),
        conditionalPanel(
          condition = "input.dist == 'Normal'",
          sliderInput("normal_mu1", "Mean", value = 0,
                      min = -10, max = 10, step = .1),
          sliderInput("normal_sd1", "SD", value = 1,
                      min = 0, max = 10, step = .1),
          checkboxInput("add_norm", "Add a comparative plot?",
                        FALSE),
          conditionalPanel(
            condition = "input.add_norm",
            sliderInput("normal_mu2", "Mean", value = 0,
                        min = -10, max = 10, step = .1),
            sliderInput("normal_sd2", "SD", value = 1,
                        min = 0, max = 10, step = .1)
          )
        ),
        conditionalPanel(
          condition = "input.dist == 'Beta'",
          sliderInput("beta_a1", "Alpha", value = 2,
                      min = 0, max = 10, step = .1),
          sliderInput("beta_b1", "Beta", value = 5,
                      min = 0, max = 10, step = .1),
          checkboxInput("add_beta", "Add a comparative plot?",
                        FALSE),
          conditionalPanel(
            condition = "input.add_beta",
            sliderInput("beta_a2", "Alpha", value = 2,
                        min = 0, max = 10, step = .1),
            sliderInput("beta_b2", "Beta", value = 5,
                        min = 0, max = 10, step = .1)
          )
        ),
        conditionalPanel(
          condition = "input.dist == 'Gamma'",
          sliderInput("gamma_a1", "Shape (Alpha)", value = 2,
                      min = 0, max = 10, step = .1),
          sliderInput("gamma_b1", "Rate (Beta)", value = 5,
                      min = 0, max = 10, step = .1),
          checkboxInput("add_gamma", "Add a comparative plot?",
                        FALSE),
          conditionalPanel(
            condition = "input.add_gamma",
            sliderInput("gamma_a2", "Shape (Alpha)", value = 2,
                        min = 0, max = 10, step = .1),
            sliderInput("gamma_b2", "Rate (Beta)", value = 5,
                        min = 0, max = 10, step = .1)
          )
        ),
        conditionalPanel(
          condition = "input.dist == 't'",
          sliderInput("t_df1", "Degrees of Freedom", value = 0,
                      min = 1, max = 10, step = .1),
          sliderInput("t_ncp1", "Noncentrality Parameter", value = 0,
                      min = 0, max = 10, step = .1),
          checkboxInput("add_t", "Add a comparative plot?",
                        FALSE),
          conditionalPanel(
            condition = "input.add_t",
            sliderInput("t_df2", "Degrees of Freedom", value = 0,
                        min = 1, max = 10, step = .1),
            sliderInput("t_ncp2", "Noncentrality Parameter", value = 0,
                        min = 0, max = 10, step = .1)
          )
        )
      ),
      mainPanel(
        plotOutput("plot1", height = "250px") %>%
          shinycssloaders::withSpinner(type = 6, size = .8, color = #1DD6A2")
          )
      )
    )
  )
)

#### SERVER ####

server <- function(input, output) {

  plot <- reactive({
    if (input$dist == "Normal") {
      mu1 <- input$normal_mu1
      sd1 <- input$normal_sd1
      if (input$add_norm) {
        mu2 <- input$normal_mu2
        sd2 <- input$normal_sd2
        ggplot(data = data.frame(x = c(min(mu1, mu2) - 3 * max(sd1, sd2),
                                       max(mu1, mu2) + 3 * max(sd1, sd2))), aes(x)) +
          stat_function(fun = dnorm, geom = "line", col = "tomato2",
                        args = list(mean = mu1, sd = sd1)) +
          stat_function(fun = dnorm, geom = "area",
                        alpha = .3, fill = "tomato2",
                        args = list(mean = mu1, sd = sd1)) +
          stat_function(fun = dnorm, geom = "line", col = "turquoise4",
                        args = list(mean = mu2, sd = sd2)) +
          stat_function(fun = dnorm, geom = "area",
                        alpha = .3, fill = "turquoise4",
                        args = list(mean = mu2, sd = sd2))
      } else {
        ggplot(data = data.frame(x = c(mu1 - 3 * sd1, mu1 + 3 * sd1)), aes(x)) +
          stat_function(fun = dnorm, geom = "line", col = "tomato2",
                        args = list(mean = mu1, sd = sd1)) +
          stat_function(fun = dnorm, geom = "area",
                        alpha = .3, fill = "tomato2",
                        args = list(mean = mu1, sd = sd1))
      }
    } else if (input$dist == "Beta") {
      a1 <- input$beta_a1
      b1 <- input$beta_b1
      p1 <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
        stat_function(fun = dbeta, geom = "line", col = "tomato2",
                      args = list(shape1 = a1, shape2 = b1)) +
        stat_function(fun = dbeta, geom = "area",
                      alpha = .3, fill = "tomato2",
                      args = list(shape1 = a1, shape2 = b1))
      if (input$add_beta) {
        a2 <- input$beta_a2
        b2 <- input$beta_b2
        p1 +
          stat_function(fun = dbeta, geom = "line", col = "turquoise4",
                        args = list(shape1 = a2, shape2 = b2)) +
          stat_function(fun = dbeta, geom = "area",
                        alpha = .3, fill = "turquoise4",
                        args = list(shape1 = a2, shape2 = b2))
      } else {
        p1
      }
    } else if (input$dist == "Gamma") {
      a1 <- input$gamma_a1
      b1 <- input$gamma_b1
      p1 <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
        stat_function(fun = dgamma, geom = "line", col = "tomato2",
                      args = list(shape = a1, rate = b1)) +
        stat_function(fun = dgamma, geom = "area",
                      alpha = .3, fill = "tomato2",
                      args = list(shape = a1, rate = b1))
      if (input$add_gamma) {
        a2 <- input$gamma_a2
        b2 <- input$gamma_b2
        p1 +
          stat_function(fun = dgamma, geom = "line", col = "turquoise4",
                        args = list(shape = a2, rate = b2)) +
          stat_function(fun = dgamma, geom = "area",
                        alpha = .3, fill = "turquoise4",
                        args = list(shape = a2, rate = b2))
      } else {
        p1
      }
    } else if (input$dist == "t") {
      df1 <- input$t_df1
      ncp1 <- input$t_ncp1
      if (input$add_t) {
        df2 <- input$t_df2
        ncp2 <- input$t_ncp2
        ggplot(data = data.frame(
          x = c(-(min(df1, df2) * min(ncp1, ncp2) + 4),
                max(df1, df2) * max(ncp1, ncp2)^2 + 4)
        ), aes(x)) +
          stat_function(fun = dt, geom = "line", col = "tomato2", n = 101,
                        args = list(df = df1, ncp = ncp1)) +
          stat_function(fun = dt, geom = "area", n = 101,
                        alpha = .3, fill = "tomato2",
                        args = list(df = df1, ncp = ncp1)) +
          stat_function(fun = dt, geom = "line", col = "turquoise4", n = 101,
                        args = list(df = df2, ncp = ncp2)) +
          stat_function(fun = dt, geom = "area", n = 101,
                        alpha = .3, fill = "turquoise4",
                        args = list(df = df2, ncp = ncp2))
      } else {
        ggplot(data = data.frame(x = c(-(df1 * ncp1 + 4), df1 * ncp1^2 + 4)), aes(x)) +
          stat_function(fun = dt, geom = "line", col = "tomato2", n = 101,
                        args = list(df = df1, ncp = ncp1)) +
          stat_function(fun = dt, geom = "area", n = 101,
                        alpha = .3, fill = "tomato2",
                        args = list(df = df1, ncp = ncp1))
      }
    }
  })

  output$plot1 <- renderPlot({
    plot()
  })
}

shinyApp(ui, server)
