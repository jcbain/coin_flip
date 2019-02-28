library(shiny)
library(tidyr)
library(dplyr)
library(nessy)
library(ggplot2)

flips <- crossing(trial = 1:1000, flip = 1:100) %>%
  mutate(heads = rbinom(n(), 1, .5)) %>% 
  group_by(trial) %>%
  mutate(next_flip = lead(heads),
         hh = heads & next_flip,
         ht = heads & !next_flip)


ui <- cartridge(
  title = "{nessy}",
  container_with_title(
    title = "Buttons",
    button("Normal", "Flip"),
    button_primary("Primary", "Primary"),
    button_success("Success", "Success"),
    button_warning("Warning", "Warning"),
    button_error("Error", "Error"),
    radio_buttons("sure", "Are you sure?", c("yes", "no"))
  ),
  cartridge(title= "something"),
  container_with_title(
    title = "params",
    text_input("pick_number", "Pick-a-Number", value = "10"), 
    container(plotOutput("plot2"))
    
  ),
  cartridge(title = "flipity"),
  container(
    imageOutput("flip_tracker_1", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_2", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_3", width = "2%", inline = T, height = "20px")
  ),
  container(
    uiOutput("doge_image")
  ),
  container(
    container(imageOutput("flip_image")),
    button_primary("flip", "flipity")
  )
)

server <- function(input, output, session) {
  all_flips <- reactiveValues('flip_1' = 'empty', 'flip_2' = 'empty')
  # current flip count
  flip_count <- reactiveVal(1)
  flip_count2 <- reactiveVal(1)
  
  # Actions that occur when flip button is pushed
  ## takes the first n rows determined by flip_count()
  ## increments flip_count() by 1
  flip_event <- eventReactive(input$flip, {
    flip <- reactiveVal(flips %>% head(flip_count()))
    flip_count(flip_count() + 1L)
    
    flip()
  })
  
  flip_gatherer <- eventReactive(input$flip, {
    flip_index <- paste0("flip_", flip_count2())
    flip <- reactiveVal(flips[flip_count2(),] %>% pull(heads))
    all_flips[[flip_index]] <- flip()
    flip_count2(flip_count2() + 1L)
    
    all_flips
  })
  
  # Generate a series of random flips
  ## used in bar chart section
  tmp_dat <- reactive({
    tmp_flips <- crossing(trial = 1:input$pick_number, 
                          flip = 1:100) %>%
      mutate(heads = rbinom(n(), 1, .5)) %>% 
      group_by(trial) %>%
      mutate(next_flip = lead(heads),
             hh = heads & next_flip,
             ht = heads & !next_flip) %>%
      summarize(first_hh = which(hh)[1] + 1,
                first_ht = which(ht)[1] + 1) %>%
      summarize(first_hh = mean(first_hh), first_ht = mean(first_ht))
    
    tmp_flips
    })
  
  output$plot2<-renderPlot({
    ggplot(tmp_dat()) +
      geom_bar(aes("first hh", first_hh), stat = 'identity') +
      geom_bar(aes("first ht", first_ht), stat = 'identity') +
      theme_bw()
    })
  
  output$flip_tracker_1 <- renderImage({
    if (flip_gatherer()[['flip_1']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 30,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_1"]] == 0) {
      return(list(
        src = "www/dogecoin-png-5.png",
        filetype = "image/png",
        width = 40,
        height = 30,
        alt = "heads"
      ))
    } else {
      return(list(
        src = "www/bitcoins.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_2 <- renderImage({
    if (flip_gatherer()[['flip_2']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 30,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_2"]] == 0) {
      return(list(
        src = "www/dogecoin-png-5.png",
        filetype = "image/png",
        width = 40,
        height = 30,
        alt = "heads"
      ))
    } else {
      return(list(
        src = "www/bitcoins.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_image <- renderImage({
    if (is.null(flip_event()))
      return(NULL)
    if (flip_event() %>% tail(1) %>% pull(heads) == 0) {
      return(list(
        src = "www/dogecoin-png-5.png",
        filetype = "image/png",
        width = 40,
        height = 30,
        alt = "heads"
      ))
    } else {
      return(list(
        src = "www/bitcoins.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  c_url <- reactive({
    paste0("www/doge.png")
  })
  output$doge_image <- renderUI({
    tagList(
      balloon("Flip", style = "margin-left: 40px;"),
      tags$br(),
      tags$img(
        src = c_url(),
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "tails")
    )
  })
  
    
}

shiny::shinyApp(ui, server)