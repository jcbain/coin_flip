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
  "Flip Flip",
  tags$style(HTML(
    "body {max-width: 900px; margin: auto;}
    @media only screen and (max-width:800px) {
    body {
    font-size: 10px;
    }
    html {
    font-size: 10px;
    }
    h1 {
    font-size: 19px;
    }
    #flip_image{
    height: 20px;
    }
    }
    "
  )),
  tags$head(HTML(
    '
    <title>Flipity Flip!</title>
    <link rel="stylesheet" type="text/css" href="static/style.css">

    ')),
  
  cartridge(
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
    imageOutput("flip_tracker_3", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_4", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_5", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_6", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_7", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_8", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_9", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_10", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_11", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_12", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_13", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_14", width = "2%", inline = T, height = "20px"),
    imageOutput("flip_tracker_15", width = "2%", inline = T, height = "20px")
  ),
  container(
    uiOutput("doge_image")
  ),
  container(
    container(imageOutput("flip_image", height = "175px")),
    button_primary("flip", "flipity")
  ),
  container(plotOutput("flip_collection"))
))

server <- function(input, output) {
  heads <- "www/heads.gif"
  tails <- "www/tails.gif"
  
  all_flips <- reactiveValues('flip_1' = 'empty', 'flip_2' = 'empty', 
                              'flip_3' = 'empty', 'flip_4' = 'empty',
                              'flip_5' = 'empty', 'flip_6' = 'empty',
                              'flip_7' = 'empty', 'flip_8' = 'empty',
                              'flip_9' = 'empty', 'flip_10' = 'empty',
                              'flip_11' = 'empty', 'flip_12' = 'empty',
                              'flip_13' = 'empty', 'flip_14' = 'empty',
                              'flip_15' = 'empty')
  # current flip count
  flip_count <- reactiveVal(1)
  flip_count2 <- reactiveVal(1)
  flip_count3 <- reactiveVal(1)
  trial_index <- reactiveVal(1)
  condition_met <- reactiveVal(FALSE)

  
  trial_data <- reactive({
    trial_flips <- flips %>% filter(trial == trial_index())
    
    trial_flips
  })
  
  
  # Actions that occur when flip button is pushed
  ## takes the first n rows determined by flip_count()
  ## increments flip_count() by 1
  flip_event <- eventReactive(input$flip, {
    flip <- reactiveVal(trial_data() %>% head(flip_count()))
    flip_count(flip_count() + 1L)
    
    #trial_reset()
    flip()
  })
  
  flip_gatherer <- eventReactive(input$flip, {
    flip_index <- paste0("flip_", flip_count2())
    flip <- reactiveVal(trial_data()[flip_count2(),] %>% pull(heads))
    all_flips[[flip_index]] <- flip()
    flip_count2(flip_count2() + 1L)
    #trial_reset()
    
    all_flips
  })
  
  
  observeEvent(input$flip, {
    hh <- trial_data() %>% head(flip_count3()) %>% pull(hh)
    ht <- trial_data() %>% head(flip_count3()) %>% pull(ht)
    if (flip_count3()  > 1){
      if (condition_met()){
        trial_index(trial_index() + 1L)
        flip_count(1L)
        flip_count2(1L)
        flip_count3(1L)
      
        #lapply(all_flips, function(x){x = "empty"})
        #all_flips[["flip_2"]]<-"empty"
        purrr::map(1:15, .f = function(x){
          v <- paste0("flip_", x)
          all_flips[[v]] <- "empty"
          })
        condition_met(FALSE)
        }
      else if((any(hh[1:flip_count3()-1]) == T & any(ht[1:flip_count3()-1]) == T)){
        condition_met(TRUE)
        }}
    flip_count3(flip_count3() + 1L)
  })
  
  
  #flip_tally <- eventReactive(input$flip, {
   # need to check first instances of true hh +1 and true ht + 1 
   # next summarize action look at temp action
   # generate histogram
   # change to next trial
  #})
  
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
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_1"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
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
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_2"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_3 <- renderImage({
    if (flip_gatherer()[['flip_3']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_3"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_4 <- renderImage({
    if (flip_gatherer()[['flip_4']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_4"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_5 <- renderImage({
    if (flip_gatherer()[['flip_5']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_5"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_6 <- renderImage({
    if (flip_gatherer()[['flip_6']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_6"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_7 <- renderImage({
    if (flip_gatherer()[['flip_7']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_7"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_8 <- renderImage({
    if (flip_gatherer()[['flip_8']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_8"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_9 <- renderImage({
    if (flip_gatherer()[['flip_9']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_9"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_10 <- renderImage({
    if (flip_gatherer()[['flip_10']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_10"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_11 <- renderImage({
    if (flip_gatherer()[['flip_11']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_11"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_12 <- renderImage({
    if (flip_gatherer()[['flip_12']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_12"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_13 <- renderImage({
    if (flip_gatherer()[['flip_13']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_13"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_14 <- renderImage({
    if (flip_gatherer()[['flip_14']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_14"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  
  output$flip_tracker_15 <- renderImage({
    if (flip_gatherer()[['flip_15']]=="empty")
      return(list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "none"
      ))
    if (flip_gatherer()[["flip_15"]] == 0) {
      return(list(
        src = tails,
        filetype = "image/gif",
        width = 40,
        height = 40,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
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
        src = tails,
        filetype = "image/png",
        width = 128,
        height = 128,
        alt = "heads"
      ))
    } else {
      return(list(
        src = heads,
        filetype = "image/gif",
        width = 128,
        height = 128,
        alt = "tails"
      ))
    }}, deleteFile = FALSE)
  

  output$doge_image <- renderUI({
    tagList(
      balloon("Flip", side = "left", style = "margin-right: 300px; float:right;"),
      tags$br(),
      tags$img(src = "chi.gif", filetype = "image/gif", height = "120px", 
               style = "margin-left: 100px; width: 120px; float: right;")
    )
  })
  
}

shiny::shinyApp(ui, server)
