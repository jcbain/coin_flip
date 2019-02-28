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
    container(imageOutput("image2")),
    button_primary("flip", "flipity")
  )
)

server <- function(input, output, session) {
  score <- reactiveVal(1)
  
  
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
  
  flip_event <- eventReactive(input$flip, {
    flip <- reactiveVal(flips %>% head(score()))
    score(score() + 1L)
    
    flip()
  })
  
  output$image2<-renderImage({
    if (is.null(flip_event()))
      return(NULL)
    if (flip_event() %>% tail(1) %>% pull() == 0) {
      return(list(
        src = "face.png",
        contentType = "image/png",
        alt = "Face"
      ))
    } else {
      return(list(
        src = "second.jpg",
        filetype = "image/jpeg",
        alt = "This is a chainring"
      ))
    }}, deleteFile = FALSE)
    
  
#   # image2 sends pre-rendered images
#   output$image2 <- renderImage({
#     if (is.null(input$picture))
#       return(NULL)
#     
#     if (input$picture == "face") {
#       return(list(
#         src = "images/face.png",
#         contentType = "image/png",
#         alt = "Face"
#       ))
#     } else if (input$picture == "chainring") {
#       return(list(
#         src = "images/chainring.jpg",
#         filetype = "image/jpeg",
#         alt = "This is a chainring"
#       ))
#     }
#     
#   }, deleteFile = FALSE)
}

shiny::shinyApp(ui, server)
