shinyServer(function(input, output, session) {
  pallete <- reactive({
    RColorBrewer::brewer.pal(input$num_of_clusters,input$colorPallete)
  })
  
  resetOn <- reactive({
    input$module
    input$presentation
    return (NULL)
  })
  
  data_set <- reactive({
    if (is.na(input$module) || is.na(input$presentation)) {
      return (NULL)
    }
    
    create_dataset(input$module, input$presentation, input$num_of_clusters)
  })
  
  selected_students <- reactiveVal(NULL)
  
  colored_data <- reactive({
    
    if(is.null(data_set())){
      return (NULL)
    }
   
     if (is.null(selected_students())) {
      data_set() %>% 
        mutate(alpha = 1) %>%
        mutate(color = pallete()[cluster]) 

    }
    else{
      indexes <-
        selected_students() + 1 # rbokeh has indexes zero based hence we need to add one
     
     ds <- data_set()
     
     sel_students <- 
       ds %>% 
       filter(row_number() %in% indexes) 
     
     alphas <- sel_students %>% 
       group_by(cluster) %>% 
       summarise(occ = n()) %>% 
       mutate(alpha = occ/sum(occ))
     
     
     sel_classes <-
       sel_students %>%
       distinct(cluster) %>%
       pull()

     ds %>%
      mutate(alpha = if_else(cluster %in% sel_classes, 1,0.3)) %>%
      mutate(color = if_else(cluster %in% sel_classes, pallete()[cluster],'#dddddd'))
      
    }
  })

  output$dataset_columns1 <- renderUI({
    tags$div(
      orderInput(
        "sort11",
        "Available columns",
        colnames(data_set()),
        connect = "sort12",
        item_class = 'danger',
        width = "100%"
      )
    )
  })
  
  output$dataset_selected_columns1 <- renderUI({
    tags$div(
      orderInput(
        "sort12",
        "Displayed columns",
        resetOn(),
        connect = "sort11",
        placeholder = "Drag item here",
        item_class = 'danger',
        width = "100%"
      )
    )
  })
  
  output$dataset_columns2 <- renderUI({
    tags$div(
      orderInput(
        "sort21",
        "Available columns",
        colnames(data_set()),
        connect = "sort22",
        item_class = 'danger',
        width = "100%"
      )
    )
  })
  
  output$dataset_selected_columns2 <- renderUI({
    tags$div(
      orderInput(
        "sort22",
        "Displayed columns",
        resetOn(),
        connect = "sort21",
        placeholder = "Drag item here",
        item_class = 'danger',
        width = "100%"
      )
    )
  })
  
  
  output$parcoorplot <- renderPlotly({
    if (is.null(data_set()) || is.null(input$sort12_order)) {
      return (NULL)
    }
    
    colored_data() %>%
      mutate(final_result = replace(final_result, final_result == 'Pass', 3)) %>%
      mutate(final_result = replace(final_result, final_result == 'Distinction', 4)) %>%
      mutate(final_result = replace(final_result, final_result == 'Fail', 2)) %>%
      mutate(final_result = replace(final_result, final_result == 'Withdrawn', 1)) %>%
      plot_ly(
        type = 'parcoords',
        line = list(color = ~cluster, colorScale='Spectral'),
        dimensions = map(input$sort12_order, function(colName) {
          if (colName == 'final_result') {
            list(
              tickvals = c(1, 2, 3, 4),
              ticktext = c('F', 'W', 'P', 'D'),
              label = 'Result',
              values = ~ final_result,
              visible = TRUE
            )
          }
          else{
            list(
              range = c(round(min(
                data_set()[colName], na.rm = TRUE
              ), 2), round(max(
                data_set()[colName], na.rm = TRUE
              ), 2)),
              label = colName,
              values = pull(data_set(), colName)
            )
          }
        })
      )
  })

  output$splom_matrix <- renderUI({
    if (is.null(data_set()) || is.null(input$sort22_order)) {
      return(NULL)
    }
    
    tools <- c("pan", "wheel_zoom", "box_zoom", "reset", "box_select" ,"save", "selection", "tap")
    
    cols<- input$sort22_order
    nms <-  expand.grid(cols,cols, stringsAsFactors = FALSE)
    splom_list <- vector("list", length(cols)*length(cols))
  
    d<- colored_data()
    
    j_d<-
      d %>% 
      mutate_at(vars(cols) ,jitter)
  
    map(seq_len(nrow(nms)), 
      function(ii){
      figure(
        tools = tools, 
        data = d,
        xlab = nms$Var1[ii], 
        ylab = nms$Var2[ii]
      ) %>%
        ly_points(
          nms$Var1[ii], 
          nms$Var2[ii],
          legend = FALSE,
          color = color, 
          alpha = alpha, 
          size = 10, 
          lname = "points"
          #,hover = list(cluster)
        ) %>% 
        tool_tap(shiny_callback("data_point"), "points")
      }
    ) %>%
      grid_plot(ncol = length(cols), link_data = TRUE, width = 1000) %>% 
      list(actionButton("resetSplom", "Reset tap"), .)
    
  })
  
  
  output$ranges <-renderUI({
    if (is.null(data_set()) || is.null(input$sort12_order)) {
      return(NULL)
    }
    
    
    temp_dataset <- data_set()
    for(i in input$sort12_order){
      limits <- input[[paste0("slider", i)]]
      
      if(!is.null(limits)){
        temp_dataset <- 
          temp_dataset %>% 
          filter(get(i) >= limits[1], get(i) <= limits[2])
      }
    }
    
    total_count <- 
      temp_dataset %>% 
      nrow
  
    
    
  list(
    h3(paste0("Vybraný počet záznamů: ", total_count)),
    fluidRow(
      lapply(input$sort12_order, function(i) {
      min_value <- 
        data_set() %>% 
        select(i) %>% 
        min 
      max_value <- 
        data_set() %>% 
        select(i) %>% 
        max 
      
      limits <- input[[paste0("slider", i)]]
      
      if(is.null(limits)){
        limits <- c(min_value, max_value)
      }
      
      count <- 
        data_set() %>% 
        filter(get(i) >= limits[1], get(i) <= limits[2]) %>% 
        nrow
      
      
      
      list(
        column(3,
               sliderInput(paste0("slider", i), label = i, min = min_value, 
                           max = max_value, value = limits)
        ),
        column(1,
               h4(count, class="selected-range")
        )
      )
    })
    )
  )
  })
  
  output$before_clicks <- renderPlotly({
    if (is.na(input$module) || is.na(input$presentation)) {
      return (NULL)
    }
    
    before_ass_clicks(input$module, input$presentation, input$colorPallete, data_set(), input$num_of_clusters)
  })
  
  observeEvent(input$resetSplom, selected_students(NULL))
  observeEvent(input$data_point, selected_students(input$data_point))  
  
})
