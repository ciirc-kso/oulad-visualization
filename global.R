library(shiny)
library(shinydashboard)
library(shinyjqui)
library(magrittr)
library(oulad)
library(plotly)
library(rbokeh)
library(tidyverse)

get_week_click <- function(data) {
  max_week <- floor(max(data$date) / 7)
  
  week_clicks <-
    data %>%
    filter(date >= 0) %>%
    mutate(week = floor(date / 7)) %>%
    group_by(id_student, week) %>%
    summarise(clicks = sum(sum_click))
  
  weeks <-
    merge(
      unique(week_clicks$id_student),
      seq(0, max_week, 1),
      by = NULL,
      stringsAsFactors = FALSE
    )
  colnames(weeks) <- c('id_student', 'week')
  
  week_clicks <- weeks %>%
    left_join(week_clicks, c('id_student' = 'id_student', 'week' = 'week')) %>%
    mutate(clicks = replace(clicks, is.na(clicks), 0)) %>%
    arrange(id_student)
  
  week_clicks
}

spread_week_clicks <- function(data) {
  data  %>%
    mutate(week = str_c("w_", week)) %>%
    spread(week, clicks, fill = 0)
}


filter_modules <- function() {
  course %>%
    select(code_module) %>%
    distinct() %>%
    pull(code_module)
}

filter_presentations <- function() {
  course %>%
    select(code_presentation) %>%
    distinct() %>%
    pull(code_presentation)
}

cl_kmeans <- function(data, num_of_clusters, scaleData = FALSE) {
  set.seed(123)
  clusters <-
    data %>%
    select(-id_student) %>%
    {
      if (scaleData)
        scale(.)
      else
        .
    } %>%
    kmeans(num_of_clusters)
  
  data %>%
    mutate(cluster = clusters$cluster)
}

create_dataset <-
  function(module, presentation, num_of_clusters = 1) {
    fassessments <-
      assessment %>%
      filter(code_presentation == presentation,
             code_module == module,
             assessment_type == "TMA")
    
    fstudents <-
      student %>%
      filter(code_presentation == presentation,
             code_module == module)
    
    fstudent_vle <-
      student_vle %>%
      filter(code_presentation == presentation,
             code_module == module)
    
    fstudent_assessment <-
      student_assessment %>%
      filter(
        id_assessment %in% fassessments$id_assessment,
        id_student %in% fstudents$id_student
      )
    
    
    week_clicks <-
      fstudent_vle %>%
      filter(date >= 0) %>%
      get_week_click() %>%
      spread_week_clicks() %>%
      cl_kmeans(num_of_clusters)
    
    fstudents %>%
      right_join(fstudent_assessment, by = "id_student") %>%
      select(id_student, id_assessment, score, final_result) %>%
      mutate(id_assessment = str_c("as_", id_assessment)) %>%
      spread(id_assessment, score, fill = 0) %>%
      left_join(week_clicks, by = "id_student") %>%
      select(id_student, final_result, cluster, everything())
    
  }

before_ass_clicks <- function(module, presentation, pallete, dataset, num_clusters) {
  clusters <-
    dataset %>%
    select(id_student, cluster)
  
  
  fassessments <-
    assessment %>%
    filter(code_presentation == presentation,
           code_module == module,
           assessment_type == "TMA")
  
  fstudents <-
    student %>%
    filter(code_presentation == presentation,
           code_module == module)
  
  fstudent_vle <-
    student_vle %>%
    filter(code_presentation == presentation,
           code_module == module)
  
  fstudent_assessment <-
    student_assessment %>%
    filter(
      id_assessment %in% fassessments$id_assessment,
      id_student %in% fstudents$id_student
    )
  
  clicks <-
    fstudents %>%
    select(id_student)
  last_date <- -100
  res <- fassessments %>%
    mutate(ass_date = date) %>%
    select(ass_date, id_assessment) %>%
    pmap(function(ass_date, id_assessment) {
      name <- paste0(id_assessment)
      clicks <<-
        fstudent_vle %>%
        group_by(id_student) %>%
        filter(date < ass_date, date > last_date) %>%
        summarise(!!name := sum(sum_click)) %>%
        left_join(clicks, ., by = "id_student")
      
      last_date <<- ass_date
    })
  
  clicks <-
    clicks %>%
    gather(key = "id_assessment", value = "clicks", -id_student)
  
  results <-
    fstudents %>%
    right_join(fstudent_assessment, by = "id_student") %>%
    select(id_student, id_assessment, score, final_result)
  
  isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
    abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
  }
  
  
  data <- results %>%
    mutate(id_assessment = as.character(id_assessment)) %>%
    left_join(clicks,
              by = c("id_student" = "id_student", "id_assessment" = "id_assessment")) %>%
    select(noquote(order(colnames(.)))) %>%
    left_join(clusters, by='id_student') %>% 
    #  filter(isnt_out_mad(clicks)) %>%
    group_by(cluster, id_assessment) %>%
    summarise(
      m_c = mean(clicks, na.rm = TRUE),
      se_c = sd(clicks, na.rm = TRUE),
      m_s = mean(score, na.rm = TRUE),
      se_s = sd(score, na.rm = TRUE)
    )
  success_line <-
    data %>%
    group_by(id_assessment) %>%
    summarise(m_c = mean(m_c, na.rm = TRUE))
  
  dodge <- position_dodge(width = 0.5)
  
    ggplot(data, aes(
      x = as.numeric(id_assessment),
      y = m_c,
      colour = factor(cluster)
    )) +
      geom_point(aes(size = m_s), position = dodge, show.legend = FALSE) +
      geom_line(
        data = success_line,
        position = dodge,
        mapping = aes(x = as.numeric(id_assessment), y = m_c),
        color = 'red'
      ) +
      geom_errorbar(aes(
        ymin = m_c - se_c, ymax = m_c + se_c
      ), position = dodge) +
      labs(title = "", x = "Assessment", y = "Average number of clicks") +
      theme_classic() +
      scale_colour_manual(values = c(RColorBrewer::brewer.pal(num_clusters, pallete)))
  
  
  
}
