dashboardplot1 = function(option){
  ggplotly(
    cohort_main %>%
      select(id, phase2, colnames(cohort_main[which(colnames(cohort_main[])==option)])) %>%
      ggplot(aes(x= phase2, y =.[,3], color = phase2)) +
      geom_boxplot() +
      geom_jitter() +
      theme_minimal()+
      ylab(cohort_dictionary1$`설문 문항`[which(cohort_dictionary1$변수명[]==option)])
  )
}
dashboardplot2 = function(option){
  ggplotly(
    cohort_main%>%
      select(id, phase2,  colnames(cohort_main[which(colnames(cohort_main[])==option)])) %>%
      ggplot(aes(x=.[,3], color = phase2)) +
      geom_density() +
      theme_minimal()+
      xlab(cohort_dictionary1$`변수설명`[which(cohort_dictionary1$변수명[]==option)])
  )
}