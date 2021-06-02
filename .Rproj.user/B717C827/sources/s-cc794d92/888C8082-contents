sleepplot = function(x, data){
  ggplotly(
    data%>%
      arrange(as.numeric(ymdhms)) %>%
      filter(source == x) %>%
      mutate(sleepstage=case_when(
        stage  =="wake"  ~ 4, 
        stage  =="rem"  ~ 3, 
        stage  =="light"  ~ 2, 
        stage  =="deep"  ~ 1, 
      )) %>%
      mutate(sleepclass=case_when(
        stage  =="awake"  ~ 4, 
        stage  =="restless"  ~ 3, 
        stage  =="asleep"  ~ 2
      ))%>%
      ggplot(aes(x = ymdhms, y = sleepstage, group=group)) +
      geom_point(alpha = 0.5, size =0.7, color = 'slateblue1') +
      geom_step()+
      scale_y_continuous(breaks = c(1, 2, 3, 4),
                         labels = c('1' = 'deep',
                                    '2' = 'light',
                                    '3' = 'rem',
                                    '4' = 'wake'))+
      geom_rect(aes(xmin = ymdhms, xmax = (ymdhms+as.numeric(length)),
                    ymin = 0, ymax = sleepclass,
                fill = sleepclass),alpha = 0.4) +
      theme_minimal() +
      #theme_dark() +
      ylab("수면 단계") +
      xlab("시간")
  )
}

