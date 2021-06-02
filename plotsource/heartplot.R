heartplot = function(x, data){
  ggplotly(
    data%>%
      filter(source ==x) %>%
      mutate(hours = hour(ymdhm)) %>%
      group_by(hours) %>%
      mutate(AvgValue = mean(value)) %>%
      #ungroup() %>%
      ggplot(aes(x = ymdhm, y = value)) +
      geom_point(alpha = 0.5, size =0.7, color = 'slateblue1') +
      geom_smooth(formula = 'y ~ x', 
                  method  = 'loess', 
                  span = 0.1, 
                  se = F, 
                  size = 0.5)+
      geom_line(aes(x=ymdhm, y=AvgValue, group =as.numeric(hours)),alpha=0.5, size=0.7, color = 'steelblue4')+
      theme_minimal() +
      #theme_dark() +
      ylab("심박동수") +
      xlab("시간")
  )
}
 