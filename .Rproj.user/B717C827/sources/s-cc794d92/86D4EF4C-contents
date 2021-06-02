foodtable = function(x, data){
  fooddata = data %>%
    filter(source == x) %>%
    select(names, calories, carbs, fat, fiver, protein, sodium)
  names(fooddata) = c('음식', '칼로리', '탄수화물', '지방','섬유질','단백질','나트륨')
  return(fooddata %>% data.frame())
}
