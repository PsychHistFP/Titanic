preprocmlR <- function(df){
  # create categorical Age variable
  df$Age_cat <- cut(df$Age, breaks = c(0, 18, 55, 90))
  # remove unused variables and convert to factors
  df <- df %>% mutate_each(funs(factor),
                           c(Survived, Pclass, Sex, Embarked)) %>%
    select(-c(PassengerId, Name, Ticket, Cabin, Age))
  return(df)
}
