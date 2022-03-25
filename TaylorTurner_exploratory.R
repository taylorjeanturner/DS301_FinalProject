library(dplyr)
library(tidyverse)

data <- readr::read_csv('./DS301_FinalProject/Current_Iowa_Correctional_System_Prison_Population.csv')

# gender vs offense type
offenses <- data%>%
  group_by(`Offense Type`, Sex)%>%
  summarise(Total = n(), .groups = 'drop')
offenses <- offenses %>%
  mutate(Total = ifelse(Sex == "Male",Total,-1*Total))
breaks_values <- pretty(offenses$Total)
ggplot(data = offenses, aes(x=reorder(`Offense Type`, Total), y = Total, fill=Sex))+
  geom_bar(stat = "identity", width = 0.75)+
  coord_flip()+
  scale_y_continuous(breaks = breaks_values, labels = abs(breaks_values))+ 
  scale_fill_brewer(palette = "Set1")+
  geom_hline(yintercept = 0)+
  ggtitle("How Gender Affects Offense Type")+
  xlab("Offense")

# race vs offense type
offenses <- data%>%
  group_by(`Offense Type`, `Race & Ethnicity`)%>%
  summarise(Total = n(), .groups = 'drop')
ggplot(data = offenses, aes(x=reorder(`Offense Type`, Total), y = Total, fill=`Race & Ethnicity`))+
  geom_bar(stat = "identity", width = 0.75)+
  scale_y_continuous(breaks = breaks_values, labels = abs(breaks_values))+
  ggtitle("How Gender Affects Offense Type")+
  xlab("Offense")

ggplot(data, aes(x=Age, y=`Months Served`)) + geom_point()



df <- readr::read_csv('./DS301_FinalProject/heart_2020_cleaned.csv')
str(df)
summary(df)
ggplot(df, aes(x=BMI, y=MentalHealth)) + geom_point()
ggplot(df, aes(x=BMI, y=PhysicalHealth)) + geom_point()
ggplot(df, aes(x=MentalHealth, y=PhysicalHealth)) + geom_point()

