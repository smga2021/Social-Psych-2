library(readr)
library(dplyr)
library(lsr)
library(ggplot2)
install.packages("psych")
library(psych)
library(lsr)

#NBA CODING STUDY 1

# descriptive stats
data_frame <- data.frame(NBA_Study_S24_1_$`Win-Loss 2`, NBA_Study_S24_1_$Pronoun_Use, 
                         NBA_Study_S24_1_$Team_Connection, NBA_Study_S24_1_$Team)

#filter for condition 1, losses
filtered_df_1 <- data_frame %>%
  filter(NBA_Study_S24_1_..Win.Loss.2. == "1")

filtered_df_1 %>% 
  group_by(NBA_Study_S24_1_.Pronoun_Use) %>% 
  summarise( percent = 100 * n() / nrow( filtered_df_1 ) )

filtered_df_1 %>% 
  group_by(NBA_Study_S24_1_.Team_Connection) %>% 
  summarise( percent = 100 * n() / nrow( filtered_df_1 ) )

#filter for condition 2, wins
filtered_df_2 <- data_frame %>%
  filter(NBA_Study_S24_1_..Win.Loss.2. == "2")

filtered_df_2 %>% 
  group_by(NBA_Study_S24_1_.Pronoun_Use) %>% 
  summarise( percent = 100 * n() / nrow( filtered_df_1 ) )

filtered_df_2 %>% 
  group_by(NBA_Study_S24_1_.Team_Connection) %>% 
  summarise( percent = 100 * n() / nrow( filtered_df_1 ) )


#chi-square test of independence, pronoun use

# contingency table 

# Subset the data frame to include only Pronoun_Use less than 3
subset_data <- NBA_Study_S24[NBA_Study_S24$Pronoun_Use < 3, ]

# Create a new data frame with the subsetted values
new_data_frame <- subset_data[, c("Win-Loss", "Pronoun_Use")]

# Create contingency table
contingency_table <- table(new_data_frame$`Win-Loss`, new_data_frame$Pronoun_Use)
print(contingency_table)

# Perform chi-square test for independence
result <- chisq.test(contingency_table, correct = FALSE)
print(result)

#calculate Phi Coefficient
phi(contingency_table)


#chi-square test of independence, team connection

# Subset the data frame to include only team connection less than 3
subset_data2 <- NBA_Study_S24[NBA_Study_S24$Team_Connection < 3, ]

# Create a new data frame with the subsetted values
new_data_frame2 <- subset_data2[, c("Win-Loss", "Team_Connection")]

# Create contingency table
contingency_table2 <- table(new_data_frame2$`Win-Loss`, new_data_frame2$Team_Connection)
print(contingency_table2)

# Perform chi-square test for independence
result2 <- chisq.test(contingency_table2, correct = FALSE)
print(result2)

#calculate Phi Coefficient
phi(contingency_table2)

#tables

new_data_frame %>% 
  group_by(Pronoun_Use, `Win-Loss`) %>% 
  summarise( percent = 100 * n() / nrow( new_data_frame ) )

new_data_frame2 %>% 
  group_by(Team_Connection, `Win-Loss`) %>% 
  summarise( percent = 100 * n() / nrow( new_data_frame2 ) )


#QUIZ CODING STUDY 2

#Independent samples t test
QM_different <- t.test(Quiz_Show_Demo_S24$`Quizmaster 1`, Quiz_Show_Demo_S24$`Quizmaster 2`, var.equal = TRUE)
print(QM_different)

mean(Quiz_Show_Demo_S24$`Quizmaster 1`, na.rm = TRUE)
mean(Quiz_Show_Demo_S24$`Quizmaster 2`, na.rm = TRUE)
sd(Quiz_Show_Demo_S24$`Quizmaster 1`, na.rm = TRUE)
sd(Quiz_Show_Demo_S24$`Quizmaster 2`, na.rm = TRUE)

cohensD(Quiz_Show_Demo_S24$`Quizmaster 1`, Quiz_Show_Demo_S24$`Quizmaster 2`)

#filter for role 1 (contestant)
new_QM1 <- Quiz_Show_Demo_S24 %>%
  filter(`Role 2` == "1")

mean(new_QM1$`Quizmaster 2`)

#filter for role 2 (quizmaster)
new_QM2 <- Quiz_Show_Demo_S24 %>%
  filter(`Role 2` == "2")

mean(new_QM2$`Quizmaster 2`)

#Independent samples t test
QM_role <- t.test(new_QM1$`Quizmaster 2`, new_QM2$`Quizmaster 2`, var.equal = TRUE)
print(QM_role)

sd(new_QM1$`Quizmaster 2`, na.rm = TRUE)
sd(new_QM2$`Quizmaster 2`, na.rm = TRUE)

cohensD(new_QM1$`Quizmaster 2`, new_QM2$`Quizmaster 2`)

#Paired samples t test
QM_similar <- t.test(Quiz_Show_Demo_S24$`Quizmaster 2`, Quiz_Show_Demo_S24$Contestant, paired=TRUE, var.equal = TRUE)
print(QM_similar)

mean(Quiz_Show_Demo_S24$Contestant, na.rm = TRUE)
mean(Quiz_Show_Demo_S24$`Quizmaster 2`, na.rm = TRUE)
sd(Quiz_Show_Demo_S24$Contestant, na.rm = TRUE)
sd(Quiz_Show_Demo_S24$`Quizmaster 2`, na.rm = TRUE)

cohensD(Quiz_Show_Demo_S24$`Quizmaster 2`, Quiz_Show_Demo_S24$Contestant)

#Independent samples t test, lab students vs. non-lab students on contestant ratings

#RECODE for lab & non-lab student
Quiz_Show_Demo_S24$`Student Type` <- case_when(
  Quiz_Show_Demo_S24$`Student Type` == "L" ~ 1,
  Quiz_Show_Demo_S24$`Student Type` == "NL" ~ 2
)

# Subset the data to get ratings for lab students and non-lab students
ratings_lab <- Quiz_Show_Demo_S24$Contestant[Quiz_Show_Demo_S24$`Student Type` == 1]
ratings_nonlab <- Quiz_Show_Demo_S24$Contestant[Quiz_Show_Demo_S24$`Student Type` == 2]

# Perform the t-test
lab_nonlab <- t.test(ratings_lab, ratings_nonlab, var.equal = TRUE)
print(lab_nonlab)

sd(ratings_lab, na.rm = TRUE)
sd(ratings_nonlab, na.rm = TRUE)

cohensD(ratings_lab, ratings_nonlab)

#Independent samples t test, lab students vs. non-lab students on quizmaster ratings
ratings_lab2 <- Quiz_Show_Demo_S24$Quizmaster[Quiz_Show_Demo_S24$`Student Type` == 1]
ratings_nonlab2 <- Quiz_Show_Demo_S24$Quizmaster[Quiz_Show_Demo_S24$`Student Type` == 2]

# Perform the t-test
lab_nonlab2 <- t.test(ratings_lab2, ratings_nonlab2, var.equal = TRUE)
print(lab_nonlab2)

sd(ratings_lab2, na.rm = TRUE)
sd(ratings_nonlab2, na.rm = TRUE)

cohensD(ratings_lab2, ratings_nonlab2)

#box plot
Quiz_Show_Demo_S24 <- Quiz_Show_Demo_S24 %>%
  mutate(`Student Type` = case_when(
    `Student Type` == 1 ~ "Lab Student",
    `Student Type` == 2 ~ "Non-lab Student"
  ))

ggplot(Quiz_Show_Demo_S24, aes(x = `Student Type`, y = Quizmaster, fill = `Student Type`)) +
  geom_boxplot() +  
  labs(x = "Student Type",
       y = "Quizmaster Ratings") +
  scale_fill_manual(values = c("blue", "red"))

