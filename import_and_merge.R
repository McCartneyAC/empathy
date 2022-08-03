## Empathy

# Empathy -----------------------------------------------------------------


# for Rose. Start date 8/2/2022

#initial business:
library(dplyr)
library(readr)
library(readxl)
library(tidytext)
library(sjPlot)
library(gt)
library(ggplot2)


## ROSE  IF ANY OF THESE THROW AN ERROR RUN THIS CODE: 

install.packages("dplyr")
install.packages("readr")
install.packages("readxl")
install.packages("tidytext")
install.packages("sjPlot")
install.packages("gt")
install.packages("ggplot2")
# import and tidy ---------------------------------------------------------



# import empathy word loadings
empathy <- read_csv("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\empathy\\empathy_lexicon.csv")
empathy
# import the text data as one unit
df <- readxl::read_xlsx("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\empathy\\Dummy_data.xlsx")
df <- df %>% 
  dplyr::select(-`...4`)
df

# tokenize df

df_unnested <- df  %>% 
  unnest_tokens(word, Text)

df_unnested %>% 
  inner_join(empathy, by = "word" ) %>% 
  group_by(`Participant ID`) %>% 
  summarize(mean_rating = mean(rating, na.rm = T), 
            score = mean(`Primary coder score`, na.rm = T)) -> 
  analytic_data


# Analysis ----------------------------------------------------------------


analytic_data %>% 
  select(where(is.numeric)) %>% 
  psych::describe(fast = T) %>% 
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt::gt() %>%
  gt::tab_options(
    column_labels.font.size = "small",
    table.font.size = "small",
    row_group.font.size = "small",
    data_row.padding = px(3)
  ) %>%
  gt::tab_header(
    title = paste0("Data Description")
  )

# what's the correlation? 
cor(analytic_data$score, analytic_data$mean_rating)

# regression

analytic_data %>% 
  lm(score ~ mean_rating, data = .) %>% 
  sjPlot::tab_model()

# plot: 
analytic_data %>% 
  ggplot(aes(x = mean_rating, y = score)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Human Score predicted by Empathy Rating")+ 
  ggplot2::theme_light()+
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) 

