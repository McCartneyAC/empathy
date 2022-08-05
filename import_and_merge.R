## Empathy

# Empathy -----------------------------------------------------------------


# for Rose. Start date 8/2/2022

# updated: 8/5/2022

#initial business:
library(dplyr)
library(readr)
library(readxl)
library(tidytext)
library(sjPlot)
library(gt)
library(ggplot2)
install.packages("GGally")
library(GGally)

## ROSE  IF ANY OF THESE THROW AN ERROR RUN THIS CODE: 
# 
# install.packages("dplyr")
# install.packages("readr")
# install.packages("readxl")
# install.packages("tidytext")
# install.packages("sjPlot")
# install.packages("gt")
# install.packages("ggplot2")
# import and tidy ---------------------------------------------------------

setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\empathy\\")

# import empathy word loadings
empathy <- read_csv("empathy_lexicon.csv")
empathy

# import distress
distress <-read_csv("distress_lexicon.csv")
distress
# import affect
affect <- read_csv("Affect_and_intensity.csv")
affect %>% 
  filter(term == "harry potter")

# step 1 split / widen affect and intensity
# step 2 full join on term / word

affect %>% 
  tidyr::pivot_wider(id_cols=c("term"), names_from = category, values_from =  weight)  %>% 
  rename(word = term) %>% 
  full_join(distress) %>% 
  rename(distress = rating) %>% 
  full_join(empathy) %>% 
  rename(empathy = rating, 
         affect = AFFECT_AVG, 
         intensity = INTENSITY_AVG) -> dictionary



# import the text data as one unit
df <- readxl::read_xlsx("Dummy_data.xlsx")
df <- df %>% 
  dplyr::select(-`...4`)
df

# tokenize df

df_unnested <- df  %>% 
  unnest_tokens(word, Text)

df_unnested %>% 
  inner_join(dictionary)  %>% 
  group_by(`Participant ID`) %>% 
  summarize(mean_affect = mean(affect, na.rm = T), 
            mean_intensity = mean(intensity, na.rm = T),
            mean_empathy = mean(empathy, na.rm = T),
            mean_distress = mean(distress, na.rm = T),
            score = mean(`Primary coder score`, na.rm = T))->
  analytic_data


analytic_data
# Analysis ----------------------------------------------------------------


analytic_data %>% 
  select(where(is.numeric)) %>% 
  psych::describe(fast = T) %>% 
  tibble::rownames_to_column() %>%
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


analytic_data %>%
  select(-`Participant ID`) %>% 
  ggpairs()

# regression

analytic_data %>% 
  lm(score ~ mean_affect + mean_intensity + mean_empathy+mean_distress, data = .) %>% 
  sjPlot::tab_model()

# plot: 
# analytic_data %>% 
#   ggplot(aes(x = mean_rating, y = score)) +
#   geom_point() +
#   geom_smooth(method = "lm") + 
#   labs(title = "Human Score predicted by Empathy Rating")+ 
#   ggplot2::theme_light()+
#   ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) 


# Secondary Analysis ------------------------------------------------------


# top 20 broken code (ignore) ---------------------------------------------

 
topten <- function(feeling, top) {
 df <-  df_unnested %>% 
   inner_join(dictionary)  
   if (top == TRUE) {
     df %>% 
      arrange(desc(.$feeling)) %>% 
     distinct(word,.$feeling) %>%
       head(10) %>%
       dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
       gt() %>% 
       tab_header(
         title = paste0("Top Ten most", deparse(substitute(feeling)),  "words in data")
       )
  } else {
    df %>% 
      arrange((.$feeling)) %>% 
      distinct(word,.$feeling) %>%
      head(10) %>%
      dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
      gt() %>% 
      tab_header(
        title = paste0("Top Ten least", deparse(substitute(feeling)),  "words in data")
      )
  }

}
topten(feeling = "distress", TRUE)
df_unnested %>% 
  inner_join(dictionary) %>% 
  arrange(desc(.$distress)) %>% 
  distinct(word,.$distress) %>% 
  head(10) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>% 
  gt() %>%  
  tab_header(
    title = paste0("Top Ten most", names()[2],  "words in data")
  )
distress
df_unnested %>% 
  inner_join(dictionary)



# actual top 20s ----------------------------------------------------------

# distress

 df_unnested %>% 
  inner_join(dictionary)  %>% 
    arrange(desc(distress)) %>% 
    distinct(word,distress) %>%
    head(20) %>%
    dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
    gt() %>% 
    tab_header(
      title = paste0("Top Ten most `distress` words in data")
    )
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange((distress)) %>% 
  distinct(word,distress) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten least `distress` words in data")
  )

# empathy
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange(desc(empathy)) %>% 
  distinct(word,empathy) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten most `empathy` words in data")
  )
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange((empathy)) %>% 
  distinct(word,empathy) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten least `empathy` words in data")
  )
# affect 
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange(desc(affect)) %>% 
  distinct(word,affect) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten most `affect` words in data")
  )
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange((affect)) %>% 
  distinct(word,affect) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten least `affect` words in data")
  )

# intensity
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange(desc(intensity)) %>% 
  distinct(word,intensity) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten most `intensity` words in data")
  )
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange((intensity)) %>% 
  distinct(word,intensity) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten least `intensity` words in data")
  )
