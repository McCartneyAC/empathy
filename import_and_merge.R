## Empathy

# Empathy -----------------------------------------------------------------


# for Rose. Start date 8/2/2022

# updated: 8/5/2022
# updated 1/31/2023

#initial business:
library(dplyr)
library(readr)
library(readxl)
library(tidytext)
library(sjPlot)
library(gt)
library(ggplot2)
library(GGally)
library(SnowballC)
## ROSE  IF ANY OF THESE THROW AN ERROR RUN THIS CODE: 
install.packages("SnowballC")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("readxl")
# install.packages("tidytext")
# install.packages("sjPlot")
# install.packages("gt")
# install.packages("ggplot2")
#install.packages("GGally")

`%notin%` <- function(x, y) {
  !(x %in% y)
}

# import and tidy ---------------------------------------------------------

setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\empathy\\")

# import empathy word loadings
empathy <- read_csv("https://raw.githubusercontent.com/McCartneyAC/empathy/main/empathy_lexicon.csv")
empathy

# import distress
distress <-read_csv("https://raw.githubusercontent.com/McCartneyAC/empathy/main/distress_lexicon.csv")
distress
# import affect
affect <- read_csv("https://raw.githubusercontent.com/McCartneyAC/empathy/main/Affect_and_intensity.csv")
affect %>% 
  filter(term == "harry potter")
#import sentiments (afinn library)
afinn<-get_sentiments("afinn")

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
         intensity = INTENSITY_AVG) %>% 
  full_join(afinn) %>% 
  rename(sentiment = value)-> 
  dictionary

dictionary


# import the text data as one unit
df <- readxl::read_xlsx("Dummy_data.xlsx")
# df <- df %>% 
#   dplyr::select(-`...4`)
df



# tokenize df

df_unnested <- df  %>% 
  unnest_tokens(word, Text) %>% 
  # remove stops at the start. 
  anti_join(stop_words) %>% 
  filter(word %notin% c("Katie", "katie", "gotta", "cuz", "kinda"))


df_unnested %>% 
  inner_join(dictionary)  %>% 
  group_by(`Participant ID`) %>% 
  summarize(mean_affect = mean(affect, na.rm = T), 
            mean_intensity = mean(intensity, na.rm = T),
            mean_empathy = mean(empathy, na.rm = T),
            mean_distress = mean(distress, na.rm = T),
            mean_sentiment = mean(sentiment, na.rm = T),
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

analytic_data %>%
  select(-`Participant ID`) %>% 
  ggpairs()

# regression

analytic_data %>% 
  lm(score ~ mean_affect + mean_intensity + mean_sentiment + mean_empathy+mean_distress, data = .) %>% 
  sjPlot::tab_model()
analytic_data %>% 
  lm(score ~ mean_sentiment, data = .) %>% 
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





# actual top 20s ----------------------------------------------------------

# distress

 df_unnested %>% 
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
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
  anti_join(stop_words) %>%
  inner_join(dictionary)  %>% 
  arrange((intensity)) %>% 
  distinct(word,intensity) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten least `intensity` words in data")
  )

# Sentiment

df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange(desc(sentiment)) %>% 
  distinct(word,sentiment) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten most `sentiment` words in data")
  )
df_unnested %>% 
  inner_join(dictionary)  %>% 
  arrange((sentiment)) %>% 
  distinct(word,sentiment) %>%
  head(20) %>%
  dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
  gt() %>% 
  tab_header(
    title = paste0("Top Ten least `sentiment` words in data")
  )


# most common words in things scored ... x --------------------------------


df_unnested %>% 
  anti_join(stop_words) %>% #now unnecessary
  group_by(`Primary coder score`, word) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(`Primary coder score`) %>% 
  arrange(desc(`Primary coder score`)) %>% 
  arrange(desc(n)) %>% 
  ungroup()->
  word_counts
word_counts %>% 
  filter(`Primary coder score` == 3) %>% 
  filter(!is.na(word)) %>% 
  head(10)%>% 
  select(word)  %>% 
  rename(top_3 = word)-> top_ten_3
word_counts %>% 
  filter(`Primary coder score` == 2) %>% 
  filter(!is.na(word)) %>% 
  head(10) %>% 
  select(word)%>% 
  rename(top_2 = word) -> top_ten_2
word_counts %>% 
  filter(`Primary coder score` == 1) %>% 
  filter(!is.na(word)) %>% 
  head(10) %>% 
  select(word) %>% 
  rename(top_1 = word) -> top_ten_1
top_ten_1
toptens <- tibble(top_ten_3, top_ten_2, top_ten_1)
toptens %>% 
  gt() %>% 
  tab_header(
    title = paste0("Most common words by coder rating")
  )


# stems for empathy  ------------------------------------------------------
install.packages("SnowballC"); library(SnowballC)

stem_df_unnested <-df_unnested%>% 
  filter(!is.na(word)) %>% 
  mutate(word = wordStem(word)) %>% 
  inner_join(dictionary) 

stem_dictionary<-dictionary %>% 
  mutate(word = wordStem(word)) %>% 
  distinct(word, .keep_all = TRUE)

stem_df_unnested %>% 
  inner_join(stem_dictionary)  %>% 
  group_by(`Participant ID`) %>% 
  summarize(mean_affect = mean(affect, na.rm = T), 
            mean_intensity = mean(intensity, na.rm = T),
            mean_empathy = mean(empathy, na.rm = T),
            mean_distress = mean(distress, na.rm = T),
            mean_sentiment = mean(sentiment, na.rm = T),
            score = mean(`Primary coder score`, na.rm = T))->
  stem_analytic_data



stem_analytic_data


stem_analytic_data %>% 
  lm(score ~ mean_empathy, data = .) %>% 
  sjPlot::tab_model()
