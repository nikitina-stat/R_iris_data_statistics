library(tidyverse)
library(gtsummary)
library(stargazer) 

df <- iris
summary(df)

df <- mutate(df, sl_type = ifelse(Sepal.Length < 5, "< 5",
                     ifelse(Sepal.Length >=5 & Sepal.Length < 6, ">=5 AND <6",
                     ifelse(Sepal.Length >=6 & Sepal.Length < 7, ">=6 AND <7", 
                            ">= 7"))))

df <- mutate(df, sw_type = ifelse(Sepal.Width < 3, "< 3",
                           ifelse(Sepal.Width >=3 & Sepal.Width < 3.5, ">=3 AND <3.5",
                           ifelse(Sepal.Width >=3.5 & Sepal.Width < 4, ">=3.5 AND <4", 
                                         ">= 4"))))
df$sl_type<-as.factor(df$sl_type)
df$sw_type<-as.factor(df$sw_type)
summary(df)
str(df)

iris_ds <- df %>% 
  select (Sepal.Length,sl_type, Sepal.Width, sw_type, Species) %>% 
  tbl_summary(
    by = Species,
    type = c(all_continuous() ~ "continuous2"),
    statistic = list(all_continuous() ~ c(
      "{N_nonmiss}",
      "{mean}",
      "{min}",
      "{median}",
      "{max}",
      "{sd}")),
    missing = "no",
    digits = list(all_continuous() ~ c(0, 2,1,2,1,2),
                  all_categorical() ~ c(0,1)),
    label = c(Sepal.Length ~ "SEPAL LENGTH, [CM]", 
              sl_type ~ "SEPAL LENGTH",
              Sepal.Width ~ "SEPAL WIDTH, [CM]",
              sw_type ~ "SEPAL WIDTH")) %>% 
    add_overall(last="T", col_label="Total N = {N}") %>% 
    add_stat_label(label = list(all_continuous() ~ c("N", "MEAN", "MIN", "MEDIAN", 
                                                   "MAX", "STANDARD DEVIATION"),
                                all_categorical() ~ "(%)")) %>% 
    modify_header(list(label ~ "All Flowers",
                       stat_1 ~ "Iris Setosa N = {n}",
                       stat_2 ~ "Iris Versicolor N = {n}",
                       stat_3 ~ "Iris Virginica N = {n}")) %>% 
    modify_footnote(everything() ~ NA) %>% 
    as_data_frame() 

stargazer(iris_ds,
          type = "text",
          summary = FALSE,
          out = "iris-flowers.txt",
          rownames=FALSE,
          title="Iris Flower Summary",
          align=TRUE)




