#1
> zip <- "C:/Users/73113/Downloads/hw4_cahmi_data.zip"
> folder <- "C:/Users/73113/Downloads/data"
> if (!dir.exists(folder)) {dir.create(folder)}
> unzip(zip, exdir = folder)
> data_file <- "C:/Users/73113/Downloads/data/cahmi_health_conditions.csv"
> cahc_df <- read.csv(data_file)
> table(cahc_df$age)

0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
1852 2260 2817 2530 2537 2498 2222 2326 2504 2572 2641 2745 2925 3067 3214 
15   16   17 
3596 3963 3943 
> table(cahc_df$fam_pov_lev)

(100, 200] (200, 400)  (50, 100]      <= 50     >= 400 
8003      15526       3098       1831      21754 
> table(cahc_df$sex)

Female   Male 
24479  25733 
> table(cahc_df$birth_wt_oz)

72   73   74   75   76   77   78   79   80   81   82   83   84   85   86 
1463   68   65  105   61   53   51   58  344  128  129  152  146  164  305 
87   88   89   90   91   92   93   94   95   96   97   98   99  100  101 
168  251  186  229  240  215  187  171  195  999  312  462  478  436  541 
102  103  104  105  106  107  108  109  110  111  112  113  114  115  116 
604  805 1018  743  762  751  802  632  533  459 1953  719  990  843  864 
117  118  119  120  121  122  123  124  125  126  127  128  129  130  131 
774 1604  760 1640  814  878 1411  913  646  744  592 1904  671  796  799 
132  133  134  135  136  137  138  139  140  141  142  143  144  145  146 
646  563 1061  482  614  619  630  496  546  407  368  316  607  238  298 
147  148  149  150  151  152  153  154  155 
279  186  163  253  111  245  145  242 1502
#the stat show on fam_pov_lev, the number of people greater than 400 is 
#large, and there are 1904 children birth weight is 128 oz.

#2
> library(tidyr)
cahc_long <- cahc_df %>%
  pivot_longer(
    cols = starts_with("ever_had_"),
    names_to = "ever_had_condition",
    values_to = "ever_had",
    names_pattern = "ever_had_(.*)"
  ) %>%
  pivot_longer(
    cols = starts_with("currently_has_") | starts_with("severity_"),
    names_to = c(".value", "condition"),
    names_pattern = "(.*)_(.*)"
  )

#3
library(dplyr)
library(ggplot2)
> conditions <- cahc_long %>% group_by(cid, sex) %>% summarize
(num_conditions = sum(currently_has == "Yes", na.rm = TRUE)) %>%
 ungroup()
> sex_conditions <- conditions %>% group_by(sex, num_conditions) %>% 
  summarize(num_children = n()) %>% ungroup()
> ggplot(sex_conditions, aes(x = num_conditions, y = num_children, fill = sex))
geom_bar(stat = "identity", position = "dodge") + labs(x = "Conditions", 
y = "Children") + ggtitle("Conditions by Sex") + theme_minimal() +
theme(legend.position = "top")
# the number of males with conditions is greater than female, the number of 
# female with no conditions is larger than male.

#4
> threshold <- 0.05
> total_children <- nrow(cahc_long)
> proportion <- cahc_long %>% mutate(race_category = ifelse(table(race)[race] /
+total_children >= threshold, race, "Other")) %>%
  +     group_by(race_category, condition) %>%
  +     summarize(proportion = n() / total_children) %>%
  +     ungroup()
> pw_table <- proportion %>% pivot_wider( id_cols = condition, names_from = 
+race_category,values_from = proportion,values_fill = 0)
# "White alone" have more proportions for many health conditions compared to the
#other, such as "ADD/ADHD," "Anxiety," "Asthma." The "Other" have he lowest
#proportions for most health conditions


