library(agricolae)
library(emmeans)
library(wrapr)

# need to set contrasts to helmert

options(contrasts = c("contr.helmert", "contr.poly"))

xy <- car::Anova(lm(run_speed ~ sex * age * gt, data=catstat_df), type=3)
#type_3
#broom::tidy(type_3)


#type_3 %>% tidy() %>% mutate(p.value = round(p.value, 3))

# custom function to generate a tidy type 3 ANOVA
type_3_aov <- function(x, dataframe) {
  broom::tidy(car::Anova(lm(x ~ sex * age * gt, 
                            data=dataframe), 
                         type=3)) %>% 
    mutate(p.value = round(p.value, 3))
}

run_aov <- type_3_aov(catstat_df$run_speed, catstat_df)

# custom function to generate a tukey post-hoc
post_hoc_lm <- function(x, dataframe){
  summary(lsmeans::lsmeans(lm(x ~ sex * age * gt, data = dataframe), 
                           pairwise ~ sex:age:gt, 
                           adjust = "tukey"))$contrasts %>% 
    mutate(p.value = round(p.value, 3)) %>% 
    filter(p.value < 0.051)
}


post_hoc_lm(catstat_df$hind_swingspd, catstat_df)


var_names <- qc(names(catstat_df))

var_names
mod1 <- lm(run_speed ~ sex * age * gt, 
           data=catstat_df)


x_post <- HSD.test(mod1, "sex", group = T)
x_post
TukeyHSD(xy)

structure(xy)
head(catstat_df)

library(agricolae)


tukey_test <- lsmeans::lsmeans(mod1,
                               pairwise ~ sex:age:gt,
                               adjust = "tukey")
lm_mod <- summary(lsmeans::lsmeans(lm(run_speed ~ sex * age * gt, data = catstat_df), 
                           pairwise ~ sex:age:gt, 
                           adjust = "tukey"))$contrasts %>% 
          mutate(p.value = round(p.value, 3)) %>% 
  filter(p.value < 0.11)


lm_mod

summary(lm_mod)

summary(tukey_test$contrasts) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  filter(p.value < 0.11)
  

tukey_test_tidy <- as.data.frame(tukey_test$contrasts) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  filter(p.value < 0.06)

install.packages("emmeans")
library(emmeans)
