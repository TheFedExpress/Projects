library(tidyverse)
library(car)
library(MASS)
library(GGally)
library(corrplot)
library(grid)
library(gridExtra)
library(broom)

setwd('C:\\Users\\pgood\\OneDrive\\Documents\\Semester 3\\DATA 621\\Project 1')
moneyball <- read_csv('moneyball-training-data.csv') %>%
  dplyr::select(-INDEX)

ggpairs(moneyball)

summary(moneyball)
str(moneyball)

mean(filter(moneyball, hit_differential == 0)$TARGET_WINS)
cor(filter(moneyball, hit_differential > 0)$TARGET_WINS, filter(moneyball, hit_differential > 0)$TEAM_PITCHING_H)
cor(moneyball_cat$TEAM_PITCHING_H, moneyball_cat$TARGET_WINS)
summary(lm(TARGET_WINS ~ TEAM_PITCHING_H + TEAM_BATTING_H, data = moneyball))

only_numeric <- moneyball %>%
  drop_na() %>%
  keep(is.numeric) %>%
  as.matrix() %>%
  cor() %>%
  corrplot(type = 'upper')

objs <- list()
cols = names(moneyball)
for (i in 1:length(cols)){
  objs[[i]] <- ggplot(moneyball) + geom_density(aes(x = cols[i]))
}

do.call(grid.arrange, objs)

standardize <- function(x) (x - mean(x))/sd(x)

moneyball %>% 
  dplyr::select(-c(TEAM_BATTING_HBP, TEAM_BASERUN_CS, hit_differential, hr_differential, bb_differential)) %>%
  drop_na() %>%
  mutate_all(standardize) %>%
  gather(key = 'metric') %>%
  ggplot() + geom_density(aes(x = value)) + facet_wrap(~metric)

moneyball %>% 
  dplyr::select(-c(TEAM_BATTING_HBP, TEAM_BASERUN_CS, hit_differential, hr_differential, bb_differential)) %>%
  drop_na() %>%
  mutate_all(standardize) %>%
  gather(key = 'metric', value = 'value', - TARGET_WINS) %>%
  ggplot() + geom_point(aes(x = value, y = TARGET_WINS), alpha = .2, size = .1) + facet_wrap(~metric)


moneyball$hit_differential <- moneyball$TEAM_BATTING_H - moneyball$TEAM_PITCHING_H
moneyball$hr_differential <- moneyball$TEAM_BATTING_HR - moneyball$TEAM_PITCHING_HR
moneyball$bb_differential <- moneyball$TEAM_BATTING_BB - moneyball$TEAM_PITCHING_BB

max_hits <- max(moneyball$TEAM_BATTING_H)
median_p_hits <-  quantile(filter(moneyball, hit_differential > 0)$TEAM_PITCHING_H, .50)
median_hits <-  quantile(moneyball$TEAM_BATTING_H, .50)

moneyball_imp <- filter(moneyball, hit_differential != 0 & hr_differential != 0 & TEAM_PITCHING_H <= max_hits)
summary(h_mod <- lm(TEAM_PITCHING_H ~ TEAM_BATTING_H + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO, data = moneyball_imp))
summary(hr_mod <- lm(TEAM_PITCHING_HR ~ TEAM_BATTING_H + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO, data = moneyball_imp))
bb_mod <- lm(TEAM_PITCHING_BB ~ TEAM_BATTING_H + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO, data = moneyball_imp)
so_mod <- lm(TEAM_PITCHING_SO ~ TEAM_BATTING_H + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO, data = moneyball_imp)

moneyball_pred <- filter(moneyball, hr_differential == 0 & hit_differential == 0) %>%
  dplyr::select(TEAM_BATTING_H, TEAM_BATTING_HR, TEAM_BATTING_BB, TEAM_BATTING_SO)

hits_pred <- predict.lm(h_mod, moneyball_pred)
hr_pred <- predict.lm(hr_mod, moneyball_pred)
bb_pred <- predict.lm(bb_mod, moneyball_pred)
so_pred <- predict.lm(so_mod, moneyball_pred)

bottom_half <- moneyball %>%
  filter(hr_differential == 0 & hit_differential == 0)

top_half <- moneyball %>%
  filter(hr_differential != 0 | hit_differential != 0)

bottom_half$TEAM_PITCHING_H<- hits_pred
bottom_half$TEAM_PITCHING_HR<- hr_pred
bottom_half$TEAM_PITCHING_BB <- bb_pred
bottom_half$TEAM_PITCHING_SO <- so_pred

moneyball_nomissing <- rbind(top_half, bottom_half)

max_hits <- max(moneyball$TEAM_BATTING_H)
median_p_hits <-  quantile(filter(moneyball, hit_differential > 0)$TEAM_PITCHING_H, .50)
median_hits <-  quantile(moneyball$TEAM_BATTING_H, .50)
median_hr <-  quantile(moneyball$TEAM_BATTING_HR, .50)
median_so <-  quantile(moneyball$TEAM_BATTING_SO, .50, na.rm = TRUE)
median_bb <-  quantile(moneyball$TEAM_BATTING_BB, .50)

new_df <- moneyball_nomissing %>%
  mutate(include_pitching = ifelse(hit_differential == 0 & hr_differential == 0, 0, 1),
         TEAM_PITCHING_H = ifelse((hit_differential == 0 & hr_differential == 0),
                                  median_hits, TEAM_PITCHING_H),
         TEAM_PITCHING_HR = ifelse((hit_differential == 0 & hr_differential == 0),
                                   median_hr, TEAM_PITCHING_HR),
         TEAM_PITCHING_SO = ifelse((hit_differential == 0 & hr_differential == 0),
                                   median_so, TEAM_PITCHING_SO),
         TEAM_PITCHING_BB = ifelse((hit_differential == 0 & hr_differential == 0),
                                   median_bb, TEAM_PITCHING_BB),
         singles = TEAM_BATTING_H - TEAM_BATTING_HR - TEAM_BATTING_2B - TEAM_BATTING_3B,
         slugging = (singles + TEAM_BATTING_2B*2 + TEAM_BATTING_3B*3 + TEAM_BATTING_HR*4)/(TEAM_BATTING_H + 27*162),
         onbase = (TEAM_BATTING_H + TEAM_BATTING_BB)/(TEAM_BATTING_H + 27*162 + TEAM_BATTING_BB)
         
         ) %>%
  dplyr::select(-c(TEAM_BATTING_HBP, TEAM_BASERUN_CS, hit_differential,
                  hr_differential, bb_differential, TEAM_BATTING_H)) %>%
  drop_na()



simple_lm <- lm(TARGET_WINS ~ ., data  = new_df)
summary(pwr <- powerTransform(cbind(singles, TEAM_BATTING_2B, TEAM_BATTING_3B, TEAM_BATTING_HR, TEAM_BASERUN_SB,
                                    TEAM_BATTING_BB, TEAM_BATTING_SO, TEAM_FIELDING_E, TEAM_FIELDING_DP,
                                    TEAM_PITCHING_H, TEAM_PITCHING_HR, TEAM_PITCHING_SO, TEAM_PITCHING_BB,
                                    slugging, onbase
                                    ) ~ 1, 
                              data = new_df))
params <- pwr$roundlam

bc_df <- bcPower(as.matrix(dplyr::select(new_df, - TARGET_WINS)), params) %>%
  as.data.frame()

bc_df <-  cbind(dplyr::select(new_df, TARGET_WINS), bc_df)
colnames(bc_df) <- colnames(new_df)
new_frame <- as.data.frame(new_frame)

summary(model1 <- lm(TARGET_WINS ~ .*. , data = new_df))

invResPlot(model2, c( 0,  .5,  1))
summary(model2 <- (lm(TARGET_WINS ~ .*., data = bc_df)))
anova(model1, model2)

selection <- stepAIC(model1, direction = 'backward')
model3 <- lm(selection$call, data = new_df)
summary(model3)
plot(model3)

names
avPlot(model1, 'TEAM_FIELDING_E')

model2 <- lm(TARGET_WINS ~ slugging, data = new_df)
summary(model2)

plot(new_df$TARGET_WINS ~ new_df$slugging)
abline(model2$coefficients[1], model2$coefficients[2])
plot(model2)


summary(lm(TARGET_WINS ~ . , data = new_df, subset = (include_pitching == 1)))

transform <- function (x) hit_model$coefficients[1] + hit_model$coefficients[2]*x
imputed_hits <- transform(subset(moneyball_cat, include_pitching == 0)$TEAM_BATTING_H)
moneyball1 <- subset(moneyball_cat, include_pitching == 1)
moneyball2 <- subset(moneyball_cat, include_pitching == 0)
moneyball2$TEAM_PITCHING_H <- imputed_hits
moneyball_imputed <- rbind(moneyball1, moneyball2)

moneyball_gg <- new_frame %>%
  dplyr::select(-c(TEAM_PITCHING_BB, TEAM_PITCHING_SO, TEAM_BATTING_HBP, TEAM_BASERUN_CS, hit_differential,
                               hr_differential, bb_differential, 
                                INDEX, TEAM_PITCHING_HR, TEAM_PITCHING_H)) %>%
  gather(key = 'metric')

ggplot(moneyball_gg) + geom_density(aes(x = value)) + facet_wrap(~metric)



d <- lm(TARGET_WINS ~ TEAM_BATTING_H, moneyball_imputed)$residuals
m <- lm(TEAM_PITCHING_H ~ TEAM_BATTING_H, moneyball_imputed)$residuals
mod <- lm(d ~ m)
plot(d ~ m)
abline(0, mod$coefficients[2])
summary(mod)


filter(moneyball, TEAM_BATTING_HR < 50 & TARGET_WINS > 90)
moneyball <- mutate(moneyball, hr_ratio = TEAM_BATTING_2B/ TEAM_BATTING_HR)

test <- read_csv('moneyball-evaluation-data.csv') %>%
  dplyr::select(-INDEX)

test$hit_differential <- test$TEAM_BATTING_H - test$TEAM_PITCHING_H
test$hr_differential <- test$TEAM_BATTING_HR - test$TEAM_PITCHING_HR
test$bb_differential <- test$TEAM_BATTING_BB - test$TEAM_PITCHING_BB


test_final <- test %>%
  mutate(include_pitching = ifelse(hit_differential == 0 & hr_differential == 0, 0, 1),
         TEAM_PITCHING_H = ifelse((hit_differential == 0 & hr_differential == 0),
                                  median_hits, TEAM_PITCHING_H),
         TEAM_PITCHING_HR = ifelse((hit_differential == 0 & hr_differential == 0),
                                   median_hr, TEAM_PITCHING_HR),
         TEAM_PITCHING_SO = ifelse((hit_differential == 0 & hr_differential == 0),
                                   median_so, TEAM_PITCHING_SO),
         TEAM_PITCHING_BB = ifelse((hit_differential == 0 & hr_differential == 0),
                                   median_bb, TEAM_PITCHING_BB),
         singles = TEAM_BATTING_H - TEAM_BATTING_HR - TEAM_BATTING_2B - TEAM_BATTING_3B,
         slugging = (singles + TEAM_BATTING_2B*2 + TEAM_BATTING_3B*3 + TEAM_BATTING_HR*4)/(TEAM_BATTING_H + 27*162),
         onbase = (TEAM_BATTING_H + TEAM_BATTING_BB)/(TEAM_BATTING_H + 27*162 + TEAM_BATTING_BB)
         
  ) %>%
  dplyr::select(-c(TEAM_BATTING_HBP, TEAM_BASERUN_CS, hit_differential,
                   hr_differential, bb_differential, TEAM_BATTING_H, TARGET_WINS))

tidy(model1)

glance(model1)