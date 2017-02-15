library(ggplot2)
library(lme4)

# set contrasts 

contrasts(d$Condition) = contr.sum(4)
contrasts(d$age) = contr.sum(2)

model_slope_item_intercepts <- glmer(Response ~ Condition + age + (1 + List | Utterance) + (1|Participant), 
                                                          family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)), data = d)

summary(model_slope_item_intercepts)

regression_output <- capture.output(summary( glmer(Response ~ Condition + age + (1 + List | Utterance) + (1|Participant), 
                                                   family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)), data = d)))
write(regression_output, file = "regresson_output")

#model_max <- glmer(Response ~ Condition + age + (1 + List + age | Utterance) + (1 + Condition | Participant), 
#                    family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)), data = d)
# not able to converge 

