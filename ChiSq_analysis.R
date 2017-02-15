library(gmodels)

# passer function

participant_ms <- aggregate(Response ~ Participant + Condition + age, data = d, FUN = mean)
participant_mono_ms <- aggregate(Response ~ Participant + Condition + age, data = d_mono, FUN = mean)

Passer <- function (x) if(x > .8) "true" else "false"
participant_ms$Pass <- sapply(participant_ms$Response, Passer)
participant_mono_ms$Pass <- sapply(participant_mono_ms$Response, Passer)

# output number of passers and failures for each condition
participant_ms$Pass <- as.logical(participant_ms$Pass)
participant_mono_ms$Pass <- as.logical(participant_mono_ms$Pass)

passer_fail <- aggregate(Pass ~ Condition + age, data = participant_ms, FUN = function(Pass) {sum(Pass)})
passer_fail
write.csv(passer_fail, file ="passer_failer.csv")

# ANALYSIS 

# McNemar's test with S vs P in children 
part_ms_SP_kids <- subset(participant_ms, age=="child" & (Condition == "S" | Condition == "P"))
part_ms_SP_kids$Condition <- factor(part_ms_SP_kids$Condition)

CrossTable(part_ms_SP_kids$Condition, part_ms_SP_kids$Pass, mcnemar = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")


# Fisher's exact test with adults vs children for S and P 

part_ms_P <- subset(participant_ms, Condition == "P")
part_ms_S <- subset(participant_ms, Condition == "S")

CrossTable(part_ms_S$age, part_ms_S$Pass, chisq = TRUE, fisher = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")

CrossTable(part_ms_P$age, part_ms_P$Pass, chisq = TRUE, fisher = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")

chi_sqs <- capture.output(summary(c(McNemar, Fisher_Privileged_adhoc, Fisher_Privileged_semantic)))


# sink("chi_sqs", type = "output")
# CrossTable(part_ms_SP_kids$Condition, part_ms_SP_kids$Pass, mcnemar = TRUE,
#            expected = TRUE, sresid = TRUE, format = "SPSS")
# CrossTable(part_ms_S$age, part_ms_S$Pass, chisq = TRUE, fisher = TRUE,
#            expected = TRUE, sresid = TRUE, format = "SPSS")
# 
# CrossTable(part_ms_P$age, part_ms_P$Pass, chisq = TRUE, fisher = TRUE,
#            expected = TRUE, sresid = TRUE, format = "SPSS")
# sink(type = "output")
# sink()

