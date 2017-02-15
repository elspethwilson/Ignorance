library(ggplot2)

# import kids data (different sets with/out exclusions) and adult data

results_adults <- read.csv("Results_adults2.csv")

results_kids <- read.csv("results_kids_select.csv")


# look at kids' data 
kids_cond <- aggregate(Response ~ Condition, data = results_kids, FUN = mean)
kids_cond
              
       
# combine adult and child data

head(results_kids)
head(results_adults)

results_kids$age <- "child"
results_kids_mono$age <- "child"
results_adults$age <- "adult"
results_adults$SATest <- "NA"
results_adults$Notes <- "NA"

d <- rbind(results_kids, results_adults)
d$age <- as.factor(d$age)
d$Response <- as.numeric(d$Response)


# plot by condition and agegroup

d_plot <- ggplot(data = d, aes(Condition, Response, fill = age))
d_plot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .9)) + 
  scale_fill_brewer(palette = "Greens") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"), axis.text.x = element_text(angle = -45, hjust = 0)) +
  ylab("Proportion correct responses") +
  scale_x_discrete(limits = c("U", "C", "S", "P"), labels=c("unambiguous", "common ground \n ad hoc", "privileged \n semantic", "privileged \nad hoc")) +
  ggsave("Results.png")
  

