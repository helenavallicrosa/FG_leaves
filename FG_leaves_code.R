#### Growth. Model and Figure 2 ####
#### 1.1 Analysis ####
library(glmmTMB)
Nfix_growth <- read.csv("~/Desktop/Guaiana Papers/Treball/v5/Nfix_growth_db_GitHub.csv")
m2 <- glmmTMB(year_pond ~ Treatment*nfix + (1|Site) + (1|Topography), data=Nfix_growth, family=tweedie(link = "log"))
summary(m2)

#### 1.2 Figure 2####
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(ggeffects)

#estimate. SM table
plot_model(m2, show.values = TRUE, value.offset = .3, type="est")
sum_growth <- get_model_data(m2, type="est")

#predictions
data_predict <- get_model_data(m2, type="pred")
df_fix_effects <- data_predict$Treatment
df_fix_effects$group <- "Summary"
df_fix_effects$x <- c("N fert", "NP fert", "P fert","Control")

df_nfix <- get_model_data(m2, type="int")
df_nfix$group <- ifelse(df_nfix$group == 0, "Non-N-fix", "N-fix")
df_nfix$x <- c("N fert","N fert","NP fert","NP fert","P fert","P fert","Control","Control")

df_plot <- rbind(df_fix_effects, df_nfix)
df_plot$group <- factor(df_plot$group, levels=c("Summary", "Non-N-fix", "N-fix"))

library(ggplot2)
error <- df_plot$predicted*df_plot$std.error/2
pred <- ggplot(data=df_plot, aes(x=x, y=predicted)) +
  geom_bar(aes(fill = df_plot$group),
           stat="identity", position = position_dodge(0.8),
           width = 0.7) +
  geom_errorbar(aes(x=x, group=group, ymin=df_plot$predicted-error, ymax=df_plot$predicted+error, width=.2),
                position = position_dodge(0.8))+
  geom_hline(yintercept=df_plot$predicted[4], linetype='dotted')+
  theme_classic()+
  theme(axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
       plot.margin = margin(0,0,0,1, "cm"))+
  labs( x="Treatment", y="Yearly relative growth (cm/cm/yr)")+
  theme(legend.title = element_blank())+
  scale_fill_manual("legend", values = c("Summary" = "#db415a", "Non-N-fix" = "darkolivegreen4", "N-fix" = "#f8ae6c"))
pred

# estimates
plot_model(m2)

sum_growth <- get_model_data(m2, type="est")
sum_growth$fill <- ifelse(sum_growth$p.value < 0.05, "#db415a", "gray70")
sum_growth$term <- c("N fert", "NP fert", "P fert", "N-fixers","N-fix:N fert", "N-fix:NP fert", "N-fix:P fert")
sum_growth$term <- factor(sum_growth$term, levels=sum_growth$term)

library(ggplot2)
est <- ggplot(data=sum_growth, aes(x=term, y=estimate, label=sum_growth$p.label)) +
  geom_point(stat="identity", size=4, col=sum_growth$fill)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, col=sum_growth$fill)+
  geom_hline(yintercept=1, linetype="dashed")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(limits=c(-1,6),oob = rescale_none)+
  scale_x_discrete(limits = rev(levels(sum_growth$term)))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
       plot.margin = margin(0,1,0,0, "cm"))+
  geom_text(vjust=-1.5)+
  labs(x="", y="Estimates")
est

#Figure 2
library(ggpubr)
tiff(filename="~/Desktop/Guaiana Papers/Treball/v4/Figures/Fig. 2.tif",
     width=1500, height = 900, res= 150)
a <- ggarrange(est, pred, ncol = 2, nrow = 1, heights= c(0.8,0.8), labels= c("A)", "B)"), align="h") +
  theme(plot.margin = margin(2,0.5,0.5,0.5, "cm"))
annotate_figure(a, top = text_grob("Stem growth", vjust=2, hjust=2.8, face="bold", size=20))
dev.off()

#### Leaf N, P, N:P. Figures 3,4,5 ####
#### 2.1 Analysis ####
Leaf <- read.csv("~/Desktop/Guaiana Papers/Treball/v5/Leaf_Nfix_GitHub.csv")

mixed_Nfix <- function(elem,data){
  out <- list("vector", length(elem))
  for(i in 1:length(elem)){
    inverse <- "inverse"
    form <- paste("lmer(log(",elem[i], ") ~ treatment*Nfix + (1|season) + (1|site), data=data, na.action=na.exclude)") # family = Gamma(link = ",inverse,"))")
    m1 <- eval(parse(text=form))
    out[[i]] <- m1
  }
  return(out)
}

Leaf <- subset(Leaf, !is.na(Leaf$N))

models_leaf_lme <- mixed_Nfix(elem = colnames(Leaf)[c(22,25,26)], data = Leaf)

#### 2.2 Figures ####
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(ggeffects)

#estimate. SM table
# sum_leaf <- get_model_data(models_leaf_lme[[1]], type="est")
# write.csv(sum_growth, file="~/Desktop/Guaiana Papers/Treball/v4/SM/Growth.csv")

#predictions
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(ggeffects)

#estimate. SM table
# plot_model(models_leaf_lme[[1]], show.values = TRUE, value.offset = .3, type="pred")
# a <- summary(m2)
# control <- exp(a$coefficients$cond[1,1])
sum_leaf <- get_model_data(models_leaf_lme[[1]], type="est")
# write.csv(sum_growth, file="~/Desktop/Guaiana Papers/Treball/v4/SM/Growth.csv")

#predictions. Figure 1
#N
data_predict <- get_model_data(models_leaf_lme[[1]], type="pred")
df_fix_effects <- data_predict$treatment
df_fix_effects$group <- "Summary"
df_fix_effects$x <- c("Control","N fert", "NP fert", "P fert")

df_nfix <- get_model_data(models_leaf_lme[[1]], type="int")
df_nfix$group <- ifelse(df_nfix$group == 0, "Non-N-fix", "N-fix")
df_nfix$x <- c("Control","Control", "N fert","N fert","NP fert","NP fert","P fert","P fert")

df_plot <- rbind(df_fix_effects, df_nfix)
df_plot$group <- factor(df_plot$group, levels=c("Summary", "Non-N-fix", "N-fix"))
df_plot$elem <- "N"
df_plot_N <- df_plot

#NP
data_predict <- get_model_data(models_leaf_lme[[2]], type="pred")
df_fix_effects <- data_predict$treatment
df_fix_effects$group <- "Summary"
df_fix_effects$x <- c("Control", "N fert", "NP fert", "P fert")

df_nfix <- get_model_data(models_leaf_lme[[2]], type="int")
df_nfix$group <- ifelse(df_nfix$group == 0, "Non-N-fix", "N-fix")
df_nfix$x <- c("Control","Control","N fert","N fert","NP fert","NP fert","P fert","P fert")

df_plot <- rbind(df_fix_effects, df_nfix)
df_plot$group <- factor(df_plot$group, levels=c("Summary", "Non-N-fix", "N-fix"))
df_plot$elem <- "NP"
df_plot_NP <- df_plot

#P
data_predict <- get_model_data(models_leaf_lme[[3]], type="pred")
df_fix_effects <- data_predict$treatment
df_fix_effects$group <- "Summary"
df_fix_effects$x <- c("Control", "N fert", "NP fert", "P fert")

df_nfix <- get_model_data(models_leaf_lme[[3]], type="int")
df_nfix$group <- ifelse(df_nfix$group == 0, "Non-N-fix", "N-fix")
df_nfix$x <- c("Control","Control","N fert","N fert","NP fert","NP fert","P fert","P fert")

df_plot <- rbind(df_fix_effects, df_nfix)
df_plot$group <- factor(df_plot$group, levels=c("Summary", "Non-N-fix", "N-fix"))
df_plot$elem <- "P"
df_plot_P <- df_plot

df_plot <- rbind(df_plot_N, df_plot_NP, df_plot_P)
# write.csv(df_plot, file="~/Desktop/Guaiana Papers/Treball/v4/SM/Leaf_predict.csv")
library(ggplot2)

pred_N <- ggplot(data=df_plot_N, aes(x=x, y=predicted)) +
  geom_bar(aes(fill = df_plot_N$group),
           stat="identity", position = position_dodge(0.8),
           width = 0.7) +
  # geom_text(aes(label = Percentage), vjust = c(0,-1,0,-1,0,-1,0,1.3,0,1.3,0,-1), size = 3,
  # position = position_dodge(0)) +
  geom_errorbar(aes(x=x, group=group, ymin=conf.low, ymax=conf.high, width=.2),
                position = position_dodge(0.8))+
  # geom_text(aes(label = sum_growth$sign, group = group), 
  #           position = position_dodge(width = .9), vjust = c(0,-1.1,0,-1.5,0,-0.7,0,0), hjust= 1.5, size = 20 / .pt) +
  geom_hline(yintercept=df_plot_N$predicted[1], linetype='dotted')+
  theme_classic()+
  # ylim(c(0,0.10))+
  theme(axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        plot.margin = margin(0,0,0,1, "cm"))+
  labs( x="Treatment", y="Foliar N %")+
  theme(legend.title = element_blank())+
  scale_fill_manual("legend", values = c("Summary" = "#db415a", "Non-N-fix" = "darkolivegreen4", "N-fix" = "#f8ae6c"))
pred_N

pred_NP <- ggplot(data=df_plot_NP, aes(x=x, y=predicted)) +
  geom_bar(aes(fill = df_plot_NP$group),
           stat="identity", position = position_dodge(0.8),
           width = 0.7) +
  # geom_text(aes(label = Percentage), vjust = c(0,-1,0,-1,0,-1,0,1.3,0,1.3,0,-1), size = 3,
  # position = position_dodge(0)) +
  geom_errorbar(aes(x=x, group=group, ymin=conf.low, ymax=conf.high, width=.2),
                position = position_dodge(0.8))+
  # geom_text(aes(label = sum_growth$sign, group = group), 
  #           position = position_dodge(width = .9), vjust = c(0,-1.1,0,-1.5,0,-0.7,0,0), hjust= 1.5, size = 20 / .pt) +
  geom_hline(yintercept=df_plot_NP$predicted[1], linetype='dotted')+
  theme_classic()+
  # ylim(c(0,0.10))+
  theme(axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        plot.margin = margin(0,0,0,1, "cm"))+
  labs( x="Treatment", y="Foliar N:P")+
  theme(legend.title = element_blank())+
  scale_fill_manual("legend", values = c("Summary" = "#db415a", "Non-N-fix" = "darkolivegreen4", "N-fix" = "#f8ae6c"))
pred_NP

pred_P <- ggplot(data=df_plot_P, aes(x=x, y=predicted)) +
  geom_bar(aes(fill = df_plot_P$group),
           stat="identity", position = position_dodge(0.8),
           width = 0.7) +
  # geom_text(aes(label = Percentage), vjust = c(0,-1,0,-1,0,-1,0,1.3,0,1.3,0,-1), size = 3,
  # position = position_dodge(0)) +
  geom_errorbar(aes(x=x, group=group, ymin=conf.low, ymax=conf.high, width=.2),
                position = position_dodge(0.8))+
  # geom_text(aes(label = sum_growth$sign, group = group), 
  #           position = position_dodge(width = .9), vjust = c(0,-1.1,0,-1.5,0,-0.7,0,0), hjust= 1.5, size = 20 / .pt) +
  geom_hline(yintercept=df_plot_P$predicted[1], linetype='dotted')+
  theme_classic()+
  # ylim(c(0,0.10))+
  theme(axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        plot.margin = margin(0,0,0,1, "cm"))+
  labs( x="Treatment", y="Foliar P %")+
  theme(legend.title = element_blank())+
  scale_fill_manual("legend", values = c("Summary" = "#db415a", "Non-N-fix" = "darkolivegreen4", "N-fix" = "#f8ae6c"))
pred_P

# tiff(filename="~/Desktop/Guaiana Papers/Treball/v4/Figures/Predicted_Growth.tif",
#      width=700, height = 800, res= 150)
# pred
# dev.off()

# estimates
sum_est_leaf_N <- get_model_data(models_leaf_lme[[1]], type="est")
sum_est_leaf_N$fill <- ifelse(sum_est_leaf_N$p.value < 0.05, "#db415a", "gray70")
sum_est_leaf_N$term <- c("N fert", "NP fert", "P fert", "N-fixers","N-fix:N fert", "N-fix:NP fert", "N-fix:P fert")
sum_est_leaf_N$term <- factor(sum_est_leaf_N$term, levels=sum_est_leaf_N$term)

library(ggplot2)
est_N <- ggplot(data=sum_est_leaf_N, aes(x=term, y=estimate, label=sum_est_leaf_N$p.label)) +
  geom_point(stat="identity", size=4, col=sum_est_leaf_N$fill)+
  # geom_text(aes(label = Percentage), vjust = c(0,-1,0,-1,0,-1,0,1.3,0,1.3,0,-1), size = 3,
  # position = position_dodge(0)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, col=sum_est_leaf_N$fill)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(limits=c(-0.5,0.5),oob = rescale_none)+
  scale_x_discrete(limits = rev(levels(sum_est_leaf_N$term)))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        plot.margin = margin(0,1,0,0, "cm"))+
  geom_text(vjust=-1.5)+
  labs(x="", y="Estimates")
est_N

#NP
sum_est_leaf_NP <- get_model_data(models_leaf_lme[[2]], type="est")
sum_est_leaf_NP$fill <- ifelse(sum_est_leaf_NP$p.value < 0.05, "#db415a", "gray70")
sum_est_leaf_NP$term <- c("N fert", "NP fert", "P fert", "N-fixers","N-fix:N fert", "N-fix:NP fert", "N-fix:P fert")
sum_est_leaf_NP$term <- factor(sum_est_leaf_NP$term, levels=sum_est_leaf_NP$term)

library(ggplot2)
est_NP <- ggplot(data=sum_est_leaf_NP, aes(x=term, y=estimate, label=sum_est_leaf_NP$p.label)) +
  geom_point(stat="identity", size=4, col=sum_est_leaf_NP$fill)+
  # geom_text(aes(label = Percentage), vjust = c(0,-1,0,-1,0,-1,0,1.3,0,1.3,0,-1), size = 3,
  # position = position_dodge(0)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, col=sum_est_leaf_NP$fill)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(limits=c(-0.5,0.5),oob = rescale_none)+
  scale_x_discrete(limits = rev(levels(sum_est_leaf_NP$term)))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        plot.margin = margin(0,1,0,0, "cm"))+
  geom_text(vjust=-1.5)+
  labs(x="", y="Estimates")
est_NP

# P
sum_est_leaf_P <- get_model_data(models_leaf_lme[[3]], type="est")
sum_est_leaf_P$fill <- ifelse(sum_est_leaf_P$p.value < 0.05, "#db415a", "gray70")
sum_est_leaf_P$term <- c("N fert", "NP fert", "P fert", "N-fixers","N-fix:N fert", "N-fix:NP fert", "N-fix:P fert")
sum_est_leaf_P$term <- factor(sum_est_leaf_P$term, levels=sum_est_leaf_P$term)

library(ggplot2)
est_P <- ggplot(data=sum_est_leaf_P, aes(x=term, y=estimate, label=sum_est_leaf_P$p.label)) +
  geom_point(stat="identity", size=4, col=sum_est_leaf_P$fill)+
  # geom_text(aes(label = Percentage), vjust = c(0,-1,0,-1,0,-1,0,1.3,0,1.3,0,-1), size = 3,
  # position = position_dodge(0)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, col=sum_est_leaf_P$fill)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(limits=c(-0.5,0.5),oob = rescale_none)+
  scale_x_discrete(limits = rev(levels(sum_est_leaf_P$term)))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18))+
  geom_text(vjust=-1.5)+
  labs(x="", y="Estimates")
est_P

#all
library(ggpubr)
tiff(filename="~/Desktop/Guaiana Papers/Treball/v4/Figures/Fig. 3.tif",
     width=1500, height = 900, res= 150)
a <- ggarrange(est_N, pred_N, ncol = 2, nrow = 1, heights= c(0.8,0.8), labels= c("A)", "B)"),
               align="h") + theme(plot.margin = margin(2,0.5,0.5,0.5, "cm"))
annotate_figure(a, top = text_grob("Foliar nitrogen", vjust=2, hjust=2.3, face="bold", size=20))
dev.off()

tiff(filename="~/Desktop/Guaiana Papers/Treball/v4/Figures/Fig. 4.tif",
     width=1500, height = 900, res= 150)
a <- ggarrange(est_P, pred_P, ncol = 2, nrow = 1, heights= c(0.8,0.8), labels= c("A)", "B)"),
               align="h") + theme(plot.margin = margin(2,0.5,0.5,0.5, "cm"))
annotate_figure(a, top = text_grob("Foliar phosphorus", vjust=2, hjust=1.8, face="bold", size=20))
dev.off()

tiff(filename="~/Desktop/Guaiana Papers/Treball/v4/Figures/Fig. 5.tif",
     width=1500, height = 900, res= 150)
a <- ggarrange(est_NP, pred_NP, ncol = 2, nrow = 1, heights= c(0.8,0.8), labels= c("A)", "B)"),
               align="h") + theme(plot.margin = margin(2,0.5,0.5,0.5, "cm"))
annotate_figure(a, top = text_grob("Foliar N:P ratio", vjust=2, hjust=2.1, face="bold", size=20))
dev.off()
