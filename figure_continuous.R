theme_set(
  theme_classic() +
    theme(legend.position = "right")
)

# remove outliers
#host_max <- max(dat_na_rm$Delta_H)
#dat_na_rm <- dat_na_rm[dat_na_rm$Delta_H < host_max,]

# calculate the 95% interval for delta values for the neutral scenario
t_test_host <- t.test(dat_na_rm$Delta_H[dat_na_rm$Scenario=='i'], conf.level = 0.999999999999999)
host_min <- t_test_host$conf.int[1]
host_max <- t_test_host$conf.int[2]

t_test_non <- t.test(dat_na_rm$Delta_D[dat_na_rm$Scenario=='i'], conf.level = 0.999999999999999)
non_min <- t_test_non$conf.int[1]
non_max <- t_test_non$conf.int[2]

# For hosts
# plot the proportion of local individuals against delta
p1 <- ggplot(dat_na_rm, aes(x=Delta_H, y=Proportion_of_locals_among_host_individuals)) +
  geom_point(aes(color=Scenario, shape=Scenario))+
  geom_smooth(aes(colour = NA), colour = 'black', method=loess, se=T, fullrange=TRUE)+
  geom_vline(xintercept=host_min, linetype='dashed', color='black', linewidth=0.5)+
  geom_vline(xintercept=host_max, linetype='dashed', color='black', linewidth=0.5)+
  xlab(expression(bar(italic(Delta * z))))+
  ylab('Proportion of native individuals')+
  ggtitle('(a)')+
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_text(x=0.125, y=0.228, label="Stabilizing", aes(angle=90), size=3)+
  geom_text(x=0.14, y=0.228, label="Neutral", aes(angle=90), size=3)+
  geom_text(x=0.155, y=0.228, label="Destabilizing", aes(angle=90), size=3)

p4 <- ggplot(dat_na_rm, aes(x=Delta_D, y=Proportion_of_locals_among_dependent_individuals)) +
  geom_point(aes(color=Scenario, shape=Scenario))+
  geom_smooth(aes(colour = NA), colour = 'black', method=loess, se=T, fullrange=TRUE)+
  geom_vline(xintercept=non_min, linetype='dashed', color='black', linewidth=0.5)+
  geom_vline(xintercept=non_max, linetype='dashed', color='black', linewidth=0.5)+
  xlab(expression(bar(italic(Delta * z))))+
  ylab('Proportion of native individuals')+
  ggtitle('(d)')+
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_text(x=0.224, y=0.2425, label="Stabilizing", aes(angle=90), size=3)+
  geom_text(x=0.242, y=0.2425, label="Neutral", aes(angle=90), size=3)+
  geom_text(x=0.26, y=0.2425, label="Destabilizing", aes(angle=90), size=3)

p2 <- ggplot(dat_na_rm, aes(x=Delta_H, y=Difficulty_of_hybridization_H)) +
  geom_point(aes(color=Scenario, shape=Scenario))+
  geom_smooth(aes(colour = NA), colour = 'black', method=loess, se=T, fullrange=TRUE)+
  geom_vline(xintercept=host_min, linetype='dashed', color='black', linewidth=0.5)+
  geom_vline(xintercept=host_max, linetype='dashed', color='black', linewidth=0.5)+
  xlab(expression(bar(italic(Delta * z))))+
  ylab('Genetic distance between sites')+
  ggtitle('(b)')+
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_text(x=0.125, y=3.8, label="Stabilizing", aes(angle=90), size=3)+
  geom_text(x=0.14, y=3.8, label="Neutral", aes(angle=90), size=3)+
  geom_text(x=0.155, y=3.8, label="Destabilizing", aes(angle=90), size=3)


p5 <- ggplot(dat_na_rm, aes(x=Delta_D, y=Difficulty_of_hybridization_D)) +
  geom_point(aes(color=Scenario, shape=Scenario))+
  geom_smooth(aes(colour = NA), colour = 'black', method=loess, se=T, fullrange=TRUE)+
  geom_vline(xintercept=non_min, linetype='dashed', color='black', linewidth=0.5)+
  geom_vline(xintercept=non_max, linetype='dashed', color='black', linewidth=0.5)+
  xlab(expression(bar(italic(Delta * z))))+
  ylab('Genetic distance between sites')+
  ggtitle('(e)')+
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_text(x=0.224, y=3, label="Stabilizing", aes(angle=90), size=3)+
  geom_text(x=0.242, y=3, label="Neutral", aes(angle=90), size=3)+
  geom_text(x=0.26, y=3, label="Destabilizing", aes(angle=90), size=3)

p3 <- ggplot(dat_na_rm, aes(x=Delta_H, y=Clade_diversity_H)) +
  geom_point(aes(color=Scenario, shape=Scenario))+
  geom_smooth(aes(colour = NA), colour = 'black', method=loess, se=T, fullrange=TRUE)+
  geom_vline(xintercept=host_min, linetype='dashed', color='black', linewidth=0.5)+
  geom_vline(xintercept=host_max, linetype='dashed', color='black', linewidth=0.5)+
  xlab(expression(bar(italic(Delta * z))))+
  ylab('Species richness accumulated')+
  ggtitle('(c)')+
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_text(x=0.125, y=4, label="Stabilizing", aes(angle=90), size=3)+
  geom_text(x=0.14, y=4, label="Neutral", aes(angle=90), size=3)+
  geom_text(x=0.155, y=4, label="Destabilizing", aes(angle=90), size=3)


p6 <- ggplot(dat_na_rm, aes(x=Delta_D, y=Clade_diversity_D)) +
  geom_point(aes(color=Scenario, shape=Scenario))+
  geom_smooth(aes(colour = NA), colour = 'black', method=loess, se=T, fullrange=TRUE)+
  geom_vline(xintercept=non_min, linetype='dashed', color='black', linewidth=0.5)+
  geom_vline(xintercept=non_max, linetype='dashed', color='black', linewidth=0.5)+
  xlab(expression(bar(italic(Delta * z))))+
  ylab('Species richness accumulated')+
  ggtitle('(f)')+
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_text(x=0.224, y=22, label="Stabilizing", aes(angle=90), size=3)+
  geom_text(x=0.242, y=22, label="Neutral", aes(angle=90), size=3)+
  geom_text(x=0.26, y=22, label="Destabilizing", aes(angle=90), size=3)

combined <- ggpubr::ggarrange(
  p1, p2, p3,
  p4, p5, p6,
  ncol = 3, nrow = 2
)

pdf("figure_continuous_0920.pdf", width = 14, height = 9)
combined
dev.off()
