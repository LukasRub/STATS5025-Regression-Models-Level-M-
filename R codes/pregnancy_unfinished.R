pregnancy<-read.table("../data/pregnancy.dat",header=T)
fit1<-lm(formula = Protein ~ Gestation)
fit1
anova(fit1)
