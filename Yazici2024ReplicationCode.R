################################################################################
### Transborder Ethnic Ties and Repression of Ethnic Minorities
### 2024
### Replication Code
################################################################################

# First, load the TekGovRepData.csv!

################################################################################
#####################Table 4: Summary Statistics ###############################
################################################################################

library(stargazer)
stargazer(TekGovRepData)
################################################################################


################################################################################
################# Packages required for the models #############################
################################################################################

install.packages("glmmML")
library(glmmML)

install.packages("miceadds")
library(miceadds)

install.packages("Matrix")
library(Matrix)

install.packages("lme4")
library(lme4)

################################################################################


################################################################################
####################### Table 5: Pooled Logit ###################################
################################################################################

# without control
Model1CompositeSimple <- glm(REP ~  CincRatio + Credibility, family=binomial(link="logit"), data=TekGovRepData)
summary(Model1CompositeSimple)

# with composite IVs
Model1Composite <- glm(REP ~  CincRatio + Credibility + G_RegimeType + log(g_gdp) + 
                         Job_security + M_goal + M_coherence + M_support + peaceyears_rep + 
                         peaceyears_rep2 + peaceyears_rep3, family=binomial(link="logit"), data=TekGovRepData)
summary(Model1Composite)

# with disaggregated IVs
Model1Pooled <- glm(REP ~  CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                      Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                      EK_EthnicFract + EK_RegimeType  + GEKalliance + G_RegimeType +
                      log(g_gdp) + 
                      Job_security + M_goal + M_coherence + M_support + peaceyears_rep + 
                      peaceyears_rep2 + peaceyears_rep3, family=binomial(link="logit"), data=TekGovRepData)
summary(Model1Pooled)

#Clustered SE
library(lmtest)
library(sandwich)
coeftest(Model1Pooled, vcov. = vcovCL(Model1Pooled, cluster = TekGovRepData$dyadid, type = "HC0"))

################################################################################

################################################################################
################ Table 6: Logit with Cluster Robust Standard Errors #############
################################################################################

# without control
Model2ClusteredCompositeSimple <- miceadds::glm.cluster( data=TekGovRepData, formula=REP ~ CincRatio + Credibility,
                                                         cluster="dyadid", family=binomial(link="logit"))
coef(Model2ClusteredCompositeSimple)
vcov(Model2ClusteredCompositeSimple)
summary(Model2ClusteredCompositeSimple)


# with composite IVs
Model2ClusteredComposite <- miceadds::glm.cluster( data=TekGovRepData, formula=REP ~ CincRatio + Credibility + 
                                                     G_RegimeType + log(g_gdp) + Job_security  + M_goal + 
                                                     M_coherence + M_support + peaceyears_rep + peaceyears_rep2 + peaceyears_rep3,
                                                   cluster="dyadid", family=binomial(link="logit"))
coef(Model2ClusteredComposite)
vcov(Model2ClusteredComposite)
summary(Model2ClusteredComposite)

# with disaggregated IVs
Model2Clustered <- miceadds::glm.cluster( data=TekGovRepData, formula=REP ~ CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                                            Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                                            EK_EthnicFract + EK_RegimeType +  GEKalliance + G_RegimeType + log(g_gdp) +
                                            Job_security  + M_goal + M_coherence + M_support + peaceyears_rep + 
                                            peaceyears_rep2 + peaceyears_rep3,
                                          cluster="dyadid", family=binomial(link="logit"))
coef(Model2Clustered)
vcov(Model2Clustered)
summary(Model2Clustered)


################################################################################
############### Table 7: Multilevel Model - Mixed Effects (dyadid) #############
################################################################################

# Variables that do not vary over time are excluded (as well as the duration variables)
# without control
model4MLCompositeSimple  <- glmer(REP ~ CincRatio + Credibility + (1|dyadid), data=TekGovRepData,
                                  family=binomial(link="logit"), control = glmerControl(tolPwrss=1e-3))

summary(model4MLCompositeSimple)

# with composite IVs
model4MLComposite  <- glmer(REP ~ CincRatio + Credibility + G_RegimeType + log(g_gdp) +
                              Job_security  + M_goal + M_coherence + M_support + (1|dyadid), data=TekGovRepData,
                            family=binomial(link="logit"), control = glmerControl(tolPwrss=1e-3))

summary(model4MLComposite)


# with disaggregated IVs
model4ML <- glmer(REP ~ CincRatio + EK_sup_dip + EK_sup_econ + EK_RegimeType +  GEKalliance + G_RegimeType + log(g_gdp) +
                    Job_security  + M_goal + M_coherence + M_support +  
                    (1|dyadid), data=TekGovRepData,family=binomial(link="logit"), 
                  control = glmerControl(tolPwrss=1e-3))

summary(model4ML)

se <- sqrt(diag(vcov(model4ML)))
# table of estimates with 90% CI
(tab <- cbind(Est = fixef(model4ML), LL = fixef(model4ML) - 1.64 * se, UL = fixef(model4ML) + 1.64 *
                se))
options(scipen = 999)
exp(tab)


# Tex Tables
install.packages("stargazer") 
library(stargazer)
install.packages("texreg")
library(texreg)


texreg(list(Model1CompositeSimple, Model1Composite, Model1Pooled), title=" Results (Pooled Logistic Regression", align=TRUE,
       no.space=TRUE)

texreg(list(Model2ClusteredCompositeSimple, Model2ClusteredComposite, Model2Clustered), title=" Results (Pooled Logistic Regression", align=TRUE,
       no.space=TRUE)

texreg(list(model4MLCompositeSimple, model4MLComposite, model4ML), title=" Results (Pooled Logistic Regression", align=TRUE,
       no.space=FALSE)

################################################################################
################# Table 8:Substantive Effect Table#############################
################################################################################

library(clarify)
library(ggplot2)

set.seed(123)
sim_coefs <- sim(Model1Pooled)

#Combined Credibility and Capability Effects

# Average Capability, Average Credibility
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = 4), verbose = FALSE)
summary(sim_est)
round(sim_est, digits=2)
plot(sim_est) 

#High Capability, High Credibility
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(44), EK_sup_econ = c(1), EK_sup_dip = c(1), Diaspora = c(1), 
                                        M_EK_LangRel = c(2), EK_EthnicFract = c(0.059), Dual_citizenship = c(1),
                                        G_EK_Contiguity = c(1), EK_RegimeType = c(3), 
                                        Dispersion2 = c(1), GEKalliance = c(0)), verbose = FALSE)
summary(sim_est)
round(sim_est, digits=2)
plot(sim_est) 

################################################################################


################################################################################
################# ROBUSTNESS TESTS (Table 2 in Online Appendix) ################
################################################################################
library(plm)
install.packages("car")
library(car)
#Random effects linear model (clustered)
linear1 <- plm(REP ~  CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                 Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                 EK_RegimeType + EK_EthnicFract + GEKalliance + G_RegimeType + 
                 Job_security  + M_goal + M_coherence + M_support + peaceyears_rep + 
                 peaceyears_rep2 + peaceyears_rep3, data=TekGovRepData, 
               model = "random", index= c("dyadid", "year"), 
               effect="individual")

summary(linear1)

#Test multicollinearity
vif(linear1)
#Joint significance test
pwaldtest(linear1, test = "F")

#One-way fixed effects linear model
linear2fe <- plm(REP ~  CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                   Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                   EK_RegimeType + EK_EthnicFract + GEKalliance + G_RegimeType + log(g_gdp) +
                   Job_security  + M_goal + M_coherence + M_support + peaceyears_rep + 
                   peaceyears_rep2 + peaceyears_rep3, data=TekGovRepData, 
                 model = "within", index= c("dyadid", "year"), 
                 effect="individual")
summary(linear2fe)

phtest(linear1, linear2fe)



# Tex Tables
install.packages("stargazer") 
library(stargazer)

stargazer(linear1, linear2fe,title=" Results", align=TRUE,
          dep.var.labels=c("Random Effects", "One-Way Fixed Effects", "Two-Way Fixed Effects"), 
          omit.stat=c("LL","ser","f"),no.space=TRUE)

texreg(list(linear1, linear2fe), title=" Results", align=TRUE,
       no.space=TRUE)

