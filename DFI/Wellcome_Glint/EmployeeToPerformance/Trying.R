setwd("C:/Users/choyinch/Downloads/Wellcome_Glint")
predictor_vars <- c(variables,variable)
all_vars_in_df <- intersect(all_vars, colnames(tdata))
df_scaled <- tdata %>%
  mutate(across(
    all_of(all_vars_in_df),
    ~ (.-median(x, na.rm=TRUE)) / IQR(., na.rm=TRUE)
    ))%>%
      mutate(
        cluster = factor(Store.Cluster),
        scale = factor(Format_y)
      )

{group1<-data_standardized1 %>% filter(tdata$productivity_sum >10000)
group2<-anti_join(data_standardized1, group1)
group3<-data_standardized1 %>% filter(tdata$SOP...T.Area >1000)
group4<-anti_join(data_standardized1, group3)

urban1<-group %>% filter(cluster =="Urban Hub - Local (Wellcome)"|
                                       cluster =="Urban Hub - High Income (Upscale)")
residential1<-group1 %>% filter(cluster =="Residential - Western & Top Income (Upscale)"|
                                             cluster =="Residential - Local (Wellcome)"|
                                             cluster =="Residential - High Income (Wellcome)")
office1<-group1 %>% filter(cluster =="Office - Local (Wellcome)"|
                                        cluster ==  "Office - High Income (Upscale)")
mass1<-group1 %>% filter(scale =="Mass")
upper1<-group1 %>% filter(scale =="Upscales")
urban2<-group2 %>% filter(cluster =="Urban Hub - Local (Wellcome)"|
                                       cluster =="Urban Hub - High Income (Upscale)")
residential2<-group2 %>% filter(cluster =="Residential - Western & Top Income (Upscale)"|
                                             cluster =="Residential - Local (Wellcome)"|
                                             cluster =="Residential - High Income (Wellcome)")
office2<-group2 %>% filter(cluster =="Office - Local (Wellcome)"|
                                        cluster ==  "Office - High Income (Upscale)")
mass2<-group2 %>% filter(scale =="Mass")
upper2<-group2 %>% filter(scale =="Upscales")}###
{urban3<-group3 %>% filter(cluster =="Urban Hub - Local (Wellcome)"|
                             cluster =="Urban Hub - High Income (Upscale)")
  residential3<-group3 %>% filter(cluster =="Residential - Western & Top Income (Upscale)"|
                                    cluster =="Residential - Local (Wellcome)"|
                                    cluster =="Residential - High Income (Wellcome)")
  office3<-group3 %>% filter(cluster =="Office - Local (Wellcome)"|
                               cluster ==  "Office - High Income (Upscale)")
  mass3<-group3 %>% filter(scale =="Mass")
  upper3<-group3 %>% filter(scale =="Upscales")
  urban4<-group4 %>% filter(cluster =="Urban Hub - Local (Wellcome)"|
                              cluster =="Urban Hub - High Income (Upscale)")
  residential4<-group4 %>% filter(cluster =="Residential - Western & Top Income (Upscale)"|
                                    cluster =="Residential - Local (Wellcome)"|
                                    cluster =="Residential - High Income (Wellcome)")
  office4<-group4 %>% filter(cluster =="Office - Local (Wellcome)"|
                               cluster ==  "Office - High Income (Upscale)")
  mass4<-group4 %>% filter(scale =="Mass")
  upper4<-group4 %>% filter(scale =="Upscales")}
##
formula1 <- as.formula(paste("productivity_sum", "~", paste(predictor_vars, collapse = " + ")))
reg1<-lm(formula1, data = group1)
reg1A<-lm(formula1, data = group2)
reg1M<-lm(formula, data = mass1)
reg1S<-lm(formula, data = upper1)
reg1R<-lm(formula, data = residential1)
reg1U<-lm(formula, data = urban1)
reg1O<-lm(formula, data = office1)

reg1M2<-lm(formula, data = mass2)
reg1S2<-lm(formula, data = upper2)
reg1R2<-lm(formula, data = residential2)
reg1U2<-lm(formula, data = urban2)
reg1O2<-lm(formula, data = office2)
model <- list(reg1,reg1A,reg1M, reg1S, reg1R, reg1U, reg1O)
screenreg(model, custom.model.names = c("OLS 1(G1)", "OLS 2(G2)", "G1(Mass)", "G1(Upperscale)",
                                         "G1(Residential)", "G1(Urban)",
                                         "G1(Office)"),
          file = "./models_summary_ps(try1).txt")
model <- list(reg1,reg1A,reg1M2, reg1S2, reg1R2, reg1U2, reg1O2)
screenreg(model, custom.model.names = c("OLS 1(G1)", "OLS 2(G2)", "G2(Mass)", "G2(Upperscale)",
                                        "G2(Residential)", "G2(Urban)",
                                        "G2(Office)"),
          file = "./models_summary_ps(try2).txt")
formula <- as.formula(paste("SOP...T.Area", "~", paste(predictor_vars, collapse = " + ")))
reg2<-lm(formula, data = group3)
reg2A<-lm(formula, data = group4)
reg2M<-lm(formula, data = mass3)
reg2S<-lm(formula, data = upper3)
reg2R<-lm(formula, data = residential3)
reg2U<-lm(formula, data = urban3)
reg2O<-lm(formula, data = office3)

reg2M2<-lm(formula, data = mass4)
reg2S2<-lm(formula, data = upper4)
reg2R2<-lm(formula, data = residential4)
reg2U2<-lm(formula, data = urban4)
reg2O2<-lm(formula, data = office4)
model <- list(reg2,reg2A,reg2M, reg2S, reg2R, reg2U, reg2O)
screenreg(model, custom.model.names = c("OLS 1(G1)", "OLS 2(G2)", "G1(Mass)", "G1(Upperscale)",
                                        "G1(Residential)", "G1(Urban)",
                                        "G1(Office)"),
          file = "./models_summary_sop(t1).txt")
model <- list(reg2,reg2A,reg2M2, reg2S2, reg2R2, reg2U2, reg2O2)
screenreg(model, custom.model.names = c("OLS 1(G1)", "OLS 2(G2)", "G2(Mass)", "G2(Upperscale)",
                                        "G2(Residential)", "G2(Urban)",
                                        "G2(Office)"),
          file = "./models_summary_sop(t2).txt")
#reg3A<-lm(formula, data = group1)
#formula2 <- update(formula, . ~ . * cluster)
#reg3AA <- lm(formula2, data = group1)
#sink("./try4.txt")
#summary(reg3AA)
#sink()
reg3B<-lm(formula, data = df_scaled)
reg3M2<-lm(formula, data = mass2)
reg3S2<-lm(formula, data = upper2)
reg3U2<-lm(formula, data = urban2)
reg3R2<-lm(formula, data = residential2)
reg3O2<-lm(formula, data = office2)
setwd("C:/Users/choyinch/Downloads/Wellcome_Glint/EmployeeToPerformance")
model3 <- list(reg3B, reg3M2, reg3S2, reg3U2, reg3R2, reg3O2)
screenreg(model3, custom.model.names = c("OLS 1(Overall", "OLS 2(Mass)", "OLS 3(Upperscale)",
                                         "OLS 4(Urban)", "OLS 5(Residential)", 
                                         "OLS 6(Office)"),
          file = "./models_summary_Sop(try5).txt")







dv<-c("productivity_sum","SOP...T.Area")
all_vars <- c(variable, variables, dv)
library(tidyr)
long_df <- tdata %>%
  select(store_name, all_of(all_vars)) %>%
  pivot_longer(
    cols = all_of(all_vars),
    names_to = "variable",
    values_to = "value"
  )
library(ggplot2)
ggplot(long_df, aes(x = value, fill = store_name)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Variables by Store")