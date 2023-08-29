# LINEAR MIXED-EFFECTS REGRESSION

## **LOAD LIBRARIES**

```{r}
library(lmerTest)
library(visreg)
library(effects)
library(ggpubr)
library(emmeans)
library(gtsummary)
library(flextable)
library(report)
```

## **ATTACH DATA**

```{r}
df <- read.csv(file.choose())
attach(df)
View(df)
df$DoseF <- cut(df$Dose, breaks=c(3,4,5,6), 
             labels=c("3 to 4", "4 to 5", "5 to 6"))
df$WtF <- cut(df$Wt, breaks=c(50,60,70,80,90), 
             labels=c("50 to 60", "60 to 70", "70 to 80", "80 to 90"))
```

## **DESCRIPTIVE ANALYSIS**

```{r}
table1 <- tbl_summary(df)
table1
```

## VISUALIZING DATA

```{r}
ggscatter(data=df, x="Time", y="conc", add="reg.line", conf.int=T)
ggscatter(data=df, x="Time", y="conc", color="DoseF", add="reg.line", conf.int=T)
ggscatter(data=df, x="Time", y="conc", color="WtF", add="reg.line", conf.int=T)
```

## **LINEAR MIXED-EFFECTS REGRESSION ASSUMPTIONS**

1.  Linearity of predictors: There exists a linear relationship between the independent variables and the dependent variable.

    ```{r}
    ggscatter(data=df, x="Time", y="conc", add="reg.line", conf.int=T)
    ```

2.  Independence between subjects: All subjects are analysed independent of each other.

3.  Independence of residuals: Should not be any systematic pattern.

    ```{r}
    plot(Time, residuals(fit2))
    ```

4.  Homoscedasticity: The residuals have constant variance at every level of the independent variable; there should be no pattern/trend.

    ```{r}
    plot(fitted(fit2), residuals(fit2))
    ```

5.  Normal distribution of residuals: The residuals of the model are normally distributed.

    ```{r}
    qqnorm(residuals(fit2)); qqline(residuals(fit2))
    hist(residuals(fit2))
    ```

## **UNADJUSTED MODEL**

```{r}
fit1 <- lmer(conc ~ Time + (1|Subject), data=df)
summary(fit1)
report(fit1)
table2 <- tbl_regression(fit1)
```

## ADJUSTED MODEL

```{r}
fit2 <- lmer(conc ~ Time + WtF + DoseF + (1|Subject), data=df)
summary(fit2)
report(fit2)
fit3 <- lmer(conc ~ Time + WtF + DoseF + Time*WtF + Time*DoseF + (1|Subject), data=df)
table3 <- tbl_regression(fit2)
table4 <- tbl_merge(tbls=list(table2,table3),tab_spanner=c("Unadjusted","Adjusted"))
table4
```

## VISUALIZING MODELS

```{r}
visreg(fit2, "Time")
visreg(fit3, "Time", by="DoseF", overlay=T)
visreg(fit3, "Time", by="WtF", overlay=T)
```

## ESTIMATED MARGINAL MEANS/LEAST SQUARES MEANS

```{r}
emmeans(fit2, pairwise ~ WtF)
emmeans(fit2, pairwise ~ DoseF)
```
