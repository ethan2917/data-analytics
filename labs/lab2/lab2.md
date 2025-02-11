# Model Summaries

## Model 1 Summary
```
Call:
lm(formula = log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS + BATH, 
    data = dataset)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.60603 -0.19460 -0.03885  0.18549  0.95310 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          2.280599   0.065604  34.763   <2e-16 ***
log10(PROPERTYSQFT)  1.174297   0.022104  53.126   <2e-16 ***
BEDS                -0.038502   0.002930 -13.142   <2e-16 ***
BATH                 0.035569   0.003957   8.989   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2813 on 3174 degrees of freedom
Multiple R-squared:  0.6051,	Adjusted R-squared:  0.6047 
F-statistic:  1621 on 3 and 3174 DF,  p-value: < 2.2e-16
```

## Model 2 Summary
```
Call:
lm(formula = log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS, data = dataset)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.1519 -0.1984 -0.0442  0.1900  0.9948 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          2.098515   0.063178  33.216   <2e-16 ***
log10(PROPERTYSQFT)  1.240642   0.021096  58.810   <2e-16 ***
BEDS                -0.021743   0.002288  -9.502   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2848 on 3175 degrees of freedom
Multiple R-squared:  0.595,	Adjusted R-squared:  0.5947 
F-statistic:  2332 on 2 and 3175 DF,  p-value: < 2.2e-16
```

## Model 3 Summary
```
Call: lm(formula = log10(PRICE) \~ BATH + BEDS, data = dataset)

Residuals: Min 1Q Median 3Q Max -3.9340 -0.2206 -0.0578 0.1751 1.8256

Coefficients: Estimate Std. Error t value Pr(\>\|t\|)\
(Intercept) 5.739369 0.011088 517.599 \<2e-16 ***BATH 0.105752 0.005125
20.633 \<2e-16*** BEDS -0.001031 0.003908 -0.264 0.792\
--- Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3865 on 3175 degrees of freedom Multiple
R-squared: 0.2539, Adjusted R-squared: 0.2534 F-statistic: 540.1 on 2
and 3175 DF, p-value: \< 2.2e-16
```
