---
title: "Determining Sample Size for Randomized Experiments in Education"
author: "Courtney Hall"
date: "April 21, 2018"
output: 
  html_document:
    theme: simplex
    css: styles.css
    highlight: NULL
    keep_md: true
    toc: true
    toc_depth: 3
    toc_float: true
    number_sections: false
---



# Calculating Power with Simulations

For simple experiments, [Online calculators](http://powerandsamplesize.com), [G*power software](http://www.gpower.hhu.de/en.html), [STATA](https://www.stata.com/features/power-and-sample-size/), or [R packages](https://www.statmethods.net/stats/power.html) all provide simple power calculations for basic experimental designs such as 2 or more sample comparisons (ANOVA) or crossed designs (ANCOVA). More complicated experimental designs and cases where the data is nested or hierarchical (such as students within schools) require more assumptions and are more complicated (or impossible) to calculate through these calculators.

When we have a more complicated design, instead of calculating power and sample sizes directly we can use simulation. A power simulation simulates what would happen if we were to conduct our desired study with under a particular set of conditions (sample size, effect size, and outcome variation) before having to actually conduct the experiment. That way, we can determine if the sample size we are considering including in our study will give we a reasonable amount of power for our expected effect size and outcome variation.

## Scenario

Suppose you work for Sylvan High School, which has a student body of 1456 students and you want to conduct a randomized experiment on a new student mentoring program. The intervention pairs students with community members to do a project in the community and the goal is to increase engagement and reduce absenteeism in school. If the program works, your school might decide to assign mentors to all students, but your principal wants to be confident that the program will help improve teaching before spending the time and resources finding and training mentors. You are asked to estimate how many students should be included in a pilot so that you can determine whether the program works and whether the effect of the program on absenteeism is big enough to warrant using valuable time to run and manage the program.

Generally, we want the power of any ranomized experiment to be at least 0.8, so the goal is to find the minimum sample size that will meet that criteria. We will start by testing to see if including 200 students in the study (100 in the treatment and 100 in the control) will have enough power.

We have to make some assumptions to start: 
- Our outcome of interest is the percent absent at the end of this year (has not been collected yet)
- Our "pretest" measure is how often a student was absent in middle school (pct_absent_in_ms)
- If the intervention works, the effect size is 0.2 SD
- The variance of the outcome is equivalent to the variance of pct_absent_in_ms

We will start by taking one sample from the data to test whether we'll get a type II error. 

0. **Define Parameters to test**: In this case, we need to define the sample size and effect size that we are testing based on our assumptions above. 

    
    ```r
    set.seed(5)
    
    # Define sample size to test
    sample.size <- 200
    
    # Define effect size in standard deviations
    effect.size.SD <- -0.2 # (Effect in SD) Effect size is negative because intervention reduces absences
    ```


1. **Select a subset of student to participate**: The code below creates a new subset of the full dataset called "S.sample" which has a random sample of 200 students (the sample size we are testing)

    
    ```r
    total.n <- 1456 # Total number of students in the school
    S.sample <- Sylvan[sample(1:total.n, sample.size),]
    ```

2. **Assign students to treatment**: Create a variable called "T.Status" which is 1 for students assigned to treatment and 0 for students assigned to the control. In the code below we assign half of the students to the treatment group and half to the control group, but you could change the proportion if you want a larger or smaller number of students to receieve treatment.  
      
    
    ```r
    # Among the sample chosen, assign half to treatment and half to control
    S.sample$T.status <- 0 
    S.sample$T.status[sample(1:sample.size, sample.size*0.5)] <- 1
    table(S.sample$T.status) # Frequency table of how many students are assigned to treatment and control
    ```
    
    ```
    ## 
    ##   0   1 
    ## 100 100
    ```
    
3. **Assign outcome to treated and control students**: Because we haven't *actually* run the experiment yet, we don't know the true absence percent outcome for each student. However, for the simulation we will assume that if students are in the control group, we will assume that their absenteism will not change from their rate in 8th grade. For the treatment group, we will assume that there is a homogenous treatment effect size of 0.2 SD. This means that if students are in the treatment group, we will assign their percent absent to be 0.2 SD less than their 8th grade percent absent. In this example, one standard deviation is  or 6.87%, so 0.2 SD would be a 1.37% reduction in absences during the school year (about 2.5 days).  
         
    
    ```r
    effect.size <- effect.size.SD * sd(S.sample$pct_absent_in_ms, na.rm = TRUE) # Effect size in % days absent
    S.sample$outcome <-S.sample$pct_absent_in_ms
    S.sample$outcome[S.sample$T.status == 1] <- S.sample$pct_absent_in_ms[S.sample$T.status == 1] + effect.size
    ```
        
        
4. **Estimate treatment effect**: The treatment effect can be estimated in several ways, including using a simple difference in means or a linear model (as shown in the code below). As shown below, the treatment effect is estimated to be -0.7985 days. You can also include additional covariates in the model at this step. 
    
    
    ```r
    out.mod <- lm(outcome ~ T.status, data = S.sample) 
    
    summary(out.mod)
    ```
    
    ```
    ## 
    ## Call:
    ## lm(formula = outcome ~ T.status, data = S.sample)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -7.850 -4.868 -2.190  2.982 30.330 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.2754     0.6878  10.578   <2e-16 ***
    ## T.status     -0.7985     0.9726  -0.821    0.413    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.878 on 198 degrees of freedom
    ## Multiple R-squared:  0.003392,	Adjusted R-squared:  -0.001641 
    ## F-statistic: 0.6739 on 1 and 198 DF,  p-value: 0.4127
    ```
      
5. **Extract key statistics**: Using your model output, extract the treatment effect coefficient, standard error, degrees of freedom, p value, and the number of records with data that were used (N). In the code below, each of these statistics is stored as a variable which will later be stored together as a vector of statistics representing the results from this sample from the data. 

    
    ```r
     T.coef <- summary(out.mod)$coef["T.status" , "Estimate"] # Treatment estimate
     T.se   <- summary(out.mod)$coef["T.status" ,  "Std. Error"] # Standard Error of Treatment estimate
     df <- out.mod$df.residual # degrees of freedom 
     T.pval <- 2*(pt(-abs(T.coef / T.se) , df = df)) # cdf t for 2 tailed pvalue
     N <- sum(complete.cases(S.sample[, c("outcome" , "T.status")])) # final N count of students 
                                                                     # who could be included in the analysis
    ```
        
6. **Determine if you made a type II error**: Because of the way that we set up the data, we have already made the assumption that there is an effect of the intervention. In otherwords, we are assuming that the alternative hypothesis is true. If we fail to reject the null hypothesis by getting a p value of less than 0.05, then we have made a type II error.  The code below creates a new variable which is "True" if at type II error was made and "False" if we made the correct decision.

    
    ```r
    typeIIerror <- T.pval >= 0.05
    ```

7. **Store model statistics as a vector**: Here we combine all of our results and statistics from this sample together into a single vector called "sample.stats". When we run the simulation, we will want to combine all of the individual sample statistics together into a matrix where each row represents the statistics from one sample. 

    
    ```r
    sample.stats <- c(itteration = 1, sample.size = sample.size ,          
       effect.size = effect.size.SD ,
       T.coef = T.coef , 
       T.se = T.se , T.pval = T.pval , df = df , 
       typeIIerror = typeIIerror , N = N)
    
    sample.stats
    ```
    
    ```
    ##  itteration sample.size effect.size      T.coef        T.se      T.pval 
    ##   1.0000000 200.0000000  -0.2000000  -0.7984710   0.9726431   0.4126740 
    ##          df typeIIerror           N 
    ## 198.0000000   1.0000000 200.0000000
    ```


### What about power? 

Because power is the liklihood of NOT making a type II error, we cannot calculate the power from one sample of the dataset. The method and code presented above for one sample can be repeated many times using loops. Each time a new sample is drawn and sample statistics calculated, we call it an itteration. A simulation is made up of many itterations. We can then calculate the type II error rate ($\beta$) by calculating the proportion of itterations where a type II error was made. The power is the inverse: ($1-\beta$). 

The next section demonstrates how to use loops to run a simulation with multiple itterations.

## Looping over many itterations

The code below puts Steps 1 through 7 in a loop so that multiple samples are drawn from the data, students are assigned randomly to treatment, and a treatment effect and p value are calculated. For each itteration of the simulation, two steps are added to store the results and clear the results from one itteration to prepare for the next: 

8. **Save itteration results**: In the previous step, all of the results for the given sample were saved into a vector. This step adds each subsequent results vector (sample.stats) to a matrix which will contain all of the results for all itterations, one vector per row. 

9. **Remove all itteration-specific variables (except all.stats results matrix)**: To clean up the simulation along the way, it is good practice to remove all sample-specific variables before the next itteration begins. Doing so prevents errors in the code from slipping through from one itteration to the next. 


The code below continues the previous example but shows how all 9 steps can be combined into a loop with 1000 itterations. Our goal is to find the power of a randomized experiment where the effect size is 0.2SD and the sample size is 200. 


```r
# ---------- Simulation Preparation ----------
# STEP 0: Define Parameters
set.seed(5)

# Define sample size to test
sample.size <- 200

# Define effect size in standard deviations
effect.size.SD <- -0.2 # (Effect in SD) Effect size is negative because intervention reduces absences

# ----------- Run Simulation ---------------

for(i in 1:1000){
  # STEP 1: Select a subset of student to participate
  total.n <- 1456 # Total number of students in the school
  sample.size <- 200 # The sample size of interest
  S.sample <- Sylvan[sample(1:total.n, sample.size),]
  
  # STEP 2: Assign students to treatment
  # Among the sample chosen, assign half to treatment and half to control
  S.sample$T.status <- 0 
  S.sample$T.status[sample(1:sample.size, sample.size*0.5)] <- 1
  
  # STEP 3: Assign outcome to treated and control students
  effect.size.SD <- -0.2 # (Effect in SD) Effect size is negative because intervention reduces absences
  effect.size <- effect.size.SD * sd(S.sample$pct_absent_in_ms, na.rm = TRUE) # Effect size in % days absent
  S.sample$outcome <-S.sample$pct_absent_in_ms
  S.sample$outcome[S.sample$T.status == 1] <- S.sample$pct_absent_in_ms[S.sample$T.status == 1] + effect.size
  
  # STEP 4: Estimate treatment effect
  out.mod <- lm(outcome ~ T.status, data = S.sample) 
  
  # STEP 5: Extract Key Statistics
  T.coef <- summary(out.mod)$coef["T.status" , "Estimate"] # Treatment estimate
  T.se   <- summary(out.mod)$coef["T.status" ,  "Std. Error"] # Standard Error of Treatment estimate
  df <- out.mod$df.residual # degrees of freedom 
  T.pval <- 2*(pt(-abs(T.coef / T.se) , df = df)) # cdf t for 2 tailed pvalue
  N <- sum(complete.cases(S.sample[, c("outcome" , "T.status")])) # final N count of students who could be included in the analysis

  # STEP 6: Determine if you made a type II error
  typeIIerror <- T.pval >= 0.05
  
  # STEP 7: Store model statistics as a vector
  sample.stats <- c(itteration = i, sample.size = sample.size ,          
   effect.size = effect.size.SD , 
   T.coef = T.coef , 
   T.se = T.se , T.pval = T.pval , df = df , 
   typeIIerror = typeIIerror , N = N)

  # STEP 8: Save itteration results
  if(i == 1){results <- data.frame(t(sample.stats))} # if this is the first itteration, there will only be one row in the results matrix
  else{results <- rbind(results, sample.stats)} # each subsequent itteration stats will be added to the prior
  
  # STEP 9: Remove all itteration-specific variables (except all.stats results matrix)
  remove(sample.stats, S.sample, T.coef, T.se, df, T.pval, N, typeIIerror, out.mod)
  
}
```

### Calculating Power

From the results matrix (`results`), we can calculate the type II error rate by calculating the proportion of itterations where we made a type II error.  


```r
table(results$typeIIerror) # frequency count of type II error (1) and correct decision (0)
```

```
## 
##   0   1 
## 302 698
```

In this example, 698 samples out of 1000 failed to reject the null hypothesis (p>0.05) despite there being a treatment effect, so $\beta = .698$. Thus, we can expect that if we choose 200 students for the randomized study and the true effect size is 0.2, the power would be .302 or 30.2%. This power is lower than the desired 0.8. 

### Test Multiple Sample Sizes and Effect sizes!

If you are trying to determine an appropriate sample size, you'll want to calculate the power of more than one option so you can see how much your power changes if you increase or decrease your sample size. You may also want to test different effect sizes to see how sensitive your power will be if the effect size is different than you assume. 

To test different parameters, you can simply run the same simulation again using the changed parameters, or you can add additional loops so that you can test all of the parameters at once.  The code in the next section gives an example of how to do the second option. The results from this multi-parameter simulation allow the opportunity to make graphs that show the relationship between your sample size, effect size, and power (called Power curves).

## Multi-Parameter Simulation

To test multiple parameters (sample size, effect size) as part of the same simulation, only slight changes need to be made to the code: 

- Rather than define parameters as a single value, specify vectors that contain the parameter values that you wish to test.

- Add in two additional nested loops: one loop over the `k` sample sizes and one loop over the `j` effect sizes

- Everywhere in the code where the `sample.size` or `effect.size.SD` variable are used, change to `sample.size[k]` and `effect.size[j]` to select only the relevant effect and sample sizes.





```r
# ---------- Simulation Preparation ----------
# STEP 0: Define Parameters
set.seed(5)

# Define sample size to test
sample.size <- c(200, 400, 800)

# Define effect size in standard deviations
effect.size.SD <- c(-0.1, -0.2, -0.3) # (Effect in SD) Effect size is negative because intervention reduces absences

# ----------- Run Simulation ---------------
for(k in 1:length(sample.size)){
  for(j in 1:length(effect.size.SD)){
    for(i in 1:1000){
      # STEP 1: Select a subset of student to participate
      total.n <- 1456 # Total number of students in the school
      S.sample <- Sylvan[sample(1:total.n, sample.size[k]),]
      
      # STEP 2: Assign students to treatment
      # Among the sample chosen, assign half to treatment and half to control
      S.sample$T.status <- 0 
      S.sample$T.status[sample(1:sample.size[k], sample.size[k]*0.5)] <- 1
      
      # STEP 3: Assign outcome to treated and control students
      effect.size <- effect.size.SD[j] * sd(S.sample$pct_absent_in_ms, na.rm = TRUE) # Effect size in % days absent
      S.sample$outcome <-S.sample$pct_absent_in_ms
      S.sample$outcome[S.sample$T.status == 1] <- S.sample$pct_absent_in_ms[S.sample$T.status == 1] + effect.size
      
      # STEP 4: Estimate treatment effect
      out.mod <- lm(outcome ~ T.status, data = S.sample) 
      
      # STEP 5: Extract Key Statistics
      T.coef <- summary(out.mod)$coef["T.status" , "Estimate"] # Treatment estimate
      T.se   <- summary(out.mod)$coef["T.status" ,  "Std. Error"] # Standard Error of Treatment estimate
      df <- out.mod$df.residual # degrees of freedom 
      T.pval <- 2*(pt(-abs(T.coef / T.se) , df = df)) # cdf t for 2 tailed pvalue
      N <- sum(complete.cases(S.sample[, c("outcome" , "T.status")])) # final N count 
    
      # STEP 6: Determine if you made a type II error
      typeIIerror <- T.pval >= 0.05
      
      # STEP 7: Store model statistics as a vector
      sample.stats <- c(itteration = i, sample.size = sample.size[k] ,          
       effect.size = effect.size.SD[j] , 
       T.coef = T.coef , 
       T.se = T.se , T.pval = T.pval , df = df , 
       typeIIerror = typeIIerror , N = N)
    
      # STEP 8: Save itteration results
      if(i == 1 & j == 1 & k == 1){results <- data.frame(t(sample.stats))} # if this is the first itteration, 
                                                         # there will only be one row in the results matrix
      else{results <- rbind(results, sample.stats)} # each subsequent itteration stats 
                                                          # will be added to the prior
      
      # STEP 9: Remove all itteration-specific variables (except all.stats results matrix)
      remove(sample.stats, S.sample, T.coef, T.se, df, T.pval, N, typeIIerror, out.mod)
      
    }
  }
}
```

### Calculating Power with multiple parameters

Unlike when the simulation had only one set of parameters (sample size = 200, effect size = 0.2), now we have 9 combinations of parameters for which we can calculate type II error rates and power.  You can individually find the proportion of itterations for each set of parameters, or you can use the `aggregate` function to calculate the proportion of type II errors made for each set of parameters simultaneously, as shown in the code below


```r
# summarize results to calculate type II error and power
# Average the number of type II errors made over the input values for proportion treated, n size, and effect size
summary.results <- aggregate(list(typeIIerror = results$typeIIerror), 
                             by = list( sample.size = results$sample.size, 
                                        effect.size = results$effect.size ), mean)

summary.results
```

```
##   sample.size effect.size typeIIerror
## 1         200        -0.3       0.456
## 2         400        -0.3       0.151
## 3         800        -0.3       0.009
## 4         200        -0.2       0.724
## 5         400        -0.2       0.498
## 6         800        -0.2       0.216
## 7         200        -0.1       0.892
## 8         400        -0.1       0.819
## 9         800        -0.1       0.685
```

We can then calculate the power by taking the inverse of the type II error rate: 


```r
# calculate the power
summary.results$power <- 1-summary.results$typeIIerror

summary.results
```

```
##   sample.size effect.size typeIIerror power
## 1         200        -0.3       0.456 0.544
## 2         400        -0.3       0.151 0.849
## 3         800        -0.3       0.009 0.991
## 4         200        -0.2       0.724 0.276
## 5         400        -0.2       0.498 0.502
## 6         800        -0.2       0.216 0.784
## 7         200        -0.1       0.892 0.108
## 8         400        -0.1       0.819 0.181
## 9         800        -0.1       0.685 0.315
```



### Plotting Simulation Results

Results from your power simulation can also be plotted using a "power curve" plot. This type of chart displays the relationship between the effect size (on the x axis) with the the power of the study (on the y axis). For each sample size in the simulation, we add a separate power curve. In the plot below, a horizontal dotted line has been added to show the 80% power threshold that we are aiming for. 

The code below uses the package "ggplot2" to create these graphs. The primary data structure required to make the plot is the same as the summary of results output (`summary.results`) above. Each row of the dataset must represent the input parameters for the simulation (effect.size and sample.size) and the calculated power ($1 - mean(TypeIIError)$).



```r
# --------------------------
# Plot Results
# --------------------------
library(ggplot2)

power.plot <- ggplot(summary.results, aes(x = effect.size, y = power, group = sample.size))

power.plot + geom_line(aes(color = as.factor(sample.size)), size = .8) + ggtitle("Power by Effect Size and Sample Size") + xlab("Effect Size") + ylab("Power") + theme_classic() + geom_hline(yintercept = 0.8, linetype = "dashed") + labs(color = "Sample Size")
```

![](SampleSizeDraft4_files/figure-html/plotresults-1.png)<!-- -->

### Interpreting the results

Based on the summary of results and the power plot, we can see: 

1. Power is larger when we use a larger sample size.

2. Power is larger when we can assume a larger effect size.

Based on this information, how do you make your final decision about how many participants to include in your study? 

**Statistical Decision**: From a statistical perspective, the rule of thumb is to pick a sample size that is big enough to achieve 80% power for a reasonable effect size. For example, in this example, the power plot shows three power curves for each of 3 sample sizes (200, 400, or 800 students) at three effect sizes (-0.1 SD, -0.2 SD, and -0.3 SD). We can rule out using a sample size of 200 students because the power is never above 80%. If we assume that the effect size of the intervention will be large (-0.3), then including 400 students in the study will likely yield large enough power. However, if we want to be able to detect smaller effect sizes (as low as -0.2), then we should advocate for including as many as 800 students in the study.  

**Logistical/Practical Decision**: Sometimes, we won't have enough subjects (schools, etc.) to be able to have a large enough sample size to have high power. Other times, financial or resource limitations will prevent you from being able to collect data from enough subjects to have high power or to detect low effect sizes. In still other circumstances, the political reasons for running a study will outweigh the statistical concerns and a study will have to be conducted with lower power. When these factors come into play, power calculations might help you to objectively weigh the impact of giving an intervention to a smaller sample than needed. If you don't consider power, your organization risks spending money, resources, and time on an intervention that might in fact work, but if your sample size is too small you might not be able to prove that it does.

Unfortunately, we generally don't have control over the true effect size, so when choosing a sample size, it is better to assume as low of an effect size as possible so that if there is a small effe 
