# Get started with \`bayesian\`

``` r
library(bayesian)
```

``` r
library(recipes)
library(workflows)
```

As a simple example, we will model the seizure counts in epileptic
patients to investigate whether the treatment (represented by variable
`Trt`) can reduce the seizure counts and whether the effect of the
treatment varies with the baseline number of seizures a person had
before treatment (variable `Base`) and with the age of the person
(variable `Age)`. As we have multiple observations per `person`, a
group-level intercept is incorporated to account for the resulting
dependency in the data. In a first step, we use the `recipes` package to
prepare (a recipe for) the `epilepsy` data. This data set is shipped
with the `brms` package, which is automatically loaded by `bayesian`.

``` r
epi_recipe <- epilepsy |>
  recipe() |>
  update_role(count, new_role = "outcome") |>
  update_role(Trt, Age, Base, patient, new_role = "predictor") |>
  add_role(patient, new_role = "group") |>
  step_normalize(Age, Base)
```

``` r
print(epi_recipe)
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:         1
    ## predictor:       4
    ## group:           1
    ## undeclared role: 4

    ## 

    ## ── Operations

    ## • Centering and scaling for: Age Base

Above, we not only define the roles of the relevant variables but also
normalized the `Age` and `Base` predictors to facilitate model fitting
later on. In the next step, we use `bayesian` to set up a basic model
structure.

``` r
epi_model <- bayesian(
    family = poisson()
  ) |>
  set_engine("brms") |>
  set_mode("regression")
```

``` r
print(epi_model)
```

    ## Bayesian Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   family = poisson()
    ## 
    ## Computational engine: brms

The `bayesian` function is the main function of the package to
initialize a Bayesian model. We can set up a lot of the information
directly within the function or update the information later on, via the
`update` method. For example, if we didn’t specify the family initially
or set it to something else that we now wanted to change, we could use
the `update` method as follows

``` r
epi_model <- epi_model |>
  update(family = poisson())
```

Next, we define a workflow via the `workflows` package, by combining the
above defined data processing recipe and the model plus the actual model
formula to be passed to the `brms` engine.

``` r
epi_workflow <- workflow() |>
  add_recipe(epi_recipe) |>
  add_model(
    spec = epi_model,
    formula = count ~ Trt + Base + Age + (1 | patient)
  )
```

``` r
print(epi_workflow)
```

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: bayesian()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_normalize()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Bayesian Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   family = poisson()
    ## 
    ## Computational engine: brms

We are now ready to fit the model by calling the `fit` method with the
data set we want to train the model on.

``` r
epi_workflow_fit <- epi_workflow |>
  fit(data = epilepsy)
```

    ## Compiling Stan program...

    ## Start sampling

``` r
print(epi_workflow_fit)
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: bayesian()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_normalize()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: count ~ Trt + Base + Age + (1 | patient) 
    ##    Data: ~data (Number of observations: 236) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Multilevel Hyperparameters:
    ## ~patient (Number of levels: 59) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.57      0.07     0.46     0.72 1.00      885     1793
    ## 
    ## Regression Coefficients:
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     1.78      0.12     1.55     2.01 1.01      558     1325
    ## Trt1         -0.27      0.16    -0.60     0.04 1.01      676     1456
    ## Base          0.73      0.08     0.58     0.89 1.00      658     1344
    ## Age           0.09      0.08    -0.08     0.25 1.01      707     1261
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

To extract the parsnip model fit from the workflow

``` r
epi_fit <- epi_workflow_fit |>
  extract_fit_parsnip()
```

The `brmsfit` object can be extracted as follows

``` r
epi_brmsfit <- epi_workflow_fit |>
  extract_fit_engine()
```

``` r
class(epi_brmsfit)
```

    ## [1] "brmsfit"

We can use the trained workflow, which includes the fitted model, to
conveniently `predict` using new data without having to worry about all
the data reprocessing, which is automatically applied using the workflow
preprocessor (recipe).

``` r
newdata <- epilepsy[1:5, ]
```

``` r
epi_workflow_fit |>
  predict(
    new_data = newdata,
    type = "conf_int",
    level = 0.95
  )
```

    ## # A tibble: 5 × 2
    ##   .pred_lower .pred_upper
    ##         <dbl>       <dbl>
    ## 1           0           8
    ## 2           0           8
    ## 3           0           7
    ## 4           0           8
    ## 5           6          23

To add the standard errors on the scale of the linear predictors

``` r
epi_workflow_fit |>
  predict(
    new_data = newdata,
    type = "conf_int",
    level = 0.95,
    std_error = TRUE
  )
```

    ## # A tibble: 5 × 3
    ##   .pred_lower .pred_upper .std_error
    ##         <dbl>       <dbl>      <dbl>
    ## 1           0           8       2.04
    ## 2           0           8       2.05
    ## 3           0           7       1.83
    ## 4           0           8       2.00
    ## 5           7          23       4.18
