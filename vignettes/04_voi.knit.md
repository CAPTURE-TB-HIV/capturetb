---
title: "Value of Information Analysis"
author: "Alex Hill"
date: "2025-07-16"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Value of Information Analysis}
  %\VignetteEngine{quarto::html}
  %\VignetteEncoding{UTF-8}
---




::: {.cell}

```{.r .cell-code}
library(capturetb)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```
:::


## Cost-effectiveness analyses

When using model estimates in the context of an HTA, model uncertainty will have a certain cost associated with it. The `capturetb::unitcost` model has a built-in method for calulating the Expected Value of Perfect Information (EVPI) - in other words, the cost of model uncertainty - given a willingness-to-pay threshold.

To visualise model uncertainty, we predict costs for an unseen facility:


::: {.cell}

```{.r .cell-code}
model <- capturetb::unitcost()

# Include all covariates and facility country `fc_country`
new_inputs <- list(
  log_USD_p_bldgspace = 1,
  logVisits = 6.9, 
  logVisitsPP_TB = -1.29, 
  secondary = FALSE, 
  urban = FALSE, 
  public = TRUE,
  fc_country = "Ethiopia"
)

pred <- model$predict(new_inputs, scale = "natural", summarised = TRUE)

# Expected unit cost is mean prediction
expected_unit_cost <- pred$mean
knitr::kable(pred)
```

::: {.cell-output-display}


|Observation |     Mean|   CI|   CI_low|  CI_high|
|:-----------|--------:|----:|--------:|--------:|
|1           | 8.676577| 0.95| 2.562928| 21.91005|


:::
:::


As well as the point estimates, the model actually gives a full probability distribution for predicted costs:


::: {.cell}

```{.r .cell-code}
pred <- model$predict(new_inputs, scale = "natural", summarised = FALSE)
ggplot(data.frame(cost = pred), aes(x = cost)) + geom_density()
```

::: {.cell-output-display}
![](04_voi_files/figure-html/plot-pred-1.png){width=768}
:::
:::


We can use this to calculate the EVPI given various WTP thresholds for a single output:


::: {.cell}

```{.r .cell-code}
lambda <- c(2:10)
evpi <- model$evpi(new_inputs, lambda)
ggplot(data.frame(lambda, evpi)) + 
    geom_line(aes(x = lambda, y = evpi))
```

::: {.cell-output-display}
![](04_voi_files/figure-html/plot-evpi-1.png){width=768}
:::
:::


In this case, the maximum EVPI is about $1 per OP treatment visit - this scales linearly with the number of visits, so if an intervention includes 1000 visits, the EVPI will be $1000. 

Notice that EVPI peaks when the WTP threshold $\lambda$ is equal to the expected cost; this is because in this case, even a tiny under-prediction of costs will lead to the wrong decision being made. 

To get a general sense of the cost of model uncertainty, we can average over the input vectors in the training set and look at EVPI relative to predicted costs.


::: {.cell}

```{.r .cell-code}
lambda <- seq(0, 2.5, by = 0.25)
dat <- get_data("OP treatment visit")
pred <- model$predict(dat, summarise = TRUE, scale = "natural")[, "Mean"]
all_evpi <- lapply(1:nrow(dat),
		function(ind) sapply(lambda * pred[ind],
		function(l) model$evpi(dat[ind, ], l)))

all_evpi_normalised <- lapply(1:nrow(dat), 
	function(i) all_evpi[[i]]/pred[i])

all_evpi_df <- do.call(rbind, all_evpi_normalised)
all_evpi_quants <- apply(all_evpi_df, 2, quantile, probs = c(0.10, 0.5, 0.90, 1))

all_evpi_mean <- apply(all_evpi_df, 2, mean)
all_evpi_mean_trimmed <- apply(all_evpi_df, 2, mean, trim = 0.1)

plot(lambda, all_evpi_normalised[[1]], 
	type = "l", 
	ylab = "EVPI / predicted cost",
	xlab = "WTP / predicted cost",
	ylim = c(0, 0.25),
	col = "grey")
for (i in 2:nrow(dat)) {
	lines(lambda, all_evpi_normalised[[i]], col = "grey")
}
lines(lambda, all_evpi_mean, col = "blue")
lines(lambda, all_evpi_quants[1, ])
lines(lambda, all_evpi_quants[3, ])
```

::: {.cell-output-display}
![](04_voi_files/figure-html/all-evpi-1.png){width=768}
:::
:::


We can see that when a WTP threshold is close to the predicted cost, resolving model uncertainty completely would be worth approximately 20% of the total cost of the intervention. Of couse in practice we can never measure costs "perfectly", but this gives some indication of the magnitude of the value of collecting primary cost data when assessing cost-effectiveness.

As well as calculating EVPI for costs at a single facility, the method can be used to look at EVPI for an intervention that involve $n_i$ visits for facilities $i = ...I$:


::: {.cell}

```{.r .cell-code}
# using model training data to demonstrate
# but works for any inputs
n_output <- dat$fc_opvisits_TB
lambda <- seq(6000000, 12000000, by = 500000)
evpi <- model$evpi(dat, lambda, n_output)
ggplot(data.frame(lambda, evpi)) + geom_line(aes(lambda, evpi))
```

::: {.cell-output-display}
![](04_voi_files/figure-html/total-evpi-1.png){width=768}
:::
:::


## Budgeting and strategic planning

A formal EVPI measure can be defined for any decision context that has a loss function associated with it, but there is no single way to think about loss associated with uncertainty in a budgeting or planning context. If under or over-prediction are equally costly, we might still have a linear or a quadratic loss function. Alternatively, there may be an asymmetric loss function - e.g. over-budgeting wastes funds, under-budgeting causes a project to fail. 

Here we demonstrate how to calculate (and minimise) expected loss given two different loss functions:

1. An asymmetric linear loss functiion:

$$
L(\hat{c}, c) =
\begin{cases}
\alpha (c - \hat{c}) & \text{if } \hat{c} < c \\
\beta (\hat{c} - c) & \text{if } \hat{c} \geq c
\end{cases}
$$

Bayesian decision theory shows that the optimal decision under an asymmetric linear function is the $\dfrac{\alpha}{\alpha + \beta}$ quantile of the posterior cost distribution, call it $c*$. The expected loss associated with using this prediction is then 
$\mathop{\mathbb{E}}[L(c*, c)]$. We can estimate this using draws from the posterior. For illustration, suppose our budget includes all facilities in the training data:


::: {.cell}

```{.r .cell-code}
# for illustration purpose look at cost aggregated over
# all facilities in Kenya, and take the recorded
# number of visits at each facility as the number of outputs
# being budgeted for
dat <- dat |> dplyr::filter(fc_country == "Kenya")
n_out <- dat$fc_opvisits_TB

# get posterior predictions of total cost 
samples <- model$predict_total(dat, n_out)

# suppose over-prediction is 4 times as bad
alpha <- 1
beta <- 0.25

# loss minimising prediction
c_star <- quantile(samples, alpha/(alpha + beta))
c_mean <- mean(samples)

loss_function <- function(c, c_star) {
	diff <- abs(c - c_star)
	if (c_star < c) {
		alpha * diff
	} else {
		beta * diff
	}
}

# get expected loss by averaging loss over 
# all realisations of the total cost
loss <- sapply(samples, loss_function, c_star)

# loss if we used the mean prediction instead
loss2 <- sapply(samples, loss_function, c_mean)

# compare distributions of losses when using 
# mean vs 80th quantile prediction
ggplot(data.frame(loss, loss2)) + 
	geom_density(aes(x = loss)) + 
	geom_density(aes(x = loss2), color = "grey") + 
	scale_x_continuous(labels = scales::dollar_format()) + 
	geom_vline(aes(xintercept = mean(loss)), color = "red", linetype = "dashed") +
		geom_vline(aes(xintercept = mean(loss2)), color = "grey", linetype = "dashed") +
	scale_y_continuous(labels = scales::label_comma()) +
	ylab("") + 
	xlab("")
```

::: {.cell-output-display}
![](04_voi_files/figure-html/loss-asymmetric-1.png){width=768}
:::
:::


3. A symmetric linear loss functiion:

$$
L(\hat{c}, c) = \alpha |c - \hat{c}|
$$

This is just a special case of the asymmetric loss function where $\alpha = \beta$, and the cost minimising prediction is the 50th posterior quantile, i.e. the median. 



::: {.cell}

```{.r .cell-code}
alpha <- 1

# loss minimising prediction
c_star <- quantile(samples, 0.5)

loss_function <- function(c) {
	diff <- abs(c - c_star)
	alpha * diff
}

# get expected loss by averaging lss over 
# all realisations of the total cost
loss <- sapply(samples, loss_function)

ggplot(data.frame(loss)) + 
	geom_density(aes(x = loss)) + 
	scale_x_continuous(labels = scales::dollar_format()) + 
	geom_vline(aes(xintercept = mean(loss)), color = "red", linetype = "dashed") +
	scale_y_continuous(labels = scales::label_comma()) +
	ylab("") + 
	xlab("")
```

::: {.cell-output-display}
![](04_voi_files/figure-html/loss-symmetric-1.png){width=768}
:::
:::


These are just illustrative examples, but the same approach can be taken for arbitrary loss functions.

