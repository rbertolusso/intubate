---
title: "intubate"
author: "Roberto Bertolusso"
date: "2016-07-30"
---

The aim of `intubate` is to offer a painless way to
add R statistical functions that use formula interface
to pipelines implemented by `magrittr` with the
operator `%>%`, without having to rely on workarounds.

You can install:

* the latest released version from CRAN (0.99.2) with

```{r}
install.packages("intubate")
```

* the latest development version from github with

```{r}
# install.packages("devtools")
devtools::install_github("rbertolusso/intubate")
```
#### See also
The [*setter*](https://bitbucket.org/richierocks/setter)
package contains mutators to set attributes of variables,
that work well in a pipe (much like `stats::setNames())`.

## 2016/08/02

* Now all interfaces derive from one function only called,
  for now, `ntbt_function_data`.
  
  (Two steps to create an interface seemed way too much.
  You need one step now. Anyway, I am still not satisfied
  with the amount of labor involved and I will put my best
  effort trying to reduce it further.)

* In addition to formula versions:

```{r}
library(magrittr)
library(intubate)

USJudgeRatings %>%
  ntbt_cor.test(~ CONT + INTG)
```

you can also use, for example, the `x` `y` versions:

```{r}
USJudgeRatings %>%
  ntbt_cor.test(CONT, INTG)
```

All the examples in the documentation run (but they
are formula-only versions). Tests to see if this
works as expected are welcome. Of course it would be pure
magic if it just simply works no matter what you throw at
it, but I simply have no clue of all the possible behaviors
of the interfaced functions.
My goal is that it works for reasonable cases. I anticipate
limitations. The interface machinery has to stay powerful
yet simple.

* If interfaced function returns NULL, the interface function
  forwards invisibly the input, so you can use the data downstream.
  
```{r}
library(dplyr)
CO2 %>%
  mutate(color=sample(c("green", "red", "blue"),
                      length(conc), replace = TRUE))%>%
  ntbt_plot(conc, uptake, col = color) %>%  ## plot returns NULL
  ntbt_lm(conc ~ uptake) %>%  ## data passes through ntbt_plot
  summary()

## Create some non implemented interfaces
ntbt_legend <- ntbt_cat <- ntbt_function_data  ## One interface only

within(warpbreaks, {
  time <- seq_along(breaks)
  W.T <- wool:tension
}) %>%
  ntbt_plot(breaks ~ time, type = "b") %>%
  ntbt_text(breaks ~ time, label = W.T,
            col = 1 + as.integer(wool)) %>%
  ntbt_cat("And now we write a legend.") %>%
  ntbt_legend("top",
              legend = levels(wool),
              col = 1 + as.integer(wool)) %>%
  invisible()
```


## 2016/07/30

### Pipelines
Pipelines in R are made possible by the package `magrittr`,
by Stefan Milton Bache and Hadley Wickham.

`dplyr`, by Hadley Wickham, Romain Francois, and RStudio,
is used here to illustrate data transformation.
```{r}
## Packages needed
library(dplyr)     ## Does data transformation
library(magrittr)  ## Implements pipelines

## Data used
# devtools::install_github("hadley/yrbss")
library(yrbss)
data(survey)
```

This *machinery* allows to perform data transformations
using pipelines in the following way:
```{r}
survey %>%
  group_by(year) %>%
  summarise(count = n(),
            countNA = sum(is.na(stheight)),
            propNA = mean(is.na(stheight))) %>%
  knitr::kable()
```

Pipelines seem to be the preferred way, these days, of doing data
transformation. If you want an introduction about pipelines,
and/or to learn more about them, please follow this link (http://r4ds.had.co.nz/transform.html)
to the chapter on data transformation of the
forthcoming book "R for Data Science" by
Garrett Grolemund and Hadley Wickham.

### R statistical functions and pipelines
Suppose you want to perform a regression analysis
of the weight on height of males corresponding
to the year 2013 (assuming
for the sake of argument that it is a valid analysis
to perform. See at the very end of the document
for more on this).

As most R functions are not pipeline-aware, you
should do something like the following.

First, you perform your data science transformations
and save the result to a temporary object (`tmp` in this case).
```{r}
survey %>%
  filter(!is.na(stheight) & !is.na(stweight) &
           year == 2013 & sex == "Male"
  ) %>%
  select(stheight, stweight) ->
tmp
```

Then, you perform your regression analysis on the transformed
data stored in `tmp`.
```{r}
fit <- lm(stweight ~ stheight, tmp)
summary(fit)
```

(There is nothing wrong in this approach. In fact it is good. Jolly good. Splendid!
If you are absolutely happy with doing things this way then there is no need
to continue to devote your efforts in reading this document.
`intubate` is not for you. I am happy we could establish this in such little time.)

But what if, in addition to the data transformation, you
would also like to perform your data modeling/analysis under the
same pipeline paradigm (by adding lm to it),
which would impart notation consistency and
would avoid the need of creating the temporary object?
```{r, eval=FALSE}
survey %>%
  filter(!is.na(stheight) & !is.na(stweight) &
           year == 2013 & sex == "Male"
  ) %>%
  select(stheight, stweight) %>%
  lm(stweight ~ stheight) %>%  ## Using the original function
  summary()
```

You get an **error**.

The reason of this failure is that pipeline-aware functions (such as the ones
in `dplyr` that were specifically designed to work in pipelines) receive the data as
the **first** parameter, and most
statistical procedures that work with **formulas** to specify the **model**,
such as `lm` and lots of other rock solid reliable functions that implement
well established  statistical procedures, receive the data as the **second**
parameter.

This *minor* detail can make a difference, actually a huge one.
In fact, it may create a division line of two clearly separated cultures, that I will call, for the lack of better names, the "traditionalists" and the "modernists".

(They could be the "modeldatas" and the "datamodels".
Whichever you prefer that does not offend anyone)
 
The aim of `intubate` is to provide an easy alternative so nobody has to change the way they do things.

If you are a "traditionalist" and you want to create your new 
statistical package in the traditional way
(first `model` and then `data`), you will not potentially find yourself at a crossroad if you think you need to decide which community to serve, when in fact you can serve *both* communities without having to do anything differently to what you have done so far. You can just keep doing it in the traditional way. In fact, (please...) keep doing it in the traditional way!

Why? Because *everybody* will benefit.

     EXT. BUCOLIC PASTURE - EARLY MORNING
     
     Background music initially inaudible slowly increases in
     volume while the panning camera, starting from a small
     and fragile flower, reveals legions of smiling people
     holding hands, half dress t-shirts with a capital T, the
     other half with a capital M.
     Everybody raises their arms to the sky - still holding hands -
     as if trying to embrace the universe.
     Camera slowly raises, zooming out and tilting down, making
     sure everybody is included in the frame, while everybody
     mantains eye contact with the camera.
     Sun rays break through heavy pure snow-white clouds.
     Everybody opens their mouth and slowly inhales while closing
     their eyes as if they really mean what comes next.
     (This is critical. Make sure it looks credible.)
     Music at full volume.
     Everybody sing.
                            Everybody
         We aaaaare the Woooorld - ta ta ta ta taaaa...
         We aaaare the chiildreeeen - ta ta ta ta taaaa...)

For "traditionalist" users (as I was until a couple of months ago), nothing will have changed. In fact, they will be
completely unaware of anything different happening at all. Just 
business as usual and another fantastic
statistical procedure to add to their bag of resources.

For "modernist" users, `intubate` will do a couple of tricks behind the scenes so
they will be able to run, right at the end of any required data transformation,
your statistical procedures without any hassle using their preferred paradigm of pipelines.

There are alternatives that allow
to include `lm` (and others) in the pipeline without errors and without `intubate`.
They require workarounds
of varying levels of complexity and are illustrated later.

If you choose `intubate` is because you do not want to bother about workarounds when working with pipelines that include statistical procedures.

## intubate
The solution `intubate` proposes is to provide an *interface* to `lm`,
called `ntbt_lm`, that can be used *directly in the pipeline without error* and
without losing any of `lm`'s capabilities.
```{r}
library(intubate)
survey %>%
  filter(!is.na(stheight) & !is.na(stweight) &
           year == 2013 & sex == "Male"
         ) %>%
  select(stheight, stweight) %>%
  ntbt_lm(stweight ~ stheight) %>%  ## Using the interface function
  summary()
```

 By using
the interface the error vanishes, as the interface receives `data`
as its first parameter and `formula` second, performs some function transformations, and then calls
`lm` in the way it expects to receive the parameters (`formula` in first place,
and `data` in second place). Now `lm` can
continue to do all the good things we are used to.

(It is as if `lm` couldn't take anymore being accused by some
 of looking old. So it went to the beauty parlor, had a hair
 cut, and suddently looks "modern" and now is popular again.)

All the interfaces start with `ntbt_` followed by the name of the *interfaced* function.

Just in case, worry not! The *interfaces* do not perform any statistical computations (you should be
very suspicious if they would). The *interfaced* functions (those that are already
well tested) are the ones performing the computations.

### Interfaced libraries
`intubate` currently implements 93 interfaces that can be related to data science methodologies.
The R packages that have interfaces implemented so far are:

* `e1071`: Support Vector Machines
* `gam`: Generalized Additive Models
* `gbm`: Generalized Boosted Regression Models
* `graphics`: The R Graphics Package
* `lattice`: Trellis Graphics for R
* `leaps`: Regression Subset Selection
* `lfe`: Linear Group Fixed Effects
* `MASS`: Robust Regression, Linear Discriminant Analysis, Ridge Regression,
               Probit Regression, ...
* `nlme`: Linear and Nonlinear Mixed Effects Models
* `nnet`: Feed-Forward Neural Networks and Multinomial Log-Linear Models
* `pls`: Partial Least Squares and Principal Component Regression
* `randomForest`: Random Forests for Classification and Regression
* `rpart`: Recursive Partitioning and Regression Trees
* `stats`: The R Stats Package
* `survival`: Survival Analysis
* `tree`: Classification and Regression Trees

The aim is to continue to add interfaces to most methodologies
  used in data science.

Again, the important and sensitive work is performed by the functions provided
in those well tested packages.

`intubate` is simply a middleman that takes credit for other people's honest work.

### Interfaces "on demand"

(It used to be "on the fly", but "on demand" sounds more marketable. Right?)

What if you would like to have an interface to a non pipeline-aware function
that is not currently implemented by `intubate`?

In a vast majority of cases of R functions that have a formula interface,
as it is the case in 89 out of 93 of the functions interfaced so far,
you can create your own interface "on demand".

To help you in the process,
`intubate` exposes helper functions to assist you.

(Well... *something* had to do this lazy package...)

The helper functions available so far are:

* `ntbt_function_formula_data`
* `ntbt_function_x_data`
* `ntbt_function_object_data`
* `ntbt_function_model_data`
* `ntbt_function_fixed_data`

### Steps to create interfaces "on demand"
For the sake of argument, suppose `ntbt_lm` (the interface to `lm`)
is not implemented by `intubate`, and
you want to create an interface for it.

The first thing you need to do is

#### 1) Find out which helper function to use
  By looking at the function definition in the documentation of `lm`
  (hint: type `help(lm)` on the console)
```{r,eval=FALSE}
lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, ...)
```

you notice that the function uses `formula` followed by `data`.
This means that the corresponding helper function to create the
interface is `ntbt_function_formula_data`.

If the function would
use `object` followed by `data`, you would use
`ntbt_function_object_data`, and so on.

Confusing?

No?

Really?

OK, let's see how you do in a quiz, under the Honor Code.


            Name:
            Honor Code Statement:
            
            Question: Which helper function would you use to construct your interface
            if in the help of the target function its definition states that the
            first parameter is `x` and the second is `data`? [10 points]

(Psst!... here... don't look!... I told you not to look!... yes, yes, play dumb... you don't have to worry... someone told me there
is a solution manual somewhere in the net...)

Now you can

#### 2) Create the interface
All you need is adding the following **one line** of code
  somewhere **before** the code that uses it:
```{r,message=TRUE}
ntbt_lm <- ntbt_function_formula_data
```

Hard?

(You see, it is not enough that `intubate` is a lazy package. It also
 promotes laziness).
 
Remember: names of interfaces must start with
       `ntbt_` followed by the name of the function
       (`lm` in this case) you want to interface.

#### 3) ...
(There is no step 3, That would be too much)

You can now use the interface function in any context
in which you would use the original function.
If you do not want to name the parameters, just remember to **switch**
the order of `formula` and `data` arguments when using the interface
(first `data` argument and then `formula` argument).
As usual, you can put them in any order if you name the arguments 
(actually, there are cases in which this is not true. More on this
later)
```{r}
fit <- ntbt_lm(tmp, stweight ~ stheight)
summary(fit)
```

Of course you should want to use the interface in a pipeline context.
Otherwise, `intubate` is virtually worthless.
```{r}
tmp %>%
  ntbt_lm(stweight ~ stheight) %>%
  summary()
```

Adding interfaces to the `intubate` package also represent one-liners for me.

(Did you think I would work more than you?)

The time consuming part on my side is to prepare
the documentation, that certainly needs improvement, and make sure
the examples work.

By the way, if it weren't for the examples, `intubate` would
only have dependencies on `base`. The implementation is extremely lean.

## Discussion
#### Disclaimer:
*I have a vested interest in making* `intubate` *a success
             for egotistical purposes. As such, I may
             be overstating the strengths and understating the
             weaknesses (weaknesses?? which weaknesses??) of* `intubate`.
             *More than a discussion, this can be easily considered
             like a sales pitch for a product of dubious quality.*
             
             You have been warned. Continue at your own risk.

### What can you do if you do not want to use `intubate` and you still want to use these kind of functions in pipelines?

#### Example 1:
`lm` can still be added directly to the pipeline,
without error, by specifying the name of the parameter
associated with the model (`formula` in this case).
```{r}
tmp %>%
  lm(formula = stweight ~ stheight)
```

(So what is the big fuss about `intubate`?)

The drawback of this approach is that not all functions
use `formula` to specify the model.

So far I have encountered 5 variants:

* `formula`
* `x`
* `object`
* `model`, and
* `fixed`

This means you will have to know and remember
(yes, also some months from now) which name has been
assigned to the model by **each**
particular function.

(OK, OK, you don't need to remember. You
can go back to the documentation... over and over again!)


In fact, the following are examples of functions using the other variants.

#### Example 2:
Using `xyplot` directly in a data pipeline will raise an error
```{r, eval=FALSE}
library(lattice)
iris %>%
  xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         scales = "free", layout = c(2, 2),
         auto.key = list(x = .6, y = .7, corner = c(0, 0)))
```

unless `x` is specified.
```{r}
iris %>%
  xyplot(x = Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         scales = "free", layout = c(2, 2),
         auto.key = list(x = .6, y = .7, corner = c(0, 0)))
```

#### Example 3: 
Using `tmd` (a *different* function in the *same* package)
directly in a data pipeline will raise an error
```{r, eval=FALSE}
library(lattice)
iris %>%
  tmd(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
      scales = "free", layout = c(2, 2),
      auto.key = list(x = .6, y = .7, corner = c(0, 0)))
```

unless `object` is specified.
```{r}
iris %>%
  tmd(object = Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
      scales = "free", layout = c(2, 2),
      auto.key = list(x = .6, y = .7, corner = c(0, 0)))
```


#### Example 4:
Using `gls` directly in a data pipeline
            will raise an error
```{r, eval=FALSE}
library(nlme)
Ovary %>%
  gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
      correlation = corAR1(form = ~ 1 | Mare))
```

unless `model` is specified.
```{r}
Ovary %>%
  gls(model = follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
      correlation = corAR1(form = ~ 1 | Mare))
```


#### Example 5:
Using `lme` directly in a data pipeline
            will raise an error
```{r, eval=FALSE}
library(nlme)
try(Orthodont %>%
      lme(distance ~ age))
geterrmessage()
```

unless `fixed`(!) is specified.
```{r}
Orthodont %>%
  lme(fixed = distance ~ age)
```

##### (Subliminal message 1:
*there may be many much more different variants - possibly thousands -
lurking around in the darkness. They may hunt you and hurt you... badly...
`intubate` will keep you warm and safe, in a happy place where
everybody loves you and nothing wrong can happen to you.*)

 I find that having to remember the name of the
 parameter associated to the model in each case
 is unfortunate, error prone, and gives an
 inconsistent look and feel to an otherwise elegant
 interface.
 
 Moreover, it is consider good practice 
 in R to not specify the name of the first two parameters, and
 name the remaining.
 
 (Note the lack of citation to such categorical
 statement... ughh, sheer desperation).
 
 Not having to specify the name of the
 model argument completely hides the heterogeneity of names
 that can be associated with it. You only write the model
 and completely forget which name has been assigned to it.

##### (Subliminal message 2:

- *not using `intubate` => uncool*,
- *using `intubate` => extremely cool*)

### Other nightmares around the corner
 If you are still not convinced (well, why should you?),
 be aware that there are functions that rely on the order of the parameters
 (such as `aggregate`, `cor.test` and other 28 I found so far) that will still
 raise an error *even if you name the model*.
 
 Did you know that there are cases (and for very good reasons), where it is *not
 true* that if in a function call you name the parameters you can write them in any
 order you want?
 
 You don't believe it? How about the following
 examples corresponding to `cor.test`?

#### 1) Unnamed parameters in the natural order. Works
```{r}
cor.test(~ CONT + INTG, USJudgeRatings)
```
#### 2) Named parameters in the natural order. Works
```{r}
cor.test(formula = ~ CONT + INTG, data = USJudgeRatings)
```

#### 3) Named parameters with the order changed. Doesn't work!
```{r, eval=FALSE}
cor.test(data = USJudgeRatings, formula = ~ CONT + INTG)
```

(Convinced?)

So let's see what happens if we want to add these cases to the `%>%` pipeline.

#### Example of error 1: `cor.test`
Using cor.test directly in a data pipeline
       will raise an error
```{r, eval=FALSE}
USJudgeRatings %>%
  cor.test(~ CONT + INTG)
```

*even* when specifying `formula` (as it should be according to
the documentation).
```{r, eval=FALSE}
USJudgeRatings %>%
  cor.test(formula = ~ CONT + INTG)
```

Was it `y` then?
```{r, eval=FALSE}
USJudgeRatings %>%
  cor.test(y = ~ CONT + INTG)
```

Nope...

Was it `x` then?
```{r, eval=FALSE}
USJudgeRatings %>%
  cor.test(x = ~ CONT + INTG)
```

Nope...

#### Example of error 2: `aggregate`
Using `aggregate` directly in a data pipeline
         will raise an error
```{r, eval=FALSE}
ToothGrowth %>%
  aggregate(len ~ ., mean)
```

even when specifying `formula`
```{r, eval=FALSE}
ToothGrowth %>%
  aggregate(formula=len ~ ., mean)
```

or other variants.

#### Example of error 3: `lda`
Using `lda` directly in a data pipeline
         will raise an error
```{r, eval=FALSE, message=FALSE}
library(MASS)
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))
Iris %>%
  lda(Sp ~ .)
```

even when specifying `formula`.
```{r, eval=FALSE}
Iris %>%
  lda(formula = Sp ~ .)
```

or other variants.

Let's try another strategy. Let's see
if the %$% operator, that
expands the names of the variables inside
the data structure, can be of help.
```{r, eval=FALSE}
Iris %$%
  lda(Sp ~ .)
```

Still no...

One last try, or I give up!
```{r}
Iris %$%
  lda(Sp ~ Sepal.L. + Sepal.W. + Petal.L. + Petal.W.)
```

**Finally!** But... we had to specify all the variables 
(and they may be a lot), and use `%$%` instead of `%>%`.

There is still another workaround that allows
these functions to be used directly in a pipeline.
It requires the use of another function (`with`)
encapsulating the offending function. Here it goes:

```{r}
Iris %>%
  with(lda(Sp ~ ., .))
```

In the case of `aggregate` it goes like
```{r}
ToothGrowth %>%
  with(aggregate(len ~ ., ., mean))
```  
(Do you like it? Do you consider it safe for
your children? Really? Come on! What kind of father are you???
You must be one of those that feed unpasteurized
milk to them... Shame on you!)

In addition, there is the added complexity of
interpreting the meaning of each of those `.`
(unfortunately they do not mean the same)
which may cause confusion, particularly at a future
time when you may have to remember why you had to
do *this* to *yourself*.

 (Hint: the first is specifying to include in the
       rhs of the model all the variables in the data but `len`,
       the second is the name of the data
       structure passed by the pipe. Yes, it is called `.`!)

 Undoubtedly, there may be more elegant workarounds that
 I am unaware of. But the point is that, no matter how elegant,
 they will be, well,
 *still* workarounds. You want to *force* unbehaving functions
 into something that is unnatural to them:
 
* In one case you had to name the parameters,
* in the other you had to use `%$%` instead of `%>%` and where not allowed
 to use `.` in your model definition,
* if you wanted to use `%>%` you had to use
 also `which` and include `.` as the second parameter.
 
Does this sound right to you?
 
I certainly do not want to be distracted implementing workarounds
 when I am supposed to concentrate in producing 
 the right statistical analysis.

The idea of avoiding such "hacks"
 motivated me to write `intubate`.

(That was low, please! Were you really *that* desperate
that you had to use the word *motivation* to try to make a sell? Come on!
What is the first thing they teach you in *Trickery 101*?)

### Which was, again, the `intubate` alternative?
(Well... if you insist...)

#### For Example 1:
No need to specify `formula`.
```{r}
tmp %>%
  ntbt_lm(stweight ~ stheight)
```

#### For Example 2:
No need to specify `x`.
```{r}
iris %>%
  ntbt_xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
              scales = "free", layout = c(2, 2),
              auto.key = list(x = .6, y = .7, corner = c(0, 0)))
```

#### For Example 3:
No need to specify `object`.
```{r}
iris %>%
  ntbt_tmd(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
           scales = "free", layout = c(2, 2),
           auto.key = list(x = .6, y = .7, corner = c(0, 0)))
```

#### For Example 4:
No need to specify `model`.
```{r}
Ovary %>%
  ntbt_gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
           correlation = corAR1(form = ~ 1 | Mare))
```

#### For Example 5:
No need to specify `fixed`.
```{r}
Orthodont %>%
  ntbt_lme(distance ~ age)
```

#### For Example of error 1:
It simply works.
```{r}
USJudgeRatings %>%
  ntbt_cor.test(~ CONT + INTG)
```

#### For Example of error 2:
It simply works.
```{r}
ToothGrowth %>%
  ntbt_aggregate(len ~ ., mean)
```

#### For Example of error 3:
It simply works.
```{r, message=FALSE}
Iris %>%
  ntbt_lda(Sp ~ .)
```

 I think the approach `intubate` proposes
 looks consistent, elegant, simple and clean,
 less error prone, and easy to follow
 
 (But, remember, I have a vested interest).

 After all, the complication should be in
 the analysis you are performing,
 and not in how you are performing it.

### Conclusions
(making a huge effort to seem unbiased):

* You can use `intubate` to provide a consistent
 look and feel that allows to use non-pipe-aware
 functions in data science pipelines without
 having to rely on hacks of different levels of
 complexity.
 
* You can use the machinery provided
 to create "on demand" interfaces that have not
 been implemented (and may never be).
 
* The *real thing* is done by the *interfaced* function,
  not by the *interface* function. `intubate` simply *sells air* and
  takes all the credit by providing *only* a refined syntactic
 sugar (and let's not start on the
 potential consequences that the consumption of
 refined sugars may, or may not, have on your health...) that
 will be more or less palatable according to your
 individual taste.
 
* Perhaps the fact that the documentation
 provides working examples of statistical
 and machine learning procedures suitable
 for data science is a plus.
 
### Bottom line
 At the end of the day, you can certainly be a
 very successful data scientist (yes, what
 in the past may have been referred to as
 *a statistician with some
 computer skills*) if you already
 feel confident with your ways and decide
 not to use `intubate`. After all, it is just
 another tool.
 
 
### Final thoughts for new data scientists, unrelated to `intubate`.
(Please skip it if you still believe in Santa)

I am *not* stating that, in
the particular case presented at the beginning of the document, doing a
regression analysis is the correct thing to do.
I am *neither* stating it is wrong. I simply didn't put any effort
in establishing the merit or not of doing a regression analysis
on that particular data. This would entail asking a lot of questions
about how that data was collected. I just didn't do that. It was a little
on purpose so I can have the discussion that follows.

That example and the rest provided in this document
are only for *illustration purposes* related strictly to the *computational techniques*
described, and have nothing to do with the validity or not of using any
particular statistical methodology on any particular data.

(Well, the rest of the examples were taken from the help of the interfaced functions.
I want to believe that the authors put some effort in establishing that
they were doing what they were supposed to do.)

Sure, you may argue that,
after all, in the case of linear regression
the fitting procedure is no other thing than an optimization technique
(minimization of the sum of squared residuals) completely unrelated to
statistics, so why can't you
just find the line of best fit for the sake of it, even if it doesn't make any sense and
does not represent anything? Touché, you got me! (You are good!) You can, and nobody is
going to throw you in jail for that.

But do you want to make population parameter
estimation, statistical inferences such as testing if the
population parameters are different than
zero, or confidence intervals of the population parameters, or confidence bands
of the population regression line, or prediction intervals for
an observation?

Then you should probably start
asking yourself, *before* doing anything, if the assumptions of the linear regression *model* are met or not,
and if not, how bad is the violation of such assumptions (what about
independence of the Y? Are you sure they don't form a time series?
Are the errors iid normally distributed with mean zero and constant variance? Are you sure the error variance doesn't change when you
change the values of the independent variable?).

The statistical function (every statistical function) will only perform calculations and spit out a
collection of numerical values whose interpretation will *only* make sense
provided the data - and *you* are responsible for the data you use on the 
statistical analysis - reasonably
follows the assumptions of the particular statistical model you are
entertaining. The more you depart from the assumptions, the less interpretable
your results become.

Always remember that *there is no provision coded in that black box that is the statistical function that will protect you from
doing a statistical analysis on the wrong data*. Nothing will be corrected
or compensated on your behalf. For example, if the methodology is expecting a random
sample (the vast majority do) and you are not providing one, no *magical trick* will convert
your non-random-sample into one that is random and satisfies all the assumptions.

(Remember I warned you to skip this section if you still believed in Santa?)

Maybe you shouldn't run a statistical
procedure just *because you can*, and then report results
with interpretations that may make no meaning at all.

If you are not truly confident on what you
are doing, perhaps your best move should be to consult first
with your PCS (Primary Care Statistician) before doing something
you may regret down the line.
