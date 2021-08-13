---
layout: post
title: "More advanced plots"
subtitle: "Creating fancy figures"
background: '/img/posts/second_plot/saskuva.jpg'
---

## In this blog post we are replicating the picture below originally created in SAS

![Original SAS plot](/img/posts/second_plot/saskuva.jpg)<!-- -->

We will be using library called tidyverse in this tutorial. Tidyverse is a collection of 
packages that share underlying design philosophy, grammar and data structures. Dplyr from
tidyverse provides useful "pipes" that allows piping data forward into another expression
or funtion call.

``` r
library(tidyverse)
```

You can find more information about tidyverse and its other packages from online documentation.
Lets first generate some data to work with that we can use in our figure.

``` r
n_pat <- 25
patient <- 1:n_pat
censoring <- ceiling(rexp(n_pat, 1/30))
tumour_shrink <- (rbeta(n_pat, 2, 2) - 0.5) * 100

n_cytokines <- 15
cytokines <- paste("Cytokine", 1:n_cytokines)

response <- sample(c("PR", "NE", "CR", "PD", "SD"), size=n_pat,
                   replace = T)

missing_combination <- sample(c(TRUE, FALSE), size=n_pat, replace=T, prob = c(0.1, 0.9))

changes <- matrix(runif(n_pat * n_cytokines, 1, 100), nrow=n_pat, ncol=n_cytokines)
changes[sample(1:dim(changes)[1], 4, replace = FALSE), sample(1:dim(changes)[2], 5, replace = F)] <- NA

df <- data.frame(patient, censoring, tumour_shrink, changes, missing_combination)
colnames(df) <- c("patient", "censoring", "tumour_shrink", cytokines, "missing_combination")
head(df)
```

    ##    patient censoring tumour_shrink Cytokine 1 Cytokine 2 Cytokine 3 Cytokine 4  Cytokine 5 Cytokine 6 Cytokine 7 Cytokine 8 Cytokine 9 Cytokine 10  Cytokine 11 Cytokine 12 Cytokine 13 Cytokine 14 Cytokine 15 
    ## 1        1        50   -33.5020150  76.692716  12.905816  51.320504    6.95165    3.702113  80.529008  52.739191  35.523220  20.034390   98.995443     52.34731   89.944140   22.969047   86.286218   37.865428
    ## 2        2        18   -34.3932674  71.841917  94.354270   4.175872   40.83416   78.104306  95.018387  86.157191  44.547260  66.223263    4.477640     19.13054   57.357747   66.792806   57.220612   71.090477
    ## 3        3        19    25.5744672         NA         NA  75.877590   51.54885   59.516219  47.779858  22.964046  20.790171  27.846610   46.499506           NA   42.132381   26.674702          NA          NA
    ## 4        4        10     4.2591308  90.204811  36.336677  39.754126   72.06269    1.586673  60.106080  40.002346  47.315590  56.189063   78.099096     46.84222    6.844924   80.998685   77.085822   38.931028
    ## 5        5        14    -8.4798810  33.499890  13.695571  28.529885   87.61651   99.882826  71.494717  60.329041  58.260342  51.893355   78.442637     41.88079   75.042574   58.337938   78.939537    1.698262
          missing_combination
    ## 1                FALSE
    ## 2                FALSE
    ## 3                 TRUE
    ## 4                FALSE
    ## 5                 TRUE


Here we have generated a dataframe containing example patients, how their
tumor size has changed from start of the study until the end of study
and time after they were censored from the study (quit, died, etc). On
top of that we also have measurements on different cytokine levels.
Note that data has missing values indicated by NA. In the dataframe 
there is a column called missing\_combination which 
indicates that there was problems while gathering the data. TRUE values indicates 
problems and FALSE values indicate the data is gathered fine. Note that if you try 
to replicate the code you may get different results. You can set seed using set.seed("Seed number") 
so the data will stay same from run to run

The figure consists of four individual plots. Three smaller plots
stacked on top of each other and larger plot under those three. Lets
create the top most plot first.

``` r
p1 <- df %>%
  mutate(color = case_when(
    response == "PR" ~ "lightgreen",
    response == "NE" ~ "white",
    response == "CR" ~ "darkgreen",
    response == "PD" ~ "red",
    response == "SD" ~ "yellow"
  )) %>% 
  arrange(tumour_shrink) %>% 
  mutate(patient = factor(patient, levels=patient)) %>% 
  ggplot(aes(x=patient, y=1, fill=color)) +
  geom_raster() +
  geom_tile(color="black", size=1) +
  geom_text(aes(label=response), size=3) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust=0.57, size = 12),
        plot.margin = unit(c(5, 0, 0, 0), "pt")) +
  scale_fill_identity() +
  labs(y="Best ov. resp") +
  coord_fixed()
p1
```

![First plot](/img/posts/second_plot/plot1.png)<!-- -->

Everything else looks pretty standard except the arrange() and mutate().
We want to sort our patients by their growth of their tumor. First we
arrange them by the change of size in their tumors and after that we
modify the patient column. This changes from integer into ordinal. Main
point of this is that ggplot fills its value in (0, 1) instead of (0.5,
1.5). We also could have used only ggplot(aes(x = factor(patient))) but
in the later plot we also need the numerical value. So for the
consistency we use this approach.

scale\_fill\_identity() is useful when you want to set the colors
manually using mutate and if/else conditions.

The second and third plot are fairly similar to the first one. Again we
are using “hacks” to get our plot looking correct. We pass the patients
as x-values and keep the y-value at constant 1. In each square we plot
value that we want to plot (censoring), pass the colors in aes(…,
fill=color) and finally create the black lines around the square with
geom\_tile.

Onto the next plot!

``` r
p2 <- df %>%
  arrange(tumour_shrink) %>% 
  mutate(patient = factor(patient, levels=patient)) %>% 
  mutate(color = ifelse(missing_combination, "white", "gray")) %>% 
  ggplot(aes(x=factor(patient), y=1, fill=color)) +
  geom_raster() +
  geom_tile(color="black", size=1) +
  geom_text(aes(label=censoring), size=3) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust=0.57, size=12),
        plot.margin = unit(c(-5, 0, 0, 0), "pt")) +
  scale_fill_identity() + 
  labs(y="Censoring")+
  coord_fixed()
p2
```

![Second plot](/img/posts/second_plot/plot2.png)<!-- -->

This plot is again similar to the previous two and the code seems self explatory 
if you understood how to make the first two. Main differences in this section 
are modifying the scale\_fill\_gradient() so we get a nice gradient of colors 
from minimum of tumour\_shrink variable to the maximum value.


``` r
p3 <- df %>% 
  arrange(tumour_shrink) %>% 
  mutate(patient = factor(patient, levels=patient)) %>% 
  ggplot(aes(x=patient, y=1, fill=tumour_shrink)) +
  geom_raster(alpha=0.8) +
  geom_tile(color="black", size=1) +
  geom_text(aes(label=formatC(tumour_shrink, 0, format="f")), 
            size=3) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_text(angle = 0, vjust=0.57, size=12),
        plot.margin = unit(c(-5, 0, 0, 0), "pt"),
        legend.position = "none") +
  scale_fill_gradient(low="green", high="red") +
  labs(y="Tumour shrink")+
  coord_fixed()
p3
```

![Third plot](/img/posts/second_plot/plot3.png)<!-- -->

Now we need to transform the dataframe into long format and normalize
the values to be in the \[-100, 100\] range. For this we are using function

![Scaling equation](/img/posts/second_plot/equation.png)<!-- -->

In the previous equation x' is the scaled vector of values and x is the original 
vector. The fraction inside parenthesis normalizes the *x* values between
\[0, 1\] and then we transform them to desired \[-100, 100\] range. Here
is that as a R function.

``` r
normalize <- function(x, na.rm = TRUE) {
  up = x - min(x, na.rm=T)
  down = max(x, na.rm=T) - min(x, na.rm=T)
  return((2 * (up / down) - 1) * 100)
}
```

In the next block we will pivot the dataframe into long format and apply
our normalization function to all not NaN values. We are also creating a
column called pat which is factor(patient) but with numerical columns.
This was needed so we can sort the values in the last plot with the
tumour\_shrink values. To mimic some squares flagged with stars in the
original picture I decided to add column called “important” which we
flag with star. It could be anything we want but I decided that I would
flag values that had big absolute scaled values.

``` r
cdf <- df %>% 
  pivot_longer(all_of(cytokines)) %>% 
  mutate(scaled_val = normalize(value)) %>% 
  mutate(important = ifelse((abs(scaled_val) > 85), TRUE, FALSE)) %>% 
  replace_na(list(important = FALSE)) %>% 
  arrange(tumour_shrink) %>% 
  mutate(pat=factor(patient, levels = rev(unique(patient)), ordered=TRUE))
```

With most of the work done with creating the dataframe that we want to
plot it is pretty easy to create the plot from that. The plot itself is
similar to one created in the previous post.

``` r
p4 <- cdf %>% 
  ggplot(aes(x=pat, y=name, fill=scaled_val)) +
  geom_raster(alpha=0.85) +
  geom_text(data=filter(cdf, important), aes(label="★"), colour="black",
            size=8, vjust=0.2, alpha=0.9) +
  scale_fill_gradient2(low="blue", mid="white", high="red", guide="none") +
  scale_x_discrete(labels = paste0("Pat ", unique(cdf$patient))) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=-45, hjust=0.3),
        plot.margin = unit(c(10, 5, 5, 5), "pt")) +
  coord_fixed()
p4
```
![Fourth plot](/img/posts/second_plot/plot4.png)<!-- -->

Now all we need to do is to combine all the plots together. This time
there is no need to use cowplot as we can use a bit simpler methods from
library called Patchwork. Patchwork is a brilliant library that allows
joining plot using arithmetic operations. You may have been wondering
why we need to specify the plot margins. The three plots on top of the
bigger plot are tightly together and to mimic that we need to remove the
plot margins.

``` r
library(patchwork)
p1 / p2 / p3 / p4
```

![Final plot](/img/posts/second_plot/plot_final.png)<!-- -->

This mimics the original image pretty good. We also could have made all
three top plots in a single plotting function call but I think the ideas are more
easy to generalize to other plots and figures if we do them one by one.
In this approach we needed to play a a lot with the white space around the plot and
plot arrangement functions to create tight grouping of plots. Each
approach has its pros and cons.

Unfortunately for this post we can't publish the original SAS code.

Mikael Roto
14/8/2021