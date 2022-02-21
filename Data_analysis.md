Analysis
================
Nazmi Aydın
21 02 2022

This Exploratory Data Analytics Project aims to analyze
“googleplaystore.csv” data which includes the Google Play Store apps
with the help of RStudio. The “googleplaystore.csv” data includes 13
variables which are App Name, Category, Rating, Reviews, Size, Installs,
Type, Price, Content.Rating, Genres, Last.Updated, Current.Ver,
Android.Ver. To analyze the data, I find the appropriate needs such as
one-variable and two-variable plots, an applicable model such as linear
regression, logical regression or naive bayes. Before the analysis, with
the help of tidyverse package, I did some data cleaning to interpret the
plots and regression as in its correct way. To interpret the plots, I
use ggplot2 package. My plots has based on Category, Reviews, Rating and
App Types. The results and plots were stated below the report.

## Reading the Packages

In this project, I used ggplot2 and tidy verse packages to build the
plots. ggplot2 package was used to create the basis of the plots and
tidyverse package was used to convert some categorical variables to
numerical variables.

``` r
library(ggplot2)

library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 4.1.2

## Reading The Data

``` r
setwd("C:/Users/Mbappe from Niğde/Downloads")
getwd()
```

    ## [1] "C:/Users/Mbappe from Niğde/Downloads"

``` r
apps <- read.csv("googleplaystore.csv")
str(apps)
```

    ## 'data.frame':    10841 obs. of  13 variables:
    ##  $ App           : chr  "Photo Editor & Candy Camera & Grid & ScrapBook" "Coloring book moana" "U Launcher Lite â\200“ FREE Live Cool Themes, Hide Apps" "Sketch - Draw & Paint" ...
    ##  $ Category      : chr  "ART_AND_DESIGN" "ART_AND_DESIGN" "ART_AND_DESIGN" "ART_AND_DESIGN" ...
    ##  $ Rating        : num  4.1 3.9 4.7 4.5 4.3 4.4 3.8 4.1 4.4 4.7 ...
    ##  $ Reviews       : chr  "159" "967" "87510" "215644" ...
    ##  $ Size          : chr  "19M" "14M" "8.7M" "25M" ...
    ##  $ Installs      : chr  "10,000+" "500,000+" "5,000,000+" "50,000,000+" ...
    ##  $ Type          : chr  "Free" "Free" "Free" "Free" ...
    ##  $ Price         : chr  "0" "0" "0" "0" ...
    ##  $ Content.Rating: chr  "Everyone" "Everyone" "Everyone" "Teen" ...
    ##  $ Genres        : chr  "Art & Design" "Art & Design;Pretend Play" "Art & Design" "Art & Design" ...
    ##  $ Last.Updated  : chr  "January 7, 2018" "January 15, 2018" "August 1, 2018" "June 8, 2018" ...
    ##  $ Current.Ver   : chr  "1.0.0" "2.0.0" "1.2.4" "Varies with device" ...
    ##  $ Android.Ver   : chr  "4.0.3 and up" "4.0.3 and up" "4.0.3 and up" "4.2 and up" ...

``` r
summary(apps)
```

    ##      App              Category             Rating         Reviews         
    ##  Length:10841       Length:10841       Min.   : 1.000   Length:10841      
    ##  Class :character   Class :character   1st Qu.: 4.000   Class :character  
    ##  Mode  :character   Mode  :character   Median : 4.300   Mode  :character  
    ##                                        Mean   : 4.193                     
    ##                                        3rd Qu.: 4.500                     
    ##                                        Max.   :19.000                     
    ##                                        NA's   :1474                       
    ##      Size             Installs             Type              Price          
    ##  Length:10841       Length:10841       Length:10841       Length:10841      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Content.Rating        Genres          Last.Updated       Current.Ver       
    ##  Length:10841       Length:10841       Length:10841       Length:10841      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Android.Ver       
    ##  Length:10841      
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

## Data Cleaning And Correcting Data Types

First of all, I need to clean our data from some non-numeric signs which
are “+”, “+”, “,” and etc. Otherwise,I may have some problems making
plots in R. \* In googleplaystore dataset, before I converted some chr
variable into num variable, there were ***12*** ***chr*** and ***1***
***num*** variable. \* After I converted ***3*** of the ***chr***
variable to ***num*** variable, I have ***12*** ***chr*** and ***4***
***num*** variable. \* I did this convertion to apply ggplot package
more clearly and correctly.

``` r
apps$Reviews_num <- as.numeric(apps$Reviews)
```

    ## Warning: Zorlamadan dolayı ortaya çıkan NAs

``` r
apps$Price_num2 <- as.numeric(gsub("[//$,]", "", apps$Price))
```

    ## Warning: Zorlamadan dolayı ortaya çıkan NAs

``` r
apps$Installs_num <- as.numeric(gsub("[//+,]", "", apps$Installs))
```

    ## Warning: Zorlamadan dolayı ortaya çıkan NAs

``` r
apps$Type_Na <- na.omit(apps$Type)

new_df <-apps %>%
  filter(str_detect(Genres, "0"))


apps <- apps[-c(9149),]
apps <- apps[-c(10473),]
str(apps)
```

    ## 'data.frame':    10839 obs. of  17 variables:
    ##  $ App           : chr  "Photo Editor & Candy Camera & Grid & ScrapBook" "Coloring book moana" "U Launcher Lite â\200“ FREE Live Cool Themes, Hide Apps" "Sketch - Draw & Paint" ...
    ##  $ Category      : chr  "ART_AND_DESIGN" "ART_AND_DESIGN" "ART_AND_DESIGN" "ART_AND_DESIGN" ...
    ##  $ Rating        : num  4.1 3.9 4.7 4.5 4.3 4.4 3.8 4.1 4.4 4.7 ...
    ##  $ Reviews       : chr  "159" "967" "87510" "215644" ...
    ##  $ Size          : chr  "19M" "14M" "8.7M" "25M" ...
    ##  $ Installs      : chr  "10,000+" "500,000+" "5,000,000+" "50,000,000+" ...
    ##  $ Type          : chr  "Free" "Free" "Free" "Free" ...
    ##  $ Price         : chr  "0" "0" "0" "0" ...
    ##  $ Content.Rating: chr  "Everyone" "Everyone" "Everyone" "Teen" ...
    ##  $ Genres        : chr  "Art & Design" "Art & Design;Pretend Play" "Art & Design" "Art & Design" ...
    ##  $ Last.Updated  : chr  "January 7, 2018" "January 15, 2018" "August 1, 2018" "June 8, 2018" ...
    ##  $ Current.Ver   : chr  "1.0.0" "2.0.0" "1.2.4" "Varies with device" ...
    ##  $ Android.Ver   : chr  "4.0.3 and up" "4.0.3 and up" "4.0.3 and up" "4.2 and up" ...
    ##  $ Reviews_num   : num  159 967 87510 215644 967 ...
    ##  $ Price_num2    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Installs_num  : num  1e+04 5e+05 5e+06 5e+07 1e+05 5e+04 5e+04 1e+06 1e+06 1e+04 ...
    ##  $ Type_Na       : chr  "Free" "Free" "Free" "Free" ...

``` r
apps <- na.omit(apps)
```

## Distribution of App Categories

#### The aim of choosing this plot

Google Play Store separates apps into categories. To make our analysis
stronger we have to answers these questions:

Which category is the most active in the market?

Is any particular category dominating the market?

Which categories have the smallest number of apps?

There are ***33*** categories in my data set in present. The categories
which are ***Family*** and ***Game*** have the highest market
popularity. Surprisingly, ***Business***, ***Medical*** and ***Tools***
categories are popular. However, also surprisingly, the category
***Shopping*** has a not considerable count. In addition, I think that
the number of apps in category ***Medical*** is considerable because of
the \*\*\*\*COVID-19\*\*\* situation. As an example, there are a lot of
medical apps such as HES and E-Nabız. However, for the ***Shopping***
category, I can bound the reason behind of this reasonable number to
***COVID-19***. The interpretation of the Category section has placed on
y-axis to see all of them more precisely.

``` r
plot(ggplot(aes(x= , y=Category ), data = apps) + 
  xlab("Number of Apps") +
  ylab("Category") +
  geom_bar(width = 0.5) 
)
```

![Figure 1: Distribution of App
Categories](Data_analysis_files/figure-gfm/unnamed-chunk-4-1.png)

## Distribution of Reviews

In the histogram, I see that the distribution is ***right skewed***
which means that not all apps in our data has large number of views. I
found this information interesting because I think that the number of
the large viewed applications would be more. The reason behind is that
in our generation, we have a huge application pool and a huge percentage
of the app companies making advertisements of their apps on YouTube,
Tiktok, Twitter, Instagram, etc. everyday.

``` r
plot(ggplot(aes(x =Reviews_num), data = subset(apps)) + 
  geom_histogram(color = "black", fill = "blue") + 
  scale_x_continuous(limits = c(0,10000), breaks = seq(0, 10000, 500)) + 
  scale_y_continuous(limits = c(0,800))+
  ylab("Total Number")+
  xlab("Reviews Distribution"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 4279 rows containing non-finite values (stat_bin).

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![Figure 2: Distribution of
Reviews](Data_analysis_files/figure-gfm/unnamed-chunk-5-1.png)

## Distribution of Rating

#### The aim of choosing this plot

To see the rating distribution of this data. If the general rating
percentage high, the Google Play Store users are very enjoying many of
the apps in the app pool, but of course not all of them.

In this plot, it has seen that the graph is left-skewed. You might also
notice that there is a gap between 0-1.2 and 1.2-1.4. The reason why is
that the “count” variable has a very low value in these interval. In
general, the apps are highly rated by the users. New apps that are
released are likely to have high ratings unless there is a notable
exception. \* In order to see more clearly, interpretation of the
histogram were made with red color. \* Additionally, the y-axis’ limit
has stabled in the value 1200. If the limit of the y-axis is more than
1200, it has seen that more intervals in x-axis might dissapear.

``` r
plot(ggplot(aes(x = Rating), data = subset(apps)) + 
  geom_bar(color = "black", fill = "red") + 
  xlab("Rating Distribution") +
  ylab("Number of Ratings") +
  scale_x_continuous(limits = c(2.35,5), breaks = seq(2.35, 5, 0.2)) + 
  scale_y_continuous(limits = c(0,1200)) +
  coord_trans(x = "sqrt"))
```

    ## Warning: Removed 110 rows containing non-finite values (stat_count).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![Figure 3: Distribution of
Rating](Data_analysis_files/figure-gfm/unnamed-chunk-6-1.png)

### Popularity of Paid apps vs Free Apss

#### The aim of choosing this plot

To have a general knowledge of the popularity of the paying status of
the apps. Does it make more sense to make the app paid?

There are 5 pricing options available in the Google Play Store Market
which are: “Freemium”, “Paymium” “Paid”, “Free” and “Subscription”. In
this box plot, I only include “Free”, “Paid” to have a perspective about
paying or not paying for the apps.

I found that paid apps have fewer installs than free apps, however, the
difference is not as significant as we would be expected.

``` r
plot(ggplot(aes(x= Type_Na , y= Installs_num), data = apps) + geom_boxplot() + 
  xlab("App Types") +
  ylab("Number of Installs") +
  scale_y_continuous(limits = c(1,10000), breaks = c(1, 10000, 1000)))
```

    ## Warning: Removed 6563 rows containing non-finite values (stat_boxplot).

![Figure 4: Popularity of Paid apps vs. Free
Apps](Data_analysis_files/figure-gfm/unnamed-chunk-7-1.png)

### Reviews Number vs. Rating

#### The aim of choosing this plot

I choose this graph to analyze if the high rated apps has large number
of views, or the opposite, if the app has large number of views, is it
low rated.

In this two variable plot, it is seen that the distribution is
***left-skewed.*** While plotting the ***non-sqrt*** one of this plot,
we have struggled to observe and analyze the distribution and
relationship. To detect and analyze and make comments about the
distribution more properly and correctly, I converted our function to
***sqrt.*** There are some ***outliers*** at the ending part of our
distribution.

``` r
plot(ggplot(aes(x =Rating, y = Reviews_num), data = apps) + 
  geom_point() +
  xlab("Distribution of Ratings") + 
  ylab("Number of Reviews") +
  scale_x_continuous(limits = c(1,5), breaks = seq(1, 5, 0.2)) + 
  scale_y_continuous(limits = c(0,1.1e+07)) + 
  coord_trans(y = "sqrt"))
```

    ## Warning: Removed 68 rows containing missing values (geom_point).

![Figure 5: Reviews Number
vs. Rating](Data_analysis_files/figure-gfm/unnamed-chunk-8-1.png)

### Rating vs Price

#### The aim of choosing this plot

I decided to do the Rating vs. Price because if the apps which has high
price has a good rating(or lets say feedback) from the users.

In this two variable plot, I observed the Rating(x-axis) and
Price(y-axis). The distribution looked more of a ***left-skewed.*** In
order to see the distribution between the intervals 1.2-3.8, I limit the
y-axis to 50. I also observed that prices of the apps gets bigger when
their ratings are getting bigger.

-   Additionally, ***sqrt*** function is used on the plots to see the
    distribution more clear.

``` r
plot(ggplot(aes(x =Rating, y = Price_num2), data = apps) + 
  geom_point() +
  xlab("Distribution of Ratings") + 
  ylab("App Prices in Dollars") +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, 0.2)) +
  scale_y_continuous(limits = c(0,15), breaks = c(0, 15, 5)) + 
  coord_trans(y = "sqrt"))
```

    ## Warning: Removed 46 rows containing missing values (geom_point).

![Figure 6: Rating
vs. Price](Data_analysis_files/figure-gfm/unnamed-chunk-9-1.png) ###
Variable Conversion for Naive Bayes

I converted the Installs, Type of Apps, Rating, Content Rating, Price of
Apps to factor variable to apply Naive Bayes.

``` r
unique(apps$Installs)
```

    ##  [1] "10,000+"        "500,000+"       "5,000,000+"     "50,000,000+"   
    ##  [5] "100,000+"       "50,000+"        "1,000,000+"     "10,000,000+"   
    ##  [9] "5,000+"         "100,000,000+"   "1,000,000,000+" "1,000+"        
    ## [13] "500,000,000+"   "100+"           "500+"           "10+"           
    ## [17] "5+"             "50+"            "1+"

``` r
apps$Installs <- factor(apps$Installs)
a <- table(apps$Installs)
str(apps$Installs)
```

    ##  Factor w/ 19 levels "1,000,000,000+",..: 6 18 11 14 9 15 15 2 2 6 ...

``` r
apps$Genres <- factor(apps$Genres)
apps$Category <- factor(apps$Category)
apps$Type_Na <- factor(apps$Type_Na)
apps$Content.Rating <- factor(apps$Content.Rating)
apps$Reviews <- factor(apps$Reviews)
```

### Naive Bayes

I observed that:

The apps which has higher than 50 million Installs (excluding 100
million) are Free.

On the interval between 10k and 1m Installs, high portion of the apps
are also Free (97.5%, 91%).

``` r
xtabs(~Installs_num + Type_Na , data = apps)
```

    ##             Type_Na
    ## Installs_num Free Paid
    ##        1        1    2
    ##        5        9    0
    ##        10      51   18
    ##        50      42   14
    ##        100    242   67
    ##        500    165   36
    ##        1000   571  142
    ##        5000   365   67
    ##        10000  881  129
    ##        50000  424   43
    ##        1e+05 1060   90
    ##        5e+05  526   12
    ##        1e+06 1553   24
    ##        5e+06  752    0
    ##        1e+07 1248    3
    ##        5e+07  289    0
    ##        1e+08  409    0
    ##        5e+08   72    0
    ##        1e+09   58    0

``` r
apps2 <- with(apps, data.frame(Installs_num = Installs_num, Type_Na = Type_Na, Content.Rating = Content.Rating, Price_num2 = Price_num2 ))

s <- sample(nrow(apps2),nrow(apps2)*.8)
training_set <- apps[s,]
test_set <- apps[-s,]

nb_model1 <- naiveBayes(Installs_num ~ Type_Na + Rating + Content.Rating + Price_num2, data = training_set)
nb_model1
```

    ## 
    ## Naive Bayes Classifier for Discrete Predictors
    ## 
    ## Call:
    ## naiveBayes.default(x = X, y = Y, laplace = laplace)
    ## 
    ## A-priori probabilities:
    ## Y
    ##            1            5           10           50          100          500 
    ## 0.0004004271 0.0009343300 0.0073411639 0.0054725040 0.0340363054 0.0216230646 
    ##         1000         5000        10000        50000        1e+05        5e+05 
    ## 0.0758142018 0.0453817405 0.1081153230 0.0476508275 0.1233315537 0.0588627870 
    ##        1e+06        5e+06        1e+07        5e+07        1e+08        5e+08 
    ## 0.1680459156 0.0822210358 0.1325413775 0.0320341698 0.0429791778 0.0066737854 
    ##        1e+09 
    ## 0.0065403097 
    ## 
    ## Conditional probabilities:
    ##        Type_Na
    ## Y              Free        Paid
    ##   1     0.333333333 0.666666667
    ##   5     1.000000000 0.000000000
    ##   10    0.727272727 0.272727273
    ##   50    0.756097561 0.243902439
    ##   100   0.768627451 0.231372549
    ##   500   0.851851852 0.148148148
    ##   1000  0.790492958 0.209507042
    ##   5000  0.867647059 0.132352941
    ##   10000 0.865432099 0.134567901
    ##   50000 0.907563025 0.092436975
    ##   1e+05 0.922077922 0.077922078
    ##   5e+05 0.977324263 0.022675737
    ##   1e+06 0.983320095 0.016679905
    ##   5e+06 1.000000000 0.000000000
    ##   1e+07 0.997985901 0.002014099
    ##   5e+07 1.000000000 0.000000000
    ##   1e+08 1.000000000 0.000000000
    ##   5e+08 1.000000000 0.000000000
    ##   1e+09 1.000000000 0.000000000
    ## 
    ##        Rating
    ## Y           [,1]      [,2]
    ##   1     5.000000 0.0000000
    ##   5     4.500000 1.3228757
    ##   10    4.570909 0.8931162
    ##   50    4.368293 1.1437742
    ##   100   4.329412 0.8724782
    ##   500   4.160494 0.7775066
    ##   1000  4.068486 0.7884818
    ##   5000  4.023824 0.6480673
    ##   10000 4.033827 0.5911514
    ##   50000 4.032493 0.5497780
    ##   1e+05 4.097186 0.4921625
    ##   5e+05 4.164172 0.4457729
    ##   1e+06 4.210564 0.3547425
    ##   5e+06 4.242532 0.3181901
    ##   1e+07 4.315609 0.2748314
    ##   5e+07 4.352917 0.2242417
    ##   1e+08 4.409006 0.1618033
    ##   5e+08 4.348000 0.1656650
    ##   1e+09 4.251020 0.2052665
    ## 
    ##        Content.Rating
    ## Y       Adults only 18+     Everyone Everyone 10+   Mature 17+         Teen
    ##   1        0.0000000000 1.0000000000 0.0000000000 0.0000000000 0.0000000000
    ##   5        0.0000000000 1.0000000000 0.0000000000 0.0000000000 0.0000000000
    ##   10       0.0000000000 0.8545454545 0.0181818182 0.0545454545 0.0727272727
    ##   50       0.0000000000 0.9756097561 0.0000000000 0.0000000000 0.0243902439
    ##   100      0.0000000000 0.8352941176 0.0078431373 0.0509803922 0.1058823529
    ##   500      0.0000000000 0.8827160494 0.0123456790 0.0493827160 0.0555555556
    ##   1000     0.0000000000 0.8908450704 0.0176056338 0.0246478873 0.0669014085
    ##   5000     0.0000000000 0.9088235294 0.0117647059 0.0323529412 0.0470588235
    ##   10000    0.0000000000 0.8555555556 0.0271604938 0.0370370370 0.0802469136
    ##   50000    0.0000000000 0.8179271709 0.0280112045 0.0392156863 0.1148459384
    ##   1e+05    0.0000000000 0.8127705628 0.0357142857 0.0606060606 0.0909090909
    ##   5e+05    0.0022675737 0.7324263039 0.0612244898 0.0816326531 0.1224489796
    ##   1e+06    0.0007942812 0.7640984909 0.0492454329 0.0532168388 0.1326449563
    ##   5e+06    0.0000000000 0.7467532468 0.0487012987 0.0405844156 0.1639610390
    ##   1e+07    0.0000000000 0.7109768379 0.0694864048 0.0624370594 0.1570996979
    ##   5e+07    0.0000000000 0.6791666667 0.0500000000 0.0541666667 0.2166666667
    ##   1e+08    0.0000000000 0.6801242236 0.1024844720 0.0652173913 0.1521739130
    ##   5e+08    0.0000000000 0.7200000000 0.0600000000 0.0600000000 0.1600000000
    ##   1e+09    0.0000000000 0.6122448980 0.0816326531 0.0000000000 0.3061224490
    ##        Content.Rating
    ## Y            Unrated
    ##   1     0.0000000000
    ##   5     0.0000000000
    ##   10    0.0000000000
    ##   50    0.0000000000
    ##   100   0.0000000000
    ##   500   0.0000000000
    ##   1000  0.0000000000
    ##   5000  0.0000000000
    ##   10000 0.0000000000
    ##   50000 0.0000000000
    ##   1e+05 0.0000000000
    ##   5e+05 0.0000000000
    ##   1e+06 0.0000000000
    ##   5e+06 0.0000000000
    ##   1e+07 0.0000000000
    ##   5e+07 0.0000000000
    ##   1e+08 0.0000000000
    ##   5e+08 0.0000000000
    ##   1e+09 0.0000000000
    ## 
    ##        Price_num2
    ## Y              [,1]       [,2]
    ##   1     0.826666667  0.7583095
    ##   5     0.000000000  0.0000000
    ##   10    1.780000000  5.2167483
    ##   50    0.595853659  1.5627235
    ##   100   2.578627451 25.2097998
    ##   500   0.575740741  2.1851034
    ##   1000  4.366109155 33.5570309
    ##   5000  1.679941176 21.7384775
    ##   10000 2.964074074 29.8284546
    ##   50000 1.456442577 21.1945853
    ##   1e+05 0.825346320 13.2858838
    ##   5e+05 0.071201814  0.5143073
    ##   1e+06 0.045107228  0.4320253
    ##   5e+06 0.000000000  0.0000000
    ##   1e+07 0.008036254  0.2240033
    ##   5e+07 0.000000000  0.0000000
    ##   1e+08 0.000000000  0.0000000
    ##   5e+08 0.000000000  0.0000000
    ##   1e+09 0.000000000  0.0000000

``` r
prediction <- predict(nb_model1, test_set[,-1])
head(cbind(prediction, test_set$Installs_num))
```

    ##      prediction      
    ## [1,]         14 5e+06
    ## [2,]         14 1e+06
    ## [3,]         17 1e+04
    ## [4,]         14 5e+04
    ## [5,]         14 1e+04
    ## [6,]         14 1e+04

``` r
table(prediction, test_set$Installs_num)
```

    ##           
    ## prediction   5  10  50 100 500 1000 5000 10000 50000 1e+05 5e+05 1e+06 5e+06
    ##      1       0   2   1   0   0    0    0     0     0     0     0     0     0
    ##      5       0   0   0   4   3   15    6    16     8     8     2     3     1
    ##      10      0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      50      0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      100     0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      500     0   1   3   5   6   12    9     9     1     1     0     0     0
    ##      1000    0   0   0   0   0    1    2     0     0     0     0     0     0
    ##      5000    0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      10000   0   0   0   0   0    1    0     0     2     0     0     0     0
    ##      50000   0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      1e+05   0   0   0   3   6    9   10     9     7    17     2     3     0
    ##      5e+05   0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      1e+06   0   0   0   0   0    0    1     2     0     0     0     0     0
    ##      5e+06   2  11  11  38  22   95   51   130    73   167    67   228    96
    ##      1e+07   0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      5e+07   0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      1e+08   0   0   0   4   2   12   13    34    19    33    26    84    39
    ##      5e+08   0   0   0   0   0    0    0     0     0     0     0     0     0
    ##      1e+09   0   0   0   0   0    0    0     0     0     0     0     0     0
    ##           
    ## prediction 1e+07 5e+07 1e+08 5e+08 1e+09
    ##      1         0     0     0     0     0
    ##      5         0     0     0     0     0
    ##      10        0     0     0     0     0
    ##      50        0     0     0     0     0
    ##      100       0     0     0     0     0
    ##      500       0     0     0     0     0
    ##      1000      0     0     0     0     0
    ##      5000      0     0     0     0     0
    ##      10000     0     0     0     0     0
    ##      50000     0     0     0     0     0
    ##      1e+05     1     0     0     0     0
    ##      5e+05     0     0     0     0     0
    ##      1e+06     0     0     0     0     0
    ##      5e+06   169    31    48    12     4
    ##      1e+07     0     0     0     0     0
    ##      5e+07     0     0     0     0     0
    ##      1e+08    88    18    39    10     5
    ##      5e+08     0     0     0     0     0
    ##      1e+09     0     0     0     0     0

###Linear Regression

In the Linear Regression, I analyzed Number of Installs by Cost
Type(Free-Paid), Genres, Price, Category, Rating and Number of Reviews.
With the usage of backward, forward and both way selection,
***R-squared*** and ***Adjusted R-squared*** values were as ***0.4305***
and ***0.4283*** which is not high enough for a meaningful relationship.
Additionally the ***p-value*** was ***2.2e-16***. As shown in the
summary part, many of the independent variables have higher p-value than
0.05.

``` r
max_model <- lm(Installs_num~Type_Na+Genres+Price_num2+Category+Rating+Reviews_num, data=apps)
summary(max_model)
```

    ## 
    ## Call:
    ## lm(formula = Installs_num ~ Type_Na + Genres + Price_num2 + Category + 
    ##     Rating + Reviews_num, data = apps)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -690288830   -9733488   -2750550     152941  965936763 
    ## 
    ## Coefficients: (26 not defined because of singularities)
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                  7.426e+06  5.059e+07   0.147
    ## Type_NaPaid                                 -7.929e+06  3.031e+06  -2.616
    ## GenresAction;Action & Adventure             -1.330e+07  1.794e+07  -0.741
    ## GenresAdventure                             -8.985e+06  8.869e+06  -1.013
    ## GenresAdventure;Action & Adventure          -1.303e+06  1.991e+07  -0.065
    ## GenresAdventure;Brain Games                 -1.209e+07  6.945e+07  -0.174
    ## GenresAdventure;Education                   -1.393e+07  4.936e+07  -0.282
    ## GenresArcade                                 1.386e+07  6.028e+06   2.299
    ## GenresArcade;Action & Adventure             -2.029e+07  1.931e+07  -1.050
    ## GenresArcade;Pretend Play                   -1.911e+07  6.941e+07  -0.275
    ## GenresArt & Design                          -1.677e+07  5.108e+07  -0.328
    ## GenresArt & Design;Creativity               -1.887e+07  4.467e+07  -0.422
    ## GenresArt & Design;Pretend Play             -1.739e+07  5.526e+07  -0.315
    ## GenresAuto & Vehicles                       -1.785e+07  5.089e+07  -0.351
    ## GenresBeauty                                -1.815e+07  5.136e+07  -0.353
    ## GenresBoard                                 -4.981e+06  1.139e+07  -0.438
    ## GenresBoard;Action & Adventure              -1.429e+07  4.053e+07  -0.352
    ## GenresBoard;Brain Games                     -1.512e+07  1.933e+07  -0.782
    ## GenresBoard;Pretend Play                    -1.255e+07  6.946e+07  -0.181
    ## GenresBooks & Reference                     -9.916e+06  5.051e+07  -0.196
    ## GenresBooks & Reference;Education           -1.496e+07  4.937e+07  -0.303
    ## GenresBusiness                              -1.547e+07  5.040e+07  -0.307
    ## GenresCard                                  -6.242e+06  1.092e+07  -0.572
    ## GenresCard;Action & Adventure               -1.937e+07  4.937e+07  -0.392
    ## GenresCard;Brain Games                      -1.933e+07  6.941e+07  -0.278
    ## GenresCasino                                -7.443e+06  1.192e+07  -0.624
    ## GenresCasual                                -4.636e+06  7.985e+06  -0.581
    ## GenresCasual;Action & Adventure             -3.176e+07  1.670e+07  -1.901
    ## GenresCasual;Brain Games                    -1.694e+07  2.039e+07  -0.831
    ## GenresCasual;Creativity                     -1.327e+07  2.692e+07  -0.493
    ## GenresCasual;Education                      -1.595e+07  4.053e+07  -0.393
    ## GenresCasual;Music & Video                  -9.328e+06  4.936e+07  -0.189
    ## GenresCasual;Pretend Play                   -1.198e+07  1.436e+07  -0.834
    ## GenresComics                                -1.838e+07  5.107e+07  -0.360
    ## GenresComics;Creativity                     -1.994e+07  8.537e+07  -0.234
    ## GenresCommunication                          3.605e+07  5.040e+07   0.715
    ## GenresCommunication;Creativity              -1.845e+07  6.941e+07  -0.266
    ## GenresDating                                -1.699e+07  5.049e+07  -0.336
    ## GenresEducation                             -1.840e+07  8.292e+06  -2.219
    ## GenresEducation;Action & Adventure          -1.591e+07  2.916e+07  -0.546
    ## GenresEducation;Brain Games                 -1.690e+07  3.564e+07  -0.474
    ## GenresEducation;Creativity                  -1.475e+07  2.727e+07  -0.541
    ## GenresEducation;Education                   -1.961e+07  1.244e+07  -1.577
    ## GenresEducation;Music & Video               -1.696e+07  3.177e+07  -0.534
    ## GenresEducation;Pretend Play                -1.372e+07  1.631e+07  -0.841
    ## GenresEducational                           -1.725e+07  1.429e+07  -1.208
    ## GenresEducational;Action & Adventure         1.689e+05  3.530e+07   0.005
    ## GenresEducational;Brain Games               -1.489e+07  2.914e+07  -0.511
    ## GenresEducational;Creativity                -1.388e+07  3.175e+07  -0.437
    ## GenresEducational;Education                 -1.715e+07  1.344e+07  -1.276
    ## GenresEducational;Pretend Play              -1.079e+07  1.789e+07  -0.603
    ## GenresEntertainment                         -1.432e+07  8.197e+06  -1.748
    ## GenresEntertainment;Action & Adventure      -1.740e+07  4.053e+07  -0.429
    ## GenresEntertainment;Brain Games             -1.880e+07  2.556e+07  -0.735
    ## GenresEntertainment;Creativity              -2.096e+07  4.060e+07  -0.516
    ## GenresEntertainment;Education               -1.855e+07  6.941e+07  -0.267
    ## GenresEntertainment;Music & Video           -1.244e+07  1.542e+07  -0.807
    ## GenresEntertainment;Pretend Play            -1.635e+07  4.936e+07  -0.331
    ## GenresEvents                                -1.874e+07  5.129e+07  -0.365
    ## GenresFinance                               -1.634e+07  5.039e+07  -0.324
    ## GenresFood & Drink                          -1.717e+07  5.068e+07  -0.339
    ## GenresHealth & Fitness                      -1.531e+07  5.040e+07  -0.304
    ## GenresHealth & Fitness;Action & Adventure   -1.768e+07  6.941e+07  -0.255
    ## GenresHealth & Fitness;Education            -2.028e+07  6.941e+07  -0.292
    ## GenresHouse & Home                          -1.715e+07  5.086e+07  -0.337
    ## GenresLibraries & Demo                      -1.769e+07  5.097e+07  -0.347
    ## GenresLifestyle                             -1.686e+07  5.040e+07  -0.335
    ## GenresLifestyle;Education                   -1.911e+07  6.941e+07  -0.275
    ## GenresLifestyle;Pretend Play                -1.048e+07  8.536e+07  -0.123
    ## GenresMaps & Navigation                     -1.641e+07  5.063e+07  -0.324
    ## GenresMedical                               -1.638e+07  5.038e+07  -0.325
    ## GenresMusic                                  2.192e+06  1.550e+07   0.141
    ## GenresMusic & Audio;Music & Video           -1.868e+07  6.941e+07  -0.269
    ## GenresMusic;Music & Video                   -1.408e+07  4.054e+07  -0.347
    ## GenresNews & Magazines                       9.711e+06  5.045e+07   0.193
    ## GenresParenting                             -1.830e+07  5.142e+07  -0.356
    ## GenresParenting;Brain Games                 -1.643e+07  8.536e+07  -0.193
    ## GenresParenting;Education                   -1.661e+07  6.412e+07  -0.259
    ## GenresParenting;Music & Video               -1.772e+07  5.760e+07  -0.308
    ## GenresPersonalization                       -1.491e+07  5.039e+07  -0.296
    ## GenresPhotography                            1.493e+06  5.039e+07   0.030
    ## GenresProductivity                           1.637e+07  5.038e+07   0.325
    ## GenresPuzzle                                -7.610e+06  8.405e+06  -0.905
    ## GenresPuzzle;Action & Adventure             -5.599e+06  3.175e+07  -0.176
    ## GenresPuzzle;Brain Games                    -1.349e+07  1.750e+07  -0.771
    ## GenresPuzzle;Creativity                     -1.937e+07  4.936e+07  -0.392
    ## GenresPuzzle;Education                      -1.985e+07  6.941e+07  -0.286
    ## GenresRacing                                -4.275e+06  8.034e+06  -0.532
    ## GenresRacing;Action & Adventure             -1.303e+07  1.713e+07  -0.760
    ## GenresRacing;Pretend Play                   -1.872e+07  6.941e+07  -0.270
    ## GenresRole Playing                          -1.633e+07  9.410e+06  -1.735
    ## GenresRole Playing;Action & Adventure       -1.689e+07  2.712e+07  -0.623
    ## GenresRole Playing;Brain Games              -1.194e+07  6.941e+07  -0.172
    ## GenresRole Playing;Pretend Play             -1.500e+07  3.175e+07  -0.473
    ## GenresShopping                              -1.374e+07  5.044e+07  -0.272
    ## GenresSimulation                            -1.687e+07  8.676e+06  -1.944
    ## GenresSimulation;Action & Adventure         -1.568e+07  2.194e+07  -0.715
    ## GenresSimulation;Education                  -1.071e+07  4.027e+07  -0.266
    ## GenresSimulation;Pretend Play               -1.742e+07  3.530e+07  -0.494
    ## GenresSocial                                -8.274e+06  5.044e+07  -0.164
    ## GenresSports                                -8.985e+07  1.889e+07  -4.756
    ## GenresSports;Action & Adventure             -1.967e+07  3.531e+07  -0.557
    ## GenresStrategy                              -5.481e+07  9.238e+06  -5.934
    ## GenresStrategy;Action & Adventure           -1.582e+07  4.937e+07  -0.320
    ## GenresStrategy;Creativity                   -2.080e+07  6.941e+07  -0.300
    ## GenresStrategy;Education                    -1.921e+07  6.941e+07  -0.277
    ## GenresTools                                 -8.569e+06  5.031e+07  -0.170
    ## GenresTools;Education                       -1.549e+07  8.536e+07  -0.181
    ## GenresTravel & Local                         7.514e+06  5.045e+07   0.149
    ## GenresTravel & Local;Action & Adventure     -1.807e+07  8.536e+07  -0.212
    ## GenresTrivia                                -8.109e+06  1.355e+07  -0.599
    ## GenresVideo Players & Editors                7.126e+06  6.455e+07   0.110
    ## GenresVideo Players & Editors;Creativity    -1.707e+07  5.879e+07  -0.290
    ## GenresVideo Players & Editors;Music & Video -1.208e+07  4.578e+07  -0.264
    ## GenresWeather                               -1.569e+07  5.087e+07  -0.309
    ## GenresWord                                  -3.830e+06  1.355e+07  -0.283
    ## Price_num2                                   1.836e+04  4.668e+04   0.393
    ## CategoryAUTO_AND_VEHICLES                           NA         NA      NA
    ## CategoryBEAUTY                                      NA         NA      NA
    ## CategoryBOOKS_AND_REFERENCE                         NA         NA      NA
    ## CategoryBUSINESS                                    NA         NA      NA
    ## CategoryCOMICS                                      NA         NA      NA
    ## CategoryCOMMUNICATION                               NA         NA      NA
    ## CategoryDATING                                      NA         NA      NA
    ## CategoryEDUCATION                            4.923e+05  5.012e+07   0.010
    ## CategoryENTERTAINMENT                        8.142e+06  5.013e+07   0.162
    ## CategoryEVENTS                                      NA         NA      NA
    ## CategoryFAMILY                               4.771e+05  4.969e+07   0.010
    ## CategoryFINANCE                                     NA         NA      NA
    ## CategoryFOOD_AND_DRINK                              NA         NA      NA
    ## CategoryGAME                                -1.001e+07  5.011e+07  -0.200
    ## CategoryHEALTH_AND_FITNESS                          NA         NA      NA
    ## CategoryHOUSE_AND_HOME                              NA         NA      NA
    ## CategoryLIBRARIES_AND_DEMO                          NA         NA      NA
    ## CategoryLIFESTYLE                                   NA         NA      NA
    ## CategoryMAPS_AND_NAVIGATION                         NA         NA      NA
    ## CategoryMEDICAL                                     NA         NA      NA
    ## CategoryNEWS_AND_MAGAZINES                          NA         NA      NA
    ## CategoryPARENTING                                   NA         NA      NA
    ## CategoryPERSONALIZATION                             NA         NA      NA
    ## CategoryPHOTOGRAPHY                                 NA         NA      NA
    ## CategoryPRODUCTIVITY                                NA         NA      NA
    ## CategorySHOPPING                                    NA         NA      NA
    ## CategorySOCIAL                                      NA         NA      NA
    ## CategorySPORTS                               7.332e+07  5.355e+07   1.369
    ## CategoryTOOLS                                       NA         NA      NA
    ## CategoryTRAVEL_AND_LOCAL                            NA         NA      NA
    ## CategoryVIDEO_PLAYERS                        1.488e+06  8.094e+07   0.018
    ## CategoryWEATHER                                     NA         NA      NA
    ## Rating                                       2.616e+06  1.420e+06   1.842
    ## Reviews_num                                  1.839e+01  2.369e-01  77.604
    ##                                             Pr(>|t|)    
    ## (Intercept)                                   0.8833    
    ## Type_NaPaid                                   0.0089 ** 
    ## GenresAction;Action & Adventure               0.4585    
    ## GenresAdventure                               0.3110    
    ## GenresAdventure;Action & Adventure            0.9478    
    ## GenresAdventure;Brain Games                   0.8618    
    ## GenresAdventure;Education                     0.7777    
    ## GenresArcade                                  0.0215 *  
    ## GenresArcade;Action & Adventure               0.2936    
    ## GenresArcade;Pretend Play                     0.7831    
    ## GenresArt & Design                            0.7427    
    ## GenresArt & Design;Creativity                 0.6728    
    ## GenresArt & Design;Pretend Play               0.7531    
    ## GenresAuto & Vehicles                         0.7258    
    ## GenresBeauty                                  0.7238    
    ## GenresBoard                                   0.6618    
    ## GenresBoard;Action & Adventure                0.7245    
    ## GenresBoard;Brain Games                       0.4342    
    ## GenresBoard;Pretend Play                      0.8566    
    ## GenresBooks & Reference                       0.8444    
    ## GenresBooks & Reference;Education             0.7619    
    ## GenresBusiness                                0.7590    
    ## GenresCard                                    0.5676    
    ## GenresCard;Action & Adventure                 0.6948    
    ## GenresCard;Brain Games                        0.7807    
    ## GenresCasino                                  0.5323    
    ## GenresCasual                                  0.5615    
    ## GenresCasual;Action & Adventure               0.0573 .  
    ## GenresCasual;Brain Games                      0.4061    
    ## GenresCasual;Creativity                       0.6222    
    ## GenresCasual;Education                        0.6940    
    ## GenresCasual;Music & Video                    0.8501    
    ## GenresCasual;Pretend Play                     0.4041    
    ## GenresComics                                  0.7190    
    ## GenresComics;Creativity                       0.8153    
    ## GenresCommunication                           0.4745    
    ## GenresCommunication;Creativity                0.7903    
    ## GenresDating                                  0.7366    
    ## GenresEducation                               0.0265 *  
    ## GenresEducation;Action & Adventure            0.5853    
    ## GenresEducation;Brain Games                   0.6354    
    ## GenresEducation;Creativity                    0.5886    
    ## GenresEducation;Education                     0.1149    
    ## GenresEducation;Music & Video                 0.5936    
    ## GenresEducation;Pretend Play                  0.4003    
    ## GenresEducational                             0.2272    
    ## GenresEducational;Action & Adventure          0.9962    
    ## GenresEducational;Brain Games                 0.6095    
    ## GenresEducational;Creativity                  0.6620    
    ## GenresEducational;Education                   0.2020    
    ## GenresEducational;Pretend Play                0.5465    
    ## GenresEntertainment                           0.0806 .  
    ## GenresEntertainment;Action & Adventure        0.6677    
    ## GenresEntertainment;Brain Games               0.4622    
    ## GenresEntertainment;Creativity                0.6057    
    ## GenresEntertainment;Education                 0.7893    
    ## GenresEntertainment;Music & Video             0.4197    
    ## GenresEntertainment;Pretend Play              0.7404    
    ## GenresEvents                                  0.7148    
    ## GenresFinance                                 0.7457    
    ## GenresFood & Drink                            0.7348    
    ## GenresHealth & Fitness                        0.7613    
    ## GenresHealth & Fitness;Action & Adventure     0.7990    
    ## GenresHealth & Fitness;Education              0.7702    
    ## GenresHouse & Home                            0.7360    
    ## GenresLibraries & Demo                        0.7285    
    ## GenresLifestyle                               0.7379    
    ## GenresLifestyle;Education                     0.7831    
    ## GenresLifestyle;Pretend Play                  0.9023    
    ## GenresMaps & Navigation                       0.7458    
    ## GenresMedical                                 0.7451    
    ## GenresMusic                                   0.8875    
    ## GenresMusic & Audio;Music & Video             0.7879    
    ## GenresMusic;Music & Video                     0.7284    
    ## GenresNews & Magazines                        0.8474    
    ## GenresParenting                               0.7220    
    ## GenresParenting;Brain Games                   0.8473    
    ## GenresParenting;Education                     0.7956    
    ## GenresParenting;Music & Video                 0.7583    
    ## GenresPersonalization                         0.7673    
    ## GenresPhotography                             0.9764    
    ## GenresProductivity                            0.7452    
    ## GenresPuzzle                                  0.3653    
    ## GenresPuzzle;Action & Adventure               0.8600    
    ## GenresPuzzle;Brain Games                      0.4409    
    ## GenresPuzzle;Creativity                       0.6947    
    ## GenresPuzzle;Education                        0.7749    
    ## GenresRacing                                  0.5947    
    ## GenresRacing;Action & Adventure               0.4470    
    ## GenresRacing;Pretend Play                     0.7874    
    ## GenresRole Playing                            0.0827 .  
    ## GenresRole Playing;Action & Adventure         0.5336    
    ## GenresRole Playing;Brain Games                0.8635    
    ## GenresRole Playing;Pretend Play               0.6365    
    ## GenresShopping                                0.7853    
    ## GenresSimulation                              0.0519 .  
    ## GenresSimulation;Action & Adventure           0.4748    
    ## GenresSimulation;Education                    0.7903    
    ## GenresSimulation;Pretend Play                 0.6216    
    ## GenresSocial                                  0.8697    
    ## GenresSports                                2.00e-06 ***
    ## GenresSports;Action & Adventure               0.5776    
    ## GenresStrategy                              3.07e-09 ***
    ## GenresStrategy;Action & Adventure             0.7486    
    ## GenresStrategy;Creativity                     0.7645    
    ## GenresStrategy;Education                      0.7819    
    ## GenresTools                                   0.8648    
    ## GenresTools;Education                         0.8560    
    ## GenresTravel & Local                          0.8816    
    ## GenresTravel & Local;Action & Adventure       0.8324    
    ## GenresTrivia                                  0.5495    
    ## GenresVideo Players & Editors                 0.9121    
    ## GenresVideo Players & Editors;Creativity      0.7716    
    ## GenresVideo Players & Editors;Music & Video   0.7920    
    ## GenresWeather                                 0.7577    
    ## GenresWord                                    0.7774    
    ## Price_num2                                    0.6940    
    ## CategoryAUTO_AND_VEHICLES                         NA    
    ## CategoryBEAUTY                                    NA    
    ## CategoryBOOKS_AND_REFERENCE                       NA    
    ## CategoryBUSINESS                                  NA    
    ## CategoryCOMICS                                    NA    
    ## CategoryCOMMUNICATION                             NA    
    ## CategoryDATING                                    NA    
    ## CategoryEDUCATION                             0.9922    
    ## CategoryENTERTAINMENT                         0.8710    
    ## CategoryEVENTS                                    NA    
    ## CategoryFAMILY                                0.9923    
    ## CategoryFINANCE                                   NA    
    ## CategoryFOOD_AND_DRINK                            NA    
    ## CategoryGAME                                  0.8418    
    ## CategoryHEALTH_AND_FITNESS                        NA    
    ## CategoryHOUSE_AND_HOME                            NA    
    ## CategoryLIBRARIES_AND_DEMO                        NA    
    ## CategoryLIFESTYLE                                 NA    
    ## CategoryMAPS_AND_NAVIGATION                       NA    
    ## CategoryMEDICAL                                   NA    
    ## CategoryNEWS_AND_MAGAZINES                        NA    
    ## CategoryPARENTING                                 NA    
    ## CategoryPERSONALIZATION                           NA    
    ## CategoryPHOTOGRAPHY                               NA    
    ## CategoryPRODUCTIVITY                              NA    
    ## CategorySHOPPING                                  NA    
    ## CategorySOCIAL                                    NA    
    ## CategorySPORTS                                0.1709    
    ## CategoryTOOLS                                     NA    
    ## CategoryTRAVEL_AND_LOCAL                          NA    
    ## CategoryVIDEO_PLAYERS                         0.9853    
    ## CategoryWEATHER                                   NA    
    ## Rating                                        0.0655 .  
    ## Reviews_num                                  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 69010000 on 9240 degrees of freedom
    ## Multiple R-squared:  0.4356, Adjusted R-squared:  0.428 
    ## F-statistic: 57.51 on 124 and 9240 DF,  p-value: < 2.2e-16

``` r
bckw_model <- step(max_model, direction = "backward")
```

    ## Start:  AIC=338195.2
    ## Installs_num ~ Type_Na + Genres + Price_num2 + Category + Rating + 
    ##     Reviews_num
    ## 
    ##               Df  Sum of Sq        RSS    AIC
    ## - Genres      88 3.9917e+17 4.4399e+19 338104
    ## - Price_num2   1 7.3690e+14 4.4000e+19 338193
    ## <none>                      4.4000e+19 338195
    ## - Rating       1 1.6164e+16 4.4016e+19 338197
    ## - Type_Na      1 3.2596e+16 4.4032e+19 338200
    ## - Category     6 1.0996e+17 4.4110e+19 338207
    ## - Reviews_num  1 2.8678e+19 7.2677e+19 342893
    ## 
    ## Step:  AIC=338103.8
    ## Installs_num ~ Type_Na + Price_num2 + Category + Rating + Reviews_num
    ## 
    ##               Df  Sum of Sq        RSS    AIC
    ## - Price_num2   1 8.4131e+14 4.4400e+19 338102
    ## <none>                      4.4399e+19 338104
    ## - Rating       1 1.4063e+16 4.4413e+19 338105
    ## - Type_Na      1 3.4874e+16 4.4434e+19 338109
    ## - Category    32 1.4105e+18 4.5809e+19 338333
    ## - Reviews_num  1 2.9051e+19 7.3450e+19 342816
    ## 
    ## Step:  AIC=338102
    ## Installs_num ~ Type_Na + Category + Rating + Reviews_num
    ## 
    ##               Df  Sum of Sq        RSS    AIC
    ## <none>                      4.4400e+19 338102
    ## - Rating       1 1.3886e+16 4.4414e+19 338103
    ## - Type_Na      1 3.4218e+16 4.4434e+19 338107
    ## - Category    32 1.4096e+18 4.5809e+19 338331
    ## - Reviews_num  1 2.9055e+19 7.3455e+19 342815

``` r
summary(bckw_model)
```

    ## 
    ## Call:
    ## lm(formula = Installs_num ~ Type_Na + Category + Rating + Reviews_num, 
    ##     data = apps)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -722283193   -8489876   -2663946      12531  966146172 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 -8.614e+06  1.070e+07  -0.805 0.420693    
    ## Type_NaPaid                 -7.724e+06  2.881e+06  -2.681 0.007346 ** 
    ## CategoryAUTO_AND_VEHICLES   -9.277e+05  1.192e+07  -0.078 0.937951    
    ## CategoryBEAUTY              -1.214e+06  1.379e+07  -0.088 0.929838    
    ## CategoryBOOKS_AND_REFERENCE  7.054e+06  1.017e+07   0.693 0.488085    
    ## CategoryBUSINESS             1.444e+06  9.622e+06   0.150 0.880679    
    ## CategoryCOMICS              -1.480e+06  1.261e+07  -0.117 0.906557    
    ## CategoryCOMMUNICATION        5.351e+07  9.576e+06   5.588 2.37e-08 ***
    ## CategoryDATING              -1.084e+05  1.007e+07  -0.011 0.991412    
    ## CategoryEDUCATION           -7.678e+05  1.037e+07  -0.074 0.940962    
    ## CategoryENTERTAINMENT        1.083e+07  1.043e+07   1.038 0.299226    
    ## CategoryEVENTS              -1.772e+06  1.351e+07  -0.131 0.895645    
    ## CategoryFAMILY               8.474e+05  8.920e+06   0.095 0.924315    
    ## CategoryFINANCE              7.071e+05  9.571e+06   0.074 0.941105    
    ## CategoryFOOD_AND_DRINK      -2.419e+05  1.098e+07  -0.022 0.982418    
    ## CategoryGAME                 4.578e+06  9.013e+06   0.508 0.611479    
    ## CategoryHEALTH_AND_FITNESS   1.645e+06  9.633e+06   0.171 0.864433    
    ## CategoryHOUSE_AND_HOME      -2.205e+05  1.181e+07  -0.019 0.985105    
    ## CategoryLIBRARIES_AND_DEMO  -7.767e+05  1.225e+07  -0.063 0.949443    
    ## CategoryLIFESTYLE            1.678e+05  9.595e+06   0.017 0.986049    
    ## CategoryMAPS_AND_NAVIGATION  5.246e+05  1.074e+07   0.049 0.961043    
    ## CategoryMEDICAL              5.431e+05  9.527e+06   0.057 0.954547    
    ## CategoryNEWS_AND_MAGAZINES   2.667e+07  9.864e+06   2.703 0.006876 ** 
    ## CategoryPARENTING           -1.151e+06  1.311e+07  -0.088 0.930077    
    ## CategoryPERSONALIZATION      2.064e+06  9.600e+06   0.215 0.829732    
    ## CategoryPHOTOGRAPHY          1.856e+07  9.584e+06   1.936 0.052897 .  
    ## CategoryPRODUCTIVITY         3.336e+07  9.506e+06   3.509 0.000452 ***
    ## CategorySHOPPING             3.295e+06  9.839e+06   0.335 0.737707    
    ## CategorySOCIAL               9.197e+06  9.771e+06   0.941 0.346613    
    ## CategorySPORTS               4.386e+05  9.577e+06   0.046 0.963478    
    ## CategoryTOOLS                8.380e+06  9.136e+06   0.917 0.359048    
    ## CategoryTRAVEL_AND_LOCAL     2.436e+07  9.897e+06   2.461 0.013865 *  
    ## CategoryVIDEO_PLAYERS        2.539e+07  1.033e+07   2.457 0.014014 *  
    ## CategoryWEATHER              1.265e+06  1.184e+07   0.107 0.914929    
    ## Rating                       2.407e+06  1.409e+06   1.708 0.087647 .  
    ## Reviews_num                  1.816e+01  2.324e-01  78.134  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 68990000 on 9329 degrees of freedom
    ## Multiple R-squared:  0.4305, Adjusted R-squared:  0.4283 
    ## F-statistic: 201.5 on 35 and 9329 DF,  p-value: < 2.2e-16

``` r
min_model <- lm(Rating~1, data = apps)
summary(min_model)
```

    ## 
    ## Call:
    ## lm(formula = Rating ~ 1, data = apps)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1918 -0.1918  0.1082  0.3082  0.8082 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.191757   0.005324   787.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5152 on 9364 degrees of freedom

``` r
fwd_model<- step(min_model, direction = 'forward', scope= list(lower=min_model, upper= max_model))
```

    ## Start:  AIC=-12419.05
    ## Rating ~ 1

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts): the
    ## response appeared on the right-hand side and was dropped

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts):
    ## problem with term 5 in model.matrix: no columns are assigned

    ##                Df Sum of Sq    RSS    AIC
    ## + Category     32    75.909 2410.0 -12646
    ## + Genres      114   103.035 2382.9 -12588
    ## + Reviews_num   1    11.543 2474.4 -12461
    ## + Type_Na       1     3.895 2482.1 -12432
    ## + Price_num2    1     1.193 2484.8 -12422
    ## <none>                      2485.9 -12419
    ## 
    ## Step:  AIC=-12645.47
    ## Rating ~ Category

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts): the
    ## response appeared on the right-hand side and was dropped

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts):
    ## problem with term 5 in model.matrix: no columns are assigned

    ##               Df Sum of Sq    RSS    AIC
    ## + Reviews_num  1     9.254 2400.8 -12680
    ## + Type_Na      1     3.121 2406.9 -12656
    ## + Price_num2   1     0.784 2409.2 -12646
    ## <none>                     2410.0 -12646
    ## + Genres      88    36.763 2373.3 -12613
    ## 
    ## Step:  AIC=-12679.5
    ## Rating ~ Category + Reviews_num

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts): the
    ## response appeared on the right-hand side and was dropped

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts):
    ## problem with term 6 in model.matrix: no columns are assigned

    ##              Df Sum of Sq    RSS    AIC
    ## + Type_Na     1     3.564 2397.2 -12691
    ## + Price_num2  1     0.766 2400.0 -12680
    ## <none>                    2400.8 -12680
    ## + Genres     88    35.349 2365.4 -12642
    ## 
    ## Step:  AIC=-12691.42
    ## Rating ~ Category + Reviews_num + Type_Na

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts): the
    ## response appeared on the right-hand side and was dropped

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts):
    ## problem with term 6 in model.matrix: no columns are assigned

    ##              Df Sum of Sq    RSS    AIC
    ## + Price_num2  1     1.782 2395.4 -12696
    ## <none>                    2397.2 -12691
    ## + Genres     88    34.203 2363.0 -12650
    ## 
    ## Step:  AIC=-12696.38
    ## Rating ~ Category + Reviews_num + Type_Na + Price_num2

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts): the
    ## response appeared on the right-hand side and was dropped

    ## Warning in model.matrix.default(Terms, m, contrasts.arg = object$contrasts):
    ## problem with term 6 in model.matrix: no columns are assigned

    ##          Df Sum of Sq    RSS    AIC
    ## <none>                2395.4 -12696
    ## + Genres 88    33.816 2361.6 -12654

``` r
summary(fwd_model)
```

    ## 
    ## Call:
    ## lm(formula = Rating ~ Category + Reviews_num + Type_Na + Price_num2, 
    ##     data = apps)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2728 -0.1825  0.0966  0.3065  1.0314 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  4.353e+00  6.436e-02  67.639  < 2e-16 ***
    ## CategoryAUTO_AND_VEHICLES   -1.644e-01  8.752e-02  -1.879 0.060329 .  
    ## CategoryBEAUTY              -7.495e-02  1.013e-01  -0.740 0.459251    
    ## CategoryBOOKS_AND_REFERENCE -1.265e-02  7.473e-02  -0.169 0.865557    
    ## CategoryBUSINESS            -2.356e-01  7.063e-02  -3.335 0.000856 ***
    ## CategoryCOMICS              -1.989e-01  9.257e-02  -2.148 0.031723 *  
    ## CategoryCOMMUNICATION       -2.270e-01  7.030e-02  -3.229 0.001248 ** 
    ## CategoryDATING              -3.848e-01  7.388e-02  -5.209 1.94e-07 ***
    ## CategoryEDUCATION            3.066e-02  7.615e-02   0.403 0.687182    
    ## CategoryENTERTAINMENT       -2.326e-01  7.659e-02  -3.037 0.002396 ** 
    ## CategoryEVENTS               8.209e-02  9.924e-02   0.827 0.408142    
    ## CategoryFAMILY              -1.709e-01  6.549e-02  -2.610 0.009072 ** 
    ## CategoryFINANCE             -2.190e-01  7.031e-02  -3.115 0.001846 ** 
    ## CategoryFOOD_AND_DRINK      -1.889e-01  8.061e-02  -2.344 0.019115 *  
    ## CategoryGAME                -8.847e-02  6.619e-02  -1.337 0.181410    
    ## CategoryHEALTH_AND_FITNESS  -8.123e-02  7.075e-02  -1.148 0.250993    
    ## CategoryHOUSE_AND_HOME      -1.566e-01  8.672e-02  -1.806 0.070983 .  
    ## CategoryLIBRARIES_AND_DEMO  -1.751e-01  8.996e-02  -1.947 0.051594 .  
    ## CategoryLIFESTYLE           -2.586e-01  7.045e-02  -3.670 0.000244 ***
    ## CategoryMAPS_AND_NAVIGATION -3.079e-01  7.882e-02  -3.907 9.43e-05 ***
    ## CategoryMEDICAL             -1.847e-01  6.995e-02  -2.640 0.008300 ** 
    ## CategoryNEWS_AND_MAGAZINES  -2.245e-01  7.242e-02  -3.099 0.001944 ** 
    ## CategoryPARENTING           -5.712e-02  9.632e-02  -0.593 0.553169    
    ## CategoryPERSONALIZATION     -3.962e-02  7.051e-02  -0.562 0.574253    
    ## CategoryPHOTOGRAPHY         -1.733e-01  7.038e-02  -2.463 0.013796 *  
    ## CategoryPRODUCTIVITY        -1.500e-01  6.981e-02  -2.148 0.031727 *  
    ## CategorySHOPPING            -9.958e-02  7.226e-02  -1.378 0.168199    
    ## CategorySOCIAL              -1.237e-01  7.176e-02  -1.724 0.084779 .  
    ## CategorySPORTS              -1.383e-01  7.033e-02  -1.966 0.049281 *  
    ## CategoryTOOLS               -3.178e-01  6.703e-02  -4.741 2.16e-06 ***
    ## CategoryTRAVEL_AND_LOCAL    -2.501e-01  7.265e-02  -3.443 0.000578 ***
    ## CategoryVIDEO_PLAYERS       -2.991e-01  7.581e-02  -3.946 8.01e-05 ***
    ## CategoryWEATHER             -1.197e-01  8.698e-02  -1.376 0.168921    
    ## Reviews_num                  1.050e-08  1.704e-09   6.160 7.57e-10 ***
    ## Type_NaPaid                  9.166e-02  2.170e-02   4.223 2.43e-05 ***
    ## Price_num2                  -9.006e-04  3.419e-04  -2.634 0.008446 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5067 on 9329 degrees of freedom
    ## Multiple R-squared:  0.03641,    Adjusted R-squared:  0.03279 
    ## F-statistic: 10.07 on 35 and 9329 DF,  p-value: < 2.2e-16

``` r
both_model <- step(max_model, direction = 'both')
```

    ## Start:  AIC=338195.2
    ## Installs_num ~ Type_Na + Genres + Price_num2 + Category + Rating + 
    ##     Reviews_num
    ## 
    ##               Df  Sum of Sq        RSS    AIC
    ## - Genres      88 3.9917e+17 4.4399e+19 338104
    ## - Price_num2   1 7.3690e+14 4.4000e+19 338193
    ## <none>                      4.4000e+19 338195
    ## - Rating       1 1.6164e+16 4.4016e+19 338197
    ## - Type_Na      1 3.2596e+16 4.4032e+19 338200
    ## - Category     6 1.0996e+17 4.4110e+19 338207
    ## - Reviews_num  1 2.8678e+19 7.2677e+19 342893
    ## 
    ## Step:  AIC=338103.8
    ## Installs_num ~ Type_Na + Price_num2 + Category + Rating + Reviews_num
    ## 
    ##               Df  Sum of Sq        RSS    AIC
    ## - Price_num2   1 8.4131e+14 4.4400e+19 338102
    ## <none>                      4.4399e+19 338104
    ## - Rating       1 1.4063e+16 4.4413e+19 338105
    ## - Type_Na      1 3.4874e+16 4.4434e+19 338109
    ## + Genres      88 3.9917e+17 4.4000e+19 338195
    ## - Category    32 1.4105e+18 4.5809e+19 338333
    ## - Reviews_num  1 2.9051e+19 7.3450e+19 342816
    ## 
    ## Step:  AIC=338102
    ## Installs_num ~ Type_Na + Category + Rating + Reviews_num
    ## 
    ##               Df  Sum of Sq        RSS    AIC
    ## <none>                      4.4400e+19 338102
    ## - Rating       1 1.3886e+16 4.4414e+19 338103
    ## + Price_num2   1 8.4131e+14 4.4399e+19 338104
    ## - Type_Na      1 3.4218e+16 4.4434e+19 338107
    ## + Genres      88 3.9927e+17 4.4000e+19 338193
    ## - Category    32 1.4096e+18 4.5809e+19 338331
    ## - Reviews_num  1 2.9055e+19 7.3455e+19 342815

``` r
summary(both_model)
```

    ## 
    ## Call:
    ## lm(formula = Installs_num ~ Type_Na + Category + Rating + Reviews_num, 
    ##     data = apps)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -722283193   -8489876   -2663946      12531  966146172 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 -8.614e+06  1.070e+07  -0.805 0.420693    
    ## Type_NaPaid                 -7.724e+06  2.881e+06  -2.681 0.007346 ** 
    ## CategoryAUTO_AND_VEHICLES   -9.277e+05  1.192e+07  -0.078 0.937951    
    ## CategoryBEAUTY              -1.214e+06  1.379e+07  -0.088 0.929838    
    ## CategoryBOOKS_AND_REFERENCE  7.054e+06  1.017e+07   0.693 0.488085    
    ## CategoryBUSINESS             1.444e+06  9.622e+06   0.150 0.880679    
    ## CategoryCOMICS              -1.480e+06  1.261e+07  -0.117 0.906557    
    ## CategoryCOMMUNICATION        5.351e+07  9.576e+06   5.588 2.37e-08 ***
    ## CategoryDATING              -1.084e+05  1.007e+07  -0.011 0.991412    
    ## CategoryEDUCATION           -7.678e+05  1.037e+07  -0.074 0.940962    
    ## CategoryENTERTAINMENT        1.083e+07  1.043e+07   1.038 0.299226    
    ## CategoryEVENTS              -1.772e+06  1.351e+07  -0.131 0.895645    
    ## CategoryFAMILY               8.474e+05  8.920e+06   0.095 0.924315    
    ## CategoryFINANCE              7.071e+05  9.571e+06   0.074 0.941105    
    ## CategoryFOOD_AND_DRINK      -2.419e+05  1.098e+07  -0.022 0.982418    
    ## CategoryGAME                 4.578e+06  9.013e+06   0.508 0.611479    
    ## CategoryHEALTH_AND_FITNESS   1.645e+06  9.633e+06   0.171 0.864433    
    ## CategoryHOUSE_AND_HOME      -2.205e+05  1.181e+07  -0.019 0.985105    
    ## CategoryLIBRARIES_AND_DEMO  -7.767e+05  1.225e+07  -0.063 0.949443    
    ## CategoryLIFESTYLE            1.678e+05  9.595e+06   0.017 0.986049    
    ## CategoryMAPS_AND_NAVIGATION  5.246e+05  1.074e+07   0.049 0.961043    
    ## CategoryMEDICAL              5.431e+05  9.527e+06   0.057 0.954547    
    ## CategoryNEWS_AND_MAGAZINES   2.667e+07  9.864e+06   2.703 0.006876 ** 
    ## CategoryPARENTING           -1.151e+06  1.311e+07  -0.088 0.930077    
    ## CategoryPERSONALIZATION      2.064e+06  9.600e+06   0.215 0.829732    
    ## CategoryPHOTOGRAPHY          1.856e+07  9.584e+06   1.936 0.052897 .  
    ## CategoryPRODUCTIVITY         3.336e+07  9.506e+06   3.509 0.000452 ***
    ## CategorySHOPPING             3.295e+06  9.839e+06   0.335 0.737707    
    ## CategorySOCIAL               9.197e+06  9.771e+06   0.941 0.346613    
    ## CategorySPORTS               4.386e+05  9.577e+06   0.046 0.963478    
    ## CategoryTOOLS                8.380e+06  9.136e+06   0.917 0.359048    
    ## CategoryTRAVEL_AND_LOCAL     2.436e+07  9.897e+06   2.461 0.013865 *  
    ## CategoryVIDEO_PLAYERS        2.539e+07  1.033e+07   2.457 0.014014 *  
    ## CategoryWEATHER              1.265e+06  1.184e+07   0.107 0.914929    
    ## Rating                       2.407e+06  1.409e+06   1.708 0.087647 .  
    ## Reviews_num                  1.816e+01  2.324e-01  78.134  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 68990000 on 9329 degrees of freedom
    ## Multiple R-squared:  0.4305, Adjusted R-squared:  0.4283 
    ## F-statistic: 201.5 on 35 and 9329 DF,  p-value: < 2.2e-16
