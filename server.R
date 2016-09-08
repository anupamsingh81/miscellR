library(shiny)
library(psych)
library(car)
library(compute.es)
library(pwr)
library(lattice)
library(latticeExtra)
library(beeswarm)
library(BEST)



shinyServer(function(input, output) {
  
  options(warn=-1)
  
  bs <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    result1 <- describe(x)[2:13]
    result2 <- describe(y)[2:13]
    row.names(result1) <- ""
    row.names(result2) <- ""
    
    correl <- cor(x, y)
    
    return(list(Data.1 = result1, Data.2 = result2, Correlation = correl))
  })
  
  
  makedistPlot <- function(){
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    simple.bincount <- function(x, breaks) {
      nx <- length(x)
      nbreaks <- length(breaks)
      counts <- integer(nbreaks - 1)
      for (i in 1:nx) {
        lo <- 1
        hi <- nbreaks
        if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
          while (hi - lo >= 2) {
            new <- (hi + lo) %/% 2
            if(x[i] > breaks[new])
              lo <- new
            else
              hi <- new
          }
          counts[lo] <- counts[lo] + 1
        }
      }
      return(counts)
    }
    
    nclass.x <- nclass.FD(x)
    breaks.x <- pretty(x, nclass.x)
    counts.x <- simple.bincount(x, breaks.x)
    counts.max.x <- max(counts.x)
    
    nclass.y <- nclass.FD(y)
    breaks.y <- pretty(y, nclass.y)
    counts.y <- simple.bincount(y, breaks.y)
    counts.max.y <- max(counts.y)
    
    counts.max <- max(c(counts.max.x, counts.max.y))
    
    
    xy.min <- min(c(x,y))
    xy.min <- xy.min - xy.min*0.1
    xy.max <- max(c(x,y))
    xy.max <- xy.max + xy.max*0.1
    
    p1 <- hist(x, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
    p2 <- hist(y, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
    
    plot(p1, las=1, xlab = "Data 1 is expressed in blue; Data 2 in red. Vertical lines show the mean.",
         main = "", col = rgb(0,0,1,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3))
    plot(p2, las=1, xlab = "", main = "", col = rgb(1,0,0,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3), add = TRUE)
    
    abline(v = mean(x), col = "blue", lwd = 2)
    abline(v = mean(y), col = "red", lwd = 2)
  }
  
  output$distPlot <- renderPlot({
    print(makedistPlot())
  })
  
  
  
  makeboxPlot <- function(){
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    score <- c(x, y)
    group <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))
    
    boxplot(score ~ group, las=1, xlab= "Means and +/-1 SDs are displayed in red.")
    
    beeswarm(score ~ group, col = 4, pch = 16, add = TRUE)
    
    points(1.2, mean(x), pch = 18, col = "red", cex = 2)
    arrows(1.2, mean(x), 1.2, mean(x) + sd(x), length = 0.1, angle = 45, col = "red")
    arrows(1.2, mean(x), 1.2, mean(x) - sd(x), length = 0.1, angle = 45, col = "red")
    
    points(2.2, mean(y), pch = 18, col = "red", cex = 2)
    arrows(2.2, mean(y), 2.2, mean(y) + sd(y), length = 0.1, angle = 45, col = "red")
    arrows(2.2, mean(y), 2.2, mean(y) - sd(y), length = 0.1, angle = 45, col = "red")
  }
  
  output$boxPlot <- renderPlot({
    print(makeboxPlot())
  })
  
  
  
  makeindvPlot <- function(){
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    xy <- c(x, y)
    data.point <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))
    dat <- data.frame(xy, data.point)
    dat$indiv <- factor(c(rep(1:length(x)), rep(1:length(y))))
    each <- xyplot(xy ~ data.point, group = indiv,
                   type = c("l"), data = dat, xlab ="", ylab="")
    a <- mean(x)
    b <- mean(y)
    value <- c(a, b)
    data.point2 <- factor(c(rep("Data 1", 1), rep("Data 2", 1)))
    dat2 <- data.frame(value, data.point2)
    all <- xyplot(value ~ data.point2, col = "black",
                  lwd = 5, type = c("l"), data = dat, xlab = "", ylab = "")
    indv <- each + as.layer(all, axes = NULL)
    # print(indv)
  }
  
  output$indvPlot <- renderPlot({
    print(makeindvPlot())
  })
  
  
  
  makecorrelPlot <- function(){
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    xy.min <- min(c(x,y))
    xy.min <- xy.min - xy.min*0.1
    xy.max <- max(c(x,y))
    xy.max <- xy.max + xy.max*0.1
    
    plot(x, y, las=1, pch = 16, xlab = "Data 1", ylab = "Data 2", main = "",
         xlim = c(xy.min,xy.max), ylim = c(xy.min,xy.max))
    lines(par()$usr[1:2], par()$usr[3:4], lty = 3)
  }
  
  output$correlPlot <- renderPlot({
    print(makecorrelPlot())
  })
  
  
  
  testnorm <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    data.1ks <- ks.test(scale(x), "pnorm")
    data.1sh <- shapiro.test(x)
    
    data.2ks <- ks.test(scale(y), "pnorm")
    data.2sh <- shapiro.test(y)
    
    return(list(Data.1 = data.1ks, 'Data.1 (n < 50 w/ Outliers)' = data.1sh, Data.2 = data.2ks, 'Data.2 (n < 50 w/ Outliers)' = data.2sh))
  })
  
  
  
  makedistdiffPlot <- function(){
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    diff <- x - y
    
    simple.bincount <- function(x, breaks) {
      nx <- length(diff)
      nbreaks <- length(breaks)
      counts <- integer(nbreaks - 1)
      for (i in 1:nx) {
        lo <- 1
        hi <- nbreaks
        if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
          while (hi - lo >= 2) {
            new <- (hi + lo) %/% 2
            if(x[i] > breaks[new])
              lo <- new
            else
              hi <- new
          }
          counts[lo] <- counts[lo] + 1
        }
      }
      return(counts)
    }
    
    nclass <- nclass.FD(diff)
    breaks <- pretty(diff, nclass)
    counts <- simple.bincount(diff, breaks)
    counts.max <- max(counts)
    
    h <- hist(diff, las=1, breaks="FD", xlab= "Red vertical line shows the mean of the differences.",
              ylim=c(0, counts.max*1.2), main="", col = "cyan")
    rug(diff)
    abline(v = mean(diff, na.rm=T), col = "red", lwd = 2)
    xfit <- seq(min(diff, na.rm=T), max(diff, na.rm=T))
    yfit <- dnorm(xfit, mean = mean(diff, na.rm=T), sd = sd(diff, na.rm=T))
    yfit <- yfit * diff(h$mids[1:2]) * length(diff)
    lines(xfit, yfit, col = "blue", lwd = 2)
  }
  
  output$distdiffPlot <- renderPlot({
    print(makedistdiffPlot())
  })
  
  
  
  diffnorm <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    diff <- x - y
    
    diff.1ks <- ks.test(scale(diff), "pnorm")
    diff.1sh <- shapiro.test(diff)
    
    return(list(Diff.Score = diff.1ks, 'Diff.Score (n < 50 w/ Outliers)' = diff.1sh))
  })
  
  
  
  t <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    t.test(x, y, paired=TRUE)
  })
  
  
  
  es <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    m1 <- mean(x)
    sd1 <- sd(x)
    n1 <- length(x)
    
    m2 <- mean(y)
    sd2 <- sd(y)
    n2 <- length(y)
    
    diff <- x - y
    
    d <- abs(mean(diff)/(sd(diff)/sqrt(2*(1-cor(x, y)))))
    var.d <- ((1/n1)+((d^2)/(2*n1)))*(2*(1-cor(x, y)))
    
    df <- n1 - 1
    j <- 1 - (3/(4 * df - 1))
    g <- j * d
    var.g <- j^2 * var.d
    
    #a <- ((n1 + n2)^2)/(n1 * n2)
    #r <- d/sqrt((d^2) + a)
    #var.r <- (a^2 * var.d)/(d^2 + a)^3
    
    alpha <- 0.05
    crit <- qt(alpha/2, df, lower.tail = FALSE)
    
    lower.d <- d - crit * sqrt(var.d)
    upper.d <- d + crit * sqrt(var.d)
    
    #lower.d2 <- d - 1.96 * sqrt(var.d)
    #upper.d2 <- d + 1.96 * sqrt(var.d)
    
    lower.g <- g - crit * sqrt(var.g)
    upper.g <- g + crit * sqrt(var.g)
    
    #lower.g2 <- d - 1.96 * sqrt(var.d)
    #upper.g2 <- d + 1.96 * sqrt(var.d)
    
    
    cat("=======================================================", "\n")
    cat(" Mean difference / within-groups SD:", "\n",
        "(Based on Borenstein et al., 2009) ", "\n",
        "\n",
        "  d [95% CI] =", d, "[", lower.d, ",", upper.d, "]", "\n",
        "   var(d) =", var.d, "\n",
        "\n",
        "  g [95% CI] =", g, "[", lower.g, ",", upper.g, "]", "\n",
        "   var(g) =", var.g, "\n",
        "\n"
    )
    
    
    
    delta <- abs(mean(diff)/sd(x))
    
    cat("=======================================================", "\n")
    cat(" Mean difference / SD of Data 1 (e.g., pretest):", "\n",
        "\n",
        "  Delta =", delta, "\n",
        "\n"
    )
    
    
    
    result.dependent<-t.test(x,y,paired=TRUE)
    paired.t<-result.dependent$statistic
    r2 <- sqrt(paired.t^2/(paired.t^2+df))
    r2 <- r2[[1]]
    
    cat("=======================================================", "\n")
    cat(" r --- sqrt(paired.t^2/(paired.t^2+df)):", "\n",
        "\n",
        "  r =", r2, "\n",
        "\n"
    )
    
  })
  
  
  
  wsr <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    result <- wilcox.test(x, y, paired=TRUE, correct=FALSE)
    
    pval <- result$p.value
    z <- qnorm(1-(pval/2))
    
    r1 <- z/sqrt(length(x*2))
    
    r2 <- z/sqrt(length(x)-sum((y-x==0)))
    
    print(result)
    
    cat(" z-value =", z, "\n",
        "effect eize r =", r1, "\n",
        "effect eize r (without considering ties) =", r2, "\n")
  })
  
  
  
  power <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    
    diff <- x - y
    d <- mean(diff)/sd(diff)
    n <- length(x)
    
    posthoc <- power.t.test(n = n, delta=d, sig.level=.05, type='paired', strict = TRUE)$power
    
    future <- ceiling(power.t.test(power = 0.8, delta = d, sig.level = 0.05,
                                   type = 'paired', strict = TRUE, alternative = "two.sided")$n)
    
    cat(" Post hoc (observed) power =", round(posthoc, 3), "\n",
        "\n",
        "  Note: According to Cumming (2012), post hoc power is 'illegitimate'", "\n",
        "        and we should NEVER calculate or report it.", "\n",
        "\n",
        "\n",
        "Sample size needed for future experiment:", " n =", future, "\n",
        "  Power = 0.8, sig.level = 0.05, alternative = two.sided, d =", round(d, 2), "\n",
        "\n",
        "  Note: This is true only PROVIDED that the population effect size is", "\n",
        "        equal to the observed sample effect size (i.e., it is unrealistic).", "\n",
        "\n",
        "\n",
        "POWER ANALYSIS SHOULD BE CONDUCTED PRIOR TO THE EXPERIMENT!", "\n")
  })
  
  
  
  info <- reactive({
    info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
    info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
  })
  
  summary <- reactive ({
    BESTout <- BESTmcmc(input$data1, input$data2,numSavedSteps = 30000)
    
    summary(BESTout)
  })
  plot <- reactive({
    
    plotAll(BESTout)
    
  })
  
  output$info.out <- renderPrint({
    info()
  })
  
  
  
  output$textarea.out <- renderPrint({
    bs()
  })
  
  output$testnorm.out <- renderPrint({
    testnorm()
  })
  
  output$diffnorm.out <- renderPrint({
    diffnorm()
  })
  
  output$t.out <- renderPrint({
    t()
  })
  
  output$es.out <- renderPrint({
    es()
  })
  
  output$wsr.out <- renderPrint({
    wsr()
  })
  output$summary.out <- renderPrint({
    summary()
  })
  
  output$plot.out <- renderPlot({
    print(plot())
  })
  
  output$power.out <- renderPrint({
    power()
  })
  
})


