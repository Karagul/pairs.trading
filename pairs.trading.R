#  MIT LICENSE
#  
#  Copyright (C) 2012 by Wiktor Wojtylak
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in
#  all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#  THE SOFTWARE.

library(tseries)
library(xts)

coint.tests <- function(x, y, x.title = "", y.title = "", input.unit.root.p.value = 0.05) {
    pairs <- merge(x, y, all = FALSE)
    x <- pairs[,1]
    y <- pairs[,2]
                
    adf.result.x <- adf.test(as.matrix(x)[,1], alternative = "stationary")              
    adf.result.y <- adf.test(as.matrix(y)[,1], alternative = "stationary")      
    input.unit.root = TRUE
    if (adf.result.x$p.value < input.unit.root.p.value || adf.result.y$p.value < input.unit.root.p.value)
        input.unit.root = FALSE 
    
    adf.result.residuals <- NULL
    if (input.unit.root == TRUE) {
        reg.model <- lm(x ~ y + 0)
        reg.model.beta <- coef(reg.model)[1]
        spread_df <- x - reg.model.beta * y
        adf.result.residuals <- adf.test(as.matrix(spread_df)[,1], alternative = "stationary")
    }
    
    po.result <- po.test(pairs)
    
    result <- list(EngleGranger = adf.result.residuals, PhilipsOuliaris = po.result,
        input.unit.root = input.unit.root, x = x, y = y, xTitle = x.title, yTitle = y.title,
        spread = spread_df)
    class(result) <- "coint.tests"
    result
}

get.adj.close.price <- function(stock, from = as.Date("1800-01-01"), to = Sys.Date() - 1, online = TRUE) {
    if (!is.character(stock)) 
        stop("'stock' must be a character (string)")
        
    append.to.data <- function(x, file) {
        if (file.exists(file)) {
            old.objects <- load(file)
            save(list = c(old.objects[!old.objects == x], x), file = file)
        }
        else {
            save(list = (x), file = file)
        }
    }   
        
    data.file.name = "quote.data.rda"   
    if (online) {
        assign(stock, get.hist.quote(instrument = stock, start = as.Date("1800-01-01"), end = Sys.Date() - 1, quote = "AdjClose"))
        append.to.data(stock, data.file.name)
    }
    else {
        load(data.file.name)
    }
    stock_data <- as.xts(get(stock))
    as.zoo(stock_data[paste(from, "/", to, sep="")])
}

coint.tests.stocks <- function(stock1, stock2, from = as.Date("1800-01-01"), to = Sys.Date() - 1, online = TRUE) {
    if (!is.character(stock1)) 
        stop("'stock1' must be a character (string)")
    if (!is.character(stock2)) 
        stop("'stock2' must be a character (string)")   
             
    coint.tests(get.adj.close.price(stock1, from, to, online), get.adj.close.price(stock2, from, to, online), stock1, stock2)
}

is.coint.tests <- function(x) class(x) == "coint.tests"

validate.coint.tests.class <- function(x) {
    if (!is.coint.tests(x)) 
        stop("'plot.coint.tests' applied to non coint tests result")
}

plot.coint.tests <- function(x, ...) {
    validate.coint.tests.class(x)
    
    plot(cbind(x$x, x$y, x$spread), main = "Cointegration Analysis Input",
        ylab = c(ifelse(is.character(x$xTitle), x$xTitle, "x"),
        ifelse(is.character(x$yTitle), x$yTitle, "y"), "spread"),
        col = c("blue", "red", "gray"))
}

print.coint.tests <- function(x, ...) {
    validate.coint.tests.class(x)

    if (is.character(x$xTitle) && is.character(x$yTitle))
        cat("\nCointegration Analysis between:", x$xTitle, "and", x$yTitle)
    
    if (x$input.unit.root == TRUE) {
        cat("\n\nEngle-Granger Test (Augmented Dickey-Fuller for residuals):\n")
        cat("\tp-value:", x$EngleGranger$p.value, "\n\tstatistic:",  x$EngleGranger$statistic,
            "\n\tlag:", x$EngleGranger$parameter)
    } else {
        cat("\n\nEngle-Granger Test (Augmented Dickey-Fuller for inputs):\n")   
        cat("\tInput series are not non-stationary I(1) (at least one of them)")    
    }
        
    cat("\n\nPhilips-Ouliaris Test:\n")
    cat("\tp-value:", x$PhilipsOuliaris$p.value, ifelse(x$PhilipsOuliaris$p.value >= 0.15, "(or more)", ""),
        "\n\tstatistic:",  x$PhilipsOuliaris$statistic, "\n\tlag:",  x$PhilipsOuliaris$parameter, "\n\n")   
}