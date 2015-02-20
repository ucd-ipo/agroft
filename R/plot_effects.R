# new argument: linetype - can be 'l', 'p', or 'b'. 'l' is the default
# plots with two factors will no longer show symbols for points in the legend
# set linetype = 'p' for one factor plots to remove lines between factor levels if desired 
# include horizontal lines at factor levels for one factor plot (hzline=FALSE)

plot_effects <- function (x, 
                          x.var = which.max(levels), 
                          z.var = which.min(levels), 
                          multiline = is.null(x$se), 
                          rug = FALSE, 
                          linetype = 'l',
                          hzline=FALSE,
                          btyn=FALSE,
                          xlab, ylab, 
                          main = paste(effect), colors=palette(), symbols = 1:10, 
                          lines = 1:10, cex = 1.5, ylim, xlim = NULL, 
                          factor.names = TRUE, 
                          ci.style, 
                          type = c("response", "link"), 
                          ticks = list(at = NULL, n = 5), 
                          alternating = TRUE, rotx = 0, roty = 0, grid = FALSE, 
                          layout, rescale.axis = TRUE, transform.x = NULL, ticks.x = NULL, 
                          key.args = NULL, row = 1, col = 1, nrow = 1, ncol = 1, more = FALSE, 
                          ...) 
{
  ci.style <- if (missing(ci.style)) 
    NULL
  else match.arg(ci.style, c("bars", "lines", "none"))
  type <- match.arg(type)
  linetype <- match.arg(linetype, c('l','p','b'))
  thresholds <- x$thresholds
  est <- x$fit
  has.thresholds <- !is.null(thresholds)
  if (missing(ylab)) {
    ylab <- if (has.thresholds) 
      paste(x$response, ": ", paste(x$y.levels, collapse = ", "), 
            sep = "")
    else x$response
  }
  if (has.thresholds) {
    threshold.labels <- abbreviate(x$y.levels, minlength = 1)
    threshold.labels <- paste(" ", paste(threshold.labels[-length(threshold.labels)], 
                                         threshold.labels[-1], sep = " - "), " ", sep = "")
  }
  trans.link <- x$transformation$link
  trans.inverse <- x$transformation$inverse
  if (!rescale.axis) {
    x$lower[!is.na(x$lower)] <- trans.inverse(x$lower[!is.na(x$lower)])
    x$upper[!is.na(x$upper)] <- trans.inverse(x$upper[!is.na(x$upper)])
    x$fit[!is.na(x$fit)] <- trans.inverse(x$fit)[!is.na(x$fit)]
    trans.link <- trans.inverse <- I
  }
  require(lattice)
  split <- c(col, row, ncol, nrow)
  ylab
  x.data <- x$data
  effect <- paste(sapply(x$variables, "[[", "name"), collapse = "*")
  vars <- x$variables
  x <- as.data.frame(x, transform = I)
  for (i in 1:length(vars)) {
    if (!(vars[[i]]$is.factor)) 
      next
    x[, i] <- factor(x[, i], levels = vars[[i]]$levels)
  }
  has.se <- !is.null(x$se)
  n.predictors <- ncol(x) - 1 - 3 * has.se
  if (n.predictors == 1) {
    if (is.factor(x[, 1])) {
      ci.style <- if (is.null(ci.style)) 
        "bars"
      else ci.style
      range <- if (has.se & ci.style != "none") 
        range(c(x$lower, x$upper), na.rm = TRUE)
      else range(x$fit, na.rm = TRUE)
      ylim <- if (!missing(ylim)) 
        ylim
      else c(range[1] - 0.025 * (range[2] - range[1]), 
             range[2] + 0.025 * (range[2] - range[1]))
      tickmarks <- if (type == "response") 
        make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                   at = ticks$at, n = ticks$n)
      else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                      n = ticks$n)
      levs <- levels(x[, 1])
      plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                                             names(x)[1], ")"))), strip = function(...) strip.default(..., 
                                                                                                      strip.names = c(factor.names, TRUE)), 
                     panel = function(x, y, lower, upper, has.se, ...) {
                       if (hzline) {
                         panel.abline(h=est, reference=TRUE)}
                       if(grid){
                         panel.grid()
                       }
                       good <- !is.na(y)
                       if (has.se) {
                         if (ci.style == "bars") {
                           larrows(x0 = x[good], y0 = lower[good], 
                                   x1 = x[good], y1 = upper[good], angle = 90,
                                   code = 3, col = 'black', length = 0.125 * cex/1.5)
                         }
                         else {
                           if (ci.style == "lines") {
                             llines(x[good], lower[good], lty = 2, 
                                    col = colors[2])
                             llines(x[good], upper[good], lty = 2, 
                                    col = colors[2])
                           }
                         }
                       }
                       llines(x[good], y[good], lwd = 2, col = colors[1], 
                              type = linetype, pch = 19, cex = cex, ...)
                       if (has.thresholds) {
                         panel.abline(h = thresholds, lty = 3)
                         panel.text(rep(current.panel.limits()$xlim[1], 
                                        length(thresholds)), thresholds, threshold.labels, 
                                    adj = c(0, 0), cex = 0.75)
                         panel.text(rep(current.panel.limits()$xlim[2], 
                                        length(thresholds)), thresholds, threshold.labels, 
                                    adj = c(1, 0), cex = 0.75)

                       }
                       if(btyn){
                         lims <- current.panel.limits()
                         panel.abline(h=lims$ylim[1], v=lims$xlim[1], cex=2)
                       }
                     }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                                                                                                        names(x)[1]
                     else xlab, scales = list(x = list(at = 1:length(levs), 
                                                       labels = levs, rot = rotx), y = list(at = tickmarks$at, 
                                                                                            labels = tickmarks$labels, rot = roty), alternating = alternating, 
                                              y = roty), main = main, lower = x$lower, upper = x$upper, 
                     has.se = has.se, data = x, ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    else {
      ci.style <- if (is.null(ci.style)) 
        "lines"
      else ci.style
      range <- if (has.se & ci.style != "none") 
        range(c(x$lower, x$upper), na.rm = TRUE)
      else range(x$fit, na.rm = TRUE)
      ylim <- if (!missing(ylim)) 
        ylim
      else c(range[1] - 0.025 * (range[2] - range[1]), 
             range[2] + 0.025 * (range[2] - range[1]))
      tickmarks <- if (type == "response") 
        make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                   at = ticks$at, n = ticks$n)
      else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                      n = ticks$n)
      nm <- names(x)[1]
      x.vals <- x.data[, nm]
      if (nm %in% names(ticks.x)) {
        at <- ticks.x[[nm]]$at
        n <- ticks.x[[nm]]$n
      }
      else {
        at <- NULL
        n <- 5
      }
      xlm <- if (nm %in% names(xlim)) {
        xlim[[nm]]
      }
      else range.adj(x[nm])
      tickmarks.x <- if ((nm %in% names(transform.x)) && 
                           !(is.null(transform.x))) {
        trans <- transform.x[[nm]]$trans
        make.ticks(trans(xlm), link = transform.x[[nm]]$trans, 
                   inverse = transform.x[[nm]]$inverse, at = ticks.x$at, 
                   n = ticks.x$n)
      }
      else {
        trans <- I
        make.ticks(xlm, link = I, inverse = I, at = at, 
                   n = n)
      }
      plot <- xyplot(eval(parse(text = paste("fit ~ trans(", 
                                             names(x)[1], ")"))), strip = function(...) strip.default(..., 
                                                                                                      strip.names = c(factor.names, TRUE)), panel = function(x, 
                                                                                                                                                             y, x.vals, rug, lower, upper, has.se, ...) {
                                                                                                        if (grid) 
                                                                                                          panel.grid()
                                                                                                        good <- !is.na(y)
                                                                                                        axis.length <- diff(range(x))
                                                                                                        llines(x[good], y[good], lwd = 2, col = colors[1], 
                                                                                                               ...)
                                                                                                        if (rug) 
                                                                                                          lrug(x.vals)
                                                                                                        if (has.se) {
                                                                                                          if (ci.style == "bars") {
                                                                                                            larrows(x0 = x[good], y0 = lower[good], 
                                                                                                                    x1 = x[good], y1 = upper[good], angle = 90, 
                                                                                                                    code = 3, col = eval(colors[2]), length = 0.125 * 
                                                                                                                      cex/1.5)
                                                                                                          }
                                                                                                          else {
                                                                                                            if (ci.style == "lines") {
                                                                                                              llines(x[good], lower[good], lty = 2, 
                                                                                                                     col = colors[2])
                                                                                                              llines(x[good], upper[good], lty = 2, 
                                                                                                                     col = colors[2])
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                        if (has.thresholds) {
                                                                                                          panel.abline(h = thresholds, lty = 3)
                                                                                                          panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                                                         length(thresholds)), thresholds, threshold.labels, 
                                                                                                                     adj = c(0, 0), cex = 0.75)
                                                                                                          panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                                                         length(thresholds)), thresholds, threshold.labels, 
                                                                                                                     adj = c(1, 0), cex = 0.75)
                                                                                                        }
                                                                                                        if(btyn){
                                                                                                          lims <- current.panel.limits()
                                                                                                          panel.abline(h=lims$ylim[1], v=lims$xlim[1], cex=2)
                                                                                                        }
                                                                                                      }, ylim = ylim, xlim = trans(xlm), ylab = ylab, 
                     xlab = if (missing(xlab)) 
                       names(x)[1]
                     else xlab, x.vals = x.vals, rug = rug, main = main, 
                     lower = x$lower, upper = x$upper, has.se = has.se, 
                     data = x, scales = list(y = list(at = tickmarks$at, 
                                                      labels = tickmarks$labels, rot = roty), x = list(at = tickmarks.x$at, 
                                                                                                       labels = tickmarks.x$labels, rot = rotx), 
                                             alternating = alternating), ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    return(result)
  }
  predictors <- names(x)[1:n.predictors]
  levels <- sapply(apply(x[, predictors], 2, unique), length)
  if (is.character(x.var)) {
    which.x <- which(x.var == predictors)
    if (length(which.x) == 0) 
      stop(paste("x.var = '", x.var, "' is not in the model.", 
                 sep = ""))
    x.var <- which.x
  }
  if (is.character(z.var)) {
    which.z <- which(z.var == predictors)
    if (length(which.z) == 0) 
      stop(paste("z.var = '", z.var, "' is not in the model.", 
                 sep = ""))
    z.var <- which.z
  }
  if (x.var == z.var) 
    z.var <- z.var + 1
  if (multiline) {
    ci.style <- if (is.null(ci.style)) 
      "none"
    else ci.style
    if (ci.style == "lines") {
      cat("Confidence interval style 'lines' changed to 'bars'\n")
      ci.style <- "bars"
    }
    range <- if (has.se && ci.style != "none") 
      range(c(x$lower, x$upper), na.rm = TRUE)
    else range(x$fit, na.rm = TRUE)
    ylim <- if (!missing(ylim)) 
      ylim
    else c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
             0.025 * (range[2] - range[1]))
    tickmarks <- if (type == "response") 
      make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                 at = ticks$at, n = ticks$n)
    else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                    n = ticks$n)
    zvals <- unique(x[, z.var])
    if (length(zvals) > min(c(length(colors), length(lines), 
                              length(symbols)))) 
      stop(paste("Not enough colors, lines, or symbols to plot", 
                 length(zvals), "lines"))
    if (is.factor(x[, x.var])) {
      levs <- levels(x[, x.var])
      key <- list(text = list(as.character(zvals)), 
                  lines = list(col = colors[1:length(zvals)], 
                               lty = lines[1:length(zvals)], lwd = 2), 
                  points = list(pch = rep('',length(zvals))))
      key <- c(key, key.args)
      plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                                             predictors[x.var], ")", if (n.predictors > 2) 
                                               paste(" |", paste(predictors[-c(x.var, z.var)])), 
                                             collapse = "*"))), strip = function(...) strip.default(..., 
                                                                                                    strip.names = c(factor.names, TRUE)), panel = function(x, 
                                                                                                                                                           y, subscripts, z, lower, upper, show.se, ...) {
                                                                                                      if (grid) 
                                                                                                        panel.grid()
                                                                                                      for (i in 1:length(zvals)) {
                                                                                                        sub <- z[subscripts] == zvals[i]
                                                                                                        good <- !is.na(y[sub])
                                                                                                        os <- if (show.se) 
                                                                                                          (i - (length(zvals) + 1)/2) * (2/(length(zvals) - 
                                                                                                                                              1)) * 0.01 * (length(zvals) - 1)
                                                                                                        else 0
                                                                                                        llines(x[sub][good] + os, y[sub][good], lwd = 2, 
                                                                                                               type = linetype, col = colors[i], 
                                                                                                               pch = symbols[i],                                                                             lty = lines[i], cex = cex, ...)
                                                                                                        if (show.se) {
                                                                                                          larrows(x0 = x[sub][good] + os, y0 = lower[subscripts][sub][good], 
                                                                                                                  x1 = x[sub][good] + os, y1 = upper[subscripts][sub][good], 
                                                                                                                  angle = 90, code = 3, col = 'black', 
                                                                                                                  length = 0.125 * cex/1.5)
                                                                                                        }
                                                                                                      }
                                                                                                      if (has.thresholds) {
                                                                                                        panel.abline(h = thresholds, lty = 3)
                                                                                                        panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                                                       length(thresholds)), thresholds, threshold.labels, 
                                                                                                                   adj = c(0, 0), cex = 0.75)
                                                                                                        panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                                                       length(thresholds)), thresholds, threshold.labels, 
                                                                                                                   adj = c(1, 0), cex = 0.75)
                                                                                                      }
                                                                                                      if(btyn){
                                                                                                        lims <- current.panel.limits()
                                                                                                        panel.abline(h=lims$ylim[1], v=lims$xlim[1], cex=2)
                                                                                                      }
                                                                                                    }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                                                                                                      predictors[x.var]
                     else xlab, z = x[, z.var], scales = list(x = list(at = 1:length(levs), 
                                                                       labels = levs, rot = rotx), y = list(at = tickmarks$at, 
                                                                                                            labels = tickmarks$labels, rot = roty), alternating = alternating), 
                     zvals = zvals, main = main, key = key, lower = x$lower, 
                     upper = x$upper, show.se = has.se && ci.style == 
                       "bars", data = x, ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    else {
      nm <- names(x)[x.var]
      x.vals <- x.data[, nm]
      if (nm %in% names(ticks.x)) {
        at <- ticks.x[[nm]]$at
        n <- ticks.x[[nm]]$n
      }
      else {
        at <- NULL
        n <- 5
      }
      xlm <- if (nm %in% names(xlim)) {
        xlim[[nm]]
      }
      else range.adj(x[nm])
      tickmarks.x <- if ((nm %in% names(transform.x)) && 
                           !(is.null(transform.x))) {
        trans <- transform.x[[nm]]$trans
        make.ticks(trans(xlm), link = transform.x[[nm]]$trans, 
                   inverse = transform.x[[nm]]$inverse, at = ticks.x$at, 
                   n = ticks.x$n)
      }
      else {
        trans <- I
        make.ticks(xlm, link = I, inverse = I, at = at, 
                   n = n)
      }
      key <- list(text = list(as.character(zvals)), 
                  lines = list(col = colors[1:length(zvals)], 
                               lty = lines[1:length(zvals)], lwd = 2))
      key <- c(key, key.args)
      plot <- xyplot(eval(parse(text = paste("fit ~trans(", 
                                             predictors[x.var], ")", if (n.predictors > 2) 
                                               paste(" |", paste(predictors[-c(x.var, z.var)])), 
                                             collapse = "*"))), strip = function(...) strip.default(..., 
                                                                                                    strip.names = c(factor.names, TRUE)), panel = function(x, 
                                                                                                                                                           y, subscripts, x.vals, rug, z, lower, upper, 
                                                                                                                                                           show.se, ...) {
                                                                                                      if (grid) 
                                                                                                        panel.grid()
                                                                                                      if (rug) 
                                                                                                        lrug(x.vals)
                                                                                                      axis.length <- diff(range(x))
                                                                                                      for (i in 1:length(zvals)) {
                                                                                                        sub <- z[subscripts] == zvals[i]
                                                                                                        good <- !is.na(y[sub])
                                                                                                        llines(x[sub][good], y[sub][good], lwd = 2, 
                                                                                                               type = "l", col = colors[i], lty = lines[i], 
                                                                                                               cex = cex, ...)
                                                                                                        if (show.se) {
                                                                                                          os <- (i - (length(zvals) + 1)/2) * (2/(length(zvals) - 
                                                                                                                                                    1)) * 0.01 * axis.length
                                                                                                          larrows(x0 = x[sub][good] + os, y0 = lower[subscripts][sub][good], 
                                                                                                                  x1 = x[sub][good] + os, y1 = upper[subscripts][sub][good], 
                                                                                                                  angle = 90, code = 3, col = eval(colors[i]), 
                                                                                                                  length = 0.125 * cex/1.5)
                                                                                                        }
                                                                                                      }
                                                                                                      if (has.thresholds) {
                                                                                                        panel.abline(h = thresholds, lty = 3)
                                                                                                        panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                                                       length(thresholds)), thresholds, threshold.labels, 
                                                                                                                   adj = c(0, 0), cex = 0.75)
                                                                                                        panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                                                       length(thresholds)), thresholds, threshold.labels, 
                                                                                                                   adj = c(1, 0), cex = 0.75)
                                                                                                      }
                                                                                                      if(btyn){
                                                                                                        lims <- current.panel.limits()
                                                                                                        panel.abline(h=lims$ylim[1], v=lims$xlim[1], cex=2)
                                                                                                      }
                                                                                                    }, ylim = ylim, xlim = trans(xlm), ylab = ylab, 
                     xlab = if (missing(xlab)) 
                       predictors[x.var]
                     else xlab, x.vals = x.vals, rug = rug, z = x[, 
                                                                  z.var], zvals = zvals, main = main, key = key, 
                     lower = x$lower, upper = x$upper, show.se = has.se && 
                       ci.style == "bars", data = x, scales = list(y = list(at = tickmarks$at, 
                                                                            labels = tickmarks$labels), rot = roty, x = list(at = tickmarks.x$at, 
                                                                                                                             labels = tickmarks.x$labels, rot = rotx), 
                                                                   alternating = alternating), ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    return(result)
  }
  ci.style <- if (is.null(ci.style)) {
    if (is.factor(x[, x.var])) 
      "bars"
    else "lines"
  }
  else ci.style
  range <- if (has.se && ci.style != "none") 
    range(c(x$lower, x$upper), na.rm = TRUE)
  else range(x$fit, na.rm = TRUE)
  ylim <- if (!missing(ylim)) 
    ylim
  else c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
           0.025 * (range[2] - range[1]))
  tickmarks <- if (type == "response") 
    make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
               at = ticks$at, n = ticks$n)
  else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                  n = ticks$n)
  if (is.factor(x[, x.var])) {
    levs <- levels(x[, x.var])
    plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                                           predictors[x.var], ") |", paste(predictors[-x.var], 
                                                                           collapse = "*")))), strip = function(...) strip.default(..., 
                                                                                                                                   strip.names = c(factor.names, TRUE)), panel = function(x, 
                                                                                                                                                                                          y, subscripts, lower, upper, has.se, ...) {
                                                                                                                                     if (grid) 
                                                                                                                                       panel.grid()
                                                                                                                                     good <- !is.na(y)
                                                                                                                                     if (has.se) {
                                                                                                                                       if (ci.style == "bars") {
                                                                                                                                         larrows(x0 = x[good], y0 = lower[subscripts][good], 
                                                                                                                                                 x1 = x[good], y1 = upper[subscripts][good], 
                                                                                                                                                 angle = 90, code = 3, col = colors[2], length = 0.125 * 
                                                                                                                                                   cex/1.5)
                                                                                                                                       }
                                                                                                                                       else {
                                                                                                                                         if (ci.style == "lines") {
                                                                                                                                           llines(x[good], lower[subscripts][good], 
                                                                                                                                                  lty = 2, col = colors[2])
                                                                                                                                           llines(x[good], upper[subscripts][good], 
                                                                                                                                                  lty = 2, col = colors[2])
                                                                                                                                         }
                                                                                                                                       }
                                                                                                                                     }
                                                                                                                                     llines(x[good], y[good], lwd = 2, type = "b", col = colors[1], 
                                                                                                                                            pch = 19, cex = cex, ...)
                                                                                                                                     if (has.thresholds) {
                                                                                                                                       panel.abline(h = thresholds, lty = 3)
                                                                                                                                       panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                                                                                      length(thresholds)), thresholds, threshold.labels, 
                                                                                                                                                  adj = c(0, 0), cex = 0.75)
                                                                                                                                       panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                                                                                      length(thresholds)), thresholds, threshold.labels, 
                                                                                                                                                  adj = c(1, 0), cex = 0.75)
                                                                                                                                     }
                                                                                                                                     
                                                                                                                                     if(btyn){
                                                                                                                                       lims <- current.panel.limits()
                                                                                                                                       panel.abline(h=lims$ylim[1], v=lims$xlim[1], cex=2)
                                                                                                                                     }
                                                                                                                                   }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                                                                                                                                     predictors[x.var]
                   else xlab, scales = list(x = list(at = 1:length(levs), 
                                                     labels = levs, rot = rotx), y = list(at = tickmarks$at, 
                                                                                          labels = tickmarks$labels, rot = roty), alternating = alternating), 
                   main = main, lower = x$lower, upper = x$upper, has.se = has.se, 
                   data = x, ...)
    result <- update(plot, layout = if (missing(layout)) 
      c(0, prod(dim(plot)))
      else layout)
    result$split <- split
    result$more <- more
    class(result) <- c("plot.eff", class(result))
  }
  else {
    nm <- names(x)[x.var]
    x.vals <- x.data[, nm]
    if (nm %in% names(ticks.x)) {
      at <- ticks.x[[nm]]$at
      n <- ticks.x[[nm]]$n
    }
    else {
      at <- NULL
      n <- 5
    }
    xlm <- if (nm %in% names(xlim)) {
      xlim[[nm]]
    }
    else range.adj(x[nm])
    tickmarks.x <- if ((nm %in% names(transform.x)) && !(is.null(transform.x))) {
      trans <- transform.x[[nm]]$trans
      make.ticks(trans(xlm), link = transform.x[[nm]]$trans, 
                 inverse = transform.x[[nm]]$inverse, at = ticks.x$at, 
                 n = ticks.x$n)
    }
    else {
      trans <- I
      make.ticks(xlm, link = I, inverse = I, at = at, 
                 n = n)
    }
    plot <- xyplot(eval(parse(text = paste("fit ~ trans(", 
                                           predictors[x.var], ") |", paste(predictors[-x.var], 
                                                                           collapse = "*")))), strip = function(...) strip.default(..., 
                                                                                                                                   strip.names = c(factor.names, TRUE)), panel = function(x, 
                                                                                                                                                                                          y, subscripts, x.vals, rug, lower, upper, has.se, 
                                                                                                                                                                                          ...) {
                                                                                                                                     if (grid) 
                                                                                                                                       panel.grid()
                                                                                                                                     good <- !is.na(y)
                                                                                                                                     llines(x[good], y[good], lwd = 2, col = colors[1], 
                                                                                                                                            ...)
                                                                                                                                     if (rug) 
                                                                                                                                       lrug(x.vals)
                                                                                                                                     if (has.se) {
                                                                                                                                       if (ci.style == "bars") {
                                                                                                                                         larrows(x0 = x[good], y0 = lower[subscripts][good], 
                                                                                                                                                 x1 = x[good], y1 = upper[subscripts][good], 
                                                                                                                                                 angle = 90, code = 3, col = eval(colors[2]), 
                                                                                                                                                 length = 0.125 * cex/1.5)
                                                                                                                                       }
                                                                                                                                       else {
                                                                                                                                         if (ci.style == "lines") {
                                                                                                                                           llines(x[good], lower[subscripts][good], 
                                                                                                                                                  lty = 2, col = colors[2])
                                                                                                                                           llines(x[good], upper[subscripts][good], 
                                                                                                                                                  lty = 2, col = colors[2])
                                                                                                                                         }
                                                                                                                                       }
                                                                                                                                     }
                                                                                                                                     if (has.thresholds) {
                                                                                                                                       panel.abline(h = thresholds, lty = 3)
                                                                                                                                       panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                                                                                      length(thresholds)), thresholds, threshold.labels, 
                                                                                                                                                  adj = c(0, 0), cex = 0.75)
                                                                                                                                       panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                                                                                      length(thresholds)), thresholds, threshold.labels, 
                                                                                                                                                  adj = c(1, 0), cex = 0.75)
                                                                                                                                     }
                                                                                                                                     
                                                                                                                                     if(btyn){
                                                                                                                                       lims <- current.panel.limits()
                                                                                                                                       panel.abline(h=lims$ylim[1], v=lims$xlim[1], cex=2)
                                                                                                                                     }
                                                                                                                                   }, ylim = ylim, xlim = trans(xlm), ylab = ylab, xlab = if (missing(xlab)) 
                                                                                                                                     predictors[x.var]
                   else xlab, x.vals = x.vals, rug = rug, main = main, 
                   lower = x$lower, upper = x$upper, has.se = has.se, 
                   data = x, scales = list(y = list(at = tickmarks$at, 
                                                    labels = tickmarks$labels, rot = roty), x = list(at = tickmarks.x$at, 
                                                                                                     labels = tickmarks.x$labels, rot = rotx), alternating = alternating), 
                   ...)
    result <- update(plot, layout = if (missing(layout)) 
      c(0, prod(dim(plot)))
      else layout)
    result$split <- split
    result$more <- more
    class(result) <- c("plot.eff", class(result))
  }
  return(result)
}

make.ticks <- function (range, link, inverse, at, n) 
{
  warn <- options(warn = -1)
  on.exit(warn)
  link <- if (is.null(link)) 
    function(x) nlm(function(y) (inverse(y) - x)^2, mean(range))$estimate
  else link
  if (is.null(n)) 
    n <- 5
  labels <- if (is.null(at)) {
    labels <- pretty(sapply(range, inverse), n = n + 1)
  }
  else at
  ticks <- sapply(labels, link)
  list(at = ticks, labels = format(labels))
}

range.adj <- function (x) 
{
  range <- range(x, na.rm = TRUE)
  c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 0.025 * 
      (range[2] - range[1]))
}

