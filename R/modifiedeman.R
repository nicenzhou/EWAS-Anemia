
library(ggplot2)
modifiedeman <- function (d, ewas = TRUE, groups = NULL, line = NULL, title = NULL, 
                  morecolors = FALSE, file = "eman", hgt = 7, wi = 12, res = 300) 
{
  t1 <- Sys.time()
  print("Running...")
  if (!requireNamespace(c("ggplot2"), quietly = TRUE) == TRUE) {
    stop("Please install ggplot2 to create visualization.", 
         call. = FALSE)
  }
  gap_size <- 4
  if (ewas == TRUE) {
    if ("Variable_pvalue" %in% names(d) & "LRT_pvalue" %in% 
        names(d)) {
      d$pvalue <- ifelse(!is.na(d$Variable_pvalue), d$Variable_pvalue, 
                         ifelse(!is.na(d$LRT_pvalue), d$LRT_pvalue, NA))
    }
    else if ("Variable_pvalue" %in% names(d)) {
      d$pvalue <- d$Variable_pvalue
    }
    else if ("LRT_pvalue" %in% names(d)) {
      d$pvalue <- d$LRT_pvalue
    }
  }
  if (is.null(groups) == TRUE) {
    if ("Shape" %in% names(d)) {
      p <- ggplot2::ggplot() + ggplot2::geom_point(data = d, 
                                                   aes(x = factor(Variable), y = -log10(pvalue), 
                                                       shape = factor(Shape)))
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                                  hjust = 0.95, vjust = 0.2), axis.title.x = ggplot2::element_blank(), 
                              legend.position = "bottom", legend.title = ggplot2::element_blank())
    }
    else {
      p <- ggplot2::ggplot(d, aes(x = factor(Variable), 
                                  y = -log10(pvalue)))
      p <- p + ggplot2::geom_point()
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), 
                              axis.title.x = ggplot2::element_blank())
    }
  }
  else {
    subd <- d[, colnames(d) %in% c("Variable", "pvalue", 
                                   "Shape")]
    colnames(groups)[2] <- "Color"
    dg <- merge(subd, groups, by = "Variable")
    dg_order <- dg[order(dg$Color, dg$Variable), ]
    dg_order$Variable <- factor(dg_order$Variable, levels = unique(dg_order$Variable))
    dg_order$Color <- factor(dg_order$Color, levels = unique(dg_order$Color))
    dg_order$pos_index <- as.integer(dg_order$Variable) + 
      (gap_size * as.integer(dg_order$Color))
    maxRows <- by(dg_order, dg_order$Color, function(x) x[which.max(x$pos_index), 
                                                          ])
    minRows <- by(dg_order, dg_order$Color, function(x) x[which.min(x$pos_index), 
                                                          ])
    milimits <- do.call(rbind, minRows)
    malimits <- do.call(rbind, maxRows)
    lims <- merge(milimits, malimits, by = "Color")
    print(lims)
    if ("Shape" %in% names(dg)) {
      names(lims) <- c("Color", "Varx", "px", "Shapex", 
                       "posmin", "Vary", "py", "Shapey", "posmax")
    }
    else {
      names(lims) <- c("Color", "Varx", "px", "posmin", 
                       "Vary", "py", "posmax")
    }
    lims$av <- (lims$posmin + lims$posmax)/2
    lims$shademap <- rep(c("shade_ebebeb", "shade_fffff"), 
                         each = 1, length = nrow(lims))
    ncolors <- nlevels(lims$Color)
    if (morecolors == TRUE) {
      if (!requireNamespace(c("RColorBrewer"), quietly = TRUE) == 
          TRUE) {
        stop("Please install RColorBrewer to add color attribute.", 
             call. = FALSE)
      }
      getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 
                                                                        "Spectral"))
      newcols <- c(getPalette(ncolors), "#EBEBEB", "#FFFFFF")
    }
    else {
      newcols <- c(rep(x = c("#53868B", "#4D4D4D"), length.out = ncolors, 
                       each = 1), "#EBEBEB", "#FFFFFF")
    }
    names(newcols) <- c(levels(factor(lims$Color)), levels(factor(lims$shademap)))
    p <- ggplot2::ggplot() + ggplot2::geom_rect(data = lims, 
                                                aes(xmin = posmin - gap_size/2 - 0.5, xmax = posmax + 
                                                      gap_size/2 + 0.5, ymin = 0, ymax = Inf, fill = factor(shademap)), 
                                                alpha = 0.7)
    if ("Shape" %in% names(dg)) {
      d_label = d[, colnames(d) %in% c("Variable", "Label")]
      dg_order = merge(dg_order, d_label, by = "Variable")
      p <- p + ggplot2::geom_point(data = dg_order, aes(x = pos_index, 
                                                        y = -log10(pvalue), color = Color, fill = Color, shape = factor(Shape)))
    }
    else {
      p <- p + ggplot2::geom_point(data = dg_order, aes(x = pos_index, 
                                                        y = -log10(pvalue), color = Color, fill = Color))
    }
    p <- p + ggplot2::scale_x_continuous(breaks = lims$av, 
                                         labels = lims$Color, expand = c(0, 0))
    p <- p + geom_text(data = dg_order, aes(x = pos_index, 
                                            y = -log10(pvalue),label = Label), hjust = -0.1,  vjust = -0.2, size = 3, angle = 0)
    #p <- p + ggplot2::scale_y_continuous(name="Discovery", sec.axis = sec_axis(~ 1*., name="Repliation"))
    p <- p + ggplot2::geom_hline(yintercept = -log10(0.05), colour="red")
    p <- p + scale_shape_manual(values=c(25,24))
    p <- p + ggplot2::geom_rect(data = lims, aes(xmin = posmin - 
                                                   gap_size/2 - 0.5, xmax = posmax + gap_size/2 + 0.5, 
                                                 ymin = -Inf, ymax = 0, fill = Color), alpha = 1)
    p <- p + ggplot2::scale_colour_manual(name = "Color", 
                                          values = newcols, ggplot2::guides(alpha = "none"))
    p <- p + ggplot2::scale_fill_manual(name = "Color", values = newcols, 
                                        ggplot2::guides(alpha = "none"))
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                                hjust = 0.95, vjust = 0.2), panel.grid.minor.x = ggplot2::element_blank(), 
                            panel.grid.major.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), 
                            legend.position = "bottom", legend.title = ggplot2::element_blank())
    
    p <- p+guides(color = "none", size = "none", fill = "none")
  }
  p <- p + ggplot2::ggtitle(title) + ggplot2::ylab(expression(paste("-log"[10], 
                                                                    "(Bonferroni adjusted p-value)", sep = "")))
  if (!is.null(line)) {
    p <- p + ggplot2::geom_hline(yintercept = -log10(line), 
                                 colour = "red")
  }
  print(paste("Saving plot to ", file, ".png", sep = ""))
  ggplot2::ggsave(p, filename = paste(file, ".png", sep = ""), 
                  dpi = res, units = "in", height = hgt, width = wi)
  print(p)
  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2, 
                                                       t1, units = "secs")), 6), "secs", sep = " "))
  return(p)
}

