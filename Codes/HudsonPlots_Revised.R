library(ggplot2)
modifiedhudson <- function (top, bottom, tline, bline, log10 = TRUE, yaxis, opacity = 1, 
                            toptitle = NULL, bottomtitle = NULL, annotate_var, annotate_p, 
                            highlight_var, highlight_p, highlighter = "red", color1 = "#AAAAAA", 
                            color2 = "#4D4D4D", groupcolors, rotatelabels = FALSE, labelangle, 
                            freey = FALSE, background = "variegated", grpblocks = FALSE, 
                            file = "emirror", hgtratio = 0.5, hgt = 7, wi = 12, res = 300) 
{
  topn <- names(top)
  bottomn <- names(bottom)
  top$Location <- "Top"
  bottom$Location <- "Bottom"
  d <- rbind(top, bottom)
  if (log10 == TRUE) {
    d$pval <- -log10(d$pvalue)
    yaxislab1 <- expression(paste("-log"[10], "(Bonferroni adjusted p-value)", 
                                  sep = ""))
    yaxislab2 <- expression(paste("-log"[10], "(Bonferroni adjusted p-value)", 
                                  sep = ""))
    if (!missing(tline)) {
      tredline <- -log10(tline)
    }
    if (!missing(bline)) {
      bredline <- -log10(bline)
    }
  }
  else {
    d$pval <- d$pvalue
    yaxislab1 <- yaxis[1]
    yaxislab2 <- yaxis[2]
    if (!missing(tline)) {
      tredline <- tline
    }
    if (!missing(bline)) {
      bredline <- bline
    }
  }
  yaxismax1 <- ifelse(freey == FALSE, max(d$pval[which(d$pval < 
                                                         Inf)]), max(d$pval[which(d$pval < Inf) & d$Location == 
                                                                              "Top"]))
  yaxismax2 <- ifelse(freey == FALSE, max(d$pval[which(d$pval < 
                                                         Inf)]), max(d$pval[which(d$pval < Inf) & d$Location == 
                                                                              "Bottom"]))
  yaxismin1 <- ifelse(freey == FALSE, 0, min(d$pval[d$Location == 
                                                      "Top"]))
  yaxismin2 <- ifelse(freey == FALSE, 0, min(d$pval[d$Location == 
                                                      "Bottom"]))
  backpanel1 <- ifelse(background == "white", "NULL", "geom_rect(data = lims, aes(xmin = posmin-.5, xmax = posmax+.5, ymin = yaxismin1, ymax = Inf, fill=factor(shademap)), alpha = 0.5)")
  backpanel2 <- ifelse(background == "white", "NULL", "geom_rect(data = lims, aes(xmin = posmin-.5, xmax = posmax+.5, ymin = yaxismin2, ymax = Inf, fill=factor(shademap)), alpha = 0.5)")
  d$rowid <- seq.int(nrow(d))
  dinfo <- d[, colnames(d) %in% c("rowid", "Color", "pval", 
                                  "Location", "Shape"), drop = FALSE]
  subd <- d[, c("Variable", "Group", "pvalue", "rowid")]
  d_order <- subd[order(subd$Group, subd$Variable), ]
  d_order$pos_index <- seq.int(nrow(d_order))
  subd <- d_order[, colnames(d_order) != "rowid"]
  maxRows <- by(subd, subd$Group, function(x) x[which.max(x$pos_index), 
                                                ])
  minRows <- by(subd, subd$Group, function(x) x[which.min(x$pos_index), 
                                                ])
  milimits <- do.call(rbind, minRows)
  malimits <- do.call(rbind, maxRows)
  lims <- merge(milimits, malimits, by = "Group")
  names(lims) <- c("Color", "Varx", "px", "posmin", "Vary", 
                   "py", "posmax")
  lims$av <- (lims$posmin + lims$posmax)/2
  lims$shademap <- rep(c("shade_ffffff", "shade_ebebeb"), each = 1, 
                       length.out = nrow(lims))
  nvarcolors <- nlevels(factor(lims$Color))
  if ("Color" %in% names(d)) {
    if (!missing(groupcolors)) {
      dcols <- c(rep(x = c(color1, color2), length.out = nvarcolors, 
                     each = 1), "#FFFFFF", "#EBEBEB")
      names(dcols) <- c(levels(factor(lims$Color)), "shade_ffffff", 
                        "shade_ebebeb")
      topcols <- c(dcols, groupcolors)
      bottomcols <- c(dcols, groupcolors)
    }
    else {
      ngroupcolors <- nlevels(factor(d$Color[d$Location == 
                                               "Top"]))
      if (ngroupcolors > 15) {
        if (!requireNamespace(c("RColorBrewer"), quietly = TRUE) == 
            TRUE) {
          stop("Please install RColorBrewer to add color attribute for more than 15 colors.", 
               call. = FALSE)
        }
        else {
          getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 
                                                                            "Spectral"))
          topcols <- c(rep(x = c(color1, color2), length.out = nvarcolors, 
                           each = 1), getPalette(ngroupcolors), "#FFFFFF", 
                       "#EBEBEB")
        }
      }
      else {
        pal <- pal <- c("#009292", "#920000", "#490092", 
                        "#db6d00", "#24ff24", "#ffff6d", "#000000", 
                        "#006ddb", "#004949", "#924900", "#ff6db6", 
                        "#6db6ff", "#b66dff", "#ffb6db", "#b6dbff")
        topcols <- c(rep(x = c(color1, color2), length.out = nvarcolors, 
                         each = 1), pal[1:ngroupcolors], "#FFFFFF", 
                     "#EBEBEB")
      }
      names(topcols) <- c(levels(factor(lims$Color)), levels(factor(d$Color[d$Location == 
                                                                              "Top"])), "shade_ffffff", "shade_ebebeb")
      ngroupcolors <- nlevels(factor(d$Color[d$Location == 
                                               "Bottom"]))
      if (ngroupcolors > 15) {
        if (!requireNamespace(c("RColorBrewer"), quietly = TRUE) == 
            TRUE) {
          stop("Please install RColorBrewer to add color attribute for more than 15 colors.", 
               call. = FALSE)
        }
        else {
          getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 
                                                                            "Spectral"))
          bottomcols <- c(rep(x = c(color1, color2), 
                              length.out = nvarcolors, each = 1), getPalette(ngroupcolors), 
                          "#FFFFFF", "#EBEBEB")
        }
      }
      else {
        pal <- pal <- c("#009292", "#920000", "#490092", 
                        "#db6d00", "#24ff24", "#ffff6d", "#000000", 
                        "#006ddb", "#004949", "#924900", "#ff6db6", 
                        "#6db6ff", "#b66dff", "#ffb6db", "#b6dbff")
        bottomcols <- c(rep(x = c(color1, color2), length.out = nvarcolors, 
                            each = 1), pal[1:ngroupcolors], "#FFFFFF", 
                        "#EBEBEB")
      }
      names(bottomcols) <- c(levels(factor(lims$Color)), 
                             levels(factor(d$Color[d$Location == "Bottom"])), 
                             "shade_ffffff", "shade_ebebeb")
    }
  }
  else {
    names(d_order)[names(d_order) == "Group"] <- "Color"
    topcols <- c(rep(x = c(color1, color2), length.out = nvarcolors, 
                     each = 1), "#FFFFFF", "#EBEBEB")
    names(topcols) <- c(levels(factor(lims$Color)), "shade_ffffff", 
                        "shade_ebebeb")
    bottomcols <- c(rep(x = c(color1, color2), length.out = nvarcolors, 
                        each = 1), "#FFFFFF", "#EBEBEB")
    names(bottomcols) <- c(levels(factor(lims$Color)), "shade_ffffff", 
                           "shade_ebebeb")
  }
  d_order <- merge(d_order, dinfo, by = "rowid")
  p1 <- ggplot() + eval(parse(text = backpanel1))
  if ("Shape" %in% names(d)) {
    p1 <- p1 + geom_point(data = d_order[d_order$Location == 
                                           "Top", ], aes(x = pos_index, y = pval, color = Color, 
                                                         shape = factor(Shape)), alpha = opacity)
    p1 <- p1 + scale_shape_manual(values=c(25,24))
  }
  else {
    p1 <- p1 + geom_point(data = d_order[d_order$Location == 
                                           "Top", ], aes(x = pos_index, y = pval, color = Color), 
                          alpha = opacity)
  }
  p1 <- p1 + scale_x_continuous(breaks = lims$av, labels = lims$Color, 
                                expand = c(0, 0))
  if (grpblocks == TRUE) {
    if (freey == TRUE) {
      print("Sorry, drawing grpblocks with freey=TRUE is currently unsupported and will be ignored.")
    }
    else {
      p1 <- p1 + geom_rect(data = lims, aes(xmin = posmin - 
                                              0.5, xmax = posmax + 0.5, ymin = -Inf, ymax = min(d_order$pval), 
                                            fill = as.factor(Color)), alpha = 1)
    }
  }
  p1 <- p1 + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), 
                   axis.title.x = element_blank(), legend.position = "top", 
                   legend.title = element_blank())
  p2 <- ggplot() + eval(parse(text = backpanel2))
  if ("Shape" %in% bottomn) {
    p2 <- p2 + geom_point(data = d_order[d_order$Location == 
                                           "Bottom", ], aes(x = pos_index, y = pval, color = Color, 
                                                            shape = factor(Shape)), alpha = opacity)
    p2 <- p2 + scale_shape_manual(values=c(25,24))
  }
  else {
    p2 <- p2 + geom_point(data = d_order[d_order$Location == 
                                           "Bottom", ], aes(x = pos_index, y = pval, color = Color), 
                          alpha = opacity)
  }
  p2 <- p2 + scale_x_continuous(breaks = lims$av, labels = lims$Color, 
                                expand = c(0, 0))
  if (grpblocks == TRUE) {
    p2 <- p2 + geom_rect(data = lims, aes(xmin = posmin - 
                                            0.5, xmax = posmax + 0.5, ymin = -Inf, ymax = min(d_order$pval), 
                                          fill = as.factor(Color)), alpha = 1)
  }
  p2 <- p2 + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), 
                   axis.title.x = element_blank(), legend.position = "bottom", 
                   legend.title = element_blank())
  if ("Color" %in% names(d)) {
    p1 <- p1 + scale_colour_manual(name = "Color", values = topcols) + 
      scale_fill_manual(name = "Color", values = topcols)
    p1 <- p1 + guides(fill = "none")
    p2 <- p2 + scale_colour_manual(name = "Color", values = bottomcols) + 
      scale_fill_manual(name = "Color", values = bottomcols)
    p2 <- p2 + guides(fill = "none")
  }
  else {
    p1 <- p1 + scale_colour_manual(name = "Color", values = topcols) + 
      scale_fill_manual(name = "Color", values = topcols)
    p1 <- p1 + guides(color = "none", fill = "none")
    p2 <- p2 + scale_colour_manual(name = "Color", values = bottomcols) + 
      scale_fill_manual(name = "Color", values = bottomcols)
    p2 <- p2 + guides(color = "none", fill = "none")
  }
  if (!missing(highlight_var)) {
    if ("Shape" %in% topn) {
      p1 <- p1 + geom_point(data = d_order[d_order$Variable %in% 
                                             highlight_var & d_order$Location == "Top", ], 
                            aes(x = pos_index, y = pval, shape = Shape), 
                            colour = highlighter)
      p1 <- p1 + guides(shape = guide_legend(override.aes = list(colour = "black")))
    }
    else {
      p1 <- p1 + geom_point(data = d_order[d_order$Variable %in% 
                                             highlight_var & d_order$Location == "Top", ], 
                            aes(x = pos_index, y = pval), colour = highlighter)
    }
    if ("Shape" %in% bottomn) {
      p2 <- p2 + geom_point(data = d_order[d_order$Variable %in% 
                                             highlight_var & d_order$Location == "Bottom", 
                                           ], aes(x = pos_index, y = pval, shape = Shape), 
                            colour = highlighter)
      p2 <- p2 + guides(shape = guide_legend(override.aes = list(colour = "black")))
    }
    else {
      p2 <- p2 + geom_point(data = d_order[d_order$Variable %in% 
                                             highlight_var & d_order$Location == "Bottom", 
                                           ], aes(x = pos_index, y = pval), colour = highlighter)
    }
  }
  if (!missing(highlight_p)) {
    if ("Shape" %in% topn) {
      p1 <- p1 + geom_point(data = d_order[d_order$pvalue < 
                                             highlight_p[1] & d_order$Location == "Top", ], 
                            aes(x = pos_index, y = pval, shape = Shape), 
                            colour = highlighter)
      p1 <- p1 + guides(shape = guide_legend(override.aes = list(colour = "black")))
    }
    else {
      p1 <- p1 + geom_point(data = d_order[d_order$pvalue < 
                                             highlight_p[1] & d_order$Location == "Top", ], 
                            aes(x = pos_index, y = pval), colour = highlighter)
    }
    if ("Shape" %in% bottomn) {
      p2 <- p2 + geom_point(data = d_order[d_order$pvalue < 
                                             highlight_p[2] & d_order$Location == "Bottom", 
                                           ], aes(x = pos_index, y = pval, shape = Shape), 
                            colour = highlighter)
      p2 <- p2 + guides(shape = guide_legend(override.aes = list(colour = "black")))
    }
    else {
      p2 <- p2 + geom_point(data = d_order[d_order$pvalue < 
                                             highlight_p[2] & d_order$Location == "Bottom", 
                                           ], aes(x = pos_index, y = pval), colour = highlighter)
    }
  }
  if (!missing(tline)) {
    for (i in 1:length(tline)) {
      p1 <- p1 + geom_hline(yintercept = tredline[i], colour = "red")
    }
  }
  if (!missing(bline)) {
    for (i in 1:length(bline)) {
      p2 <- p2 + geom_hline(yintercept = bredline[i], colour = "red")
    }
  }
  if (!missing(annotate_p)) {
    if (!requireNamespace(c("ggrepel"), quietly = TRUE) == 
        TRUE) {
      print("Consider installing 'ggrepel' for improved text annotation")
      p1 <- p1 + geom_text(data = d_order[d_order$pvalue < 
                                            annotate_p[1] & d_order$Location == "Top", ], 
                           aes(pos_index, pval, label = Variable))
      p2 <- p2 + geom_text(data = d_order[d_order$pvalue < 
                                            annotate_p[2] & d_order$Location == "Bottom", 
                                          ], aes(pos_index, pval, label = Variable))
    }
    else {
      p1 <- p1 + ggrepel::geom_text_repel(data = d_order[d_order$pvalue < 
                                                           annotate_p[1] & d_order$Location == "Top", ], 
                                          aes(pos_index, pval, label = Variable), size=3)
      p2 <- p2 + ggrepel::geom_text_repel(data = d_order[d_order$pvalue < 
                                                           annotate_p[2] & d_order$Location == "Bottom", 
                                                         ], aes(pos_index, pval, label = Variable), size=3)
    }
  }
  if (!missing(annotate_var)) {
    if (!requireNamespace(c("ggrepel"), quietly = TRUE) == 
        TRUE) {
      print("Consider installing 'ggrepel' for improved text annotation")
      p1 <- p1 + geom_text(data = d_order[d_order$Variable %in% 
                                            annotate_var & d_order$Location == "Top", ], 
                           aes(pos_index, pval, label = Variable))
      p2 <- p2 + geom_text(data = d_order[d_order$Variable %in% 
                                            annotate_var & d_order$Location == "Bottom", 
                                          ], aes(pos_index, pval, label = Variable))
    }
    else {
      p1 <- p1 + ggrepel::geom_text_repel(data = d_order[d_order$Variable %in% 
                                                           annotate_var & d_order$Location == "Top", ], 
                                          aes(pos_index, pval, label = Variable))
      p2 <- p2 + ggrepel::geom_text_repel(data = d_order[d_order$Variable %in% 
                                                           annotate_var & d_order$Location == "Bottom", 
                                                         ], aes(pos_index, pval, label = Variable))
    }
  }
  p1 <- p1 + ylab(yaxislab1)
  p2 <- p2 + ylab(yaxislab2)
  if (grpblocks == TRUE) {
    p1 <- p1 + theme(axis.text.x = element_text(vjust = 1), 
                     axis.ticks.x = element_blank()) + ylim(c(yaxismin1, 
                                                              yaxismax1))
    p2 <- p2 + scale_y_reverse(limits = c(yaxismax2, yaxismin2)) + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  else {
    p1 <- p1 + theme(axis.text.x = element_text(vjust = 1), 
                     axis.ticks.x = element_blank()) + scale_y_continuous(limits = c(yaxismin1, 
                                                                                     yaxismax1), expand = expansion(mult = c(0, 0.1)))
    p2 <- p2 + scale_y_reverse(limits = c(yaxismax1, yaxismin2), 
                               expand = expansion(mult = c(0.1, 0))) + theme(axis.text.x = element_blank(), 
                                                                             axis.ticks.x = element_blank())
  }
  if (background == "white") {
    p1 <- p1 + theme(panel.background = element_rect(fill = "white"))
    p2 <- p2 + theme(panel.background = element_rect(fill = "white"))
  }
  if (rotatelabels == TRUE) {
    p1 <- p1 + theme(axis.text.x = element_text(angle = labelangle))
  }
  print(paste("Saving plot to ", file, ".png", sep = ""))
  p <- grid.arrange(arrangeGrob(p1, top = toptitle), arrangeGrob(p2, 
                                                                 bottom = bottomtitle), padding = 0, heights = c(hgtratio, 
                                                                                                                 1 - hgtratio))
  ggsave(p, filename = paste(file, ".png", sep = ""), dpi = res, 
         units = "in", height = hgt, width = wi)
  return(p)
}

VarCat_nopf <- read.delim("/Users/Nicen/Desktop/Anemia/Data/VarCat_nopf.txt")
colnames(VarCat_nopf) <- c("Variable","Desc")
VarDescription <- read.csv("~/Desktop/Anemia/Data/VarDescription.csv")
VarDescriptionR = VarDescription[VarDescription$series == "2005-2006",]
VarDesc = VarDescriptionR[,c("var","var_desc")]

# Hemoglobin
EWAS_hemo_Children_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_hemo_Children_replication_results_adj.txt", sep="")
EWAS_hemo_Children_replication_results_adj$Shape <- "Children"
EWAS_hemo_adult_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_hemo_adult_replication_results_adj.txt", sep="")
EWAS_hemo_adult_replication_results_adj$Shape <- "Adults"

EWAS_hemo_replication_results_adj <- rbind.data.frame(EWAS_hemo_Children_replication_results_adj,EWAS_hemo_adult_replication_results_adj)
EWAS_hemo_replication_results_adj$Direction = ifelse(EWAS_hemo_replication_results_adj$Beta< 0,"Negative","Positive")

Desc_EWAS_hemo_replication_results_adj = merge.data.frame(EWAS_hemo_replication_results_adj,VarCat_nopf,by="Variable")
hemo_rep_emirror = merge.data.frame(Desc_EWAS_hemo_replication_results_adj,VarDesc,by.x="Variable",by.y="var")

hemo_rep_emirror = hemo_rep_emirror[,c(18,13,15,17,16)]
colnames(hemo_rep_emirror) = c("Variable","pvalue","Shape","Group","Direction")

hemo_rep_emirror_child <- hemo_rep_emirror[hemo_rep_emirror$Shape == "Children",]
hemo_rep_emirror_child <- hemo_rep_emirror_child[,c(1,2,5,4)]
colnames(hemo_rep_emirror_child) = c("Variable","pvalue","Shape","Group")

hemo_rep_emirror_adult <- hemo_rep_emirror[!hemo_rep_emirror$Shape == "Children",]
hemo_rep_emirror_adult <- hemo_rep_emirror_adult[,c(1,2,5,4)]
colnames(hemo_rep_emirror_adult) = c("Variable","pvalue","Shape","Group")

hudson_hemo <- modifiedhudson(top=hemo_rep_emirror_adult, bottom=hemo_rep_emirror_child, tline=0.05, bline=0.05, annotate_p = c(0.05,0.05),
                        highlight_p= c(0.05,0.05), rotatelabels=1, labelangle=90, highlighter="red", color1='#53868B', color2 = '#4D4D4D', toptitle = "EWAS results for hemoglobin concentration in adults", 
                        bottomtitle = "EWAS results for hemoglobin concentration in children")

# Vitamin B12
EWAS_vb12_Children_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_vb12_Children_replication_results_adj.txt", sep="")
EWAS_vb12_Children_replication_results_adj$Shape <- "Children"
EWAS_vb12_adult_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_vb12_adult_replication_results_adj.txt", sep="")
EWAS_vb12_adult_replication_results_adj$Shape <- "Adults"

EWAS_vb12_replication_results_adj <- rbind.data.frame(EWAS_vb12_Children_replication_results_adj,EWAS_vb12_adult_replication_results_adj)
EWAS_vb12_replication_results_adj$Direction = ifelse(EWAS_vb12_replication_results_adj$Beta< 0,"Negative","Positive")

Desc_EWAS_vb12_replication_results_adj = merge.data.frame(EWAS_vb12_replication_results_adj,VarCat_nopf,by="Variable")
vb12_rep_emirror = merge.data.frame(Desc_EWAS_vb12_replication_results_adj,VarDesc,by.x="Variable",by.y="var")

vb12_rep_emirror = vb12_rep_emirror[,c(18,13,15,17,16)]
colnames(vb12_rep_emirror) = c("Variable","pvalue","Shape","Group","Direction")

vb12_rep_emirror_child <- vb12_rep_emirror[vb12_rep_emirror$Shape == "Children",]
vb12_rep_emirror_child <- vb12_rep_emirror_child[,c(1,2,5,4)]
colnames(vb12_rep_emirror_child) = c("Variable","pvalue","Shape","Group")

vb12_rep_emirror_adult <- vb12_rep_emirror[!vb12_rep_emirror$Shape == "Children",]
vb12_rep_emirror_adult <- vb12_rep_emirror_adult[,c(1,2,5,4)]
colnames(vb12_rep_emirror_adult) = c("Variable","pvalue","Shape","Group")

hudson_vb12 <- modifiedhudson(top=vb12_rep_emirror_adult, bottom=vb12_rep_emirror_child, tline=0.05, bline=0.05, annotate_p = c(0.05,0.05),
                              highlight_p= c(0.05,0.05), rotatelabels=1, labelangle=90, highlighter="red", color1='#53868B', color2 = '#4D4D4D', toptitle = "EWAS results for vitamin B12 concentration in adults", 
                              bottomtitle = "EWAS results for vitamin B12 concentration in children")

# RBC Folate
EWAS_RBF_Children_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_RBF_Children_replication_results_adj.txt", sep="")
EWAS_RBF_Children_replication_results_adj$Shape <- "Children"
EWAS_RBF_adult_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_RBF_adult_replication_results_adj.txt", sep="")
EWAS_RBF_adult_replication_results_adj$Shape <- "Adults"

EWAS_RBF_replication_results_adj <- rbind.data.frame(EWAS_RBF_Children_replication_results_adj,EWAS_RBF_adult_replication_results_adj)
EWAS_RBF_replication_results_adj$Direction = ifelse(EWAS_RBF_replication_results_adj$Beta< 0,"Negative","Positive")

Desc_EWAS_RBF_replication_results_adj = merge.data.frame(EWAS_RBF_replication_results_adj,VarCat_nopf,by="Variable")
RBF_rep_emirror = merge.data.frame(Desc_EWAS_RBF_replication_results_adj,VarDesc,by.x="Variable",by.y="var")

RBF_rep_emirror = RBF_rep_emirror[,c(18,13,15,17,16)]
colnames(RBF_rep_emirror) = c("Variable","pvalue","Shape","Group","Direction")

RBF_rep_emirror_child <- RBF_rep_emirror[RBF_rep_emirror$Shape == "Children",]
RBF_rep_emirror_child <- RBF_rep_emirror_child[,c(1,2,5,4)]
colnames(RBF_rep_emirror_child) = c("Variable","pvalue","Shape","Group")

RBF_rep_emirror_adult <- RBF_rep_emirror[!RBF_rep_emirror$Shape == "Children",]
RBF_rep_emirror_adult <- RBF_rep_emirror_adult[,c(1,2,5,4)]
colnames(RBF_rep_emirror_adult) = c("Variable","pvalue","Shape","Group")

hudson_RBF <- modifiedhudson(top=RBF_rep_emirror_adult, bottom=RBF_rep_emirror_child, tline=0.05, bline=0.05, annotate_p = c(0.05,0.05),
                              highlight_p= c(0.05,0.05), rotatelabels=1, labelangle=90, highlighter="red", color1='#53868B', color2 = '#4D4D4D', toptitle = "EWAS results for RBC folate concentration in adults", 
                              bottomtitle = "EWAS results for RBC folate concentration in children")

# Iron
EWAS_IRN_Children_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_IRN_Children_replication_results_adj.txt", sep="")
EWAS_IRN_Children_replication_results_adj$Shape <- "Children"
EWAS_IRN_adult_replication_results_adj <- read.csv("~/Desktop/Anemia/EWAS_IRN_adult_replication_results_adj.txt", sep="")
EWAS_IRN_adult_replication_results_adj$Shape <- "Adults"

EWAS_IRN_replication_results_adj <- rbind.data.frame(EWAS_IRN_Children_replication_results_adj,EWAS_IRN_adult_replication_results_adj)
EWAS_IRN_replication_results_adj$Direction = ifelse(EWAS_IRN_replication_results_adj$Beta< 0,"Negative","Positive")

Desc_EWAS_IRN_replication_results_adj = merge.data.frame(EWAS_IRN_replication_results_adj,VarCat_nopf,by="Variable")
IRN_rep_emirror = merge.data.frame(Desc_EWAS_IRN_replication_results_adj,VarDesc,by.x="Variable",by.y="var")

IRN_rep_emirror = IRN_rep_emirror[,c(18,13,15,17,16)]
colnames(IRN_rep_emirror) = c("Variable","pvalue","Shape","Group","Direction")

IRN_rep_emirror_child <- IRN_rep_emirror[IRN_rep_emirror$Shape == "Children",]
IRN_rep_emirror_child <- IRN_rep_emirror_child[,c(1,2,5,4)]
colnames(IRN_rep_emirror_child) = c("Variable","pvalue","Shape","Group")

IRN_rep_emirror_adult <- IRN_rep_emirror[!IRN_rep_emirror$Shape == "Children",]
IRN_rep_emirror_adult <- IRN_rep_emirror_adult[,c(1,2,5,4)]
colnames(IRN_rep_emirror_adult) = c("Variable","pvalue","Shape","Group")

hudson_IRN <- modifiedhudson(top=IRN_rep_emirror_adult, bottom=IRN_rep_emirror_child, tline=0.05, bline=0.05, annotate_p = c(0.05,0.05),
                             highlight_p= c(0.05,0.05), rotatelabels=1, labelangle=90, highlighter="red", color1='#53868B', color2 = '#4D4D4D', toptitle = "EWAS results for iron concentration in adults", 
                             bottomtitle = "EWAS results for iron concentration in children")
