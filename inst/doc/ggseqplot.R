## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8,
  fig.height=4.94
)

old <- options()        
on.exit(options(old))
options(rmarkdown.html_vignette.check_title = FALSE)

pkgs <- c("colorspace", "ggplot2", "ggthemes", "ggseqplot", "hrbrthemes", 
          "patchwork", "purrr", "TraMineR")

# Load all packages to library and adjust options
lapply(pkgs, library, character.only = TRUE)



## ----libraries, eval=FALSE----------------------------------------------------
#  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ## Load and download (if necessary) required packages ----
#  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  
#  
#  ## Save package names as a vector of strings
#  pkgs <- c("colorspace", "ggplot2", "ggthemes", "hrbrthemes",
#            "patchwork", "purrr", "TraMineR")
#  
#  
#  ## Install uninstalled packages
#  lapply(pkgs[!(pkgs %in% installed.packages())],
#         install.packages, repos = getOption("repos")["CRAN"])
#  
#  
#  ## Load all packages to library and adjust options
#  lapply(pkgs, library, character.only = TRUE)
#  
#  ## Don't forget to load ggseqplot
#  library(ggseqplot)
#  

## ----setup, message=FALSE-----------------------------------------------------


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Creating state sequence objects from example data sets ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## biofam data
data(biofam)

biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
                "Child", "Left+Child", "Left+Marr+Child", "Divorced")

biofam.seq <- seqdef(biofam[501:600, ], 10:25, # we only use a subsample
                     labels = biofam.lab,
                     weights = biofam$wp00tbgs[501:600])

## actcal data
data(actcal)

actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")

actcal.seq <- seqdef(actcal,13:24,
                     labels=actcal.lab)

## ex1 data
data(ex1)
ex1.seq <- seqdef(ex1, 1:13, 
                  weights=ex1$weights)


## ----seqstatd-----------------------------------------------------------------
seqstatd(actcal.seq)

## ----dplot-data---------------------------------------------------------------
dplot <- ggseqdplot(actcal.seq)
dplot$data

## ---- dplot1------------------------------------------------------------------
# ggseqplot::ggseqdplot
ggseqdplot(actcal.seq)

## ---- message=FALSE, warning=FALSE--------------------------------------------
ggseqdplot(actcal.seq) +
  scale_fill_discrete_sequential("heat") +
  scale_x_discrete(labels = month.abb) +
  labs(title = "State distribution plot",
       x = "Month") +
  guides(fill=guide_legend(title="Alphabet")) +
  theme_ipsum(base_family = "") + # ensures that this works on different OS
  theme(plot.title = element_text(size = 30, 
                                  margin=margin(0,0,20,0)),
        plot.title.position = "plot")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# Save plot using weights
p1 <- ggseqdplot(ex1.seq, 
                 with.entropy = TRUE) + 
  ggtitle("Weighted data")

# Save same plot without using weights
p2 <- ggseqdplot(ex1.seq, 
                 with.entropy = TRUE,
                 weighted = FALSE) + 
  ggtitle("Unweighted data")

# Arrange and refine plots using patchwork
p1 + p2 + 
  plot_layout(guides = "collect") &
  scale_fill_manual(values= canva_palettes$`Fun and tropical`[1:4]) &
  theme_ipsum(base_family = "") &
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank())

## -----------------------------------------------------------------------------
ggseqtrplot(actcal.seq, 
            group = actcal$sex)

## -----------------------------------------------------------------------------

p1 <- ggseqtrplot(biofam.seq, 
                  dss = FALSE, 
                  x_n.dodge = 2,
                  labsize = 3) +
  ggtitle("STS Sequences") +
  theme(plot.margin = unit(c(5,10,5,5), "points"))

p2 <- ggseqtrplot(biofam.seq, 
                  x_n.dodge = 2,
                  labsize = 3) +
    ggtitle("DSS Sequences") +
  theme(plot.margin = unit(c(5,5,5,10), "points"))

p1 + p2 &
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5))


## -----------------------------------------------------------------------------

p2 <- p2 +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank())

p1 + p2 &
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5))

## -----------------------------------------------------------------------------
## default plot
ggseqmtplot(actcal.seq, no.n = TRUE, error.bar = "SE") 

## flipped version
ggseqmtplot(actcal.seq, no.n = TRUE, error.bar = "SE") +
 coord_flip() +
 theme(axis.text.y=element_blank(),
       axis.ticks.y = element_blank(),
       panel.grid.major.y = element_blank(),
       legend.position = "top")

## -----------------------------------------------------------------------------
## default plot
ggseqiplot(actcal.seq, sortv = "from.end") + 
  scale_x_discrete(labels = month.abb) 

## flipped version
ggseqiplot(actcal.seq, sortv = "from.end") + 
  scale_x_discrete(labels = month.abb) +
  coord_flip()


## -----------------------------------------------------------------------------
## compute dissimilarity matrix required for plot
diss <- seqdist(biofam.seq, method = "LCS")

## Relative Frequency Sequence Plot
## default version
ggseqrfplot(biofam.seq, diss = diss, k = 12) 

## adjusted version
ggseqrfplot(biofam.seq, diss = diss, k = 12) &
  theme_ipsum(base_family = "") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 12)) &
  plot_annotation(title = "Relative Frequency Sequence Plot")

## -----------------------------------------------------------------------------
ggseqrfplot(biofam.seq, diss = diss, k = 12) +
  scale_x_discrete(labels = 15:30)

## -----------------------------------------------------------------------------
## save & view original plot
p <- ggseqrfplot(biofam.seq, diss = diss, k = 12)
p

## change appearance of sub-plots

## first component: index plot
p[[1]] <- p[[1]] +
  scale_x_discrete(labels = 15:30)

## second component: boxplot
p[[2]] <- p[[2]] + labs(title = "Changed title")

## adjusted plot
p

## -----------------------------------------------------------------------------
## Compute a pairwise dissimilarity matrix
diss <- seqdist(actcal.seq, method = "LCS")

## original plot
p <- ggseqrplot(actcal.seq, 
                diss = diss,
                nrep = 3,
                group = actcal$sex)
p

## adjusted sequence index subplots
p[[3]] <- p[[3]] +
  scale_x_discrete(labels = month.abb) 

p[[4]] <- p[[4]] +
  scale_x_discrete(labels = month.abb) 

p


## -----------------------------------------------------------------------------

diss <- seqdist(biofam.seq, method = "LCS")

sex <- biofam[501:600, "sex"]

p <- map2(
  levels(sex), # input x
  c("Men", "Women"), # input y
  function(x, y) {
    p <- ggseqrfplot(biofam.seq[sex == x,],
                     diss = diss[sex == x,sex == x],
                     k = 12)
    p[[1]] <- p[[1]] + labs(tag = y)
    return(p)
    }
  )

names(p) <- levels(sex)

(p$man & theme(legend.position = "none")) / p$woman

    

