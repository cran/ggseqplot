# Example data come from TraMineR
library(TraMineR)
# biofam data set
data(biofam)
# We use only a sample of 300 cases
set.seed(10)
biofam <- biofam[sample(nrow(biofam), 300), ]
biofam.lab <- c(
  "Parent", "Left", "Married", "Left+Marr",
  "Child", "Left+Child", "Left+Marr+Child", "Divorced"
)
biofam.seq <- seqdef(biofam, 10:25, labels = biofam.lab, weights = biofam$wp00tbgs)
biofam2.seq <- seqdef(biofam, 10:25, labels = biofam.lab)
group <- biofam$sex

data(ex1)
ex1.seq <- seqdef(ex1, 1:13, weights = ex1$weights)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqtrplot(biofam))
  expect_error(ggseqtrplot(biofam.seq, labsize = 1:10))
  expect_error(ggseqtrplot(biofam.seq, axislabs = "something"))
  expect_error(ggseqtrplot(biofam.seq, group = group[1:100]))
  expect_error(ggseqtrplot(biofam.seq, weighted = group))
  expect_error(ggseqrfplot(biofam.seq, diss = diss, which.plot = "medods"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("check if output of ggseqtrplot is ggplot", {
  expect_s3_class(ggseqtrplot(biofam.seq), "ggplot")
  expect_s3_class(ggseqtrplot(biofam2.seq), "ggplot")
  expect_s3_class(ggseqtrplot(biofam.seq, axislabs = "alphabet"), "ggplot")
  expect_s3_class(ggseqtrplot(biofam.seq, dss = FALSE), "ggplot")
  expect_s3_class(ggseqtrplot(biofam.seq, group = biofam$sex), "ggplot")
  expect_s3_class(ggseqtrplot(ex1.seq, weighted = FALSE), "ggplot")
  expect_s3_class(ggseqtrplot(ex1.seq, with.missing = TRUE), "ggplot")
  expect_s3_class(ggseqtrplot(ex1.seq, no.n = TRUE), "ggplot")
})
