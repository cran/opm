

library(testthat)
context("Testing the multiple-testing functions of the OPM package")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


EXPL.OPMS <- c(THIN.AGG, THIN.AGG)
EXPL.DF <- extract(EXPL.OPMS,
  as.labels = list("organism", "run"), subset = "A", dataframe = TRUE)


################################################################################


## check_mcp_sep
## UNTESTED


## opm_mcp
test_that("opm_mcp outputs converted data frames", {
  # Without computation of multiple comparisons of means
  x <- opm_mcp(EXPL.DF, model = list("organism", "run"), output = "data")
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(384L, 5L))
  expect_equal(attr(x, "joined.columns"), NULL)
  y <- opm_mcp(EXPL.DF, model = ~ organism + run, output = "data")
  expect_equal(x, y)
  y <- opm_mcp(EXPL.OPMS, model = ~ organism + run, output = "data")
  expect_equal(x, y)
  # conduct joining of columns
  y <- opm_mcp(EXPL.OPMS, model = ~ J(organism + run), output = "data")
  expect_equal(dim(y), c(384L, 6L))
  expect_equivalent(x, y[, setdiff(colnames(y), "organism.run")])
  expect_equal(attr(y, "joined.columns"),
    list(organism.run = c("organism", "run")))
  # with the wells
  y <- opm_mcp(EXPL.OPMS, model = ~ J(organism, Well) + run, output = "data")
  expect_equal(dim(y), c(384L, 6L))
  expect_equivalent(x, y[, setdiff(colnames(y), "organism.Well")])
  expect_equal(attr(y, "joined.columns"),
    list(organism.Well = c("organism", "Well")))
  # with the wells
  y <- opm_mcp(EXPL.OPMS, model = ~ J(organism, Well, run), output = "data")
  expect_equal(dim(y), c(384L, 6L))
  expect_equivalent(x, y[, setdiff(colnames(y), "organism.Well.run")])
  expect_equal(attr(y, "joined.columns"),
    list(organism.Well.run = c("organism", "Well", "run")))
})


## opm_mcp
test_that("opm_mcp converts 'model' arguments", {
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    output = "model")
  expect_equal(got, Value ~ organism + run)
  expect_equal(attr(got, "combine"), NULL)
  got <- opm_mcp(EXPL.DF, model = list("foo", c("bar", "baz")),
    output = "model")
  expect_equal(got, Value ~ foo + bar.baz)
  expect_equal(attr(got, "combine"), NULL)
  got <- opm_mcp(EXPL.DF, model = ~ foo + bar$baz, output = "model")
  expect_equal(got, Value ~ foo + bar.baz)
  expect_equal(attr(got, "combine"), NULL)
  got <- opm_mcp(EXPL.DF, model = ~ k & J(foo + bar$baz), output = "model")
  expect_equal(got, Value ~ k & foo.bar.baz)
  expect_equal(attr(got, "combine"), list(foo.bar.baz = c("foo", "bar.baz")))
  got <- opm_mcp(EXPL.DF,
    model = ~ k | J(`foo-foo`, bar$`?baz`, Well), output = "model")
  expect_equal(got, Value ~ k | foo.foo.bar..baz.Well)
  expect_equal(attr(got, "combine"),
    list(`foo-foo.bar.?baz.Well` = c("foo-foo", "bar.?baz", "Well")))
})


if (suppressWarnings(suppressPackageStartupMessages(
  require(multcomp, quietly = TRUE, warn.conflicts = FALSE)))) {

## opm_mcp
test_that("opm_mcp converts numeric 'linfct' arguments", {
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    linfct = c(Tukey = 1), output = "linfct")
  expect_equal(got, multcomp::mcp(organism = "Tukey"))
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    linfct = c(Tukey = 2), output = "linfct")
  expect_equal(got, multcomp::mcp(run = "Tukey"))
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    linfct = c(Dunnett = 1, Tukey = 2), output = "linfct")
  expect_equal(got, multcomp::mcp(organism = "Dunnett", run = "Tukey"))
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    linfct = 1:2, output = "linfct")
  expect_equal(got, multcomp::mcp(organism = opm_opt("contrast.type"),
    run = opm_opt("contrast.type")))
})


## opm_mcp
test_that("opm_mcp converts other 'linfct' arguments", {
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    linfct = list("organism", "run"), output = "linfct")
  expect_equal(got, multcomp::mcp(organism = opm_opt("contrast.type"),
    run = opm_opt("contrast.type")))
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    linfct = ~ organism + run, output = "linfct")
  expect_equal(got, multcomp::mcp(organism = opm_opt("contrast.type"),
    run = opm_opt("contrast.type")))
})


## opm_mcp
test_that("opm_mcp converts Pairs-like 'linfct' arguments", {
  got <- opm_mcp(EXPL.DF, model = ~ J(Well, run),
    linfct = c(Pairs = 1L), output = "linfct")
  expect_is(got$Well.run, "character")
  expect_is(names(got$Well.run), "character")
  expect_equal(length(got$Well.run), 96L) # one comparison per well
  expect_equal(do.call(multcomp::mcp, list(Well.run = got$Well.run)), got)
  got.2 <- opm_mcp(EXPL.OPMS, model = ~ J(Well, run),
    linfct = c(Pairs.Well = 1L), output = "linfct")
  expect_equal(got, got.2)
  expect_error(opm_mcp(EXPL.DF, model = ~ J(Well, organism) + run,
    linfct = c(Pairs = 1L), output = "linfct")) # no pairs
})


## annotated
test_that("Pairs-like tests are converted by annotated() to continuous data", {
  expl.opms <- EXPL.OPMS[, , 2:5]
  # full substrate names, wells first
  x <- opm_mcp(expl.opms, model = ~ J(Well, run),
    linfct = c(Pairs = 1L), output = "mcp")
  got <- annotated(x)
  expect_is(got, "numeric")
  expect_equal(length(got), 4L)
  expect_is(names(got), "character")
  expect_true(!any(is.na(names(got))))
  # full substrate names, wells second
  x <- opm_mcp(expl.opms, model = ~ J(run, Well),
    linfct = c(Pairs.Well = 1L), output = "mcp")
  got.2 <- annotated(x)
  expect_equal(got, got.2)
  # full substrate names, wells first, brackets
  x <- opm_mcp(expl.opms, model = ~ J(Well, run),
    linfct = c(Pairs = 1L), output = "mcp", brackets = TRUE)
  got.2 <- annotated(x)
  expect_equal(got, got.2)
  # full substrate names, wells second, brackets
  x <- opm_mcp(expl.opms, model = ~ J(run, Well),
    linfct = c(Pairs.Well = 1L), output = "mcp", brackets = TRUE)
  got.2 <- annotated(x)
  expect_equal(got, got.2)
  # abbreviated substrate names, wells first
  x <- opm_mcp(expl.opms, model = ~ J(Well, run),
    linfct = c(Pairs = 1L), output = "mcp", full = FALSE)
  got.2 <- annotated(x)
  expect_equal(got, got.2)
  # abbreviated substrate names, wells second
  x <- opm_mcp(expl.opms, model = ~ J(run, Well),
    linfct = c(Pairs.Well = 1L), output = "mcp", full = FALSE)
  got.2 <- annotated(x)
  expect_equal(got, got.2)
  # substrate names w/o coordinates, wells first
  x <- opm_mcp(expl.opms, model = ~ J(Well, run),
    linfct = c(Pairs = 1L), output = "mcp", in.parens = FALSE)
  got.2 <- annotated(x)
  expect_is(got.2, "numeric")
  expect_equal(length(got.2), 4L)
  expect_true(setequal(names(got.2), names(got)))
  # substrate names w/o coordinates, wells second
  x <- opm_mcp(expl.opms, model = ~ J(run, Well),
    linfct = c(Pairs.Well = 1L), output = "mcp", in.parens = FALSE)
  got.2 <- annotated(x)
  expect_is(got.2, "numeric")
  expect_equal(length(got.2), 4L)
  expect_true(setequal(names(got.2), names(got)))
})


## annotated
test_that("Pairs-like tests are converted by annotated() to binary data", {
  x <- opm_mcp(EXPL.OPMS[, , 2:5], model = ~ J(Well, run),
    linfct = c(Pairs = 1L), output = "mcp")

  got <- annotated(x, output = "different")
  expect_is(got, "logical")
  expect_equal(length(got), 4L)
  expect_is(names(got), "character")
  expect_true(!any(is.na(names(got))))
  expect_true(all(got))

  got.2 <- annotated(x, output = "!0")
  expect_equal(got, got.2)
  got.2 <- annotated(x, output = "different")
  expect_equal(got, got.2)

  got.2 <- annotated(x, output = "=0")
  expect_equal(names(got), names(got.2))
  expect_is(got.2, "logical")
  expect_true(all(!got.2))
  got.3 <- annotated(x, output = "equal")
  expect_equal(got.3, got.2)

  got.2 <- annotated(x, output = ">0")
  expect_is(got.2, "logical")
  expect_true(all(!got.2))
  got.3 <- annotated(x, output = "larger")
  expect_equal(got.3, got.2)

  got.2 <- annotated(x, output = "<0")
  expect_is(got.2, "logical")
  expect_true(all(got.2))
  got.3 <- annotated(x, output = "smaller")
  expect_equal(got.3, got.2)

})


## opm_mcp
test_that("opm_mcp generates contrast matrices", {
  got <- opm_mcp(EXPL.DF[, 1:7], model = list("run", "Well"),
    linfct = list("run", "Well"), output = "contrast")
  expect_is(got, "list")
  expect_true(all(vapply(got, inherits, logical(1L), "contrMat")))
  expect_equal(names(got), c("run", "Well"))
})


## opm_mcp
test_that("opm_mcp yields an error with a missing model", {
  # Without computation of multiple comparisons of means
  # error model missing
  expect_error(opm_mcp(EXPL.DF, m.type = "lm",
    linfct = multcomp::mcp(run = "Dunnett")))
})


## opm_mcp
test_that("opm_mcp yields an error if factors are not variable", {
  # Without computation of multiple comparisons of means
  # error
  expect_error(x <- opm_mcp(EXPL.DF, m.type = "lm", model = list("run",
    "organism"), linfct = multcomp::mcp(organism = "Dunnett")))
})


## opm_mcp
test_that("opm_mcp runs an mcp with specified m.type and with linfct", {
  # when 'model' is missing, default model is used
  x <- opm_mcp(EXPL.DF, model = list("run"),
    m.type = "lm", linfct = multcomp::mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(attr(x, opm_string()), NULL)
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
  y <- opm_mcp(EXPL.OPMS, model = list("run"),
    m.type = "lm", linfct = multcomp::mcp(run = "Dunnett"))
  annot <- attr(y, opm_string())
  expect_is(y, "glht")
  expect_equal(names(y), names(x))
  expect_true(is.list(annot))
  expect_equal(annot$plate.type, "PM01")
})


## opm_mcp
test_that("mcp with specified m.type and with linfct, version 2", {
  # model is missing, op is stated
  # wrong 'model' is given
  expect_error(x <- opm_mcp(EXPL.DF,
    model = list("run", "dummyColName"), ops = "+", m.type = "lm",
    linfct = multcomp::mcp(run = "Dunnett")))
})

## opm_mcp
test_that("mcp with specified m.type and with linfct, version 3", {
  x <- opm_mcp(EXPL.DF, model = list("run"),
    op = "+", m.type = "lm", linfct = multcomp::mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified model", {
  # simple model statement, warning from glht()
  x <- opm_mcp(EXPL.DF, model = list("run"),
    linfct = multcomp::mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified model as list #1", {
  # no op, warning from glht()
  x <- opm_mcp(EXPL.DF, model = list("run", "Well"),
    linfct = multcomp::mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("mcp with specified model as list #2", {
  # m.type = aov and op
  x <- opm_mcp(EXPL.DF, model = list("run", "Well"), m.type = "aov", op = "+",
    linfct = multcomp::mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("misspecified 'linfct' yields an error", {
  # Error in mcp2matrix(model, linfct = linfct) :
  # Variable(s) 'run' have been specified in 'linfct'
  # but cannot be found in 'model'!
  expect_error(opm_mcp(EXPL.DF, model = Value ~ Well,
    linfct = multcomp::mcp(run = "Dunnett")))
})


## opm_mcp
test_that("without model, linfct and glht.arg specified", {
  # very simple
  x <- opm_mcp(EXPL.DF, model = list("run"),
    linfct = multcomp::mcp(run = "Dunnett"),
    alternative = "less")
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("with model, linfct and glht.arg specified", {
  x <- opm_mcp(EXPL.DF, model = ~ Well, m.type = "lm",
    linfct = multcomp::mcp(Well = "Dunnett"),
    alternative = "less")
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 95)
})

## opm_mcp
test_that("subset of wells with directly defined contrast matrix", {
  # only three comparisons. quite fast.
  rem <- -ncol(EXPL.DF):-(ncol(EXPL.DF) - 91L)
  x <- opm_mcp(EXPL.DF[, rem], model = Value ~ Well,
    linfct = multcomp::mcp(Well = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 3)
})

## opm_mcp
test_that("linfct as predefined object", {
  # TODO Lea: what precisely does this test that isn't tested elsewhere?
  # that linfct is stored in an object is trivial
  # see above
  a <- multcomp::mcp(Well = "Dunnett")
  rem <- -ncol(EXPL.DF):-(ncol(EXPL.DF) - 91L)
  x <- opm_mcp(EXPL.DF[, rem],
    model = Value ~ Well, m.type = "lm", linfct = a)
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 3)
})


## opm_mcp
test_that("linfct as predefined matrix-object", {
  x <- EXPL.DF[, -ncol(EXPL.DF):-(ncol(EXPL.DF) - 91L)]
  contr <- opm_mcp(x, model = ~ Well, output = "contrast")
  expect_is(contr, "list")
  expect_true(all(vapply(contr, inherits, NA, "contrMat")))
  contr <- contr$Well[c(1:3, 6), ]
  x <- opm_mcp(x, model = ~ Well, m.type = "lm", linfct = contr)
  expect_is(x, "glht")
  expect_equal(x$type, NULL)
  expect_true(is.list(x))
  expect_equal(length(x), 8)
  expect_equal(length(coef(x)), 4)
})


## opm_mcp
test_that("non-syntactic names can be present", {
  x <- EXPL.DF[, 1:10]
  colnames(x)[colnames(x) == "run"] <- "run nonsys"
  y <- opm_mcp(x, output = "data",
    model = ~ J(Well, `run nonsys`), linfct = c(Pairs = 1))
  expect_is(y, "data.frame")
  expect_equal(dim(y), c(28, 6))
})


## opm_mcp
test_that("'Pairs' contrast type can be combined with non-syntactic names", {
  x <- EXPL.DF[, 1:10]
  colnames(x)[colnames(x) == "run"] <- "run nonsys"
  y <- opm_mcp(x, output = "mcp", model = ~ J(Well, `run nonsys`),
    linfct = c(Pairs.Well = 1))
  expect_is(y, "glht")
  expect_equal(y$type, "User-defined")
  expect_true(is.list(y))
  expect_equal(length(y), 9)
  expect_equal(length(coef(y)), 7)
})


}


################################################################################


## convert_annotation_vector
## UNTESTED


################################################################################


## annotated
## UNTESTED




