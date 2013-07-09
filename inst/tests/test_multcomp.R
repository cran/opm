

library(testthat)
context("Testing the multiple-testing functions of the OPM package")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


EXPL.OPMS <- c(THIN.AGG, THIN.AGG)
EXPL.DF <- extract(EXPL.OPMS,
  as.labels = list("organism", "run"), subset = "A", dataframe = TRUE)


################################################################################


## opm_mcp
test_that("opm_mcp runs without actually performing mcp", {
  # Without computation of multiple comparisons of means
  x <- opm_mcp(EXPL.DF, model = list("organism", "run"), output = "data")
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(384L, 5L))
  y <- opm_mcp(EXPL.DF, model = ~ organism + run, output = "data")
  expect_equal(x, y)
  y <- opm_mcp(EXPL.OPMS, model = ~ organism + run, output = "data")
  expect_equal(x, y)
  # conduct joining of columns
  y <- opm_mcp(EXPL.OPMS, model = ~ J(organism + run), output = "data")
  expect_equal(dim(y), c(384L, 6L))
  expect_equivalent(x, y[, setdiff(colnames(y), "organism.run")])
  # with the wells
  y <- opm_mcp(EXPL.OPMS, model = ~ J(organism, Well) + run, output = "data")
  expect_equal(dim(y), c(384L, 6L))
  expect_equivalent(x, y[, setdiff(colnames(y), "organism.Well")])
})


## opm_mcp
test_that("opm_mcp converts 'model' arguments", {
  got <- opm_mcp(EXPL.DF, model = list("organism", "run"),
    output = "model")
  expect_equal(got, Value ~ organism + run)
  got <- opm_mcp(EXPL.DF, model = list("foo", c("bar", "baz")),
    output = "model")
  expect_equal(got, Value ~ foo + bar.baz)
  got <- opm_mcp(EXPL.DF, model = ~ foo + bar$baz, output = "model")
  expect_equal(got, Value ~ foo + bar.baz)
  got <- opm_mcp(EXPL.DF, model = ~ k & J(foo + bar$baz), output = "model")
  expect_equal(got, Value ~ k & foo.bar.baz)
  got <- opm_mcp(EXPL.DF, model = ~ k | J(foo, bar$baz, Well), output = "model")
  expect_equal(got, Value ~ k | foo.bar.baz.Well)
})


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
test_that("opm_mcp generates contrast matrices", {
  got <- opm_mcp(EXPL.DF[, 1:7], model = list("run", "Well"),
    linfct = list("run", "Well"), output = "contrast")
  expect_is(got, "list")
  expect_true(all(vapply(got, inherits, logical(1L), "contrMat")))
  expect_equal(names(got), c("run", "Well"))
})


## opm_mcp
test_that("error missing model", {
  # Without computation of multiple comparisons of means
  # error model missing
  expect_error(opm_mcp(EXPL.DF, m.type = "lm",
    linfct = multcomp::mcp(run = "Dunnett")))
})

## opm_mcp
test_that("test on assert_all_factors_are_variable", {
  # Without computation of multiple comparisons of means
  # error
  expect_error(x <- opm_mcp(EXPL.DF, m.type = "lm", model = list("run",
    "organism"), linfct = multcomp::mcp(organism = "Dunnett")))
})

## opm_mcp
test_that("mcp with specified m.type and with linfct", {
  # when 'model' is missing -> defaul-model is used
  x <- opm_mcp(EXPL.DF, model = list("run"),
    m.type = "lm", linfct = multcomp::mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
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
  suppressWarnings(x <- opm_mcp(EXPL.DF, model = list("run"),
    linfct = multcomp::mcp(run = "Dunnett")))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified model as list #1", {
  # no op, warning from glht()
  suppressWarnings(x <- opm_mcp(EXPL.DF, model = list("run", "Well"),
    linfct = multcomp::mcp(run = "Dunnett")))
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
  # very simple, warning from glht()
  suppressWarnings(x <- opm_mcp(EXPL.DF, model = list("run"),
    linfct = multcomp::mcp(run = "Dunnett"),
    glht.arg = list(alternative = "less")))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("with model, linfct and glht.arg specified", {
  # number of performed comparisons exceeds 20
  expect_warning(x <- opm_mcp(EXPL.DF, model = ~ Well, m.type = "lm",
    linfct = multcomp::mcp(Well = "Dunnett"),
    glht.arg = list(alternative = "less")))
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
  expect_true(all(vapply(contr, inherits, logical(1L), "contrMat")))
  contr <- contr$Well[c(1:3, 6), ]
  x <- opm_mcp(x, model = ~ Well, m.type = "lm", linfct = contr)
  expect_is(x, "glht")
  expect_equal(x$type, NULL)
  expect_true(is.list(x))
  expect_equal(length(x), 8)
  expect_equal(length(coef(x)), 4)
})

