### R code from vignette source 'opm.Rnw'

###################################################
### code chunk number 1: chunk.setup
###################################################
options(prompt = "R> ")
options(continue = "   ")
options(useFancyQuotes = FALSE)
library("methods")


###################################################
### code chunk number 2: load.library
###################################################
library(opm)


###################################################
### code chunk number 3: data.objects
###################################################
vaas_1
vaas_4


###################################################
### code chunk number 4: big.data
###################################################
data(vaas_et_al, package = "opmdata")


###################################################
### code chunk number 5: files
###################################################
files <- opm_files("testdata")
files


###################################################
### code chunk number 6: file.splitting
###################################################
multi.plate.file <- grep("Multiple", files,
  value = TRUE, ignore.case = TRUE)
multi.plate.file
list.files()
split_files(multi.plate.file, '^("Data File",|Data File)', getwd())
list.files()


###################################################
### code chunk number 7: tidy.up.I
###################################################
rm(multi.plate.file)
unlink(grep("Multiple-0000", list.files(),
  value = TRUE, ignore.case = TRUE))


###################################################
### code chunk number 8: example.opm
###################################################
example.opm <- read_opm(files, include = "*Example_?.csv.xz")
summary(example.opm)


###################################################
### code chunk number 9: many.plates
###################################################
many.plates <- read_opm(files, exclude = "*Multiple*", convert = "grp")
summary(many.plates)
summary(many.plates$PM01)
rm(many.plates) # tidy up


###################################################
### code chunk number 10: batch.opm.I
###################################################
batch_opm(files, include = "*Example_?.csv.xz",
  aggr.args = list(boot = 100, method = "opm-fast"),
  outdir = ".", demo = TRUE)


###################################################
### code chunk number 11: batch.opm.II
###################################################
batch.result <- batch_opm(files, include = "*Example_?.csv.xz",
  outdir = ".",
  aggr.args = list(boot = 100, method = "opm-fast"))


###################################################
### code chunk number 12: scripts
###################################################
opm_files("scripts")


###################################################
### code chunk number 13: collect.template.I
###################################################
metadata.example <- collect_template(files, include = "*Example_?.csv.xz")


###################################################
### code chunk number 14: collect.template.II
###################################################
collect_template(files, include = "*Example_?.csv.xz",
  outfile = "example_metadata.csv")


###################################################
### code chunk number 15: to.metadata.I
###################################################
metadata.example <- to_metadata("example_metadata.csv")


###################################################
### code chunk number 16: add.meta.column
###################################################
metadata.example$Colour <- c("blue", "red", "yellow")
metadata.example$Integer <- c(10L, 20L, 30L)


###################################################
### code chunk number 17: include.metadata.I
###################################################
example.opm <- include_metadata(example.opm, md = metadata.example)


###################################################
### code chunk number 18: metadata
###################################################
metadata(example.opm)


###################################################
### code chunk number 19: tidy.up.II
###################################################
unlink("example_metadata.csv")


###################################################
### code chunk number 20: to.metadata.II
###################################################
metadata(example.opm)
metadata(example.opm) <- to_metadata(csv_data(example.opm))[, 
  c("Strain Name", "Sample Number")]
metadata(example.opm)


###################################################
### code chunk number 21: map.metadata.I
###################################################
metadata(example.opm)
metadata(map_metadata(example.opm, Organism ~ `Sample Number`))


###################################################
### code chunk number 22: clean.metadata
###################################################
metadata(map_metadata(example.opm,
  Organism + `Sample Number` ~ list(`Sample Number`, NULL)))


###################################################
### code chunk number 23: OPMS.clean.metadata
###################################################
metadata(example.opm) <- Organism + `Sample Number` ~ 
  list(`Sample Number`, NULL)
metadata(example.opm)


###################################################
### code chunk number 24: metadata.expression
###################################################
metadata(example.opm) <- to_metadata(csv_data(example.opm))[, 
  c("Strain Name", "Sample Number")] # reset
metadata(example.opm)
metadata(example.opm) <- expression(Organism <- `Sample Number`,
  rm(`Sample Number`))
metadata(example.opm)


###################################################
### code chunk number 25: empty.metadata
###################################################
metadata(example.opm, "Organism") <- NULL
metadata(example.opm)
metadata(example.opm) <- list()
metadata(example.opm)


###################################################
### code chunk number 26: assign.metadata
###################################################
metadata(example.opm[2]) <- list(Organism = "Elephas maximus",
  Size = "3 meters")
metadata(example.opm)
metadata(example.opm[2]) <- list()
metadata(example.opm)


###################################################
### code chunk number 27: include.metadata.II
###################################################
example.opm <- include_metadata(example.opm, md = metadata.example)
metadata(example.opm)
example.opm <- map_metadata(example.opm, Character ~ as.character(Integer))
metadata(example.opm)
example.opm <- map_metadata(example.opm, `Times 10` ~ (Integer * 10))
metadata(example.opm)


###################################################
### code chunk number 28: metadata.chars
###################################################
example.opm <- include_metadata(example.opm, md = metadata.example)
md.map <- metadata_chars(example.opm, values = FALSE)
md.map


###################################################
### code chunk number 29: map.metadata.II
###################################################
md.map["Colour"] <- "Colony colour"
example.opm <- map_metadata(example.opm, md.map, values = FALSE)
metadata(example.opm)


###################################################
### code chunk number 30: map.metadata.III
###################################################
md.map <- metadata_chars(example.opm, values = TRUE)
md.map
md.map["red"] <- "green"
example.opm <- map_metadata(example.opm, md.map, values = TRUE)
metadata(example.opm)


###################################################
### code chunk number 31: merge.metadata.I
###################################################
metadata.example$Colour.Position <- as.character(interaction(
  metadata.example$Colour, 
  metadata.example$Position, sep = ".", drop = TRUE))


###################################################
### code chunk number 32: merge.metadata.II
###################################################
metadata(example.opm) <- Col.Int ~ paste(`Colony colour`, Integer, sep = ".")
metadata(example.opm)


###################################################
### code chunk number 33: load.data
###################################################
data(vaas_1)
vaas_1


###################################################
### code chunk number 34: aggregation
###################################################
vaas_1.reaggr <- do_aggr(vaas_1, boot = 100, method = "opm-fast")


###################################################
### code chunk number 35: aggregation.settings
###################################################
aggr_settings(vaas_1)
aggr_settings(vaas_1.reaggr)


###################################################
### code chunk number 36: aggregated
###################################################
summary(aggregated(vaas_1))
summary(aggregated(vaas_1.reaggr))


###################################################
### code chunk number 37: smoothing.splines
###################################################
op <- set_spline_options(type = "smooth.spline")
vaas_1.aggr2 <- do_aggr(vaas_1, boot = 10, method = "spline", options = op)


###################################################
### code chunk number 38: data.vaas.small
###################################################
data("vaas_et_al", package = "opmdata")
vaas.small <- vaas_et_al[, , c("A01", "G11", "H11")]
dim(vaas.small)


###################################################
### code chunk number 39: query.metadata.k.I
###################################################
"Experiment" %k% vaas_et_al
vaas_et_al %k% "Experiment" # equivalent
vaas_et_al %k% ~ Experiment # equivalent
(~ Experiment) %k% vaas_et_al # equivalent, parentheses needed


###################################################
### code chunk number 40: query.metadata.k.II
###################################################
c("Experiment", "Species") %k% vaas_et_al
vaas_et_al %k% ~ c(Experiment, Species) # equivalent


###################################################
### code chunk number 41: query.metadata.q.I
###################################################
c(Experiment = "First replicate",
  Species = "Escherichia coli") %q% vaas_et_al
vaas_et_al %q% ~ Experiment == "First replicate" & 
  Species == "Escherichia coli"


###################################################
### code chunk number 42: query.metadata.q.II
###################################################
list(Species = c("Escherichia coli", "Bacillus subtilis")) %q% vaas_et_al
vaas_et_al %q% ~ Species %in% c("Escherichia coli", "Bacillus subtilis")


###################################################
### code chunk number 43: query.metadata.q.III
###################################################
vaas.e.coli.1 <- vaas_et_al[c(Experiment = "First replicate",
  Species = "Escherichia coli") %q% vaas_et_al]
summary(vaas.e.coli.1)
rm(vaas.e.coli.1) # tidy up


###################################################
### code chunk number 44: check.metadata.key
###################################################
metadata_chars(vaas_et_al, values = FALSE)


###################################################
### code chunk number 45: check.metadata.values
###################################################
metadata(vaas_et_al, "Species")


###################################################
### code chunk number 46: subset
###################################################
vaas.1.6 <- subset(vaas_et_al,
  query = list(Experiment = "First replicate", 'Plate number' = 6))
summary(vaas.1.6)


###################################################
### code chunk number 47: extract.simple
###################################################
vaas.mu <- extract(vaas_et_al, dataframe = TRUE,
  as.labels = NULL, subset = "mu")


###################################################
### code chunk number 48: extract.complex
###################################################
vaas.mu <- extract(vaas_et_al, dataframe = TRUE,
  as.labels = list("Experiment","Number of sample time point",
    "Plate number", "Slot", "Species", "Strain", "Time point in min"),
  subset = "mu")


###################################################
### code chunk number 49: extract.with.joining
###################################################
vaas.mu <- extract(vaas_et_al, dataframe = TRUE,
  as.labels = ~ J(Species, Strain), subset = "mu")


###################################################
### code chunk number 50: xy.plot
###################################################
xy_plot(vaas.1.6, main = "E. coli vs. P. aeruginosa",
  include = list("Species", "Strain"))


###################################################
### code chunk number 51: opm.Rnw:1201-1203
###################################################
print(xy_plot(vaas.1.6, main = "E. coli vs. P. aeruginosa",
  include = list("Species", "Strain")))


###################################################
### code chunk number 52: xy.plot.II
###################################################
xy_plot(vaas.1.6[, , c("A01", "G11", "H11")],
  main = "E. coli vs. P. aeruginosa", include = list("Species", "Strain"))


###################################################
### code chunk number 53: opm.Rnw:1235-1238
###################################################
print(xy_plot(vaas.1.6[, , c("A01", "G11", "H11")],
  main = "E. coli vs. P. aeruginosa",
  include = list("Species", "Strain")))


###################################################
### code chunk number 54: level.plot.I
###################################################
level_plot(vaas.1.6, main = "E. coli vs. P. aeruginosa",
  include = list("Species", "Strain"))


###################################################
### code chunk number 55: opm.Rnw:1259-1261
###################################################
print(level_plot(vaas.1.6, main = "E. coli vs. P. aeruginosa",
  include = list("Species", "Strain")))


###################################################
### code chunk number 56: heat.map.I
###################################################
vaas.1.6.A <- heat_map(vaas.1.6, as.labels = "Strain",
  as.groups = "Species")


###################################################
### code chunk number 57: opm.Rnw:1294-1295
###################################################
heat_map(vaas.1.6, as.labels = "Strain", as.groups = "Species")


###################################################
### code chunk number 58: radial.plot.I
###################################################
radial_plot(vaas_4[, , c(1:5, 10)], as.labels = list("Species", "Strain"),
  x = 150, y = 200)


###################################################
### code chunk number 59: opm.Rnw:1325-1327
###################################################
radial_plot(vaas_4[, , c(1:5, 10)], as.labels = list("Species", "Strain"),
  x = 150, y = 200)


###################################################
### code chunk number 60: ci.plot.I
###################################################
ci_plot.legend <- ci_plot(vaas.1.6[, , c("A01", "A02", "A03")],
  as.labels = list("Species", "Strain"), subset = "A",
  legend.field = NULL, x = 170, y = 3)


###################################################
### code chunk number 61: opm.Rnw:1353-1354
###################################################
ci_plot(vaas.1.6[, , c("A01", "A02", "A03")], as.labels = list("Species", "Strain"), subset = "A", legend.field = NULL, x = 170, y = 3)


###################################################
### code chunk number 62: extract.dataframe
###################################################
x <- extract(vaas_et_al, as.labels = list("Species", "Strain"),
  dataframe = TRUE)


###################################################
### code chunk number 63: ci.plot.II
###################################################
# without normalisation
y <- extract(x, as.groups = TRUE,  norm.per = "none")
ci_plot(y[, c(1:7, 13)], legend.field = NULL, x = 350, y = 0)


###################################################
### code chunk number 64: opm.Rnw:1391-1392
###################################################
ci_plot(y[, c(1:7, 13)], legend.field = NULL, x = 350, y = 0)


###################################################
### code chunk number 65: normalisation
###################################################
# normalisation by plate means (figure not shown)
y <- extract(x, as.groups = TRUE,  norm.per = "row")
ci_plot(y[, c(1:7, 13)], legend.field = NULL, x = 150, y = 0)

# normalisation by well means (figure not shown)
y <- extract(x, as.groups = TRUE,  norm.per = "column")
ci_plot(y[, c(1:7, 13)], legend.field = NULL, x = 150, y = 0)


###################################################
### code chunk number 66: norm.well.A10
###################################################
# normalisation by subtraction of the well means of well A10 only
y <- extract(x, as.groups = TRUE,  norm.per = "row", norm.by = 10,
  subtract = TRUE)
ci_plot(y[, c(1:7, 13)], legend.field = NULL, x = 0, y = 0)


###################################################
### code chunk number 67: opm.Rnw:1436-1437
###################################################
ci_plot(y[, c(1:7, 13)], legend.field = NULL, x = 0, y = 0)


###################################################
### code chunk number 68: data.vaas.subset
###################################################
vaas.subset <- subset(vaas_et_al[, , "G06"], 
  list(Experiment = "First replicate"))


###################################################
### code chunk number 69: plot.vaas.subset
###################################################
xy_plot(vaas.subset, include = ~ Strain, neg.ctrl = FALSE,
  space = "right")


###################################################
### code chunk number 70: opm.Rnw:1474-1476
###################################################
xy_plot(vaas.subset, include = ~ Strain, neg.ctrl = FALSE, 
  space = "right")


###################################################
### code chunk number 71: opm.mcp.principle
###################################################
vaas.subset.mcp <- opm_mcp(vaas.subset, model = ~ Strain, m.type = "aov",
  linfct = c(Tukey = 1))


###################################################
### code chunk number 72: opm.mcp.linfct
###################################################
opm_mcp(vaas.subset, model = ~ Strain, m.type = "aov",
  linfct = c(Tukey = 1), output = "linfct")


###################################################
### code chunk number 73: plot.opm.mcp.linfct
###################################################
library(multcomp) # now needed
old.mar <- par(mar = c(3, 15, 3, 2)) # adapt margins in the plot
plot(vaas.subset.mcp)
par(old.mar) # reset to default plotting settings


###################################################
### code chunk number 74: opm.Rnw:1530-1534
###################################################
library(multcomp)
old.mar <- par(mar = c(3, 15, 3, 2))
plot(vaas.subset.mcp)
par(old.mar)


###################################################
### code chunk number 75: summary.opm.mcp
###################################################
mcp.summary <- summary(vaas.subset.mcp)
mcp.summary$model$call <- NULL # avoid some unnecessary output
mcp.summary


###################################################
### code chunk number 76: contrMat
###################################################
n <- c(10, 20, 30, 40)
names(n) <- paste("group", 1:4, sep = "")
contrMat(n, type = "Tukey")


###################################################
### code chunk number 77: mcp.summary.linfct
###################################################
mcp.summary$linfct


###################################################
### code chunk number 78: contrast.matrix.I
###################################################
contr <- opm_mcp(vaas_4[, , 1:4], model = ~ Well, linfct = c(Tukey = 1),
  output = "contrast")
contr


###################################################
### code chunk number 79: mcp.wells
###################################################
vaas4.mcp <- opm_mcp(vaas_4[, , 1:4], model = ~ Well, m.type = "lm",
  linfct = contr$Well[c(1:3, 6), ])


###################################################
### code chunk number 80: summary.mcp.wells
###################################################
summary(vaas4.mcp)$linfct


###################################################
### code chunk number 81: opm.Rnw:1633-1636
###################################################
old.mar <- par(mar = c(3, 20, 3, 2)) # adapt plotting settings
plot(vaas4.mcp)
par(old.mar) # reset plotting settings


###################################################
### code chunk number 82: model.mcp
###################################################
vaas4.mcp <- opm_mcp(vaas_4, model = ~ Species, m.type = "lm",
  linfct = mcp(Species = "Dunnett"))


###################################################
### code chunk number 83: mcp.output.data
###################################################
vaas4.mcp <- opm_mcp(vaas_4, model = ~ Species + Strain,
  output = "data")
head(vaas4.mcp)


###################################################
### code chunk number 84: data.G06
###################################################
vaas.G06 <- vaas_et_al[, , "G06"]
metadata(vaas.G06)[114]
metadata(vaas.G06) <- Str.Exp ~ paste(Strain, Experiment, sep = ".")
metadata(vaas.G06)[114]


###################################################
### code chunk number 85: mcp.G06
###################################################
vaas.G06.mcp <- opm_mcp(vaas.G06, model = ~ Str.Exp, 
  linfct = c(Dunnett = 1))


###################################################
### code chunk number 86: opm.Rnw:1688-1691
###################################################
old.mar <- par(mar = c(3, 22, 3, 1)) # adapt plotting settings
plot(vaas.G06.mcp)
par(old.mar) # reset plotting settings


###################################################
### code chunk number 87: mcp.G06.output.contrast
###################################################
contr <- opm_mcp(vaas.G06, model = ~ Str.Exp,
  linfct = c(Dunnett = 1), output = "contrast")$Str.Exp
colnames(contr)


###################################################
### code chunk number 88: mcp.G06.user.def.contrasts
###################################################
contr <- contr[1:3, ] # keeps the column names
rownames(contr) <- c(
  "First repl. - Second repl.",
  "First repl. - Time series",
  "DSM 1707 #1 - Second repl."
)
contr[1, ] <- c(1/4, -1/4, 1/4, -1/4, 1/4, -1/4, 0, 1/4, -1/4)
contr[2, ] <- c(1/4, 0, 1/4, 0, 1/4, 0, -1, 1/4, 0)
contr[3, ] <- c(0, -1/4, 1, -1/4, 0, -1/4, 0, 0, -1/4)
contr
vaas6.mcp <- opm_mcp(vaas.G06, model = ~ Str.Exp, m.type = "lm",
  linfct = mcp(Str.Exp = contr))


###################################################
### code chunk number 89: opm.Rnw:1737-1740
###################################################
old.mar <- par(mar = c(3, 12, 3, 2)) # adapt plotting settings
plot(vaas6.mcp)
par(old.mar) # reset plotting settings


###################################################
### code chunk number 90: discretize
###################################################
vaas.repl <- subset(vaas_et_al,
  query = list(Experiment = c("First replicate", "Second replicate")))
vaas.repl <- do_disc(vaas.repl)


###################################################
### code chunk number 91: list.discretize
###################################################
listing(vaas.repl, as.groups = NULL)
listing(vaas.repl, as.groups = list("Species"))


###################################################
### code chunk number 92: phylo.data.I
###################################################
phylo_data(listing(vaas.repl, as.groups = NULL))
phylo_data(listing(vaas.repl, as.groups = list("Species")))


###################################################
### code chunk number 93: phylo.data.II
###################################################
phylo_data(listing(vaas.repl, as.groups = NULL, html = TRUE))
phylo_data(listing(vaas.repl, as.groups = list("Species"), html = TRUE))


###################################################
### code chunk number 94: opm.css.file
###################################################
opm_opt(css.file = grep("[.]css$", opm_files("auxiliary"), value = TRUE))


###################################################
### code chunk number 95: opm.html.file
###################################################
vaas.html <- phylo_data(vaas.repl, format = "html",
  as.labels = list("Species", "Strain"), outfile = "vaas.html")


###################################################
### code chunk number 96: wd.css.file
###################################################
file.copy(grep("[.]css$", opm_files("auxiliary"), value = TRUE),
  "opm_styles.css", overwrite = TRUE)
opm_opt(css.file = "opm_styles.css")


###################################################
### code chunk number 97: prep.disc.data
###################################################
vaas.repl <- subset(vaas_et_al,
  query = list(Experiment = c("First replicate", "Second replicate")))
vaas.repl <- extract(vaas.repl,
  as.labels = list("Species", "Strain", "Experiment", "Plate number"))


###################################################
### code chunk number 98: disc.data.I
###################################################
vaas.repl.disc <- discrete(vaas.repl, range = c(0, 400))


###################################################
### code chunk number 99: phylo.data.III
###################################################
phylo_data(vaas.repl.disc, outfile = "example_replicates.epf")


###################################################
### code chunk number 100: disc.data.II
###################################################
vaas.repl.disc <- discrete(vaas.repl, range = c(120.2, 236.6), gap = TRUE)


###################################################
### code chunk number 101: disc.data.III
###################################################
vaas.repl.disc <- discrete(vaas.repl, range = c(120.2, 236.6),
  gap = TRUE, middle.na = FALSE)


###################################################
### code chunk number 102: find.substrate.I
###################################################
substrates <- find_substrate(c("Glutamine", "Glutamic acid"))
substrates


###################################################
### code chunk number 103: find.substrate.II
###################################################
substrates <- find_substrate(c("L-Glutamine", "L-Glutamic acid"), "glob")
substrates


###################################################
### code chunk number 104: find.substrat.III
###################################################
substrates <- find_substrate(c("*L-Glutamine", "*L-Glutamic acid"), "glob")
substrates


###################################################
### code chunk number 105: find.positions
###################################################
positions <- find_positions(substrates)
positions


###################################################
### code chunk number 106: substrate.info
###################################################
subst.info <- substrate_info(substrates)
subst.info


###################################################
### code chunk number 107: substrate.info
###################################################
subst.info <- substrate_info(substrates, "all")
subst.info


