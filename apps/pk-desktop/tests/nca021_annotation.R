source("apps/pk-desktop/tests/nca020_annotation.R")

test_that("mixed units flag each affected profile without altering recorded data", {
  f <- fixture020()
  weight <- f$vs$VSTESTCD == "WEIGHT" & f$vs$VISIT == "BASELINE"
  f$vs$VSSTRESU[weight] <- c(" kg ", "LB")
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  mixed <- x$trace$issues[x$trace$issues$reason == "mixed_units", ]
  expect_equal(nrow(mixed), 4)
  expect_equal(unique(mixed$field), "VS_WEIGHT")
  expect_equal(x$counts[["issues"]], 4)
  expect_identical(x$data[names(f$result$data)], f$result$data)
  expect_identical(unique(x$data$VS_WEIGHT_UNIT[x$data$ID == "001"]), " kg ")
  expect_identical(unique(x$data$VS_WEIGHT_UNIT[x$data$ID == "002"]), "LB")
  expect_equal(unique(x$data$VS_WEIGHT[x$data$ID == "002"]), 80)
  dir <- tempfile(); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  engine <- new.env(parent = emptyenv()); engine$project <- dir; core$desktop_tables_init(engine)
  saved <- core$desktop_annotation_save(engine, "Annotations", list(result = f$result, dm = f$dm, vs = f$vs), x, list())
  replay <- pkapp_replay(core$desktop_table_path(engine, saved$id))
  expect_identical(replay$data, x$data)
  expect_identical(replay$trace$issues, x$trace$issues)
})

test_that("only selected, matched finite values participate within each measurement", {
  f <- fixture020()
  weight <- f$vs$VSTESTCD == "WEIGHT" & f$vs$VISIT == "BASELINE"
  f$vs$VSSTRESU[weight] <- c("kg", " kg ")
  f$vs$VSSTRESU[f$vs$VISIT == "FOLLOWUP"] <- "other"
  extra <- f$vs[which(weight)[1], ]; extra$USUBJID <- "unmatched"; extra$VSSTRESU <- "LB"
  f$vs <- rbind(f$vs, extra)
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  expect_false(any(x$trace$issues$reason == "mixed_units"))
  f$vs$VSSTRESU[which(weight)[2]] <- "KG"
  f$vs$VSSTRESN[which(weight)[2]] <- NA_real_
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  expect_false(any(x$trace$issues$reason == "mixed_units"))
  expect_true(any(x$trace$issues$reason == "missing_value"))
  f$vs$VSSTRESN[which(weight)[2]] <- 80
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  expect_equal(sum(x$trace$issues$reason == "mixed_units"), 4)
  f$vs$VSSTRESU[which(weight)[2]] <- "  "
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  expect_false(any(x$trace$issues$reason == "mixed_units"))
  expect_equal(sum(x$trace$issues$reason == "missing_unit"), 2)
})

test_that("period scoped selections flag only profiles with comparable units", {
  f <- fixture020(); f$vs$PERIOD <- ifelse(f$vs$VISIT == "BASELINE", 1, 2)
  s <- f$spec; s$vs$scope <- "keys"; s$vs$keys$PERIOD <- "PERIOD"; s$vs$selection <- list(mode = "all_unique")
  weight <- which(f$vs$VSTESTCD == "WEIGHT")
  f$vs$VSSTRESU[weight] <- c("kg", "kg", "LB", NA)
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, s)
  mixed <- x$trace$issues[x$trace$issues$reason == "mixed_units", ]
  expect_equal(nrow(mixed), 3)
  expect_equal(sum(x$trace$issues$reason == "missing_unit"), 1)
  expect_equal(unique(mixed$field), "VS_WEIGHT")
  expect_identical(x$data[names(f$result$data)], f$result$data)
})
