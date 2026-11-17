# Run from the project root: Rscript apps/pk-desktop/tests/csv_parser.R
# Optional second argument is the original data.R for differential audit.
args <- commandArgs(trailingOnly = TRUE)
candidate_path <- if (length(args)) args[1] else 'apps/pk-review/R/data.R'
candidate <- new.env(parent = globalenv()); source(candidate_path, local = candidate)
fixture <- tempfile(fileext = '.csv')
write_fixture <- function(text) {
  writeBin(if (is.raw(text)) text else charToRaw(enc2utf8(text)), fixture)
  fixture
}
checks <- 0L
check_records <- function(text, expected) {
  path <- write_fixture(text)
  stopifnot(identical(candidate$pk_csv_records(path), expected))
  checks <<- checks + 1L
}
check_error <- function(text, pattern) {
  path <- write_fixture(text)
  outcome <- tryCatch({candidate$pk_csv(path); NULL}, error = conditionMessage)
  stopifnot(!is.null(outcome), grepl(pattern, outcome, fixed = TRUE))
  checks <<- checks + 1L
}
check_records('', list())
check_records('\ufeff', list())
for (ending in c('\n', '\r', '\r\n')) {
  check_records(ending, list(character()))
  check_records(paste0('""', ending), list(''))
  check_records(paste0(',', ending), list(c('', '')))
  check_records(paste0('A,B', ending, '001,2', ending), list(c('A', 'B'), c('001', '2')))
  check_records(paste0('A,B', ending, '001,'), list(c('A', 'B'), c('001', '')))
  check_records(paste0('A,B', ending, '001,', ending, ending), list(c('A', 'B'), c('001', ''), character()))
  check_error(paste0('A,B', ending, ending), 'field count')
  check_error(paste0('A,B', ending, '1'), 'field count')
  check_error(paste0('A,B', ending, '1,2,3'), 'field count')
}
check_records(',', list(c('', '')))
check_records(',,', list(c('', '', '')))
check_records('""', list(''))
check_records('""""', list('"'))
check_records('"a""b",x', list(c('a"b', 'x')))
check_records('A\r\nB\rC\n', list('A', 'B', 'C'))
check_records('\ufeffID,LABEL\r\n001,"日本語\rA\nB\r\nC, ""quoted"""',
  list(c('ID', 'LABEL'), c('001', '日本語\rA\nB\r\nC, "quoted"')))
for (text in c('', '\ufeff', '\n', '""\n', 'A,A\n', 'A, B\n', ',B\n')) check_error(text, 'Invalid or duplicate CSV header')
for (text in c('A\nx"y', 'A\n"x"y', 'A\n"unterminated', 'A\n"x" ', 'A\n"x"\t',
               'A,B\nx,y"', 'A,B\n1,"x"y', 'A\n"x""', 'A\n"x"\r\n"bad')) {
  check_error(text, 'Invalid CSV quoting')
}
for (bytes in list(as.raw(0), c(charToRaw('A\nx'), as.raw(0)), c(charToRaw('A\n"x'), as.raw(0), charToRaw('"')))) {
  check_error(bytes, 'embedded NUL byte')
}
path <- write_fixture('ID,LABEL,STUDYID,ARM\n001,NA, NA ,\n0002, NA ,study,A\n')
before <- tools::md5sum(path)
expected <- data.frame(ID = c('001', '0002'), LABEL = c(NA_character_, ' NA '),
                       STUDYID = c(NA_character_, 'study'), ARM = c(NA_character_, 'A'), stringsAsFactors = FALSE)
stopifnot(identical(candidate$pk_csv(path), expected), identical(tools::md5sum(path), before))
checks <- checks + 1L
for (text in c('A,B', 'A,B\n', 'A,B\r', 'A,B\r\n')) {
  stopifnot(identical(candidate$pk_csv(write_fixture(text)), data.frame(A = character(), B = character())))
  checks <- checks + 1L
}
# Independent encoder and expected character values, including all CSV delimiters.
set.seed(20261109)
alphabet <- c('a', 'b', ',', '"', '\r', '\n', '\t', ' ', '\\', '日', '本', '語')
labels <- replicate(240L, paste0(sample(alphabet, sample.int(60L, 1L), replace = TRUE), collapse = ''))
values <- data.frame(ID = sprintf('%04d', seq_along(labels)), LABEL = labels, stringsAsFactors = FALSE)
escape <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
for (ending in c('\n', '\r', '\r\n')) for (terminate in c(TRUE, FALSE)) {
  records <- paste(escape(values$ID), escape(values$LABEL), sep = ',')
  text <- paste0('\ufeff"ID","LABEL"', ending, paste(records, collapse = ending), if (terminate) ending else '')
  stopifnot(identical(candidate$pk_csv(write_fixture(text)), values))
  checks <- checks + 1L
}
# Long valid and malformed fields exercise quote handling without arbitrary limits.
for (value in c(strrep('a', 100000L), strrep('a"日本語\r\n', 10000L))) {
  check_records(paste0('ID,LABEL\n001,', escape(value)), list(c('ID', 'LABEL'), c('001', value)))
  check_error(paste0('ID,LABEL\n001,', escape(value), 'x'), 'Invalid CSV quoting')
  check_error(paste0('ID,LABEL\n001,', substr(escape(value), 1, nchar(escape(value)) - 1L)), 'Invalid CSV quoting')
}
cat(sprintf('CSV acceptance: %d checks passed, 0 failures\n', checks))
if (length(args) >= 2L) {
  original <- new.env(parent = globalenv()); source(args[2], local = original)
  alphabet <- c('a', ',', '"', '\r', '\n')
  strings <- ''
  differential <- 0L
  for (width in 0:6) {
    if (width > 0L) strings <- as.vector(outer(strings, alphabet, paste0))
    for (text in strings) {
      path <- write_fixture(text)
      old <- tryCatch(list(ok = TRUE, value = original$pk_csv_records(path)), error = function(e) list(ok = FALSE))
      new <- tryCatch(list(ok = TRUE, value = candidate$pk_csv_records(path)), error = function(e) list(ok = FALSE))
      if (!identical(old, new)) stop('Differential mismatch: ', encodeString(text))
      differential <- differential + 1L
    }
  }
  cat(sprintf('CSV exhaustive differential: %d strings passed, 0 mismatches\n', differential))
}
unlink(fixture)
