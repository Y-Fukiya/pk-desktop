# Deliberately pre-specified synthetic study reference; not an admiral output.
# Run from source root. Optional argument: a new output directory.
source('apps/pk-desktop/pkapp.R')
folder <- 'apps/pk-desktop/examples/adpp_hand_reference'
args <- commandArgs(trailingOnly=TRUE)
if (length(args)>1L) stop('Supply at most one new output directory.',call.=FALSE)
outdir <- if (length(args)) args[[1]] else tempfile('adpp-reference-')
if (file.exists(outdir)) stop('Output directory already exists.',call.=FALSE)
if (!dir.create(outdir,recursive=TRUE,showWarnings=FALSE)) stop('Cannot create output directory.',call.=FALSE)
read_text <- function(path) utils::read.csv(path,colClasses='character',na.strings='',check.names=FALSE,stringsAsFactors=FALSE)
raw <- read_text(file.path(folder,'concentrations.csv'));raw$TIME<-as.numeric(raw$TIME);raw$CONC<-as.numeric(raw$CONC)
ref <- read_text(file.path(folder,'reference_adpp.csv'));ref$AVAL<-as.numeric(ref$AVAL)
spec <- list(id='USUBJID',groups=list('STUDYID','PERIOD','TRT','ANALYTE','SPECIMEN'),
  group_roles=list(list(column='STUDYID',role='study'),list(column='PERIOD',role='period'),
    list(column='TRT',role='treatment'),list(column='ANALYTE',role='analyte'),list(column='SPECIMEN',role='specimen')),
  time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none',auc_method='linear')
nca <- pkapp_nca(raw,spec)
# This mapping is supplied explicitly from the hypothetical study convention.
map <- data.frame(NCAPCD=rep(c('CMAX','TMAX','AUCLST'),2),
  ANALYTE=rep(c('PARENT','METAB'),each=3),SPECIMEN='PLASMA',
  PARAMCD=c('CMAX','TMAX','AUCLST','CMAXM','TMAXM','AUCLSTM'),stringsAsFactors=FALSE)
candidate <- pkapp_adpp_draft(nca,source_input=raw,parameter_mapping=map)$data
candidate$REFERENCE_EXCL_PRESENT <- ifelse(!is.na(candidate$NCAEXCL) & nzchar(candidate$NCAEXCL),'Y','N')
candidate$REFERENCE_WARN_PRESENT <- ifelse(!is.na(candidate$NCAWARN) & nzchar(candidate$NCAWARN),'Y','N')
raw_candidate_names <- setdiff(names(candidate),c('REFERENCE_EXCL_PRESENT','REFERENCE_WARN_PRESENT'))
raw_reference_names <- names(ref)
# Variable mapping is an explicit study-review correspondence, not an ADaM conversion.
names(ref)[match(c('APERIOD','TRTA','PARCAT1','PPSPEC','EXCL_PRESENT','WARN_PRESENT'),names(ref))] <-
  c('PERIOD','TRT','ANALYTE','SPECIMEN','REFERENCE_EXCL_PRESENT','REFERENCE_WARN_PRESENT')
keys <- c('STUDYID','USUBJID','ANALYTE','SPECIMEN','PARAMCD')
if (anyDuplicated(ref[keys]) || anyDuplicated(candidate[keys])) stop('Nonunique comparison keys.',call.=FALSE)
ref$REFERENCE_ROW_PRESENT <- TRUE;candidate$CANDIDATE_ROW_PRESENT <- TRUE
joined <- merge(ref,candidate,by=keys,all=TRUE,suffixes=c('_reference','_candidate'),sort=TRUE)
missing_reference <- sum(is.na(joined$REFERENCE_ROW_PRESENT))
missing_candidate <- sum(is.na(joined$CANDIDATE_ROW_PRESENT))
same_value <- function(a,b) (is.na(a)&is.na(b)) | (!is.na(a)&!is.na(b)&abs(a-b)<=1e-10*pmax(1,abs(a)))
same_text <- function(a,b) (is.na(a)&is.na(b)) | (!is.na(a)&!is.na(b)&a==b)
compared <- c('PERIOD','TRT','AVAL','AVALU','NCAPSTAT','PROFSTAT','REFERENCE_EXCL_PRESENT','REFERENCE_WARN_PRESENT')
differences <- lapply(compared,function(field) {
 a<-joined[[paste0(field,'_reference')]];b<-joined[[paste0(field,'_candidate')]]
 same<-if (field=='AVAL') same_value(a,b) else same_text(a,b)
 bad<-which(is.na(same) | !same | is.na(joined$REFERENCE_ROW_PRESENT) | is.na(joined$CANDIDATE_ROW_PRESENT))
 if (!length(bad)) return(NULL)
 cbind(joined[bad,keys,drop=FALSE],field=field,reference=as.character(a[bad]),candidate=as.character(b[bad]),stringsAsFactors=FALSE)
})
differences <- Filter(Negate(is.null),differences)
value_diffs <- if (length(differences)) do.call(rbind,differences) else data.frame(STUDYID=character(),USUBJID=character(),ANALYTE=character(),SPECIMEN=character(),PARAMCD=character(),field=character(),reference=character(),candidate=character())
label_bad <- which(!same_text(joined$PARAM_reference,joined$PARAM_candidate))
label_diffs <- cbind(joined[label_bad,keys,drop=FALSE],reference=joined$PARAM_reference[label_bad],candidate=joined$PARAM_candidate[label_bad],stringsAsFactors=FALSE)
utils::write.csv(value_diffs,file.path(outdir,'value_differences.csv'),row.names=FALSE,na='')
utils::write.csv(label_diffs,file.path(outdir,'parameter_label_differences.csv'),row.names=FALSE,na='')
report <- list(reference='hand-authored_synthetic_study_specification', independent_admiral_comparison=FALSE,
  note='One pre-defined synthetic example; reference and implementation were not reviewed by separate teams; no ADaM compliance conclusion.',
  rows_reference=nrow(ref),rows_candidate=nrow(candidate),missing_reference=missing_reference,missing_candidate=missing_candidate,
  compared_fields=as.list(compared),value_or_status_differences=nrow(value_diffs),parameter_label_differences=nrow(label_diffs),
  candidate_only_variables=as.list(setdiff(raw_candidate_names,raw_reference_names)),reference_only_variables=as.list(setdiff(raw_reference_names,raw_candidate_names)),
  explicitly_mapped_variables=list(APERIOD='PERIOD',TRTA='TRT',PARCAT1='ANALYTE',PPSPEC='SPECIMEN'),
  absolute_and_relative_tolerance=1e-10,
  input_sha256=digest::digest(file=file.path(folder,'concentrations.csv'),algo='sha256',serialize=FALSE),
  reference_sha256=digest::digest(file=file.path(folder,'reference_adpp.csv'),algo='sha256',serialize=FALSE))
jsonlite::write_json(report,file.path(outdir,'comparison.json'),auto_unbox=TRUE,pretty=TRUE,null='null')
cat('Reference comparison output:',outdir,'\n')
cat('Rows:',nrow(ref),'reference /',nrow(candidate),'candidate; value, unit, status and presence differences:',nrow(value_diffs),'\n')
cat('Study-specific PARAM label differences:',nrow(label_diffs),'(review; not counted as value failures)\n')
if (missing_reference || missing_candidate || nrow(value_diffs)) stop('Reference comparison has differences.',call.=FALSE)
