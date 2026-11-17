# Compare PK Desktop ADPP candidate with the separately implemented Python
# arithmetic reference. This is a synthetic example, not an admiral comparison.
source('apps/pk-desktop/pkapp.R')
folder <- 'apps/pk-desktop/examples/adpp_independent_pk'
args <- commandArgs(trailingOnly=TRUE)
if (length(args)>1L) stop('Supply at most one new output directory.',call.=FALSE)
outdir <- if (length(args)) args[[1]] else tempfile('adpp-independent-pk-')
if (file.exists(outdir) || !dir.create(outdir,recursive=TRUE,showWarnings=FALSE)) stop('Cannot create a new output directory.',call.=FALSE)
read_text <- function(path) utils::read.csv(path,colClasses='character',na.strings='',check.names=FALSE,stringsAsFactors=FALSE)
raw<-read_text(file.path(folder,'concentrations.csv'));raw$TIME<-as.numeric(raw$TIME);raw$CONC<-as.numeric(raw$CONC);raw$DOSE<-as.numeric(raw$DOSE)
ref<-read_text(file.path(folder,'expected.csv'));ref$AVAL<-as.numeric(ref$AVAL)
spec<-list(id='USUBJID',groups=list('STUDYID','PERIOD','TRT','ANALYTE'),
 group_roles=list(list(column='STUDYID',role='study'),list(column='PERIOD',role='period'),list(column='TRT',role='treatment'),list(column='ANALYTE',role='analyte')),
 time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none',auc_method='lin up/log down',
 terminal_method='manual',terminal_start_h=2,terminal_end_h=8,dosing_context='single',dose_history_confirmed=TRUE,
 dose=list(amount_column='DOSE',unit_column='DOSEU',basis='absolute',meaning='actual_administered',reference='declared_profile',route_column='ROUTE',extravascular_values=list('ORAL')),
 derived_parameters=list('CMAXD','AUCIFOD','CLFO','VZFO'))
nca<-pkapp_nca(raw,spec)
codes<-c('CMAX','TMAX','AUCLST','LAMZ','LAMZHL','AUCIFO','AUCPEO','CMAXD','AUCIFOD','CLFO','VZFO')
map<-data.frame(NCAPCD=rep(codes,2),ANALYTE=rep(c('PARENT','METAB'),each=length(codes)),PARAMCD=c(codes,paste0(codes,'M')),stringsAsFactors=FALSE)
candidate<-pkapp_adpp_draft(nca,source_input=raw,parameter_mapping=map)$data
keys<-c('STUDYID','USUBJID','PERIOD','TRT','ANALYTE','NCAPCD')
if (anyDuplicated(ref[keys]) || anyDuplicated(candidate[keys])) stop('Nonunique comparison keys.',call.=FALSE)
ref$REFERENCE_ROW_PRESENT<-TRUE;candidate$CANDIDATE_ROW_PRESENT<-TRUE
joined<-merge(ref,candidate,by=keys,all=TRUE,suffixes=c('_reference','_candidate'),sort=TRUE)
missing_ref<-sum(is.na(joined$REFERENCE_ROW_PRESENT));missing_cand<-sum(is.na(joined$CANDIDATE_ROW_PRESENT))
same_text<-function(a,b) (is.na(a)&is.na(b)) | (!is.na(a)&!is.na(b)&a==b)
same_value<-function(a,b) (is.na(a)&is.na(b)) | (!is.na(a)&!is.na(b)&abs(a-b)<=1e-10*pmax(1,abs(a)))
fields<-c('PARAMCD','AVAL','AVALU','NCAPSTAT')
diffs<-lapply(fields,function(field){
 a<-joined[[paste0(field,'_reference')]];b<-joined[[paste0(field,'_candidate')]]
 equal<-if(field=='AVAL') same_value(a,b) else same_text(a,b)
 bad<-which(is.na(equal)|!equal|is.na(joined$REFERENCE_ROW_PRESENT)|is.na(joined$CANDIDATE_ROW_PRESENT))
 if (!length(bad)) return(NULL)
 cbind(joined[bad,keys,drop=FALSE],field=field,reference=as.character(a[bad]),candidate=as.character(b[bad]),stringsAsFactors=FALSE)
})
diffs<-Filter(Negate(is.null),diffs)
out<-if(length(diffs)) do.call(rbind,diffs) else data.frame(STUDYID=character(),USUBJID=character(),PERIOD=character(),TRT=character(),ANALYTE=character(),NCAPCD=character(),field=character(),reference=character(),candidate=character())
utils::write.csv(out,file.path(outdir,'differences.csv'),row.names=FALSE,na='')
valid<-!is.na(joined$AVAL_reference)&!is.na(joined$AVAL_candidate)
relative<-if(any(valid)) max(abs(joined$AVAL_reference[valid]-joined$AVAL_candidate[valid])/pmax(abs(joined$AVAL_reference[valid]),.Machine$double.xmin)) else NA_real_
report<-list(reference='independent_python_arithmetic_not_admiral',reviewer_independent=FALSE,
 rows_reference=nrow(ref),rows_candidate=nrow(candidate),numeric_values=sum(!is.na(ref$AVAL)),missing_values=sum(is.na(ref$AVAL)),
 missing_reference=missing_ref,missing_candidate=missing_cand,differences=nrow(out),max_relative_difference=relative,
 parameters=as.list(codes),tolerance='absolute or relative 1e-10 using max(1, abs(reference))',
 source_sha256=digest::digest(file=file.path(folder,'concentrations.csv'),algo='sha256',serialize=FALSE),
 reference_sha256=digest::digest(file=file.path(folder,'expected.csv'),algo='sha256',serialize=FALSE),
 limitations='Synthetic data; identical author; manual 2-8h terminal window; not admiral, CDISC validation, BLQ, auto-selection or repeated dosing.')
jsonlite::write_json(report,file.path(outdir,'comparison.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=16)
cat('Independent Python arithmetic vs candidate:',nrow(ref),'rows;',sum(!is.na(ref$AVAL)),'numeric values;',nrow(out),'differences; max relative',format(relative,digits=5),'\n')
if(missing_ref || missing_cand || nrow(out)) stop('Comparison failed.',call.=FALSE)
