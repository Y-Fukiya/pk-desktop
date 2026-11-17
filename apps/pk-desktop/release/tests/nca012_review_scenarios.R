source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)
count <- 0L
check <- function(label, expression) {
  force(expression)
  count <<- count + 1L
  cat(sprintf("PASS %02d %s\n",count,label))
}
equal <- function(x,y) stopifnot(isTRUE(all.equal(x,y,tolerance=1e-12,check.attributes=FALSE)))
refused <- function(expression, patterns) {
  message <- tryCatch({force(expression); "ACCEPTED"},error=conditionMessage)
  stopifnot(message != "ACCEPTED")
  for (pattern in patterns) stopifnot(grepl(pattern,message,fixed=TRUE))
  invisible(message)
}
d <- data.frame(ID="001", TIME=c(-.25,.5,1,2,4,8),CONC=c("<0.2","3","8","6","4","1.5"), BLQ=c(1,0,0,0,0,0),LLOQ=.2,UNIT="ng/mL")
spec <- list(id="ID",time="TIME",concentration="CONC",blq_column="BLQ",blq="exclude",unit_column="UNIT",c0_method="zero",auc_method="linear")
qc <- list(predose_qc_threshold_pct=5,predose_qc_window_h=1,predose_qc_lloq_source="column",predose_qc_lloq_column="LLOQ")
s <- modifyList(spec,qc)
pk <- function(x,sp=s) pkapp_nca(x,sp)
step <- list(op="blq_rules",mode="positional",concentration="CONC",blq_column="BLQ",time="TIME",profile_columns="ID",zero_time="post",pre_rule="zero",first_rule="zero",middle_rule="exclude",last_rule="exclude",all_blq_rule="exclude",lloq_source="column",lloq_column="LLOQ")
ps <- modifyList(spec,list(concentration="NCA_CONC",blq_column="NCA_BLQ",mdv_column="NCA_MDV",blq="as_recorded",predose_qc_threshold_pct=5,predose_qc_window_h=1))
p <- pkapp_prepare(d,list(step))$data
check("raw BLQ bound passes without inventing concentration", {
  r <- pk(d)
  equal(r$data$AVAL,c(8,1,31.5))
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="pass_blq"),all(r$data$PROFILE_STATUS=="ok"),all(is.na(r$data$PREDOSE_QC_CONC)),all(is.na(r$data$PREDOSE_QC_PCT)))
  equal(r$data$PREDOSE_QC_PCT_UPPER_BOUND,rep(2.5,3))
})
check("threshold equality passes but a higher bound is not an exceedance", {
  stopifnot(all(pk(transform(d,LLOQ=.4))$data$PREDOSE_QC_STATUS=="pass_blq"))
  r <- pk(transform(d,LLOQ=.5))
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="not_evaluable"),all(r$data$PREDOSE_QC_REASON=="bound_above_threshold"))
})
check("invalid optional bounds do not modify PK estimates", {
  for (limit in c(NA_real_,0,-1,Inf)) {
    r<-pk(transform(d,LLOQ=limit))
    equal(r$data$AVAL,c(8,1,31.5)); stopifnot(all(r$data$PREDOSE_QC_STATUS=="not_evaluable"))
  }
})
check("latest BLQ does not fall back to older quantifiable predose", {
  x<-rbind(transform(d[1,],TIME=-.75,CONC="1",BLQ=0),d)
  r<-pk(x)
  stopifnot(all(r$data$PREDOSE_QC_SOURCE_ROW==2),all(r$data$PREDOSE_QC_STATUS=="pass_blq"))
})
check("QC is numerically inert for PK and row-use decisions", {
  a<-pk(d); b<-pk(d,spec)
  equal(a$data$AVAL,b$data$AVAL)
  stopifnot(identical(a$trace$rows,b$trace$rows))
})
check("selected predose row unit cannot differ from postdose Cmax unit", {
  x<-d; x$UNIT[1]<-"ug/mL"
  r<-pk(x)
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="not_evaluable"),all(r$data$PREDOSE_QC_REASON=="unit_mismatch"))
})
check("prepared zero-rule BLQ uses configured bound", {
  stopifnot(core$desktop_pk_blq_context(p)$version==2)
  r<-pk(p,ps); equal(r$data$AVAL,c(8,1,31.5))
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="pass_blq"),is.na(p$NCA_LLOQ_USED[1]))
})
check("prepared CSV roundtrip and reorder preserve result", {
  path<-tempfile(fileext=".csv"); core$pk_write_csv(p,path)
  read<-pkapp_read_table(path); unlink(path)
  stopifnot(identical(read$ID,rep("001",6)))
  equal(pk(read,ps)$data$AVAL,pk(p,ps)$data$AVAL)
  r<-pk(read[6:1,],ps)
  equal(r$data$AVAL,c(8,1,31.5)); stopifnot(all(r$data$PREDOSE_QC_SOURCE_ROW==6))
})
check("changed leading-zero identity names original and changed key", {
  x<-p; x$ID<-"1"
  refused(pk(x,ps),c("001","ID","1"))
})
check("changed processed concentration names its column", {
  x<-p; x$NCA_CONC[3]<-9
  refused(pk(x,ps),c("001","NCA_CONC"))
})
check("partial row deletion has recorded and current counts", {
  refused(pk(p[-6,],ps),c("001","6","5"))
})
check("duplicate membership remains a refusal", {
  refused(pk(rbind(p,p[6,]),ps),c("001"))
})
check("whole-profile extraction stays supported", {
  both<-rbind(d,transform(d,ID="002")); prepared<-pkapp_prepare(both,list(step))$data
  equal(pk(prepared[prepared$ID=="002",],ps)$data$AVAL,c(8,1,31.5))
})
check("partial scope selection remains a refusal with profile detail", {
  x<-transform(p,SSNCAFL=c("Y","Y","Y","Y","Y","N"))
  refused(pk(x,modifyList(ps,list(scope="ss"))),c("001"))
})
check("changed LLOQ column and recipe constant are refused", {
  x<-p; x$LLOQ[1]<-.1
  refused(pk(x,ps),c("LLOQ"))
  z<-pkapp_prepare(d,list(modifyList(step,list(lloq_source="constant",lloq_value=.2))))$data
  context<-core$desktop_pk_blq_context(z); context$step$lloq_value<-.1
  z$NCA_BLQ_CONTEXT<-as.character(jsonlite::toJSON(context,auto_unbox=TRUE,null="null",digits=NA))
  refused(pk(z,ps),character())
})
check("v1 signed column remains compatible and can support QC", {
  x<-pkapp_read_table("apps/pk-desktop/release/tests/fixtures/prepared_v1_column.csv")
  equal(pk(x,ps)$data$AVAL,c(8,1,31.5))
  stopifnot(all(pk(x,ps)$data$PREDOSE_QC_STATUS=="pass_blq"))
})
check("v1 unsealed constant never supports a new QC pass", {
  x<-pkapp_read_table("apps/pk-desktop/release/tests/fixtures/prepared_v1_constant.csv")
  r<-pk(x,ps); equal(r$data$AVAL,c(8,1,31.5))
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="not_evaluable"),all(r$data$PREDOSE_QC_REASON=="unverified_lloq"))
})
check("all-BLQ policy missing Tmax does not recommend C0", {
  x<-transform(d,TIME=0:5,CONC="<0.2",BLQ=1)
  r<-pk(x,modifyList(spec,list(all_blq_policy="zero")))
  equal(r$data$AVAL,c(0,NA_real_,0))
  stopifnot(!any(grepl("C0|開始点の補完",r$warnings)))
})
check("main summary excludes QC/count internals while full output keeps them", {
  r<-pk(d)
  main<-core$desktop_nca_summary(r)
  stopifnot(all(c("ID","CMAX","TMAX","AUCLST","TLAST_QUANT_H","PROFILE_STATUS","NCA_WARNING")%in%names(main)),!any(grepl("^PREDOSE_|^N_INPUT$|^N_EXCLUDED$",names(main))))
  stopifnot("PREDOSE_QC_PCT_UPPER_BOUND"%in%names(r$data))
})
check("known flags and LLOQ excluded from time candidates when unmapped", {
  choices<-unname(core$desktop_pk_time_choices(transform(d,MDV=0,CustomElapsed=TIME)))
  stopifnot(all(c("TIME","CustomElapsed")%in%choices),!any(c("ID","BLQ","MDV","LLOQ")%in%choices))
})
check("v1 context cannot relabel another signed column as trusted LLOQ", {
  x<-pkapp_read_table("apps/pk-desktop/release/tests/fixtures/prepared_v1_constant.csv")
  context<-core$desktop_pk_blq_context(x)
  context$step$lloq_source<-"column"; context$step$lloq_column<-"BLQ"
  x$NCA_BLQ_CONTEXT<-as.character(jsonlite::toJSON(context,auto_unbox=TRUE,null="null",digits=NA))
  r<-pk(x,modifyList(ps,list(predose_qc_threshold_pct=20)))
  equal(r$data$AVAL,c(8,1,31.5))
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="not_evaluable"),all(r$data$PREDOSE_QC_REASON=="unverified_lloq"))
})
check("subnormal percentage cannot become a false zero-bound QC pass", {
  x<-data.frame(ID="tiny",TIME=c(-.25,0,1),CONC=c("<LLOQ","0","1e308"),BLQ=c(1,0,0),LLOQ=1e-17,UNIT="ng/mL")
  r<-pk(x,modifyList(s,list(predose_qc_threshold_pct=5e-324)))
  stopifnot(all(r$data$PREDOSE_QC_STATUS=="not_evaluable"),!any(r$data$PREDOSE_QC_PCT_UPPER_BOUND==0,na.rm=TRUE))
})
cat(sprintf("Independent 0.12 review scenarios: %d/%d passed\n",count,count))
