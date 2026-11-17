args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(normalizePath(args[1]))
source('apps/pk-desktop/pkapp.R')
time <- c(0,.5,1,2,4,8,12)
original <- c(0,2,8,6,4,2,1)
inputs <- list(); expected <- list()
for (pattern in 0:63) {
  flags <- c(TRUE,as.logical(intToBits(pattern)[1:6]))
  id <- sprintf('%03d',pattern)
  inputs[[pattern+1L]] <- data.frame(ID=id,TIME=time,CONC=ifelse(flags,'BLQ',as.character(original)),BLQ=as.integer(flags))
  quant <- which(!flags & original>0)
  cmax <- if(length(quant)) max(original[quant]) else NA_real_
  tmax <- if(length(quant)) min(time[quant][original[quant]==cmax]) else NA_real_
  area <- NA_real_
  if(length(quant)) {
    endpoint <- max(time[quant]); first <- min(time[quant])
    keep <- !flags | time<first
    keep <- keep & time<=endpoint
    conc <- original;conc[flags & time<first]<-0
    area <- sum(diff(time[keep])*(head(conc[keep],-1)+tail(conc[keep],-1))/2)
  }
  expected[[pattern+1L]] <- c(CMAX=cmax,TMAX=tmax,AUCLST=area)
}
d <- do.call(rbind,inputs)
set.seed(20261109); d<-d[sample(seq_len(nrow(d))),]
rownames(d)<-NULL
original_input<-d
step<-list(op='blq_rules',mode='positional',profile_columns='ID',concentration='CONC',blq_column='BLQ',time='TIME',zero_time='pre',pre_rule='zero',first_rule='zero',middle_rule='exclude',last_rule='half_lloq',all_blq_rule='zero',lloq_value=.2)
p<-pkapp_prepare(d,list(step))
stopifnot(identical(p$data[names(d)],original_input))
r<-pkapp_nca(p$data,list(id='ID',time='TIME',concentration='NCA_CONC',blq_column='NCA_BLQ',mdv_column='NCA_MDV',blq='as_recorded',concentration_unit='ng/mL',auc_method='linear'))
for(pattern in 0:63) {
  rows<-r$data[r$data$ID==sprintf('%03d',pattern),]
  actual<-setNames(rows$AVAL,rows$PARAMCD)
  stopifnot(isTRUE(all.equal(actual,expected[[pattern+1L]],tolerance=1e-12)))
}
cat('PASS: all 64 BLQ patterns across six postdose times, 192 parameter comparisons, shuffled interleaved IDs, unchanged original data.\n')
