# 3_sipp_extract_cols.R cleans raw SIPP data by extracting columns we use in our analysis.

#each wave is 2GB raw and 100MB w cols of interest. 
ptm <- proc.time()

df <- data.frame()
for (i in 1:12 ) {
  tmp <- readRDS(paste0(data_path,"sipp/sipp04w",i,".Rdata")) %>% select(
    ssuid,rfid,rsid,efnp,epppnum,spanel,srefmon,rhcalmn,rhcalyr,shhadid,eentaid,id,lgtkey,efrefper,
    esex,asex,eeducate,tage,
    euectyp5,auectyp5,t05amt,a05amt,tpmsum1,tpmsum2,apmsum1, apmsum2,tmlmsum,
    tptotinc,tpearn,tptrninc,tpothinc,tpprpinc,thtotinc,thearn,thunemp,
    tftotinc,tfearn,tfunemp,tstotinc,tsfearn,tsunemp,rfpov,
    epaothr3, efoodsc1, efoodsc2, efoodsc3, efoodsc4, efoodtp1, efoodtp2, efoodtp3, efoodtp4,
    epdjbthn,tbsocc1,absocc1,eeno1,ejbind1,tbsocc2,eeno2,ejbind2,absocc2,eppflag,epopstat,rmesr,rwksperm,
    arsend1,arsend2,ersend1,ersend2,epdjbthn,estlemp1,estlemp2,ejobcntr,
    wffinwgt,wsfinwgt,whfnwgt,wpfinwgt)
  tmp <- tmp %>% mutate(swave = i)
  df <- rbind(df,tmp) 
}

df[] <- lapply(df, unclass)
df[] <- lapply(df, function(x) {attr(x, 'label') <- NULL; x})
df[] <- lapply(df, function(x) {attr(x, 'labels') <- NULL; x})
df[] <- lapply(df, function(x) {attr(x, 'is_na') <- NULL; x})
saveRDS(df,file=paste0(data_path,"sipp/sipp04.Rdata"))
