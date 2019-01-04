# This script downloads data and convert them to Rdata format

# 2013 SCF
rscfp2013 <- tempfile()
p13i6 <- tempfile()
download.file("https://www.federalreserve.gov/econres/files/scfp2013s.zip", rscfp2013)
download.file("https://www.federalreserve.gov/econres/files/scf2013s.zip", p13i6)
saveRDS(read_dta(unz(rscfp2013, "rscfp2013.dta")), file = paste0(data_path, "/scf/rscfp2013.Rdata"))
saveRDS(read_dta(unz(p13i6, "p13i6.dta")), file = paste0(data_path, "/scf/p13i6.Rdata"))

# 2012 DCPC
dcpc_2012_public <- tempfile()
download.file("https://www.frbatlanta.org/-/media/documents/banking/consumer-payments/diary-of-consumer-payment-choice/2012/dcpc_2012_public_v3_csv.csv", dcpc_2012_public)
saveRDS(read.csv(dcpc_2012_public), file = paste0(data_path, "dcpc_2012_public.Rdata"))

# 2004 SIPP
# We download the SIPP Stata files from NBER http://www.nber.org/data/survey-of-income-and-program-participation-sipp-data.html.
# We use the 2004 core wave data files. We do not provide the raw SIPP data in our replication kit, users should download their
# own copy of SIPP. Here are the download instructions. 
# Before running the scripts below, users first need to download .zip data files and Stata .do and .dct files from the NBER website 
# shown above, and then run the corresponding .do files to convert .dat to .dta files. After the Stata cleaning process, users 
# should change path_raw to the path where the Stata .dta files are stored and run the scripts below to convert .dta to .Rdata.

path_raw <- "~/Dropbox (MIT)/SIPP/raw/"
for (i in 1:12 ) {
  saveRDS(read_dta(paste0(path_raw,"sipp04w",i,".dta")),
          file=paste0(path_raw,"sipp04w",i,".Rdata"))
}

rm(rscfp2013, p13i6, dcpc_2012_public)

