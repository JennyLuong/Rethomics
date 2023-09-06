######## Get necessary packages ##########
{
  if (!require(svDialogs, quietly=T)) {install.packages('svDialogs');library(svDialogs)}
  if (!require(data.table, quietly=T)) {install.packages('data.table');library(data.table)}
}

######## Extract movement data ##########
{
  wd <- dlg_dir(title="Choose the experiment folder")$res
  setwd(wd)
  new_folder <- file.path(wd, "movements")
  dir.create(new_folder, showWarnings = FALSE)
  for (monitor in list.files(pattern="txt")) { # loop through all monitor files
    # read in monitor file, extract rows with "MT", replace with "CT", write into tab-deliminated files
    fwrite(fread(monitor, fill=T)[V8=="MT"][V4==1][, V8 := "CT"], file=paste0(new_folder,"/",monitor), sep="\t", col.names=F)
  }
}

