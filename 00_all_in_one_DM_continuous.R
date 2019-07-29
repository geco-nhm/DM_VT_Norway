#### Distribution Model of Vegetation types in Norway ####
# updated: 07.12.2017
# author: Peter Horvath
# based on tutorial by Robert J. Hijmans and Jane Elith
# utilizing scripts from Julien Vollering (MIAmaxent::parsevsGLM)
# utilizing scripts from Boris Leroy (virtualspecies::removeCollinearity)

#### 00 ####
#### LOAD LIBRARIES #######################################
# dir.create(Sys.getenv("R_LIBS_USER"))
# install.packages(c("raster","rgdal", "rgeos", "sp", "virtualspecies", "MIAmaxent", "dismo", "data.table"),Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )

library(raster)
library(rgdal)
library(rgeos) # calculating distance to coast
library(sp)
# library(virtualspecies)
library(MIAmaxent) # model selection and FOPs
library(dismo) # evaluation
library(data.table) # saving tables
# library(markdown)

#### RUN FUNCTIONS #########################################
# read parsevsGLM function from file
parsevsGLM <- dget("E:/Project_1_RUNS/new_MODEL_RUN/parsevsGLM.R")

#remove collinearity

#### SET PATH TO DIR ######################################
# INPUT 
# vegetation data 
# train presence 
in_train_veg_presence <- "E:/Project_1_FINAL_layers/18x18_PRESENCE"
# train absence
in_train_veg_absence <- "E:/Project_1_FINAL_layers/18x18_ABSENCE"
# test presence
in_test_veg_presence <- "E:/Project_1_FINAL_layers/EVAL_PRESENCE"
# test absence
in_test_veg_absence <- "E:/Project_1_FINAL_layers/EVAL_ABSENCE"

# environmental data
# continuous
in_enviro_continuous <- "E:/Project_1_FINAL_layers/MASKED_TIFF_uncorrelated/CONTINUOUS/"
# categorical
in_enviro_categorical <- "E:/Project_1_FINAL_layers/MASKED_TIFF_uncorrelated/CATEGORICAL/ALL_binary/"
# coastline
coastline_path <- "E:/Project_1_FINAL_layers/MASKING_layer_Landmf_N50"
# OUTPUT
# saving table with presence, absence and allSDM data including enviro variables
out_SDM <- "E:/Project_1_RUNS/new_MODEL_RUN/01_PA_data/"
dir.create(file.path(out_SDM),showWarnings=F)
out_SDM_continuous <- "E:/Project_1_RUNS/new_MODEL_RUN/01_cont_PA_data/"
dir.create(file.path(out_SDM_continuous),showWarnings=F)
# saving table with presence, absence and allSDM data including enviro variables
out_SDM_test <- "E:/Project_1_RUNS/new_MODEL_RUN/01_PA_data_test/"
dir.create(file.path(out_SDM_test),showWarnings=F)
out_SDM_test_continuous <- "E:/Project_1_RUNS/new_MODEL_RUN/01_cont_PA_test_data/"
dir.create(file.path(out_SDM_test_continuous),showWarnings=F)
# saving Variable selection from parseVAR function as CSV
out_var_select <- "E:/Project_1_RUNS/new_MODEL_RUN/02_variable_selection/"
dir.create(file.path(out_var_select),showWarnings=F)
# saving GLM models
out_model <- "E:/Project_1_RUNS/new_MODEL_RUN/03_GLM_Model/"
dir.create(file.path(out_model),showWarnings=F)
# saving FOP graphs
out_FOP <- "E:/Project_1_RUNS/new_MODEL_RUN/03_FOP_curves/"
dir.create(file.path(out_FOP),showWarnings=F)
# saving raster output
out_raster <- "E:/Project_1_RUNS/new_MODEL_RUN/04_predict_raster/"
dir.create(file.path(out_raster),showWarnings=F)
# saving raster maps (low-res)
out_maps <- "E:/Project_1_RUNS/new_MODEL_RUN/04_predict_lowres/"
dir.create(file.path(out_maps),showWarnings=F)
# saving evaluation data
out_eval <- "E:/Project_1_RUNS/new_MODEL_RUN/05_EVAL_data/"
dir.create(file.path(out_eval),showWarnings=F)
# saving log file
log_files <- "E:/Project_1_RUNS/new_MODEL_RUN/06_logs/"
dir.create(file.path(log_files),showWarnings=F)  
# saving CTABLE file
ctable_path <- "E:/Project_1_RUNS/new_MODEL_RUN/"


#### SET  PARAMETERS ####
# 01
extract_pts=TRUE
# 02
run_selection=TRUE
# 03
run_model=TRUE
run_model_cluster=FALSE
save_FOP=TRUE # this still doesn't work for categorical
run_pred=TRUE #
save_maps=TRUE
# 04
run_eval=TRUE # doesn't work with categorical (due to raster still cont.)
save_eval_plots=TRUE

#### ENVIRONMENTAL VARIABLES #####################################
print("loading ENVIRONMENTAL VARIABLES")

# read all raster files inside HOME folder and add them to a list
r.list.cont <- list.files(in_enviro_continuous, pattern="tif$", full.names=TRUE)
# read in raster layers and stack them into raster stack
r.stack.cont <- stack(r.list.cont)
# basename(r.list.cont)

# categorical 
r.list.cat <- list.files(in_enviro_categorical, pattern="tif$", full.names=TRUE)
r.stack.cat <-stack(r.list.cat)
r.list.cat <- basename(r.list.cat)

# for (l in 1:nlayers(r.stack.cat)){
#   new.cat.stack[[l]] <- as.factor(r.stack.cat[[l]])
# }
r.stack <- stack(c(r.stack.cont,r.stack.cat))
print("ENVIRONMENTAL VARIABLES loaded")

# load in coastline for further use in 04 EVALUATION - calculation of distance to coast
coastline <- readOGR(coastline_path, "Landmf_N50_coastline")

#### SPECIES DATA #########################################################################################
# change vegetation type imput data according to need
# presence points
vt_list <- list(
  "1ab","1c", "2a", "2b","2c","2d","2ef","2g",
  "3ab","4a","4b","4c","4e", "4g",
  "5ab","6a","6b","7a","7b","7c",
  "8a","8b","8cd","9ad","9bc","9e",
  "10ab","10c","11b","12b", "12c"
)
vt_names <- list("Moss snowbed / Sedge and grass snowbed", 	"Frozen ground, leeward", 	"Frozen ground, ridge", 	"Dry grass heath", 	"Lichen heath", 	"Mountain avens heath", 	
                 # "Dwarf shrub heath", 	"Alpine calluna heath", 
                 "Dwarf shrub / alpine calluna heath",
                 "Alpine damp heath", 	"Low herb / forb meadow", 	"Lichen and heather birch forest", 	"Bilberry birch forest", 	"Meadow birch forest", 	"Alder forest", 	"Pasture land forest", 	"Poor / Rich broadleaf deciduous forest", 	"Lichen and heather pine forest", 	"Bilberry pine forest", 	"Lichen & heather spruce forest", 	"Bilberry spruce forest", 	"Meadow spruce forest", 	"Damp forest", 	"Bog forest", 	"Poor / rich swamp forest", 	"Bog / Mud-bottom fen and bog", 	"Deer-grass fen / fen", 	"Sedge marsh", 	"Coastal heath / Coastal calluna heath", 	"Damp heath", 	"Pastures", 
                 #"Barren land",
                 "Boulder field", 	"Exposed bedrock"
)
# create table to output EVALUATION data into 
nrows <- length(vt_list)
ctable <- data.frame(veg.type=character(nrows), num.EVs=integer(nrows), incl.EVs=character(nrows),
                     train.pres=integer(nrows), train.abs=integer(nrows),
                     test.pres=integer(nrows), test.abs=integer(nrows), train.AUC=numeric(nrows), 
                     test.AUC=numeric(nrows), correlation=numeric(nrows), kappa=numeric(nrows),
                     tr.kappa=numeric(nrows), train_prevalence=numeric(nrows), test_prevalence=numeric(nrows), 
                     train.vs.test.prev=numeric(nrows), fitted.max=numeric(nrows),
                     fitted.min=numeric(nrows), raster.max=numeric(nrows), raster.min=numeric(nrows),
                     train.avg.dist=numeric(nrows),test.avg.dist=numeric(nrows),
                     train.coast.dist=numeric(nrows), test.coast.dist=numeric(nrows), 
                     stringsAsFactors = F )

time_table<- data.frame(pred=numeric(nrows), eval=numeric(nrows), tot=numeric(nrows))

#### 01 ####
# loop through all VEG types
for (i in 1:length(vt_list)){
  VT <- noquote(vt_list[i])
  VT_name <- noquote(vt_names[i])
  print(VT)
  print(VT_name)
  print(paste("running DM for vegetation type ",VT))
  start.time.pred <- Sys.time()              
  print(Sys.time())
  #### TRAINING POINTS ############################################################################
  # extract values from rasters for presence points
  # this process takes several minutes (5 up to 15)
  if (extract_pts==TRUE){
    
    print(paste("STEP 01 - Extracting TRAIN presences for VT - ",VT))
    if(file.exists(paste0(out_SDM, VT, "_p_train.Rds"))){
      pres_values <- readRDS(file = paste0(out_SDM, VT, "_p_train.Rds"))
      VT_p <- readOGR(in_train_veg_presence, paste0(VT, "_p"))
    } else {
      # presence training points
      pres_values <- extract(r.stack, VT_p)
    }
    print(paste("STEP 01 - Extracting TRAIN absences for VT - ",VT))
    if(file.exists(paste0(out_SDM, VT, "_a_train.Rds"))){
      abs_values <- readRDS(file = paste0(out_SDM, VT, "_a_train.Rds"))
      VT_a <- readOGR(in_train_veg_absence, paste0(VT, "_a"))
    } else {
      # absence training points
      abs_values <- extract(r.stack, VT_a)
    }
    #### Render dataframe including SDM values 
    # create a dataset with number of rows corresponding to presence points + absence points 
    # (where pres are represented by 1 whewreas absence by 0)
    PA <- c(rep(1,nrow(pres_values)), rep(0,nrow(abs_values)))
    # assign values from rasters for presence and absence points
    sdm_data <- data.frame(cbind(PA,rbind(pres_values,abs_values)))
    # str(sdm_data)
    # # index list of categorical input variables 
    # catindex <- seq(ncol(sdm_data) - length(r.list.cat) + 1, ncol(sdm_data))
    # # transform lines of categorical variables to factor  
    # sdm_data[catindex] <- lapply(sdm_data[catindex], function(x) as.factor(x))
    # drops <- c()
    # # loop for identifying factors with fewer than two levels
    # for(j in catindex){
    #   if(length(levels(sdm_data[,j]))<2){
    #     drops <- c(drops,j)
    #   } 
    # }
    # # deleting factors with fewer than two levels
    # sdm_data <- sdm_data[ ,-drops] 
    #### export SDM data to disk 
    dir.create(file.path(out_SDM),showWarnings=F)
    saveRDS(sdm_data, file = paste0(out_SDM_continuous, VT, "_SDM_data_train.Rds"))
    # saveRDS(pres_values, file = paste0(out_SDM_continuous, VT, "_p_train.Rds"))
    # saveRDS(abs_values, file = paste0(out_SDM_continuous, VT, "_a_train.Rds"))
    fwrite(sdm_data, file = paste0(out_SDM_continuous, VT, "_SDM_data_train.csv"))
    # readRDS()
    
    #### TEST POINTS ############################################################################
    
    #### extract values from rasters ###
    # get  values for presence points
    # this process takes several minutes (5 up to 15)
    print(paste("STEP 01 - Extracting TEST presences for VT - ",VT))
    if (file.exists(paste0(out_SDM_test, VT, "_p_test.Rds"))){
      pres_values_test <- readRDS(file = paste0(out_SDM_test, VT, "_p_test.Rds"))
      VT_p_test <- readOGR(in_test_veg_presence, paste0("eval_", VT, "_p")) 
    } else {
      # presence points
      
      pres_values_test <- extract(r.stack, VT_p_test)
    }
    print(paste("STEP 01 - Extracting TEST absences for VT - ",VT))
    if (file.exists(paste0(out_SDM_test, VT, "_a_test.Rds"))){
      abs_values_test <- readRDS(file = paste0(out_SDM_test, VT, "_a_test.Rds")) 
      VT_a_test <- readOGR(in_test_veg_absence, paste0("eval_", VT, "_a"))
    } else {
      # absence points
      
      abs_values_test <- extract(r.stack, VT_a_test)
    }
    #### Render dataframe including SDM values 
    # create a dataset with number of rows corresponding to presence points + absence points 
    # (where pres are represented by 1 whewreas absence by 0)
    PA_test <- c(rep(1,nrow(pres_values_test)), rep(0,nrow(abs_values_test)))
    # assign values from rasters for presence and absence points
    sdm_data_test <- data.frame(cbind(PA_test,rbind(pres_values_test,abs_values_test)))
    
    # # index list of categorical input variables 
    # catindex_test <- seq(ncol(sdm_data_test) - length(r.list.cat) + 1, ncol(sdm_data_test))
    # # transform lines of categorical variables to factor  
    # sdm_data_test[catindex_test] <- lapply(sdm_data_test[catindex_test], function(x) as.factor(x))
    # drops <- c()
    # # loop for identifying factors with fewer than two levels
    # for(j in catindex_test){
    #   if(length(levels(sdm_data_test[,j]))<2){
    #     drops <- c(drops,j)
    #   } 
    # }
    # # deleting factors with fewer than two levels
    # sdm_data_test <- sdm_data_test[ ,-drops]  
    #### export TEST data to disk 
    saveRDS(sdm_data_test, file = paste0(out_SDM_test_continuous, VT, "_SDM_data_test.Rds"))
    # saveRDS(pres_values_test, file = paste0(out_SDM_test_continuous, VT, "_p_test.Rds"))
    # saveRDS(abs_values_test, file = paste0(out_SDM_test_continuous, VT, "_a_test.Rds"))             
    fwrite(sdm_data_test, file = paste0(out_SDM_test_continuous, VT, "_SDM_data_test.csv"))
  }    
  
  ### 02 ####
  ### MODEL SELECTION USING MIAMAXENT CODE ########################################################################
  if (run_selection==TRUE){
    print(paste("STEP 02 - MODEL SELECTION USING MIAMAXENT CODE",VT,  sep = " "))
    print(Sys.time())
    # check if the file is already created
    if(file.exists(paste0(out_var_select, VT, "var_select.Rds"))){
      GLM_full <- readRDS(file = paste0(out_var_select, VT, "var_select.Rds"))
    } else {
      data <- sdm_data[,2:length(sdm_data)]
      # response variable binary
      rv <- sdm_data[,1]
      # enviro variables
      ev <- list()
      for (j in 1:length(data)) {
        ev[[j]] <- data.frame(data[, j])
        names(ev[[j]]) <- names(data)[j]
        names(ev)[j] <- names(data)[j]
      }
      #### function "parsevsGLM" has to be run by this point
      GLM_full <- parsevsGLM(rv, ev, alpha = 0.001, interaction = "FALSE")
    }
    #### write output table into csv file
    write.csv(GLM_full$selection, file = paste0(out_var_select, VT, "_var_select.csv"))
    saveRDS(GLM_full, file = paste0(out_var_select, VT, "var_select.Rds"))
  }
  #### 03 ####
  #### MODEL BUILD AND PREDICTION ########################################################################
  if (run_model==TRUE){
    print(paste("STEP 03 - MODEL BUILD AND PREDICTION for VT - ", VT, sep = " "))
    print(Sys.time())
    #### extract chosen model from parseGLM_mockdata
    
    GLM_names_list <- names(GLM_full[[1]])
    # writeClipboard(GLM_names_list)
    
    # build model using significant variables from Variable selection
    GLM_string <- paste("PA", "~", paste(GLM_names_list, collapse = " + "))
    mod <- stats::glm(formula = GLM_string, family = "binomial", data=sdm_data)
    #summary(mod)
    #print(mod)
    
    print(paste("STEP 03 - Saving model files for VT - ", VT, sep = " "))
    dir.create(file.path(out_model),showWarnings=F)
    setwd(out_model)
    saveRDS(mod, file = paste0(out_model, VT, "_model.Rds"))
    #print(paste(VT, "Plotting repsponse curves - FREQUENCY OF PRESENCE", sep = " "))
  }
  if (save_FOP==TRUE){
    print(paste("STEP 03 - Saving FOP plots for VT - ", VT, sep = " "))
    for(p in 1:length(GLM_names_list)){
      #in addition save the plot #NOT RUN
      png(filename = paste0(out_FOP, VT, "_FOP_", GLM_names_list[p], ".png"))
      plotFOP(data=sdm_data, EV=GLM_names_list[p], pch=20, cex=1.1, col = "red" )
      title(main = paste( "\n\nfor VT -",VT, sep=" "))
      dev.off()
      tiff(filename = paste0(out_FOP, VT, "_FOP_", GLM_names_list[p], ".tiff"))
      plotFOP(sdm_data, GLM_names_list[p], pch=20, cex=1.1, col = "red" )
      title(main = paste("\n\nfor VT -",VT, sep=" "))
      dev.off()
    }
  }
  # #### PREDICTION using single or multiple cores!!! ####
  # # anticipate 1,5 hours prediction time *(Time difference of 1.123403 hours)
  if (run_pred==TRUE){
    print(paste("STEP 03 - Start Model PREDICT for VT - ", VT , sep = " "))
    time_pred_start <- Sys.time()
    if(file.exists(paste0(out_raster,VT,"_predict_GLM.tif"))){
      prediction_GLM <- raster(paste0(out_raster,VT,"_predict_GLM.tif"))
    } else {
      
      print(Sys.time())
      n_cores <- parallel:::detectCores()-1
      if (run_model_cluster==TRUE){
        print(paste("using cluster of ", n_cores , "cores", sep = " "))
        beginCluster(n=n_cores)
        prediction_GLM <- clusterR(r.stack, raster::predict,
                                   args=list(model=mod, predict = predict.glm, type="response"))
        endCluster()
        writeRaster(prediction_GLM, filename = paste0(out_raster,VT,"_predict_GLM.tiff"), format='GTiff', overwrite=TRUE )
      } else {
        print("using single core")
        prediction_GLM <- raster::predict(r.stack, model = mod, predict = predict.glm,
                                          type="response", progress = "text",
                                          filename = paste0(out_raster,VT,
                                                            "_predict_GLM.tiff"),
                                          format = "GTiff", overwrite = TRUE)
      }
    }
    time_pred_end <- Sys.time()
    pred_time <- (time_pred_end-time_pred_start)
    print( paste( "Multi-CORE Predict finished in", pred_time, sep=" "))
  }
  #### OUTPUT RASTERS TO FILE ####
  #1 - coarse png
  if(save_maps == TRUE){
    print(paste("STEP 03 - saving maps into files for VT - ", VT, sep = " "))
    #1 - hi-res PDF
    pdf(file = paste0(out_maps, VT, "_prediction.pdf"))
    plot(prediction_GLM)
    title(main = paste("Vegetation type", VT, "\n", VT_name, sep=" "),
          sub = paste("Probability of presence for vegetation type", " - ", VT, sep= " "))
    dev.off()
    #2 - hi-res TIFF
    tiff(filename = paste0(out_maps, VT, "_prediction.tiff"),
         height = 17, width = 17, units = 'cm', res=300)
    plot(prediction_GLM)
    title(main = paste("Vegetation type", VT, "\n", VT_name, sep=" "),
          sub = paste("Probability of presence for vegetation type", " - ", VT, sep= " "))
    dev.off()
    #3 plus presence points
    png(filename = paste0(out_maps, VT, "_prediction_p.png"))
    plot(prediction_GLM)
    plot(VT_p, add=TRUE, pch= ".", col="blue")
    plot(VT_p_test, add=TRUE, pch= ".", col="red")
    legend("bottomright",legend = c('train points', 'test points'),
           col = c('blue', 'red'),
           inset=.02, pch=1, cex=0.8)
    title(main = paste("Vegetation type", VT, "\n", VT_name, sep=" "),
          sub = paste("Probability of presence for vegetation type", " - ", VT, sep= " "))
    dev.off()
    #4 TIFF plus presence points
    tiff(filename = paste0(out_maps, VT, "_prediction_p.tiff"),
         height = 17, width = 17, units = 'cm', res=300)
    plot(prediction_GLM)
    plot(VT_p, add=TRUE, pch= ".", col="blue")
    plot(VT_p_test, add=TRUE, pch= ".", col="red")
    legend("bottomright",legend = c('train points', 'test points'),
           col = c('blue', 'red'),
           inset=.02, pch=1, cex=0.8)
    title(main = paste("Vegetation type", VT, "\n", VT_name, sep=" "),
          sub = paste("Probability of presence for vegetation type", " - ", VT, sep= " "))
    dev.off()
  }
  
  #### 04 ####
  #### MODEL EVALUATION ####
  if (run_eval==TRUE){
    print(paste("STEP 04 - MODEL EVALUATION for VT - ", VT, sep = " "))
    # read in data if necessary
    # eval_VT_pres <- readRDS(file = paste("E:/Project_1_RUNS/full_MODEL_RUN/EVAL_data/", "eval_", VT, "_p.Rds", sep = ""))
    # eval_VT_abs <- readRDS(file = paste("E:/Project_1_RUNS/full_MODEL_RUN/EVAL_data/", "eval_", VT, "_a.Rds", sep = ""))
    # mod_VT <- readRDS(file = paste("E:/Project_1_RUNS/full_MODEL_RUN/GLMmodel/", VT, "_model.Rds", sep = ""))
    # train_VT_pres <- readRDS(file = paste("E:/Project_1_RUNS/full_MODEL_RUN/PA_data/", VT, "_p.Rds", sep = ""))
    # train_VT_abs <- readRDS(file = paste("E:/Project_1_RUNS/full_MODEL_RUN/PA_data/", VT, "_a.Rds", sep = ""))
    # VT_predict <- raster(x=paste0("E:/Project_1_RUNS/full_MODEL_RUN/", VT, "_predict_GLM.tif"))
    
    #evaluate dataset
    time_eval_start <- Sys.time()
    print(time_eval_start)
    print(paste("STEP 04 - Train data EVALUATION for VT - ", VT, sep = " "))
    # run evaluate in paralel processes
    if(file.exists(paste0(out_eval, VT, "_train_eval.Rds"))){
      e_train <- readRDS(file = paste0(out_eval, VT, "_train_eval.Rds"))
    } else {
      e_train <- dismo::evaluate(p=VT_p, a=VT_a, model=mod, x=r.stack)
      
    }
    print(paste("STEP 04 - Test data EVALUATION for VT - ", VT, sep = " "))
    if(file.exists(paste0(out_eval, VT, "_test_eval.Rds"))){
      e_test <- readRDS(file = paste0(out_eval, VT, "_test_eval.Rds"))
    } else {
      e_test <- dismo::evaluate(p=VT_p_test, a=VT_a_test, model=mod, x=r.stack)
      
    }
    #
    e_train_tr <- threshold(e_train)
    e_test_tr <- threshold(e_test)
    saveRDS(e_train, file = paste0(out_eval, VT, "_train_eval.Rds"))
    saveRDS(e_test, file = paste0(out_eval, VT, "_test_eval.Rds"))
    
    #calculate euclidean distance matrix between presence points
    # VT_p_train <- readOGR(in_train_veg_presence, paste(VT, "_p", sep = ""))
    # VT_p_test <- readOGR(in_test_veg_presence, paste("eval_", VT, "_p", sep = ""))
    print(paste("STEP 04 - euclidean distance - ", VT, sep = " "))
    train_dist <- vegan::vegdist(VT_p@coords, method = "euclidean")
    test_dist <- vegan::vegdist(VT_p_test@coords, method = "euclidean")
    saveRDS(train_dist, file = paste0(out_eval, VT, "_train_dist_eval.Rds"))
    saveRDS(test_dist, file = paste0(out_eval, VT, "_test_dist_eval.Rds"))
    
    # calculate distance to coast (requires package::rgeos)
    train_coast <- gDistance(spgeom1 = VT_p, spgeom2 = coastline, byid=TRUE )
    test_coast <- gDistance(spgeom1 = VT_p_test, spgeom2 = coastline, byid=TRUE )
    saveRDS(train_coast, file = paste0(out_eval, VT, "_train_coast_eval.Rds"))
    saveRDS(test_coast, file = paste0(out_eval, VT, "_test_coast_eval.Rds"))
    
    #populate the table with values from EVALUATION
    ctable$veg.type[i] <- VT
    ctable$num.EVs[i] <- length(mod$coefficients) - 1 # to exclude "(Intercept)" in variable count
    ctable$incl.EVs[i] <- paste(names(mod[[1]])[2:length(mod$coefficients)], collapse = "; ")
    ctable$train.pres[i] <- e_train@np
    ctable$train.abs[i] <- e_train@na
    ctable$test.pres[i] <- e_test@np
    ctable$test.abs[i] <- e_test@na
    ctable$train.AUC[i] <- e_train@auc
    ctable$test.AUC[i] <- e_test@auc
    ctable$correlation[i] <- e_test@cor
    ctable$tr.kappa[i] <- e_test_tr$kappa
    ctable$kappa[i] <- max(e_test@kappa)
    ctable$train_prevalence[i] <- e_train@np/(e_train@np+e_train@na)
    ctable$test_prevalence[i] <- e_test@np/(e_test@np+e_test@na)
    ctable$train.vs.test.prev[i] <- (e_train@np/(e_train@np+e_train@na))/(e_test@np/(e_test@np+e_test@na))
    ctable$fitted.max[i] <- max(mod$fitted.values)
    ctable$fitted.min[i] <- min(mod$fitted.values)
    ctable$raster.max[i] <- prediction_GLM@data@max
    ctable$raster.min[i] <- prediction_GLM@data@min
    ctable$train.avg.dist[i] <- mean.default(train_dist)
    ctable$test.avg.dist[i] <- mean.default(test_dist)
    ctable$train.coast.dist[i] <- mean.default(train_coast)
    ctable$test.coast.dist[i] <- mean.default(test_coast)
    
    if (save_eval_plots==TRUE){
      # printing png figures into file
      print('saving evaluation plots into files')
      # ROC
      png(filename = paste0(out_eval, VT, "_ROC.png"))
      plot(e_test, 'ROC')
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      # TPR
      png(filename = paste0(out_eval, VT, "_TPR.png"))
      plot(e_test, 'TPR')
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      # density
      png(filename = paste0(out_eval, VT, "_density.png"))
      density(e_test)
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      # boxplot
      png(filename = paste0(out_eval, VT, "_boxplot.png"))
      boxplot(e_test)
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      
      # Plot TIFFs
      # ROC
      tiff(filename = paste0(out_eval, VT, "_ROC.tiff"))
      plot(e_test, 'ROC')
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      # TPR
      tiff(filename = paste0(out_eval, VT, "_TPR.tiff"))
      plot(e_test, 'TPR')
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      # density
      tiff(filename = paste0(out_eval, VT, "_density.tiff"))
      density(e_test)
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
      # boxplot
      tiff(filename = paste0(out_eval, VT, "_boxplot.tiff"))
      boxplot(e_test)
      title(main = paste("\n \n", VT, " - ", VT_name, sep=" "))
      dev.off()
    }
    # Time for evaluation 
    time_eval_end <- Sys.time()
    eval_time <- (time_eval_end-time_eval_start)
    print( paste( "Evaluation finished in", eval_time, sep=" "))
  }
  
  #### calculate total time ####
  end.time.pred <- Sys.time()
  time.taken.pred <- end.time.pred - start.time.pred
  
  print(paste0("Loop iteration finished for ", VT, " in ", time.taken.pred))
  
  time_table$pred[i] <- pred_time
  time_table$eval[i] <- eval_time
  time_table$tot[i] <- time.taken.pred
}

#### 05 ####
# Outside of main loop
# save the final table that contains info from all 31 VT iterations in loop
print(paste("Save TABLES for all VT's"))
fwrite(ctable, file = paste0(ctable_path,"Output_table.csv"))
fwrite(ctable, file = paste0(ctable_path,"Output_table2.csv"), sep = ';')
fwrite(time_table, file = paste0(ctable_path,"Time_table.csv"))
##### save log for VT into file ####
### Error in sink(con, append = TRUE) : sink stack is full
# con <- file(paste(log_files, "log_file.log", sep = ""))
# sink(con, append=TRUE)
# sink(con, append=TRUE, type="message")

#### FINISH ####
print(paste("ALL ITERATIONS WITHIN LOOP FINISHED !"))

#### publication 9x TIFF ####
# read all raster files inside HOME folder and add them to a list
r.list <- list.files(in_cropped_data_DM, pattern="tif$", full.names=TRUE)
r.list <- mixedsort(r.list)
r.stack <- stack(r.list)
#1-9
tiff(filename = paste0("D:/Project_1_RUNS/new_MODEL_RUN/04_predict_lowres/manuscript_plots/",
                       "VT1_to_9.tiff"),
     height = 29, width = 21, units = 'cm', res=300)
par(mfrow=c(3,3), oma=c(1,1,1,2))
for(i in 1:9){
  plot(r.stack[[i]], 
       sub = paste("Vegetation type", vt_list[i], "\n", vt_names[i], sep=" "))
  }
dev.off()
#10-18
tiff(filename = paste0("D:/Project_1_RUNS/new_MODEL_RUN/04_predict_lowres/manuscript_plots/",
                       "VT10_to_18.tiff"),
     height = 29, width = 21, units = 'cm', res=300)
par(mfrow=c(3,3), oma=c(1,1,1,2))
for(i in 10:18){
  plot(r.stack[[i]], 
       sub = paste("Vegetation type", vt_list[i], "\n", vt_names[i], sep=" "))
}
dev.off()
#19-27
tiff(filename = paste0("D:/Project_1_RUNS/new_MODEL_RUN/04_predict_lowres/manuscript_plots/",
                       "VT19_to_27.tiff"),
     height = 29, width = 21, units = 'cm', res=300)
par(mfrow=c(3,3), oma=c(1,1,1,2))
for(i in 19:27){
  plot(r.stack[[i]], 
       sub = paste("Vegetation type", vt_list[i], "\n", vt_names[i], sep=" "))
}
dev.off()
#28-31
tiff(filename = paste0("D:/Project_1_RUNS/new_MODEL_RUN/04_predict_lowres/manuscript_plots/",
                       "VT28_to_31.tiff"),
     height = 29, width = 21, units = 'cm', res=300)
par(mfrow=c(3,3), oma=c(1,1,1,2))
for(i in 28:31){
  plot(r.stack[[i]], 
       sub = paste("Vegetation type", vt_list[i], "\n", vt_names[i], sep=" "))
}
dev.off()
