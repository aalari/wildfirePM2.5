
library(dplyr) 
library(arrow)
library(lubridate) # Required Functions: wday, month
library(purrr)
library(tidyr)
library(tidyverse)
library("readxl")
library(splines) # Required Functions: ns, bs
library(dlnm) # Required Functions: logknots, crossbasis
library(mixmeta) # Required Function: mixmeta
library(writexl)
library(giscoR) # Required Functions: gisco_get_countries
library(sf)  # Required Functions: st_read
library(grid) # Required Functions: viewport
library(lwgeom)
library(tmap)

# Code for Akaike's Information Criterion for Overdispersed Count Data
source("ADAPTATION_EXTERNAL/aalari/Codes/Functions/QAIC.R"); 
source("ADAPTATION_EXTERNAL/aalari/Codes/Functions/FWALD.R"); # Wald Test of the Meta-Predictors
source("ADAPTATION_EXTERNAL/aalari/Codes/Functions/Map_functions/map_generator.R"); # Preparation of the Maps

# Meta Table with Nuts and Year information
nuts_info<-read.csv(file="ADAPTATION_EXTERNAL/aalari/Data/NUTS-info.csv")
# nuts_info<-read.csv(file="X:/aalari/Data/NUTS-info.csv")

## Dataframe with mortality and exposure data 
data<-read_parquet("ADAPTATION_EXTERNAL/aalari/Data/data.parquet")
# data<-read_parquet("X:/aalari/Data/data.parquet")

FOLD_RESULTS = "ADAPTATION_EXTERNAL/aalari/Results/";
FOLD_DATA_OUT = "ADAPTATION_EXTERNAL/aalari/Plots/"; 


# Main analysis + Moving average for Heat Index from 8 to 14 days


Model_list<-c("/M0_no_adj", "/M1_Heat_Index", "/M2_Main_Analysis", "/SA_M2_after2020")
Exposure_list<-c("/PM25Fire", "/PM25Other", "/PM25Total")
Outcome_list<-c("/All_cause", "/Cardio", "/Respi")


################################################################### 
Model<-Model_list[3]
Exposure<-Exposure_list[1]
Outcome<- Outcome_list[1]


data<-data %>% arrange(location, date)

data<-data %>% group_by(location) %>% mutate(heat_index_07=(lag(hi_c, 1)+ lag(hi_c, 2) + lag(hi_c, 3) + lag(hi_c, 4) + lag(hi_c, 5) + lag(hi_c, 6) + lag(hi_c, 7))/7)

nuts_info<-nuts_info %>% arrange(NUTS_ID) 

#vREG_NUTS_CODE = TABLE_INFO_NUTS$location;
 
# Creating the Vectors of Codes and Names of the NUTS Regions 
nNUTS <- length(unique(data$location))
vNUTS_CODE<-unique(data$location)
vNUTS_NAME <-pull(nuts_info %>% 
                    filter(NUTS_ID %in% vNUTS_CODE) %>% 
                    select(NUTS_NAME))




# Creating the Vectors of same length of NUTS codes vector with Codes and Names of the Countries 
nCOUNTRIES <- length(unique(nuts_info$CNTR_CODE[which(nuts_info$LEVL_CODE==0)]))

vCOUNTRIES_CODE_LONG <- nuts_info$CNTR_CODE[which(nuts_info$NUTS_ID %in% vNUTS_CODE)]
vCOUNTRIES_CODE <- unique(vCOUNTRIES_CODE_LONG)
nCOUNTRIES <-length(unique(vCOUNTRIES_CODE_LONG))
vCOUNTRIES_NAME = vCOUNTRIES_CODE

vCOUNTRIES_NAME = vCOUNTRIES_CODE
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "AL" ) ] = "Albania"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "AT" ) ] = "Austria"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "BA" ) ] = "Bosnia and Herzegovina"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "BE" ) ] = "Belgium"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "BG" ) ] = "Bulgaria"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "CH" ) ] = "Switzerland"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "CY" ) ] = "Cyprus"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "CZ" ) ] = "Czechia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "DE" ) ] = "Germany"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "DK" ) ] = "Denmark"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "EE" ) ] = "Estonia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "EL" ) ] = "Greece"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "ES" ) ] = "Spain"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "FI" ) ] = "Finland"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "FR" ) ] = "France"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "HR" ) ] = "Croatia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "HU" ) ] = "Hungary"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "IE" ) ] = "Ireland"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "IS" ) ] = "Iceland"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "IT" ) ] = "Italy"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "LI" ) ] = "Liechtenstein"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "LT" ) ] = "Lithuania"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "LU" ) ] = "Luxembourg"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "LV" ) ] = "Latvia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "ME" ) ] = "Montenegro"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "MT" ) ] = "Malta"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "NL" ) ] = "Netherlands"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "NO" ) ] = "Norway"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "PL" ) ] = "Poland"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "PT" ) ] = "Portugal"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "RO" ) ] = "Romania"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "RS" ) ] = "Serbia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "SE" ) ] = "Sweden"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "SI" ) ] = "Slovenia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "SK" ) ] = "Slovakia"
vCOUNTRIES_NAME[ which( vCOUNTRIES_NAME == "UK" ) ] = "United Kingdom"



  
# Creating the Lists with All the Regions 
DATALIST_DATA_CALI<-DATALIST_DATA_PRED<-lapply(vNUTS_CODE, function(x) data[ data$location == x, ] )

names(DATALIST_DATA_CALI)<-names(DATALIST_DATA_PRED)<-vNUTS_CODE

DATES_NUTS = as.Date( c( "2004-01-01", "2019-12-31", "2004-01-01", "2022-12-31" ) );




################################################################################
##    Location-Specific Associations (PRE-META ANALYSIS MODELS)               ##
################################################################################

# 1.Models outcomes

#Regional Akaike's Information Criterion for Overdispersed Count Data
vQAIC_MODEL = array( NA, dim = nNUTS, dimnames = list(vNUTS_CODE) );

#Partial autocorrelations of the model residuals
vPACF = array( NA, dim = nNUTS, dimnames = list(vNUTS_CODE) )


#Regional Exposure-Lag-Response Before the Meta-Analysis
CROSSPRED_NUTS_PREMETA = vector("list", nNUTS)
names(CROSSPRED_NUTS_PREMETA) <- vNUTS_CODE


# 2. Models parameters (crossbasis)

#Lag-Response: Minimum and Maximum Lag
MIN_LAG =  0 
MAX_LAG = 7

#Exposure-Response: Type of the Spline
FUNC_FIRE = "lin"
DEGR_FIRE = NA

#Exposure-Response: Percentiles of the Fire-related PM25 distribution  
PERC_FIRE = NA

#PERC_FIRE = c(10, 99)/100

#Lag-Response: Lag Knots
LAG_KNOTS = logknots( c( MIN_LAG, MAX_LAG ), 2 )

#Degrees of Freedom per Year for the Seasonal and Long-Term Trends
DF_SEAS = 8

# For saving results
if (length(PERC_FIRE)>1) {
  KNOTS<-paste0(PERC_FIRE[1],"_",PERC_FIRE[2])
} else if (is.na(PERC_FIRE)){
  KNOTS<-0
} else {
  KNOTS<-PERC_FIRE
}


# 3. Predictions parameters 

# Percentiles for the Predictions of the Cumulative Exposure-Response
# vPERC_CROSSPRED = sort( unique( c( seq(  0.0,   1.0, 0.1 ),
#                                    seq(  1.5,   5.0, 0.5 ),
#                                    seq(  6.0,  94.0, 1.0 ),
#                                    seq( 95.0,  98.5, 0.5 ),
#                                    seq( 99.0, 100.0, 0.1 ) ) / 100 ) );
# if( min(vPERC_CROSSPRED) != 0 | max(vPERC_CROSSPRED) != 1 | any( 0 > vPERC_CROSSPRED | vPERC_CROSSPRED > 1 ) ){
#   stop("ERROR: Invalid Percentile Vector for the Predictions of the Cumulative Exposure-Response !!!");
# }

# Values for the Predictions of the Cumulative Exposure-Response and Lag-Response Association
VALUES_CROSSPRED = c( 1: 10 )
VALUES_CROSSPRED_LAG=c(1,10)


# 4. Items to obtain Reduced Coefficients 

# Regional Reduced Coefficients: Cumulative Exposure-Response and Lag-Response
COEF_MODEL = vector( "list", 1 + length(VALUES_CROSSPRED_LAG) )

names(COEF_MODEL) = c( "CUMU", "LAG_1microgram", "LAG_10microgram")

if ( FUNC_FIRE == "lin") { COEF_MODEL[[1]] = matrix( data = NA, nNUTS, length(PERC_FIRE), dimnames = list(vNUTS_CODE) )
}else if( FUNC_FIRE == "ns" ){ COEF_MODEL[[1]] = matrix( data = NA, nNUTS, length(PERC_FIRE) +     1    , dimnames = list(vNUTS_CODE) )
}else if( FUNC_FIRE == "bs" ){ COEF_MODEL[[1]] = matrix( data = NA, nNUTS, length(PERC_FIRE) + DEGR_FIRE, dimnames = list(vNUTS_CODE) )
}else                        { stop("ERROR: Invalid FUNC_FIRE !!!"); }

if( length(VALUES_CROSSPRED_LAG) > 0 ){
  for( iPERC in 1:length(VALUES_CROSSPRED_LAG) ){
    COEF_MODEL[[1+iPERC]] = matrix( data = NA, nNUTS, length(LAG_KNOTS) + 2, dimnames = list(vNUTS_CODE) );
  }
  rm(iPERC);
}


# Regional Reduced Covariance Matrices: Cumulative Exposure-Response and Lag-Response
VCOV_MODEL = vector( "list", 1 + length(VALUES_CROSSPRED_LAG) );
names(VCOV_MODEL) = c( "CUMU", "LAG_1microgram", "LAG_10microgram" );
for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  VCOV_MODEL[[1+iPERC]] = vector( "list", nNUTS )
  names(VCOV_MODEL[[1+iPERC]]) = vNUTS_CODE
}
rm(iPERC)

# definition different period for calibration and prediction
for( iREG in 1:nNUTS ){
  DATALIST_DATA_CALI[[iREG]] = DATALIST_DATA_CALI[[iREG]][ which( DATALIST_DATA_CALI[[iREG]]$date >= DATES_NUTS[1] ), ] 
  DATALIST_DATA_CALI[[iREG]] = DATALIST_DATA_CALI[[iREG]][ which( DATALIST_DATA_CALI[[iREG]]$date <= (DATES_NUTS[2] + max( MAX_LAG, 0 ))),] 
}
rm(iREG);


#####################################################################
# 5. Models ##

for(iNUTS in 1:nNUTS){
  print( paste0( "  Region ", iNUTS, " / ", nNUTS, ": ", vNUTS_NAME[iNUTS], " (", vNUTS_CODE[iNUTS], ")" ) );
  
  # Formula of the Seasonality and Cross-Basis Models
  FORMULA_SEA = mort ~ ns( date, df = round( DF_SEAS * length(date) / 365.25 ) );
  FORMULA_CRB = mort ~ ns( date, df = round( DF_SEAS * length(date) / 365.25 ) ) + dow + CROSSBASIS_FIRE + ns(heat_index_07, df=3) + CROSSBASIS_PM25;
  
  # 5.a. Fitting the Seasonality Model
  GLM_MODEL_SEA = glm( formula = FORMULA_SEA,
                       DATALIST_DATA_CALI[[iNUTS]],
                       family = quasipoisson,
                       na.action = "na.exclude" );
  rm(FORMULA_SEA);
  
  # Predicting the Seasonality of Mortality from the Seasonality Model
  DATALIST_DATA_CALI[[iNUTS]]$mort_pred_seas = predict( GLM_MODEL_SEA, type = "response" );
  rm(GLM_MODEL_SEA);
  
  # Cross-Basis of Fire-related PM25
  
  if (FUNC_FIRE == "lin" ) {
    CROSSBASIS_FIRE = crossbasis (DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, 
                c( MIN_LAG, MAX_LAG ),
                arglag = list( knots = LAG_KNOTS ),
                argvar = list( fun = FUNC_FIRE))  
  }else if( FUNC_FIRE == "ns" ){
    CROSSBASIS_FIRE = crossbasis( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire,
                                  c( MIN_LAG, MAX_LAG ),
                                  arglag = list( knots = LAG_KNOTS ),
                                  argvar = list( knots = quantile( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, PERC_FIRE, na.rm = TRUE ),
                                                 fun = FUNC_FIRE,
                                                 Boundary.knots = range( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ) ) );
  }else if( FUNC_FIRE == "bs" ){
    CROSSBASIS_FIRE = crossbasis( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire,
                                  c( MIN_LAG, MAX_LAG ),
                                  arglag = list( knots = LAG_KNOTS ),
                                  argvar = list( knots = quantile( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, PERC_FIRE, na.rm = TRUE ),
                                                 fun = FUNC_FIRE,
                                                 degree = DEGR_FIRE ) );
  }else{
    stop("ERROR: Invalid FUNC_FIRE !!!");
  }
  if( iNUTS == 1 ){ CROSSBASIS_FIRE_ARGLAG = attr( CROSSBASIS_FIRE, "arglag" ); }
  
  
  ## CROSS BASIS NON-FIRE PM25
  CROSSBASIS_PM25 = crossbasis (DATALIST_DATA_CALI[[iNUTS]]$PM25Other, 
                                c( MIN_LAG, MAX_LAG ),
                                arglag = list( knots = LAG_KNOTS ),
                                argvar = list( fun = FUNC_FIRE));
  
  
  # 5.b. Fitting the Cross-Basis Model
  GLM_MODEL_CRB = glm( formula = FORMULA_CRB,
                       DATALIST_DATA_CALI[[iNUTS]],
                       family = quasipoisson,
                       na.action = "na.exclude" );
  
  # Predicting the mortality outcome from the Cross-Basis Model
  DATALIST_DATA_CALI[[iNUTS]]$mort_pred_crbs = predict( GLM_MODEL_CRB, type = "response" );
  
  # Check errors
  if( !GLM_MODEL_CRB$converged ){
    print( "    WARNING: The Cross-Basis Model Did Not Converge" );
  }
  if( any( !is.finite( summary(GLM_MODEL_CRB)[[13]] ) ) ){
    print( "    WARNING: Non-Finite Model Coefficients or Standard Errors" );
    print( summary(GLM_MODEL_CRB)[[13]] );
  }
  if( any( !is.finite( coef(GLM_MODEL_CRB) ) ) ){
    print( "    WARNING: Non-Finite Model Coefficients" );
    print( coef(GLM_MODEL_CRB) );
  }
  if( any( !is.finite( vcov(GLM_MODEL_CRB) ) ) ){
    print( "    WARNING: Non-Finite Model Covariance Matrix" );
    print( vcov(GLM_MODEL_CRB) );
  }
  rm(FORMULA_CRB);
  
# Regional Akaike's Information Criterion for Overdispersed Count Data 
  vQAIC_MODEL[iNUTS] <- QAIC(GLM_MODEL_CRB)   
  

  # 5.c. Predictions for Regional Exposure-Lag-Response with Centering to the minimum
  CROSSPRED_NUTS_PREMETA[[iNUTS]] = crosspred( CROSSBASIS_FIRE,
                                                  GLM_MODEL_CRB,
                                                  model.link = "log",
                                                  at = seq(from=round(min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ), digits=2), to=round(max( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ), digits=2), by=0.5),
                                                  bylag = 1,
                                                  cen = min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ) )
  
  # 5.d. Obtaining Regional Reduced Coefficients and Covariance Matrix: Cumulative Exposure-Response
  REDUCED = crossreduce( CROSSBASIS_FIRE,
                         GLM_MODEL_CRB,
                         model.link = "log",
                         cen = min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ) )
  COEF_MODEL[[1]][iNUTS,] = coef( REDUCED )
  VCOV_MODEL[[1]][[iNUTS]] = vcov( REDUCED )
  rm(REDUCED)
  if( any( !is.finite( COEF_MODEL[[1]][iNUTS,] ) ) ){
    print( "    WARNING: Non-Finite Reduced Cumulative Exposure-Response Coefficients" );
    print( COEF_MODEL[[1]][iNUTS,] );
  }
  if( any( !is.finite( VCOV_MODEL[[1]][[iNUTS]] ) ) ){
    print( "    WARNING: Non-Finite Reduced Cumulative Exposure-Response Covariance Matrix" );
    print( VCOV_MODEL[[1]][[iNUTS]] );
  }
  
  # 5.e. Obtaining Regional Reduced Coefficients and Covariance Matrix: Lag-Response
  if( length(VALUES_CROSSPRED) > 0 ){
    for( iPERC in 1:length(VALUES_CROSSPRED_LAG) ){
      REDUCED = crossreduce( CROSSBASIS_FIRE,
                             GLM_MODEL_CRB,
                             model.link = "log",
                             cen = min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ),
                             type = "var",
                             value = VALUES_CROSSPRED_LAG[iPERC] ) 
      COEF_MODEL[[1+iPERC]][iNUTS,] = coef( REDUCED )
      VCOV_MODEL[[1+iPERC]][[iNUTS]] = vcov( REDUCED )
      #rm(REDUCED);
      if( any( !is.finite( COEF_MODEL[[1+iPERC]][iNUTS,] ) ) ){
        print( paste0( "WARNING: Non-Finite Reduced Lag-Response Coefficients" ) );
        print( COEF_MODEL[[1+iPERC]][iNUTS,] );
      }
      if( any( !is.finite( VCOV_MODEL[[1+iPERC]][[iNUTS]] ) ) ){
        print( paste0( "WARNING: Non-Finite Reduced Lag-Response Covariance Matrix" ) );
        print( VCOV_MODEL[[1+iPERC]][[iNUTS]] );
      }
    }
    rm(iPERC);
  }
  
  #rm(CROSSBASIS_FIRE, GLM_MODEL_CRB);
  
}


TABLE_QAIC = data.frame( name = c( "Europe", vNUTS_NAME ),
                         code = c( "Europe", vNUTS_CODE ),
                         qaic = sprintf( "%.0f",
                                         round( c( sum(vQAIC_MODEL), vQAIC_MODEL ), digits = 0 ) ) );
TABLE_QAIC = rownames_to_column( TABLE_QAIC, var = "NUTS" );
write_xlsx( TABLE_QAIC, paste0(FOLD_RESULTS, "QAIC", Model, Exposure, Outcome, ".xlsx" ) );
rm(TABLE_QAIC);





################################################################################
###      Calculation of the Regional Best Linear Unbiased Predictions       ####
################################################################################

print("= Calculation of the Regional Best Linear Unbiased Predictions =")

## 1. Defying meta-analysis parameters
######################################

# 1.a. Formula for the Best Linear Unbiased Predictions and Pooled Coefficients
FORMULA_META = COEF_MODEL[[1+iPERC]] ~ MP_FIRE_IQR;
#FORMULA_META = COEF_MODEL[[1+iPERC]] ~ MP_FIRE_P99;

# 1.b. Boolean for the Country Random Effects
bCOU_RAND_EFF = TRUE;


####### Meta-predictors for Meta-regression ########
# Regional Meta-Predictors (see FORMULA_META):
#   MP_FIRE_ANN: Annual Fire-related PM2.5
#   MP_FIRE_SUM: Summer Fire-related PM2.5
#   MP_FIRE_P01: Fire-related PM2.5 Percentile 1st
#   MP_FIRE_P99: Fire-related PM2.5 Percentile 99th
#   MP_FIRE_IQR: Fire-related PM2.5 Inter-Quartile Range
#   MP_TEMP_SUM: Average Summer Temperature 
MP_FIRE_ANN = sapply( DATALIST_DATA_CALI, function(x) mean( x$PM25Fire, na.rm = TRUE ) )
MP_FIRE_SUM = sapply( DATALIST_DATA_CALI, function(x) mean( x$PM25Fire[  6 <= month(x$date) & month(x$date) <= 8 ], na.rm = TRUE ) )
# MP_FIRE_P01 = sapply( DATALIST_DATA_CALI, function(x) quantile( x$temp, 0.01, na.rm = TRUE ) ); names(MP_TEMP_P01) = vNUTS_CODE;
MP_FIRE_P99 = sapply( DATALIST_DATA_CALI, function(x) quantile( x$PM25Fire, 0.99, na.rm = TRUE ) ); names(MP_FIRE_P99) = vNUTS_CODE;
MP_FIRE_IQR = sapply( DATALIST_DATA_CALI, function(x) IQR( x$PM25Fire, na.rm = TRUE ) );
MP_HEAT_INDEX_SUM = sapply( DATALIST_DATA_CALI, function(x) mean( x$hi_c[  6 <= month(x$date) & month(x$date) <= 8 ], na.rm = TRUE ) )
########################################################


## 2. Preparing Meta-analysis outcomes ##
#########################################

# 2.a. Multivariate Meta-Analysis and Best Linear Unbiased Predictions
MVAR_META = BLUP_META = vector( "list", 1 + length(VALUES_CROSSPRED_LAG) )

names(MVAR_META) = names(BLUP_META) = c( "CUMU", "LAG_1microgram", "LAG_10microgram" )


# 2.b. Regional Cumulative Exposure-Response and Lag-Response After the Meta-Analysis
CROSSPRED_NUTS_POSTMETA = vector( "list", 1 + length(VALUES_CROSSPRED_LAG) )

names(CROSSPRED_NUTS_POSTMETA) = c( "CUMU", "LAG_1microgram", "LAG_10microgram" )

for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  CROSSPRED_NUTS_POSTMETA[[1+iPERC]] = vector( "list", nNUTS);
  names(CROSSPRED_NUTS_POSTMETA[[1+iPERC]]) = vNUTS_CODE;
}

rm(iPERC)


# 3. Meta-analysis #
####################

for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  if( iPERC == 0 ){ print(         "Multivariate Meta-Analysis of the Cumulative Exposure-Response for 1 microgram/m3 increase"                                                                                 ); }
  else            { print( paste0( "Multivariate Meta-Analysis of the Lag-Response at ", VALUES_CROSSPRED_LAG[iPERC], " microgram/m3 of Fire-PM2.5") ) ; }
  print("");
  
  # Multivariate Meta-Analysis of the Regional Reduced Coefficients and Covariance Matrix
  if( bCOU_RAND_EFF ){
    MVAR_META[[1+iPERC]] = mixmeta( FORMULA_META,
                                                      VCOV_MODEL[[1+iPERC]],
                                                      data = data.frame( vREG = vNUTS_CODE ),
                                                      control = list( showiter = TRUE, igls.inititer = 10, maxiter = 1000 ),
                                                      method = "reml",
                                                      random =~ 1 | factor(vCOUNTRIES_CODE_LONG) / factor(vNUTS_CODE) ); # random intercept
                                    #random =~ 1 | factor(vCOUNTRIES_CODE_LONG) ); 
  }else{
    MVAR_META[[1+iPERC]] = mixmeta( FORMULA_META,
                                                      VCOV_MODEL[[1+iPERC]],
                                                      data = data.frame( vREG = vNUTS_CODE ),
                                                      control = list( showiter = TRUE, igls.inititer = 10, maxiter = 1000 ),
                                                      method = "reml" );
  }
  print( summary( MVAR_META[[1+iPERC]] ) );
  print("");
  
  # Wald Test of the Meta-Predictors
  if( length( summary(MVAR_META[[1+iPERC]])$lab$p ) > 1 ){
    for( iMETAPRED in 1:length( summary(MVAR_META[[1+iPERC]])$lab$p ) ){
      print( paste0( "  Wald Test of ", summary(MVAR_META[[1+iPERC]])$lab$p[iMETAPRED], ": p = ", sprintf( "%.10f", FWALD( MVAR_META[[1+iPERC]], summary(MVAR_META[[1+iPERC]])$lab$p[iMETAPRED] ) ) ) );
    }
    rm(iMETAPRED);
    print("");
  }
  
  # Regional Best Linear Unbiased Predictions
  BLUP_META[[1+iPERC]] = blup( MVAR_META[[1+iPERC]], vcov = TRUE );
  
    }   

if ( FUNC_FIRE == "lin" ){
  BLUP_META$CUMU<-apply(BLUP_META$CUMU, 1, as.list)
#rm(COEF_MODEL,VCOV_MODEL, iPERC)
}

# 4. Estimating Regional Best Linear Unbiased Predictions (BLUP) #
##################################################################


for( iREG in 1:nNUTS ){
  print( paste0( "  Region ", iREG, " / ", nNUTS, ": ", vNUTS_NAME[iREG], " (", vNUTS_CODE[iREG], ")" ) );
  
  # Vector of Fire-related PM2.5 for the Cross-Predictions
  PM25Fire_CROSSPRED = DATALIST_DATA_CALI[[iREG]]$PM25Fire;
  
  # One-Basis of the Fire-Related PM2.5
  
  if ( FUNC_FIRE == "lin" ){
    ONEBASIS_FIRE = onebasis( PM25Fire_CROSSPRED,
                              fun= FUNC_FIRE)
  }else if( FUNC_FIRE == "ns" ){
    ONEBASIS_FIRE = onebasis( PM25Fire_CROSSPRED,
                              knots = PM25Fire_CROSSPRED[ paste0( 100 * PERC_FIRE, ".0%" ) ],
                              fun = FUNC_FIRE,
                              Bound = range( PM25Fire_CROSSPRED, na.rm = TRUE ) );
  }else if( FUNC_FIRE == "bs" ){
    ONEBASIS_FIRE = onebasis( PM25Fire_CROSSPRED,
                              knots = PM25Fire_CROSSPRED[ paste0( 100 * PERC_FIRE, ".0%" ) ],
                              fun = FUNC_FIRE,
                              degree = DEGR_FIRE,
                              Bound = range( PM25Fire_CROSSPRED, na.rm = TRUE ) );
  }else{
    stop("ERROR: Invalid FUNC_FIRE !!!");
  }
  
  # One-Basis of Lags
  ONEBASIS_LAGS = do.call( onebasis, c( list( x = seq( MIN_LAG, MAX_LAG, 1 ) ), CROSSBASIS_FIRE_ARGLAG ) );
  
  
  # Predictions for Regional Cumulative Exposure-Response with Centering 
  
  if ( FUNC_FIRE == "lin" ){
  dim(BLUP_META[[1]][[iREG]]$vcov)[1]<-length(BLUP_META[[1]][[iREG]]$blup)  
  CROSSPRED_NUTS_POSTMETA[[1]][[iREG]] = crosspred(ONEBASIS_FIRE,
                                                        coef = BLUP_META[[1]][[iREG]]$blup,
                                                        vcov = BLUP_META[[1]][[iREG]]$vcov,
                                                        model.link = "log",
                                                        at = seq(from=round(min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ), digits=2), to=round(max( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ), digits=2), by=0.5),
                                                        cen = min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ))
  
  }else if( FUNC_FIRE == "ns" | FUNC_FIRE == "bs"){
    CROSSPRED_NUTS_POSTMETA[[1]][[iREG]] = crosspred(ONEBASIS_FIRE,
                                                     coef = BLUP_META[[1]][[iREG]]$blup,
                                                     vcov = BLUP_META[[1]][[iREG]]$vcov,
                                                     model.link = "log",
                                                     at = seq(from=round(min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ), digits=2), to=round(max( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ), digits=2), by=0.5),
                                                     cen = min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ))
    }
  
  # if( length( CROSSPRED_NUTS_POSTMETA[[1]][[iREG]]$predvar ) != length( VALUES_CROSSPRED ) ){ stop("ERROR: Invalid Cross-Prediction !!!"); }
  rm(PM25Fire_CROSSPRED, ONEBASIS_FIRE);
  
  # Regional Lag-Response with Centering at Minimum level 
  if( length(VALUES_CROSSPRED_LAG) > 0 ){
    for( iPERC in 1:length(VALUES_CROSSPRED_LAG) ){
      CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]] = crosspred( ONEBASIS_LAGS,
                                                                  coef = BLUP_META[[1+iPERC]][[iREG]]$blup,
                                                                  vcov = BLUP_META[[1+iPERC]][[iREG]]$vcov,
                                                                  model.link = "log",
                                                                  at = seq( MIN_LAG, MAX_LAG, 0.1 ),
                                                                  cen = min( DATALIST_DATA_CALI[[iNUTS]]$PM25Fire, na.rm = TRUE ));
      if( length( CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$predvar ) != length( seq( MIN_LAG, MAX_LAG, 0.1 ) ) ){ stop("ERROR: Invalid Cross-Prediction !!!"); }
    }
    rm(iPERC);
  }
  rm(ONEBASIS_LAGS);
}
rm(iREG)


################################################################################
### Calculation of the European and National Cumulative Exposure-Response    ###
###                 for All Regions with Mortality Data                      ###
################################################################################


print("");
print("= Calculation of the European and National Cumulative Exposure-Response for All Regions with Mortality Data =");
print("");


# Meta-Predicted Reduced Coefficients and Covariance Matrix of the European and National Cumulative Exposure-Response and Lag-Response
BLUP_PRED_EU_POSTMETA = BLUP_PRED_COUNTRY_POSTMETA = vector( "list", 1 + length(VALUES_CROSSPRED_LAG) );
names(BLUP_PRED_EU_POSTMETA) = names(BLUP_PRED_COUNTRY_POSTMETA) = c( "CUMU", "LAG_1microgram", "LAG_10microgram"  );

for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  BLUP_PRED_EU_POSTMETA[[1+iPERC]] = vector( "list",     1     ); 
  names(BLUP_PRED_EU_POSTMETA[[1+iPERC]]) = "Europe";
  BLUP_PRED_COUNTRY_POSTMETA[[1+iPERC]] = vector("list", nCOUNTRIES); 
  names(BLUP_PRED_COUNTRY_POSTMETA[[1+iPERC]]) = vCOUNTRIES_CODE;
}
rm(iPERC);

# Objects for predictions of European and National Cumulative Exposure-Response and Lag-Response
CROSSPRED_EU_POSTMETA = CROSSPRED_COUNTRY_POSTMETA = vector( "list", 1 + length(VALUES_CROSSPRED_LAG) );
names(CROSSPRED_EU_POSTMETA ) = names(CROSSPRED_COUNTRY_POSTMETA) = c( "CUMU", "LAG_1microgram", "LAG_10microgram" );

for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  CROSSPRED_EU_POSTMETA [[1+iPERC]] = vector( "list",     1     ); names(CROSSPRED_EU_POSTMETA [[1+iPERC]]) = "Europe";
  CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]] = vector( "list", nCOUNTRIES ); names(CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]]) = vCOUNTRIES_CODE;
}
rm(iPERC);

# European and National Minimum Mortality Temperature
# MMT_DATA_TOT_POSTMETA = array( NA, dim =     1    , dimnames = list("Europe") );
# MMT_DATA_COU_POSTMETA = array( NA, dim = nCOUNTRIES, dimnames = list(vCOUNTRIES_CODE) );

for( iCOU in 0:nCOUNTRIES ){
if( iCOU == 0 ){ print( paste0( "  Europe" ) ); }
else           { print( paste0( "  Country ", iCOU, " / ", nCOUNTRIES, ": ", vCOUNTRIES_NAME[iCOU], " (", vCOUNTRIES_CODE[iCOU], ")" ) ); }

  # European and National Meta-Predictors (see FORMULA_META):
  #   MP_FIRE_ANN: Annual Fire-related PM2.5
  #   MP_FIRE_SUM: Summer Fire-related PM2.5
  #   MP_FIRE_P01: Fire-related PM2.5 Percentile 1st
  #   MP_FIRE_P99: Fire-related PM2.5 Percentile 99th
  #   MP_FIRE_IQR: Fire-related PM2.5 Inter-Quartile Range
  if( iCOU == 0 ){
    NEW_DATA = data.frame( #MP_FIRE_ANN = mean( MP_FIRE_ANN ),
                           MP_FIRE_SUM = mean( MP_FIRE_SUM ),
                           # MP_FIRE_P01 = mean( MP_FIRE_P01 ),
                           # MP_FIRE_P99 = mean( MP_FIRE_P99 ),
                           MP_FIRE_IQR = mean( MP_FIRE_IQR ) );
    
  }else{
    NEW_DATA = data.frame( #MP_FIRE_ANN = mean( MP_FIRE_ANN[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ),
                           MP_FIRE_SUM = mean( MP_FIRE_SUM[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ),
                           # MP_FIRE_P01 = mean( MP_FIRE_P01[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ),
                           # MP_FIRE_P99 = mean( MP_FIRE_P99[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ),
                           MP_FIRE_IQR = mean( MP_FIRE_IQR[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ) );
    }

# Meta-Predicted Reduced Coefficients and Covariance Matrix of the European and National Cumulative Exposure-Response and Lag-Response
for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  if( iCOU == 0 ){ BLUP_PRED_EU_POSTMETA[[1+iPERC]][[  1 ]] = predict( MVAR_META[[1+iPERC]], NEW_DATA, vcov = TRUE, format = "list" ) } # ADD HERE -before vcov-, DATA WITH META-PREDICTORS if NEEDED (check JOAN code)
  else           { BLUP_PRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]] = predict( MVAR_META[[1+iPERC]], NEW_DATA, vcov = TRUE, format = "list" ) } # ADD HERE -before vcov-, DATA WITH META-PREDICTORS if NEEDED (check JOAN code)
}
rm(NEW_DATA, iPERC);
  
  # Vector of Average Multi-Location Temperatures for the Cross-Predictions
  # FIRE_CROSSPRED = sapply( DATALIST_DATA_CALI, function(x) quantile( x$PM25Fire, vPERC_CROSSPRED, na.rm = TRUE ) )
  # FIRE_CROSSPRED = sapply( DATALIST_DATA_CALI$PM25Fire)

  # Vector of Average Multi-Location Temperatures for the Cross-Predictions
  if( iCOU == 0 ){ FIRE_CROSSPRED = data$PM25Fire 
  }else           { datatable_fire = do.call(rbind,DATALIST_DATA_CALI[vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU]])
  FIRE_CROSSPRED = datatable_fire$PM25Fire
  } 
  
  # One-Basis of Fire-related PM2.5
  if( FUNC_FIRE == "lin" ){
    ONEBASIS_FIRE = onebasis( FIRE_CROSSPRED,
                              fun = FUNC_FIRE)
  }else if( FUNC_FIRE == "ns" ){
    ONEBASIS_FIRE = onebasis( FIRE_CROSSPRED,
                              knots = FIRE_CROSSPRED[ paste0( 100 * PERC_FIRE, ".0%" ) ],
                              fun = FUNC_FIRE,
                              Bound = range( FIRE_CROSSPRED, na.rm = TRUE ) );
  }else if( FUNC_FIRE == "bs" ){
    ONEBASIS_FIRE = onebasis( FIRE_CROSSPRED,
                              knots = FIRE_CROSSPRED[ paste0( 100 * PERC_FIRE, ".0%" ) ],
                              fun = FUNC_FIRE,
                              degree = DEGR_FIRE,
                              Bound = range( FIRE_CROSSPRED, na.rm = TRUE ) );
  }else{
    stop("ERROR: Invalid FUNC_FIRE !!!");
  }
  
  # One-Basis of Lags
  ONEBASIS_LAGS = do.call( onebasis, c( list( x = seq( MIN_LAG, MAX_LAG, 1 ) ), CROSSBASIS_FIRE_ARGLAG ) );
  
  # European and National Cumulative Exposure-Response Centering
  # if( iCOU == 0 ){ MORT_CROSSPRED = ONEBASIS_FIRE %*% BLUP_PRED_EU_POSTMETA[[1]][[  1 ]]$fit; }
  # else           { MORT_CROSSPRED = ONEBASIS_FIRE %*% BLUP_PRED_COUNTRY_POSTMETA[[1]][[iCOU]]$fit; }
  
 
  # European and National Cumulative Exposure-Response and Lag-Response with Centering at Minimum
  EU_PM25_FIRE_MiN<-min(data$PM25Fire)
  
  if( iCOU == 0 ){
    CROSSPRED_EU_POSTMETA [[1]][[1]] = crosspred( ONEBASIS_FIRE,
                                                       coef = BLUP_PRED_EU_POSTMETA[[1]][[1]]$fit,
                                                       vcov = BLUP_PRED_EU_POSTMETA[[1]][[1]]$vcov,
                                                       model.link = "log",
                                                       at = seq(from=round(min( data$PM25Fire, na.rm = TRUE ), digits=2), to=round(max( data$PM25Fire, na.rm = TRUE ), digits=2), by=1),
                                                       cen = EU_PM25_FIRE_MiN );
    # if( length( CROSSPRED_EU_POSTMETA [[1]][[1]]$predvar ) != length( VALUES_CROSSPRED ) ){ stop("ERROR: Invalid Cross-Prediction !!!"); }
    
    if( length(VALUES_CROSSPRED_LAG) > 0 ){
      for( iPERC in 1:length(VALUES_CROSSPRED_LAG) ){
        CROSSPRED_EU_POSTMETA [[1+iPERC]][[1]] = crosspred( ONEBASIS_LAGS,
                                                                 coef = BLUP_PRED_EU_POSTMETA[[1+iPERC]][[1]]$fit,
                                                                 vcov = BLUP_PRED_EU_POSTMETA[[1+iPERC]][[1]]$vcov,
                                                                 model.link = "log",
                                                                 at = seq( MIN_LAG, MAX_LAG, 0.1 ),
                                                                 cen = EU_PM25_FIRE_MiN );
        if( length( CROSSPRED_EU_POSTMETA [[1+iPERC]][[1]]$predvar ) != length( seq( MIN_LAG, MAX_LAG, 0.1 ) ) ){ stop("ERROR: Invalid Cross-Prediction !!!"); }
      }
      rm(iPERC);
    }
  }else{
    COUNTRY_PM25_FIRE_MiN<-data %>% group_by(CNTR_CODE) %>% summarize(MIN_PM25=min(PM25Fire)) 
      
    CROSSPRED_COUNTRY_POSTMETA[[1]][[iCOU]] = crosspred( ONEBASIS_FIRE,
                                                          coef = BLUP_PRED_COUNTRY_POSTMETA[[1]][[iCOU]]$fit,
                                                          vcov = BLUP_PRED_COUNTRY_POSTMETA[[1]][[iCOU]]$vcov,
                                                          model.link = "log",
                                                          at = seq(from=round(min( datatable_fire$PM25Fire, na.rm = TRUE ), digits=2), to=round(max( datatable_fire$PM25Fire, na.rm = TRUE ), digits=2), by=1),
                                                          cen = COUNTRY_PM25_FIRE_MiN$MIN_PM25[iCOU] );
    # if( length( CROSSPRED_COUNTRY_POSTMETA[[1]][[iCOU]]$predvar ) != length( VALUES_CROSSPRED ) ){ stop("ERROR: Invalid Cross-Prediction !!!"); }
    
    if( length(VALUES_CROSSPRED_LAG) > 0 ){
      for( iPERC in 1:length(VALUES_CROSSPRED_LAG) ){
        CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]] = crosspred( ONEBASIS_LAGS,
                                                                    coef = BLUP_PRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$fit,
                                                                    vcov = BLUP_PRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$vcov,
                                                                    model.link = "log",
                                                                    at = seq( MIN_LAG, MAX_LAG, 0.1 ),
                                                                    cen = COUNTRY_PM25_FIRE_MiN$MIN_PM25[iCOU] );
        if( length( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar ) != length( seq( MIN_LAG, MAX_LAG, 0.1 ) ) ){ stop("ERROR: Invalid Cross-Prediction !!!"); }
      }
      rm(iPERC);
    }
  }
  rm(FIRE_CROSSPRED, ONEBASIS_FIRE,ONEBASIS_LAGS);
  
}

#rm(MP_TEMP_ANN,MP_TEMP_WIN,MP_TEMP_SUM,MP_TEMP_P01,MP_TEMP_P99,MP_TEMP_IQR, iCOU);



################################################################################
### Calculation of the Attributable Mortality for the Period of the Predictions of All Regions with Mortality Data
################################################################################


# definition different period for calibration and prediction
for( iREG in 1:nNUTS ){
  DATALIST_DATA_PRED[[iREG]] = DATALIST_DATA_PRED[[iREG]][ which( DATALIST_DATA_PRED[[iREG]]$date >= DATES_NUTS[1] ), ] 
  # DATALIST_DATA_CALI[[iREG]] = DATALIST_DATA_CALI[[iREG]][ which( DATALIST_DATA_CALI[[iREG]]$date <= (DATES_NUTS[2] + max( MAX_LAG, 0 ))),] 
}
rm(iREG);

# Vector of Time Periods for All Regions with Mortality Data: Whole Period
vPER_DATA = c( "Whole Period" );
nPER_DATA = length(vPER_DATA);

# Number of Simulations
nSIM = 1000;
if( nSIM < 1000 ){ print( paste0( "  WARNING: Only ", nSIM, " Simulations Are Used for the Attributable Numbers" ) ); }

# 95% Confidence Interval
vCI95 = c(0.025,0.975);

# Value for Seeding Random Variate Generators
SET_SEED_VALUE = 6435634;



print("");
print("= Calculation of the Attributable Mortality for the Period of the Predictions of All Regions with Mortality Data =");
print("");

# Values and Simulations of the Regional Attributable Number (in annual deaths)
ATTNUM_NUTS_VAL = array( NA, dim = c(nNUTS, nPER_DATA), dimnames = list( vNUTS_CODE, vPER_DATA) );
ATTNUM_NUTS_SIM = array( NA, dim = c(nNUTS, nPER_DATA, nSIM ), dimnames = list( vNUTS_CODE, vPER_DATA, 1:nSIM ) );

# European, National and Regional Attributable Fraction (in %) and Attributable Number (in annual deaths)
ATTFRA_EU_TOT = ATTNUM_EU_TOT = array( NA, dim = c(     1    , nPER_DATA, 3 ), dimnames = list(    "Europe"   , vPER_DATA, c( "att_val", "att_low", "att_upp" ) ) );
ATTFRA_COU = ATTNUM_COU = array( NA, dim = c( nCOUNTRIES, nPER_DATA, 3 ), dimnames = list( vCOUNTRIES_CODE, vPER_DATA, c( "att_val", "att_low", "att_upp" ) ) );
ATTFRA_NUTS = ATTNUM_NUTS = array( NA, dim = c(nNUTS, nPER_DATA, 3 ), dimnames = list( vNUTS_CODE, vPER_DATA,  c( "att_val", "att_low", "att_upp" ) ) );

# Adjusted Factor (in annual deaths) for Missing Values in the Calculation of the European, National and Regional Attributable Number (see Function "attrdl.R" and Gasparrini and Leone 2014)
ADJUSTED_FACTOR_ATTNUM_TOT = array( NA, dim = c(     1    , nPER_DATA ), dimnames = list(    "Europe"   , vPER_DATA ) );
ADJUSTED_FACTOR_ATTNUM_COU = array( NA, dim = c( nCOUNTRIES, nPER_DATA ), dimnames = list( vCOUNTRIES_CODE, vPER_DATA ) );
ADJUSTED_FACTOR_ATTNUM_NUTS = array( NA, dim = c(nNUTS, nPER_DATA ), dimnames = list( vNUTS_CODE, vPER_DATA ) );

# Thresholds of the Regional Fire-Related PM2.5 Ranges
P025_REG = sapply( DATALIST_DATA_PRED, function(x) quantile( x$PM25Fire, 0.025, na.rm = TRUE ) ); names(P025_REG) = vNUTS_CODE;
P975_REG = sapply( DATALIST_DATA_PRED, function(x) quantile( x$PM25Fire, 0.975, na.rm = TRUE ) ); names(P975_REG) = vNUTS_CODE;

for( iREG in 1:nNUTS ){
  print( paste0( "  Region ", iREG, " / ",nNUTS, ": ", vNUTS_NAME[iREG], " (", vNUTS_CODE[iREG], ")" ) );
  
  # One-Basis of Fire-Related PM2.5
  if( FUNC_FIRE == "lin" ){
    ONEBASIS_FIRE = onebasis(DATALIST_DATA_PRED[[iREG]]$PM25Fire,
                              fun = FUNC_FIRE)
  }else if( FUNC_TEMP == "ns" ){
    ONEBASIS_FIRE = onebasis(DATALIST_DATA_PRED[[iREG]]$PM25Fire,
                              knots = quantile( DATALIST_DATA_PRED[[iREG]]$PM25Fire, PERC_TEMP, na.rm = TRUE ),
                              fun = FUNC_FIRE,
                              Bound = range( DATALIST_DATA_PRED[[iREG]]$PM25Fire, na.rm = TRUE ) );
  }else if( FUNC_TEMP == "bs" ){
    ONEBASIS_FIRE = onebasis( DATALIST_DATA_PRED[[iREG]]$PM25Fire,
                              knots = quantile( DATALIST_DATA_PRED[[iREG]]$PM25Fire, PERC_TEMP, na.rm = TRUE ),
                              fun = FUNC_FIRE,
                              degree = DEGR_FIRE,
                              Bound = range( DATALIST_DATA_PRED[[iREG]]$PM25Fire, na.rm = TRUE ) );
  }else{
    stop("ERROR: Invalid FUNC_TEMP !!!");
  }
  
 
  # Matrix of Deaths with Lags (Dimensions: Time x Lags)
  LAGGED_MORT_MATRIX = tsModel::Lag( DATALIST_DATA_PRED[[iREG]]$mort, -seq( MIN_LAG, MAX_LAG ) ); 
  ### NOTA BENE: this is a forward approach, for backward approach change the sign for the lag sequence 
  
  # Average of Deaths Across Lags (Dimensions: Time x 1)
  LAGGED_MORT_VECTOR = rowMeans( LAGGED_MORT_MATRIX, na.rm = FALSE );
  
  # Daily Time Series of the Regional Attributable Number (Dimensions: Time x 1)
  ATTNUM_TS_REF = ( 1 - exp( -ONEBASIS_FIRE %*% BLUP_META[[1]][[iREG]]$blup ) ) * LAGGED_MORT_VECTOR;
  if( any( 1 - exp( -ONEBASIS_FIRE %*% BLUP_META[[1]][[iREG]]$blup ) < 0 ) ){
    print( paste0( "    WARNING: ", round( 100 * mean( 1 - exp( -ONEBASIS_FIRE %*% BLUP_META[[1]][[iREG]]$blup ) < 0 ), digits = 2 ), "% of Predicted Attributable Fractions Are Negative" ) );
    }
  
  # Perturbed Best Linear Unbiased Predictions (Dimensions: Coefficients x Simulations)
  set.seed(SET_SEED_VALUE);
  COEF_SIM = t( MASS::mvrnorm( nSIM, BLUP_META[[1]][[iREG]]$blup, BLUP_META[[1]][[iREG]]$vcov ) );
  
  # Matrix of Perturbed Daily Time Series of the Regional Attributable Number (Dimensions: Time x Simulations)
  ATTNUM_TS_SIM = ( 1 - exp( -ONEBASIS_FIRE %*% COEF_SIM ) ) * LAGGED_MORT_VECTOR;
  rm(COEF_SIM);
  
  for( iPER in 1:length(vPER_DATA) ){
    
    # Data Selection of the Time Period
    if( vPER_DATA[iPER] == "Whole Period" ){ vTIM = 1:length( DATALIST_DATA_PRED[[iREG]]$date ); 
    } else { stop("ERROR: Invalid Time Period !!!"); }
    if( length(vTIM) <= 0 ){ stop("ERROR: Invalid Data Selection of the Time Period !!!"); }
    
    # Adjusted Factor (in deaths) for Missing Values in the Calculation of the Regional Attributable Fraction and Number (see Function "attrdl.R" and Gasparrini and Leone 2014):
    #   ATTNUM_TS_REF and ADJUSTED_FACTOR_ATTFRA Are Calculated by Averaging Deaths Across Lags with "na.rm = FALSE" (see the Calculation of LAGGED_MORT_VECTOR Above)
    #   Instead, ADJUSTED_FACTOR_ATTNUM_NUTS Is Calculated by Averaging Deaths Across Lags with "na.rm = TRUE" -> so NA are removed
    #   Formulas: ATTFRA =            100             * sum( ATTNUM_TS_REF[period] ) / ADJUSTED_FACTOR_ATTFRA
    #             ATTNUM = ADJUSTED_FACTOR_ATTNUM_NUTS * sum( ATTNUM_TS_REF[period] ) / ADJUSTED_FACTOR_ATTFRA
    # DENOMINATORS:
    
    ADJUSTED_FACTOR_ATTFRA = sum( LAGGED_MORT_VECTOR[ vTIM] , na.rm = TRUE );  ## sum of all current and future event, excluding the 7 last days
    
    ADJUSTED_FACTOR_ATTNUM_NUTS[iREG,iPER] = sum( rowMeans( LAGGED_MORT_MATRIX[ vTIM, , drop=FALSE ], na.rm = TRUE ), na.rm = TRUE ); ## sum of all current and future event, excluding the future events that are not observed

    
    if( ADJUSTED_FACTOR_ATTFRA > 0 ){  ## don't really understand this condition, are there regions with 0 weekly death?
      
    vRNG_THRES =  1:length(vTIM); 
        
        if( length(vRNG_THRES) > 0 ){  ## don't really understand this condition, in which case there are 0 temporal obs?
          
          # Values and Simulations of the Regional Attributable Fraction (in %)
          ATTFRA_NUTS_VAL = 100 *     sum( ATTNUM_TS_REF[ vTIM ], na.rm = TRUE ) / ADJUSTED_FACTOR_ATTFRA;
          ATTFRA_NUTS_SIM = 100 * colSums( ATTNUM_TS_SIM[ vTIM, , drop=FALSE ], na.rm = TRUE ) / ADJUSTED_FACTOR_ATTFRA;
          if(      !is.finite( ATTFRA_NUTS_VAL )   ){ stop("ERROR: Invalid ATTFRA_NUTS_VAL !!!"); }
          if( any( !is.finite( ATTFRA_NUTS_SIM ) ) ){ stop("ERROR: Invalid ATTFRA_NUTS_SIM !!!"); }
          
          # Values and 95% Confidence Intervals of the Regional Attributable Fraction (in %)
          ATTFRA_NUTS[iREG,iPER, 1 ] =           ATTFRA_NUTS_VAL         ;
          ATTFRA_NUTS[iREG,iPER,2:3] = quantile( ATTFRA_NUTS_SIM, vCI95 );
          rm(ATTFRA_NUTS_VAL,ATTFRA_NUTS_SIM);
          
          # Values and Simulations of the Regional Attributable Number (in deaths)
          ATTNUM_NUTS_VAL[iREG,iPER] = ADJUSTED_FACTOR_ATTNUM_NUTS[iREG] *     sum( ATTNUM_TS_REF[ vTIM[vRNG_THRES]], na.rm = TRUE ) / ADJUSTED_FACTOR_ATTFRA;
          ATTNUM_NUTS_SIM[iREG,iPER,] = ADJUSTED_FACTOR_ATTNUM_NUTS[iREG] * colSums( ATTNUM_TS_SIM[ vTIM[vRNG_THRES], , drop=FALSE ], na.rm = TRUE ) / ADJUSTED_FACTOR_ATTFRA;
          if(      !is.finite( ATTNUM_NUTS_VAL[iREG,iPER] )   ){ stop("ERROR: Invalid ATTNUM_NUTS_VAL !!!"); }
          if( any( !is.finite( ATTNUM_NUTS_SIM[iREG,iPER,] ) ) ){ stop("ERROR: Invalid ATTNUM_NUTS_SIM !!!"); }
          
          # Values and 95% Confidence Intervals of the Regional Attributable Number (in deaths)
          ATTNUM_NUTS[iREG,iPER, 1 ] =           ATTNUM_NUTS_VAL[iREG,iPER ]         ;
          ATTNUM_NUTS[iREG,iPER,2:3] = quantile( ATTNUM_NUTS_SIM[iREG,iPER,], vCI95 );
          
          # Values, Simulations and 95% Confidence Intervals of the Regional Attributable Number (in annual deaths)
          ATTNUM_NUTS_VAL[iREG,iPER ] = 365.25 * ATTNUM_NUTS_VAL[iREG,iPER] / length(vTIM);
          ATTNUM_NUTS_SIM[iREG,iPER,] = 365.25 * ATTNUM_NUTS_SIM[iREG,iPER,] / length(vTIM);
          ATTNUM_NUTS    [iREG,iPER,] = 365.25 * ATTNUM_NUTS    [iREG,iPER,] / length(vTIM);
          
        } else {
          ATTFRA_NUTS    [iREG,iPER,] = 0;
          ATTNUM_NUTS_VAL[iREG,iPER ] = 0;
          ATTNUM_NUTS_SIM[iREG,iPER,] = 0;
          ATTNUM_NUTS    [iREG,iPER,] = 0;
        }
        rm(vRNG_THRES);

      } else {
      ATTFRA_NUTS    [iREG,iPER,,] = 0;
      ATTNUM_NUTS_VAL[iREG,iPER, ] = 0;
      ATTNUM_NUTS_SIM[iREG,iPER,,] = 0;
      ATTNUM_NUTS    [iREG,iPER,,] = 0;
    }
    
    # Adjusted Factor (in annual deaths) for Missing Values in the Calculation of the Regional Attributable Number (see Function "attrdl.R" and Gasparrini and Leone 2014):
    ADJUSTED_FACTOR_ATTNUM_NUTS[iREG,iPER] = 365.25 * ADJUSTED_FACTOR_ATTNUM_NUTS[iREG,iPER] / length(vTIM); # 
    
    rm(vTIM, ADJUSTED_FACTOR_ATTFRA);
    
  }
  rm(LAGGED_MORT_MATRIX,LAGGED_MORT_VECTOR, ATTNUM_TS_REF,ATTNUM_TS_SIM, iPER);
  
}


rm(P025_REG,P975_REG, iREG);

for( iCOU in 1:nCOUNTRIES ){
  print( paste0( "  Country ", iCOU, " / ", nCOUNTRIES, ": ", vCOUNTRIES_NAME[iCOU], " (", vCOUNTRIES_CODE[iCOU], ")" ) );
  
  # Vector of Regions of the Country
  iREG = which( vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] );
  if( length(iREG) == 0 ){ stop("ERROR: Invalid Vector of Regions of the Country !!!"); }
  
  # Values and 95% Confidence Intervals of the National Attributable Number (in annual deaths)
  ATTNUM_COU[iCOU, , 1] =        sum( ATTNUM_NUTS_VAL[iREG, , drop=FALSE] ) ;
  ATTNUM_COU[iCOU, , 2] = quantile(apply(ATTNUM_NUTS_SIM[iREG,,, drop=FALSE], c(2,3), sum), vCI95[1] );
  ATTNUM_COU[iCOU, , 3] = quantile(apply(ATTNUM_NUTS_SIM[iREG,,, drop=FALSE], c(2,3), sum), vCI95[2] );
  
  # Adjusted Factor (in annual deaths) for Missing Values in the Calculation of the National Attributable Number (see Function "attrdl.R" and Gasparrini and Leone 2014):
  ADJUSTED_FACTOR_ATTNUM_COU[iCOU,] = colSums( ADJUSTED_FACTOR_ATTNUM_NUTS[iREG,, drop=FALSE] );
  
  for( iPER in 1:length(vPER_DATA) ){
    
    # Values and 95% Confidence Intervals of the National Attributable Fraction (in %)
    if( ADJUSTED_FACTOR_ATTNUM_COU[iCOU,iPER] > 0 ){ ATTFRA_COU[iCOU,iPER,] = 100 * ATTNUM_COU[iCOU,iPER,] / ADJUSTED_FACTOR_ATTNUM_COU[iCOU,iPER]; 
    }else { ATTFRA_COU[iCOU,iPER,] = 0; }
    
  }
  rm(iREG, iPER);
  
}

rm(iCOU);

print( "  Europe" );

# Values and 95% Confidence Intervals of the European Attributable Number (in annual deaths)
ATTNUM_EU_TOT[1,,1] = sum( ATTNUM_NUTS_VAL)                              ;
ATTNUM_EU_TOT[1,,2] = quantile(apply(ATTNUM_NUTS_SIM, c(2,3), sum), vCI95[1] );
ATTNUM_EU_TOT[1,,3] = quantile(apply(ATTNUM_NUTS_SIM, c(2,3), sum), vCI95[2] );

# Adjusted Factor (in annual deaths) for Missing Values in the Calculation of the European Attributable Number (see Function "attrdl.R" and Gasparrini and Leone 2014):
ADJUSTED_FACTOR_ATTNUM_TOT[1,] = colSums( ADJUSTED_FACTOR_ATTNUM_NUTS );

for( iPER in 1:length(vPER_DATA) ){
  
  # Values and 95% Confidence Intervals of the European Attributable Fraction (in %)
  if( ADJUSTED_FACTOR_ATTNUM_TOT[1,iPER] > 0 ){ ATTFRA_EU_TOT[1,iPER,] = 100 * ATTNUM_EU_TOT[1,iPER,] / ADJUSTED_FACTOR_ATTNUM_TOT[1,iPER]; }
  else                                        { ATTFRA_EU_TOT[1,iPER,] = 0; }
  
}
rm(iPER);

rm(ATTNUM_NUTS_VAL,ATTNUM_NUTS_SIM, ADJUSTED_FACTOR_ATTNUM_TOT,ADJUSTED_FACTOR_ATTNUM_COU,ADJUSTED_FACTOR_ATTNUM_NUTS);


## SAVING RESULTS FOR ATTRIBUTABLE MORTALITY ##

# European, National and Regional Attributable Number (in annual deaths)
TABLE_MORT = data.frame( name = c( "Europe", vCOUNTRIES_NAME, vNUTS_NAME ),
                           code = c( "Europe", vCOUNTRIES_CODE, vNUTS_CODE ),
                           attr = sprintf( "%.0f (%.0f, %.0f)",
                                           round( c( ATTNUM_EU_TOT[ 1, "Whole Period", "att_val" ], ATTNUM_COU[ , "Whole Period", "att_val" ], ATTNUM_NUTS[ , "Whole Period", "att_val" ] ), digits = 0 ),
                                           round( c( ATTNUM_EU_TOT[ 1, "Whole Period", "att_low" ], ATTNUM_COU[ , "Whole Period", "att_low" ], ATTNUM_NUTS[ , "Whole Period", "att_low" ] ), digits = 0 ),
                                           round( c( ATTNUM_EU_TOT[ 1, "Whole Period", "att_upp" ], ATTNUM_COU[ , "Whole Period", "att_upp" ], ATTNUM_NUTS[ , "Whole Period", "att_upp" ] ), digits = 0 ) ) );
TABLE_MORT = rownames_to_column( TABLE_MORT, var = "NUTS" );
YEARS_NUTS = sapply( DATALIST_DATA_PRED, function(x) summary( year(x$date) ));
YEARS_START_NUTS = data.frame( matrix( YEARS_NUTS[1,], nrow = ncol(YEARS_NUTS), ncol = 1, dimnames = list( colnames(YEARS_NUTS), "year_star") ) )
YEARS_END_NUTS = data.frame( matrix( YEARS_NUTS[6,], nrow = ncol(YEARS_NUTS), ncol = 1, dimnames = list( colnames(YEARS_NUTS), "year_end") ) )
YEARS_NUTS = cbind(YEARS_START_NUTS, YEARS_END_NUTS)
YEARS_NUTS$number_years = YEARS_NUTS$year_end - YEARS_NUTS$year_star
YEARS_COU = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 2, dimnames = list( vCOUNTRIES_CODE, c("year_star", "year_end" )) ) );
for( iCOU in 1:nCOUNTRIES ){ YEARS_COU[iCOU,"year_star"] = median( YEARS_START_NUTS[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU], ] ); }; rm(iCOU);
for( iCOU in 1:nCOUNTRIES ){ YEARS_COU[iCOU,"year_end"] = median( YEARS_END_NUTS[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU], ] ); }; rm(iCOU);
YEARS_COU$number_years=YEARS_COU$year_end-YEARS_COU$year_star
YEARS_DATA<-rbind(NA, YEARS_COU, YEARS_NUTS)
YEARS_DATA$code<-rownames(YEARS_DATA)
YEARS_DATA$code[1]<-"Europe"
TABLE_MORT<-cbind(TABLE_MORT, YEARS_DATA)
TABLE_MORT$attr_per_year = sprintf( "%.1f (%.1f, %.1f)",
                                    c(NA, ATTNUM_COU[ , "Whole Period", "att_val" ], ATTNUM_NUTS[ , "Whole Period", "att_val" ] )/TABLE_MORT$number_years,
                                    c( NA, ATTNUM_COU[ , "Whole Period", "att_low" ], ATTNUM_NUTS[ , "Whole Period", "att_low" ])/TABLE_MORT$number_years,
                                    c( NA, ATTNUM_COU[ , "Whole Period", "att_upp" ], ATTNUM_NUTS[ , "Whole Period", "att_upp" ])/TABLE_MORT$number_years) 
TABLE_MORT$attr_per_year[1] = sprintf( "%.1f (%.1f, %.1f)",
                                       sum(ATTNUM_NUTS[ , "Whole Period", "att_val" ]/YEARS_NUTS$number_years),
                                       sum( ATTNUM_NUTS[ , "Whole Period", "att_low" ]/YEARS_NUTS$number_years),
                                       sum(ATTNUM_NUTS[ , "Whole Period", "att_upp" ]/YEARS_NUTS$number_years)) 
write_xlsx( TABLE_MORT, paste0( FOLD_RESULTS, "Health_impact_assessment", Model, Exposure, Outcome, "/TABLE_WHOLE_PERIOD_ATTNUM_1microgram.xlsx" ));
rm(TABLE_MORT);
  
# European, National and Regional Attributable Fraction (in %)
TABLE_MORT = data.frame( name = c( "Europe", vCOUNTRIES_NAME, vNUTS_NAME ),
                           code = c( "Europe", vCOUNTRIES_CODE, vNUTS_CODE ),
                           attr = sprintf( "%.2f (%.2f, %.2f)",
                                           round( c( ATTFRA_EU_TOT[ 1, "Whole Period", "att_val" ], ATTFRA_COU[ , "Whole Period", "att_val" ], ATTFRA_NUTS[ , "Whole Period", "att_val" ] ), digits = 2 ),
                                           round( c( ATTFRA_EU_TOT[ 1, "Whole Period", "att_low" ], ATTFRA_COU[ , "Whole Period", "att_low" ], ATTFRA_NUTS[ , "Whole Period", "att_low" ] ), digits = 2 ),
                                           round( c( ATTFRA_EU_TOT[ 1, "Whole Period", "att_upp" ], ATTFRA_COU[ , "Whole Period", "att_upp" ], ATTFRA_NUTS[ , "Whole Period", "att_upp" ] ), digits = 2 ) ) );
TABLE_MORT = rownames_to_column( TABLE_MORT, var = "NUTS" );
write_xlsx( TABLE_MORT, paste0( FOLD_RESULTS, "Health_impact_assessment", Model, Exposure, Outcome, "/TABLE_WHOLE_PERIOD_ATTFRA_1microgram.xlsx" ) );
rm(TABLE_MORT);
  






################################################################################
### Figure: Raw, Seasonal and Cross-Basis Mortality
################################################################################

print("");
print("= Figure: Raw, Seasonal and Cross-Basis Mortality =");
print("");

#foldout = paste0( FOLD_DATA_OUT, sSEX, "_", sAGE, "_", sCAU, "_iSEN.", iSEN, "/" );
#if( !file_test( "-d", foldout ) ){ dir.create( foldout, recursive = TRUE ); }


# if (length(PERC_FIRE)>1) {
# KNOTS<-paste0(PERC_FIRE[1],"_",PERC_FIRE[2])
# } else if (is.na(PERC_FIRE)){
#   KNOTS<-0
# } else {
#   KNOTS<-PERC_FIRE
# }

pdf( paste0( FOLD_DATA_OUT, FUNC_FIRE,"/knots_",KNOTS, Model, Exposure, Outcome, "/TIMESERIES_MORT_SEASON_CROSSBASIS.pdf" ), width = 15, height = 21 );
layout( matrix( seq(1*7), nrow = 7, byrow = TRUE ) );
par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
for( iREG in 1:nNUTS ){
  plot  ( DATALIST_DATA_CALI[[iREG]]$date, DATALIST_DATA_CALI[[iREG]]$mort          , col = rgb(0.00,0.00,0.00), lwd = 1, lty = 1, type = "l", ylim = range( c( DATALIST_DATA_CALI[[iREG]]$mort, DATALIST_DATA_CALI[[iREG]]$mort_pred_seas, DATALIST_DATA_CALI[[iREG]]$mort_pred_crbs ), na.rm = TRUE ), main = paste0( vNUTS_NAME[iREG], " (", vNUTS_CODE[iREG], ")" ), xlab = "Time (year)", ylab = "Mortality (deaths)" )
  lines ( DATALIST_DATA_CALI[[iREG]]$date, DATALIST_DATA_CALI[[iREG]]$mort_pred_seas, col = rgb(1.00,0.00,0.00), lwd = 2, lty = 1 )
  lines ( DATALIST_DATA_CALI[[iREG]]$date, DATALIST_DATA_CALI[[iREG]]$mort_pred_crbs, col = rgb(0.00,0.00,1.00), lwd = 1, lty = 1 )
  abline( h = 0, col = rgb(0.00,0.00,0.00), lwd = 1, lty = 1 )
  legend( "top", c( "Mortality", "Seasonality", "Cross-Basis" ), col = rgb(c(0.00,1.00,0.00),c(0.00,0.00,0.00),c(0.00,0.00,1.00)), lwd = 2, lty = 1, box.lty = 0, horiz = TRUE, bg = "transparent" );
}
rm(iREG)
dev.off()



#######################################################################################################
### Figure: Cumulative Regional Exposure-Response and Lag-Response Before and After the Meta-Analysis #
#######################################################################################################

print("");
print("= Figure: Cumulative Exposure-Response and Lag-Response Before and After the Meta-Analysis =");
print("");

# foldout = paste0( FOLD_DATA_OUT, sSEX, "_", sAGE, "_", sCAU, "_iSEN.", iSEN, "/" );
# if( !file_test( "-d", foldout ) ){ dir.create( foldout, recursive = TRUE ); }

# Regional Cumulative Exposure-Response Before the Meta-Analysis
pdf( paste0( FOLD_DATA_OUT,FUNC_FIRE,"/knots_",KNOTS, Model, Exposure, Outcome, "/PLOT_CUMU_EXP_RESP_", "NUTS_PREMETA_microgram.pdf" ), width = 15, height = 21 );
layout( matrix( seq(5*7), nrow = 7, byrow = TRUE ) );
par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
for( iREG in 1:nNUTS ){
  plot   (CROSSPRED_NUTS_PREMETA[[iREG]]$predvar, CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit, col = rgb(0.00,0.00,0.00), lwd = 4, lty = 1, type = "l", xlab = expression( paste( "Fire Related-PM2.5" ) ), ylab = "Relative Risk", main = paste0( vNUTS_NAME[iREG], " (", vNUTS_CODE[iREG], ")" ), ylim = c( min( 1, CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit ), max( CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit ) ), axes = T )
  polygon( c( CROSSPRED_NUTS_PREMETA[[iREG]]$predvar, rev( CROSSPRED_NUTS_PREMETA[[iREG]]$predvar ) ), c( CROSSPRED_NUTS_PREMETA[[iREG]]$allRRlow, rev( CROSSPRED_NUTS_PREMETA[[iREG]]$allRRhigh ) ), col = rgb(0.00,0.00,0.00,1/3), border = FALSE );
  lines  (CROSSPRED_NUTS_PREMETA[[iREG]]$predvar, CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit, col = rgb(0.00,0.00,0.00), lwd = 4, lty = 1 )
  abline( h = 1, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) )
}
rm(iREG)
dev.off()

# pdf( paste0( FOLD_DATA_OUT,FUNC_FIRE,"/knots_",KNOTS, Model, "/PLOT_CUMU_EXP_RESP_", "_NUTS_PREMETA_bis.pdf" ), width = 15, height = 21 );
# layout( matrix( seq(5*7), nrow = 7, byrow = TRUE ) );
# par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
# for( iREG in 1:nNUTS ){
#   plot   (CROSSPRED_NUTS_PREMETA[[iREG]], lwd = 4, lty = 1, type = "l", xlab = expression( paste( "Fire Related-PM2.5" ) ), ylab = "Relative Risk", main = paste0( vNUTS_NAME[iREG], " (", vNUTS_CODE[iREG], ")" ), ylim = c( min( 1, CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit ), max( CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit ) ), axes = T )
#   #polygon( c( CROSSPRED_NUTS_PREMETA[[iREG]]$predvar, rev( CROSSPRED_NUTS_PREMETA[[iREG]]$predvar ) ), c( CROSSPRED_NUTS_PREMETA[[iREG]]$allRRlow, rev( CROSSPRED_NUTS_PREMETA[[iREG]]$allRRhigh ) ), col = rgb(0.00,0.00,0.00,1/3), border = FALSE );
#   lines  (CROSSPRED_NUTS_PREMETA[[iREG]]$predvar, CROSSPRED_NUTS_PREMETA[[iREG]]$allRRfit, col = rgb(0.00,0.00,0.00), lwd = 4, lty = 1 )
#   abline( h = 1, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) )
# }
# rm(iREG)
# dev.off()

for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  vPERC = c( "CUMU_EXP_RESP", "LAG_1microgram", "LAG_10microgram") 
  
  if( iPERC == 0 ){ xlab_string = "Fire-Related PM2.5" }
  else            { xlab_string = "Lag (days)" }
  
  # Regional Cumulative Exposure-Response and Lag-Response After the Meta-Analysis
  pdf( paste0(FOLD_DATA_OUT, FUNC_FIRE,"/knots_",KNOTS, Model, Exposure, Outcome, "/PLOT_", vPERC[1+iPERC], "_", "NUTS_POST_META.pdf" ), width = 15, height = 21 );
  layout( matrix( seq(5*7), nrow = 7, byrow = TRUE ) );
  par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
  for( iREG in 1:nNUTS ){
    plot   (CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$predvar, CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$allRRfit, col = rgb(0.00,0.00,0.00    ), lwd = 4, lty = 1, type = "l", xlab = xlab_string, ylab = "Relative Risk", main = paste0(vNUTS_NAME[iREG], " (", vNUTS_CODE[iREG], ")" ), ylim = c( min( 1, CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$allRRfit ), max( CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$allRRfit ) ), axes = T )
    polygon(c( CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$predvar, rev( CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$predvar ) ), c( CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$allRRlow, rev( CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$allRRhigh ) ), col = rgb(0.00,0.00,0.00,1/3), border = FALSE )
    lines  (CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$predvar, CROSSPRED_NUTS_POSTMETA[[1+iPERC]][[iREG]]$allRRfit, col = rgb(0.00,0.00,0.00    ), lwd = 4, lty = 1 );
    abline( h = 1, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) )
    if( iPERC != 0 ){ abline( v = 0, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) ) }
  }
  rm(iREG)
  dev.off()
}  
  
###############################################################################################
# European and National Cumulative Exposure-Response and Lag-Response After the Meta-Analysis #
#                                        (with Meta-predictors)                               #
###############################################################################################

for( iPERC in 0:length(VALUES_CROSSPRED_LAG) ){
  vPERC = c( "CUMU_EXP_RESP",  "LAG_1microgram", "LAG_10microgram" )
  
  if( iPERC == 0 ){ xlab_string = "Fire-related PM2.5" }
  else            { xlab_string = "Lag (days)"; }
  
  # National Cumulative Exposure-Response and Lag-Response After the Meta-Analysis
  pdf( paste0(FOLD_DATA_OUT,FUNC_FIRE,"/knots_",KNOTS, Model, Exposure, Outcome, "/PLOT_", vPERC[1+iPERC], "_COUNTRY_POSTMETA.pdf" ), width = 15, height = 21 );
  layout( matrix( seq(5*7), nrow = 7, byrow = TRUE ) );
  par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
  for( iCOU in 1:nCOUNTRIES ){
    plot   (    CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar,  CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allRRfit, col = rgb(0.00,0.00,0.00    ), lwd = 4, lty = 1, type = "l", xlab = xlab_string, ylab = "Relative Risk", main = paste0( vCOUNTRIES_NAME[iCOU], " (", vCOUNTRIES_CODE[iCOU], ")" ), ylim = c( min( 1, CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allRRfit ), max( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allRRfit ) ), axes = T );
    polygon( c( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar, rev( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar ) ), c( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allRRlow, rev( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allRRhigh ) ), col = rgb(0.00,0.00,0.00,1/3), border = FALSE );
    lines  (    CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar,  CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allRRfit, col = rgb(0.00,0.00,0.00    ), lwd = 4, lty = 1 );
    abline( h = 1, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) );
    if( iPERC != 0 ){ abline( v = 0, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) ); }
  }
  rm(iCOU)
  dev.off()


  # European Cumulative Exposure-Response and Lag-Response After the Meta-Analysis
  pdf( paste0(FOLD_DATA_OUT, FUNC_FIRE,"/knots_",KNOTS, Model, Exposure, Outcome, "/PLOT_", vPERC[1+iPERC], "_EU_POSTMETA.pdf" ), width = 4, height = 4 );
  layout( matrix( seq(1*1), nrow = 1, byrow = TRUE ) );
  par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
  plot   (    CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$predvar, CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$allRRfit, col = rgb(0.00,0.00,0.00), lwd = 4, lty = 1, type = "l", xlab = xlab_string, ylab = "Relative Risk", main = "Europe", ylim = c( min( 1, CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$allRRfit ), max( CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$allRRfit ) ), axes = T );
  polygon( c( CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$predvar, rev( CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$predvar ) ), c( CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$allRRlow, rev( CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$allRRhigh ) ), col = rgb(0.00,0.00,0.00,1/3), border = FALSE );
  lines  (    CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$predvar, CROSSPRED_EU_POSTMETA[[1+iPERC]][[1]]$allRRfit, col = rgb(0.00,0.00,0.00), lwd = 4, lty = 1 );
  abline( h = 1, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) );
  if( iPERC != 0 ){ abline( v = 0, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) ); }
  dev.off();

}


#################################################################
# layout( matrix( seq(5*7), nrow = 7, byrow = TRUE ) );
# par( mex = 0.8, mgp = c(2.5,1,0), las = 0 );
# for( iCOU in 1:nCOUNTRIES ){
#   plot   (    CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar,  CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allfit, col = rgb(0.00,0.00,0.00    ), lwd = 4, lty = 1, type = "l", xlab = xlab_string, ylab = "Relative Risk", main = paste0( vCOUNTRIES_NAME[iCOU], " (", vCOUNTRIES_CODE[iCOU], ")" ), ylim = c( min( 1, CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allfit ), max( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allfit ) ), axes = T );
#   polygon( c( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar, rev( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar ) ), c( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$alllow, rev( CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allhigh ) ), col = rgb(0.00,0.00,0.00,1/3), border = FALSE );
#   lines  (    CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$predvar,  CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]]$allfit, col = rgb(0.00,0.00,0.00    ), lwd = 4, lty = 1 );
#   abline( h = 1, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) );
#   if( iPERC != 0 ){ abline( v = 0, lwd = 1, lty = 1, col = rgb(0.00,0.00,0.00) ); }
# }



# plot(CROSSPRED_COUNTRY_POSTMETA[[1+iPERC]][[iCOU]],  xlab="Fire-related PM25", ylab="RR", exp = FALSE)

###################################################################
## European Cumulative Exposure-Response After the Meta-Analysis ##
##                   (without meta-predictor)                    ##
###################################################################


# pdf( paste0(FOLD_DATA_OUT, FUNC_FIRE,"/knots_",KNOTS, Model, "/PLOT_", "cumu_pooled_EU_POSTMETA_bis.pdf" ), width = 4, height = 4 );
# plot(EU_META, xlab="Fire-related PM25", ylab="RR",  
#      ci.arg=list(density=20,angle=-45,col=4))
# dev.off()
# 
# pdf( paste0(FOLD_DATA_OUT, FUNC_FIRE,"/knots_",KNOTS, Model, "/PLOT_", "cumu_pooled_EU_POSTMETA_bis_low_values.pdf" ), width = 4, height = 4 );
# plot(EU_META, xlab="Fire-related PM25", ylab="RR",  
#      ci.arg=list(density=20,angle=-45,col=4), xlim=c(0, 50), ylim=c(0.90, 1.7))
# dev.off()


# ################################################################################
# ### Maps
# ################################################################################
# 
# print("");
# print("= Maps =");
# print("");
# 
# # foldout = paste0( FOLD_DATA_OUT, sSEX, "_", sAGE, "_", sCAU, "_iSEN.", iSEN, "/" );
# # if( !file_test( "-d", foldout ) ){ dir.create( foldout, recursive = TRUE ); }
# 
# 
# # National and Regional Shapefiles
# vFILE_SHAP_COU = "ADAPTATION_EXTERNAL/aalari/Data/MAPS/files_from_joan/shap/europe_allnuts_gisco_2021_2021_nuts0_shap.geojson"
# vFILE_SHAP_REG = "ADAPTATION_EXTERNAL/aalari/Data/MAPS/files_from_joan/shap/europe_allnuts_gisco_2003_2021_nuts3_shap.geojson" 
# 
# 
# # Simple Feature Object Containing All the Countries of the World
# file.copy( from = "ADAPTATION_EXTERNAL/aalari/Data/MAPS/custom.geojson",
#            to = gsub( "\\\\", "/", gisco_set_cache_dir( verbose = FALSE ) ),
#            overwrite = TRUE,
#            recursive = TRUE );
# 
# SFO_COU_WORLD = gisco_get_countries( year = "2020", epsg = "3035", resolution = "3", cache = TRUE, update_cache = FALSE, verbose = FALSE );
# # SFO_COU_WORLD = gisco_get_countries( year = "2020", epsg = "3035", resolution = "3", cache = TRUE, update_cache = TRUE, verbose = FALSE );
# 
# # Simple Feature Object Containing All the Countries with Data of Europe
# SFO_COU_EUROPE = st_read( vFILE_SHAP_COU );
# 
# for( sCASE_RESO in c( "DATA_REG","DATA_COU" ) ){
#   
#   # Simple Feature Object Containing All the Regions with Data of Europe
#   SFO_NUTS_EUROPE = st_read( vFILE_SHAP_REG );
#   
#   if( sCASE_RESO == "DATA_REG" ){
#     SFO_NUTS_EUROPE = SFO_NUTS_EUROPE[ SFO_NUTS_EUROPE$NUTS_ID %in% vNUTS_CODE, ];
#   }else if( sCASE_RESO == "DATA_COU" ){
#     SFO_NUTS_EUROPE = SFO_NUTS_EUROPE[ SFO_NUTS_EUROPE$NUTS_ID %in% vCOUNTRIES_CODE, ];
#   }else{
#     stop("ERROR: Invalid sCASE_RESO !!!");
#   }
#   
#   if( sCASE_RESO == "DATA_REG" | sCASE_RESO == "DATA_COU" ){
#     vCASE_PLOT = c( "RR99","RR95","RR90",
#                     "AF", "FRANN","FRSUM","FRMAX","FRP99","FRIQR" );
#   }else{
#     stop("ERROR: Invalid sCASE_RESO !!!");
#   }
#   
#   for( sCASE_PLOT in vCASE_PLOT ){
#     
#     # Creation of the Case Variable
#     if( substr( sCASE_PLOT, 1,2 ) == "RR" & 0 < as.numeric( substr( sCASE_PLOT, 3,4 ) ) & as.numeric( substr( sCASE_PLOT, 3,4 ) ) < 100 ){
#       
#       sTITLE = paste0( "Relative Risk at Fire-related PM2.5 Percentile P", substr( sCASE_PLOT, 3,4 ) );
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         CASE_DATA = data.frame( matrix( NA, nrow = nNUTS, ncol = 1, dimnames = list( vNUTS_CODE, "var" ) ) );
#         for( iREG in 1:nNUTS ){ CASE_DATA[iREG,"var"] = CROSSPRED_NUTS_POSTMETA[[1]][[iREG]]$allRRfit[ which( 100 * vPERC_CROSSPRED == as.numeric( substr( sCASE_PLOT, 3,4 ) ) ) ]; }; rm(iREG);
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         for( iCOU in 1:nCOUNTRIES ){ CASE_DATA[iCOU,"var"] = CROSSPRED_COUNTRY_POSTMETA[[1]][[iCOU]]$allRRfit[ which( 100 * vPERC_CROSSPRED == as.numeric( substr( sCASE_PLOT, 3,4 ) ) ) ]; }; rm(iCOU);
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else if( sCASE_PLOT == "AF"){
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         CASE_DATA = data.frame( matrix( NA, nrow = nNUTS, ncol = 1, dimnames = list( vNUTS_CODE, "var" ) ) );
#         if     ( sCASE_PLOT == "AF" ){ sTITLE = "Fire-related PM2.5 Attributable Fraction (%)"; CASE_DATA[,"var"] = ATTFRA_NUTS[ , "Whole Period", "att_val" ]; }
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         if     ( sCASE_PLOT == "AF" ){ sTITLE = "Fire-related PM2.5 Attributable Fraction (%)"; CASE_DATA[,"var"] = ATTFRA_COU[ , "Whole Period", "att_val" ]; }
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else if( sCASE_PLOT == "FRANN" ){
#       
#       sTITLE = "Annual Average Fire-related PM2.5 (in µg/m3)";
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         MP_FR_ANN = sapply( DATALIST_DATA_PRED, function(x) mean( x$PM25Fire, na.rm = TRUE ) );
#         CASE_DATA = data.frame( var = MP_FR_ANN );
#         rm(MP_FR_ANN);
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         MP_FR_ANN = sapply( DATALIST_DATA_CALI, function(x) mean( x$PM25Fire, na.rm = TRUE ) );
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         for( iCOU in 1:nCOUNTRIES ){ CASE_DATA[iCOU,"var"] = mean( MP_FR_ANN[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ); }; rm(iCOU);
#         rm(MP_FR_ANN);
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else if( sCASE_PLOT == "FRSUM" ){
#       
#       sTITLE = "Warm period Fire-related PM25 Median (in µg/m3)";
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         MP_FR_SUM = sapply( DATALIST_DATA_CALI, function(x) median( x$PM25Fire[  5 <= month(x$date) & month(x$date) <= 9 ], na.rm = TRUE ) );
#         CASE_DATA = data.frame( var = MP_FR_SUM );
#         rm(MP_FR_SUM);
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         MP_FR_SUM = sapply( DATALIST_DATA_CALI, function(x) mean( x$PM25Fire[  6 <= month(x$date) & month(x$date) <= 8 ], na.rm = TRUE ) );
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         for( iCOU in 1:nCOUNTRIES ){ CASE_DATA[iCOU,"var"] = mean( MP_FR_SUM[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ); }; rm(iCOU);
#         rm(MP_FR_SUM);
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else if( sCASE_PLOT == "FRMAX" ){
#       
#       sTITLE = "Maximum daily wildfire-related PM25 (in µg/m3)";
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         MP_FR_MAX = sapply( DATALIST_DATA_CALI, function(x) max( x$PM25Fire, na.rm = TRUE ) ); names(MP_FR_MAX) = vNUTS_CODE;
#         CASE_DATA = data.frame( var = MP_FR_MAX );
#         rm(MP_FR_MAX);
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         MP_FR_MAX = sapply( DATALIST_DATA_CALI, function(x) max( x$PM25Fire, na.rm = TRUE ) ); names(MP_FR_MAX) = vCOUNTRIES_CODE;
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         for( iCOU in 1:nCOUNTRIES ){ CASE_DATA[iCOU,"var"] = max( MP_FR_MAX[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ); }; rm(iCOU);
#         rm(MP_FR_MAX);
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else if( sCASE_PLOT == "FRP99" ){
#       
#       sTITLE = "All-cause Mortality";
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         MP_FR_P99 = sapply( DATALIST_DATA_CALI, function(x) quantile( x$PM25Fire, 0.99, na.rm = TRUE ) ); names(MP_FR_P99) = vNUTS_CODE;
#         CASE_DATA = data.frame( var = MP_FR_P99 );
#         rm(MP_FR_P99);
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         MP_FR_P99 = sapply( DATALIST_DATA_CALI, function(x) quantile( x$PM25Fire, 0.99, na.rm = TRUE ) ); names(MP_FR_P99) = vCOUNTRIES_CODE;
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         for( iCOU in 1:nCOUNTRIES ){ CASE_DATA[iCOU,"var"] = mean( MP_FR_P99[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ); }; rm(iCOU);
#         rm(MP_FR_P99);
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else if( sCASE_PLOT == "FRIQR" ){
#       
#       sTITLE = "Fire-related PM25 Inter-Quartile Range (in µg/m3)";
#       
#       if( sCASE_RESO == "DATA_REG" ){
#         
#         MP_FR_ANN = sapply( DATALIST_DATA_CALI, function(x) IQR( x$PM25Fire, na.rm = TRUE ) );
#         CASE_DATA = data.frame( var = MP_FR_ANN );
#         rm(MP_FR_ANN);
#         
#       }else if( sCASE_RESO == "DATA_COU" ){
#         
#         MP_FR_ANN = sapply( DATALIST_DATA_CALI, function(x) IQR( x$PM25Fire, na.rm = TRUE ) );
#         CASE_DATA = data.frame( matrix( NA, nrow = nCOUNTRIES, ncol = 1, dimnames = list( vCOUNTRIES_CODE, "var" ) ) );
#         for( iCOU in 1:nCOUNTRIES ){ CASE_DATA[iCOU,"var"] = mean( MP_FR_ANN[ vCOUNTRIES_CODE_LONG == vCOUNTRIES_CODE[iCOU] ] ); }; rm(iCOU);
#         rm(MP_FR_ANN);
#         
#       }else{
#         stop("ERROR: Invalid sCASE_RESO !!!");
#       }
#       
#     }else{
#       stop("ERROR: Invalid sCASE_PLOT !!!");
#     }
#     
#     # Adding the Rownames to the Case Variable as an Additional Column Showing the Region Id (NUTS_ID)
#     CASE_DATA = rownames_to_column( CASE_DATA, var = "NUTS_ID" );
#     
#     # Adding the Case Variable to the Simple Feature Object
#     SFO_NUTS_CASE = merge( SFO_NUTS_EUROPE, CASE_DATA, by = "NUTS_ID" );
#     rm(CASE_DATA);
#     
#     # Generation of the Map
#     MAP = map_generator( case_plot = sCASE_PLOT,
#                          case_reso = sCASE_RESO,
#                          # sex = sSEX,
#                          # age = sAGE,
#                          # cause = sCAU,
#                          title = sTITLE,
#                          countries = SFO_COU_WORLD,
#                          nuts0 = SFO_COU_EUROPE,
#                          nuts.var = SFO_NUTS_CASE );
#     rm(sTITLE, SFO_NUTS_CASE);
#     
#     if( substr( sCASE_RESO, 1,4 ) == "DATA" ){
#       
#       # Export of the Map
#       pdf( paste0( FOLD_DATA_OUT, "Maps", Model, Outcome, "/MAP_", sCASE_PLOT, "_", sCASE_RESO, "_POSTMETA.pdf" ), width = 5, height = 5 );
#       print( MAP$europe );
#       print( MAP$azores, vp = viewport( 0.13, 0.76, width = 0.20, height = 0.20 ) );
#       print( MAP$canaries, vp = viewport( 0.13, 0.60, width = 0.20, height = 0.10 ) );
#       print( MAP$cyprus, vp = viewport( 0.13, 0.49, width = 0.15, height = 0.09 ) );
#       dev.off();
#       
#     }else{
#       stop("ERROR: Invalid sCASE_RESO !!!");
#     }
#     
#     rm(MAP);
#     
#   }
#   rm(vCASE_PLOT, sCASE_PLOT);
#   
# }
# 
# rm(foldout, SFO_COU_WORLD, SFO_NUTS_EUROPE, SFO_COU_EUROPE, sCASE_RESO);
# 
# 
# rm(foldout, SFO_COU_WORLD, SFO_NUTS_EUROPE, SFO_COU_EUROPE, sCASE_RESO);
# 



####### SAVING MODELS RESULTS ##################

Unit_increase_list<-c("/99th_perc", "/1microgram")

Unit_increase<-Unit_increase_list[2]

save(BLUP_META,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/BLUP_META.RData"))
save(BLUP_PRED_COUNTRY_POSTMETA,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/BLUP_PRED_COUNTRY_POSTMETA.RData"))
save(BLUP_PRED_EU_POSTMETA,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure,Outcome, Unit_increase, "/BLUP_PRED_EU_POSTMETA.RData"))
save(CROSSPRED_COUNTRY_POSTMETA,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/CROSSPRED_COUNTRY_POSTMETA.RData"))
save(CROSSPRED_EU_POSTMETA,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/CROSSPRED_EU_POSTMETA.RData"))
save(CROSSPRED_NUTS_PREMETA,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/CROSSPRED_NUTS_PREMETA.RData"))
save(CROSSPRED_NUTS_POSTMETA,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/CROSSPRED_NUTS_POSTMETA.RData"))
save(MVAR_META,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/MVAR_META.RData"))
save(COEF_MODEL,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/COEF_MODEL.RData"))
save(VCOV_MODEL,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/VCOV_MODEL.RData"))
save(ATTNUM_COU,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/ATTNUM_COU.RData"))
save(ATTNUM_EU_TOT,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/ATTNUM_EU_TOT.RData"))
save(ATTNUM_NUTS,file=paste0(FOLD_RESULTS, "Model_outcomes",Model, Exposure, Outcome, Unit_increase, "/ATTNUM_NUTS.RData"))



