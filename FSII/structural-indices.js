//***********************************************************************************************//
// load required modules
//***********************************************************************************************//

var utils = require('users/NathanielPaulRobinson/Life_On_Land-Forest_Structure:utils').utils;
var inputs = require('users/NathanielPaulRobinson/Life_On_Land-Forest_Structure:inputs').inputs;

//***********************************************************************************************//
// user defined parameters
//***********************************************************************************************//

// year of analysis
var year = 2021;
var yearString = year.toString();

// analysis thresholds
var naturalForestThreshold = 5;
var hfpRefactor = {
  "lowThreshold": 4,
  "highThreshold": 15,
  "lowVal": 1,
  "medVal": 0.2,
  "highVal": 0.1
};

// set extent (global/lol/country)
var extent = 'country';

// if extent is a single country - set country name
var country = 'Peru'

// view layers in map (true/false)
var viewLayers = true;

// export location (set export parameters below)
var exportToDrive = false;
var exportToAsset = false;

// set drive folder name
var driveFolder = "life_on_land_exports";

// set asset path
var assetPath = "projects/mountain-scenarios/erp_outputs/";



//***********************************************************************************************//
// define the region of interest
//***********************************************************************************************//

// extract default projection parameters for export (uses scale and grid from Hansen Global Forest Cover)
var gfcProjection = inputs.gfc2000.projection();

// select biomes - 1: Tropical & Subtropical Moist Broadleaf Forests
//                 2: Tropical & Subtropical Dry Broadleaf Forests
var biomeNumbers = [1, 2];


var lolBiomes = inputs.ecoregionFC.filter(
  ee.Filter.inList('BIOME_NUM', biomeNumbers));

var biomes = utils.selectBiome(inputs.ecoregionImage, biomeNumbers, 'tropical_forest');

var globalRoi = lolBiomes.geometry()
  .bounds();
  

// select lol countries for lol analysis
var lolCountries = ['Colombia', 'Ecuador', 'Peru'];

var lolCountries = inputs.countries.filter(
  ee.Filter.inList('ADM0_NAME', lolCountries));

var lolRoi = lolBiomes.filterBounds(lolCountries)
  .geometry()
  .bounds();
  

// select individual country for analysis
var countryName = country;
country = inputs.countries.filter(ee.Filter.eq('ADM0_NAME', country));

var countryRoi = lolBiomes.filterBounds(country)
  .geometry()
  .bounds();

if (extent == 'global'){
  var roi = globalRoi;
  var erpStats = inputs.erpGlobalStats;
  var fileNameMod = 'global';
} else if (extent == 'lol'){
  var roi = lolRoi;
  var clip = lolCountries;
  var erpStats = inputs.erpLolStats;
  var fileNameMod = 'lol';
} else if (extent == 'country') {
  var roi = countryRoi;
  var clip = country;
  var erpStats = inputs.erpLolStats;
  var fileNameMod = countryName;
}


//***********************************************************************************************//
// create FSCI and FSII layers
//***********************************************************************************************//

// define natural forest
var naturalForest = utils.selectNaturalProportion(
  erpStats, 
  inputs.ecoregionImage, 
  naturalForestThreshold
);

// define erp height potential
var heightPotential = utils.calcHeightPotential(
  erpStats, 
  inputs.ecoregionImage, 
  inputs.canopyHeight
);

// define erp cover potential
var coverPotential = utils.calcCoverPotential(
  erpStats, 
  inputs.ecoregionImage, 
  inputs.cover2010
);

// create loss year image
var forestLoss = utils.calcLoss(
  inputs.gfc2000.select('loss'), 
  inputs.gfc2000.select('lossyear')
);

// reclassify HFP
var hfpClass = utils.calcHfpClass(
  inputs.hfp, 
  hfpRefactor.lowThreshold,
  hfpRefactor.highThreshold,
  hfpRefactor.lowVal,
  hfpRefactor.medVal,
  hfpRefactor.highVal
);


// create fsci
var fsci = utils.calcFsci(
  heightPotential,
  coverPotential,
  forestLoss,
  year
).updateMask(biomes)
.updateMask(naturalForest)
.setDefaultProjection(gfcProjection);


// create fsii
var fsii = utils.calcFsii(
  hfpClass,
  fsci
);

if (extent == 'country' | extent == 'lol') {
  fsci = fsci.clipToCollection(clip);
  fsii = fsii.clipToCollection(clip);
}

//***********************************************************************************************//
// outputs
//***********************************************************************************************//

// add layers to map
if (viewLayers){
  Map.addLayer(fsci, utils.vizPars().fsci, "FSCI " + yearString, true);
  Map.addLayer(fsii, utils.vizPars().fsii, "FSII " + yearString, true);
}


// exports
// create date string 
var exportDate = utils.constructDateString();


var fsciName = "fsci_erp_" + yearString + "_" + fileNameMod + "_" + exportDate;
var fsiiName = "fsii_erp_" + yearString + "_" + fileNameMod + "_" + exportDate;
var transform = utils.crsTransform(inputs.gfc2000);

if (exportToDrive){
  Export.image.toDrive({
    image: fsci,
    folder: driveFolder,
    fileNamePrefix: fsciName,
    description: "to_drive-" + fsciName,
    crs: 'EPSG:4326',
    crsTransform: transform,
    maxPixels: 1e13,
    region: roi
  });
  
  Export.image.toDrive({
    image: fsii,
    folder: driveFolder,
    fileNamePrefix: fsiiName,
    description: "to_drive-" + fsiiName,
    crs: 'EPSG:4326',
    crsTransform: transform,
    maxPixels: 1e13,
    region: roi
  });
}


if (exportToAsset){
  Export.image.toAsset({
    image: fsci,
    assetId: assetPath + fsciName,
    description: "to_asset-" + fsciName,
    crs: 'EPSG:4326',
    crsTransform: transform,
    maxPixels: 1e13,
    region: roi
  });
  
  Export.image.toAsset({
    image: fsii,
    assetId: assetPath + fsiiName,
    description: "to_asset-" + fsiiName,
    crs: 'EPSG:4326',
    crsTransform: transform,
    maxPixels: 1e13,
    region: roi
  });
}



