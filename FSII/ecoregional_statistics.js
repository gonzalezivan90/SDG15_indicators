
//***********************************************************************************************//
// load required modules
//***********************************************************************************************//
var utils = require('users/NathanielPaulRobinson/Life_On_Land-Forest_Structure:utils').utils;
var inputs = require('users/NathanielPaulRobinson/Life_On_Land-Forest_Structure:inputs').inputs;

//***********************************************************************************************//
// user defined parameters
//***********************************************************************************************//

// natural forest thresholds
var thresholds = {
  "coverThreshold": 25,
  "heightThreshold": 5,
  "hfpThreshold": 3,
};


// set calculation extent
var exportGlobal = false;
var exportLoLSubset = true;


// export parameters
var exportToDrive = true;
var driveFolder = "life_on_land_exports";
var driveFileName = "erp_percentiles";

var exportToAsset = false;
var assetPath = "projects/mountain-scenarios/erp-percentiles/";
var assetName = "erp-percentiles";

var biomeNumbers = [1, 2];
var lolCountries = ['Colombia', 'Ecuador', 'Peru'];

var lolBiomes = inputs.ecoregionFC.filter(
  ee.Filter.inList('BIOME_NUM', biomeNumbers));

var globalRoi = lolBiomes.geometry()
  .bounds();
  
var lolCountries = inputs.countries.filter(
  ee.Filter.inList('ADM0_NAME', lolCountries));

var lolRoi = lolBiomes.filterBounds(lolCountries)
  .geometry()
  .bounds();

var area = utils.areaImage('ha');

var biomes = utils.selectBiome(
  inputs.ecoregionImage, 
  biomeNumbers, 
  'tropical_forest'
);


//***********************************************************************************************//
// create layers for calculating ERP percentiles
//***********************************************************************************************//

var naturalForest = utils.delineateNaturalForest(
  inputs.cover2010, 
  thresholds.coverThreshold, 
  inputs.canopyHeight, 
  thresholds.heightThreshold, 
  inputs.hfp, 
  thresholds.hfpThreshold, 
  inputs.gfc2000.select('loss')
);

var forestImage = utils.delineateErpBands(
  biomes, 
  inputs.ecoregionImage, 
  naturalForest, 
  inputs.cover2010, 
  inputs.canopyHeight, 
  area
);


//***********************************************************************************************//
// calculate ERP percentiles
//***********************************************************************************************//
var erpPercentilesGlobal = utils.calcErpPercentiles(forestImage, globalRoi); 
var erpPercentilesLol = utils.calcErpPercentiles(forestImage, lolRoi); 

erpPercentilesGlobal = utils.prepErpTable(erpPercentilesGlobal, inputs.ecoregionFC);
erpPercentilesLol = utils.prepErpTable(erpPercentilesLol, inputs.ecoregionFC);


//***********************************************************************************************//
// outputs
//***********************************************************************************************//
var date = utils.constructDateString();

if (exportToDrive){
  if (exportGlobal){
    var driveFileNameGlobal = driveFileName + "_global_" + date; 
    Export.table.toDrive({
      collection: erpPercentilesGlobal,
      folder: driveFolder,
      fileNamePrefix: driveFileNameGlobal,
      description: "to_drive-" + driveFileNameGlobal
    });
  }
  if (exportLoLSubset){
    var driveFileNameSubset = driveFileName + "_lol_subset_" + date;
    Export.table.toDrive({
      collection: erpPercentilesLol,
      folder: driveFolder,
      fileNamePrefix: driveFileNameSubset,
      description: "to_drive-" + driveFileNameSubset
    });
  }
}

if (exportToAsset){
  if (exportGlobal){
    var assetIdGlobal = assetPath + assetName + "-global-" + date;
    Export.table.toAsset({
      collection: erpPercentilesGlobal,
      assetId: assetIdGlobal,
      description: "to_asset-" + assetName + "-global-" + date
    });
  }
  if (exportLoLSubset){
    var assetIdSubset = assetPath + assetName + "-lol_subset-" + date;
    Export.table.toAsset({
      collection: erpPercentilesLol,
      assetId: assetIdSubset,
      description: "to_asset-" + assetName + "-lol_subset-" + date
    });
  }
}

