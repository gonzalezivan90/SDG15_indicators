var inputs = require('users/NathanielPaulRobinson/Life_On_Land-Forest_Structure:inputs').inputs;



exports.utils = {
  "areaScale": areaScale,
  "areaImage": areaImage,
  "maskWater": maskWater,
  "constructDateString": constructDateString,
  "selectBiome": selectBiome,
  "delineateNaturalForest": delineateNaturalForest,
  "delineateErpBands": delineateErpBands,
  "calcErpPercentiles": calcErpPercentiles,
  "prepErpTable": prepErpTable,
  "selectNaturalProportion": selectNaturalProportion,
  "calcHeightPotential": calcHeightPotential,
  "calcCoverPotential": calcCoverPotential,
  "calcLoss": calcLoss,
  "calcFsci": calcFsci,
  "calcHfpClass": calcHfpClass,
  "calcFsii": calcFsii,
  "vizPars": vizPars,
  "crsTransform":  crsTransform,
  "exportLayers": exportLayers
};



function areaScale(unit) {
  var scaleFactor = 1;
  
     if (unit == 'sqm') {
      scaleFactor = scaleFactor;
    } else if (unit == 'sqkm') {
        scaleFactor = 0.000001;
    } else if (unit == 'ha') {
        scaleFactor = 0.0001;
    } else if (unit == 'ac') {
        scaleFactor = 0.000247105;
    } else {
        scaleFactor = 0.000001;
    }
    
    return scaleFactor;
}



function areaImage(units) {
  var scale = areaScale(units);
  return ee.Image.pixelArea()
    .multiply(scale)
    .rename('area_' + units);
}



function maskWater(image){
  var mask = ee.Image("UMD/hansen/global_forest_change_2021_v1_9")
    .select('datamask')
    .eq(1)
    .selfMask();
  return image.updateMask(mask);
}



function crsTransform(image){
  return image.projection().getInfo().transform;
}



function constructDateString(){
  var current = new Date();
  return current.toISOString().slice(0,10).replace(/-/g,"");
}



function selectBiome(ecoregions,  biomes, bandName) {
  return ecoregions.remap(biomes, biomes, 0, 'BIOME_NUM')
    .selfMask()
    .rename(bandName);
}



function delineateNaturalForest(coverImage, coverThreshold, heightImage, heightThreshold, hfpImage, hfpThreshold, lossImage) {
  return lossImage.eq(0)
    .updateMask(hfpImage.lte(hfpThreshold))
    .updateMask(coverImage.gte(coverThreshold))
    .updateMask(heightImage.gte(heightThreshold))
    .selfMask()
    .rename('natural_forest');
}


function delineateErpBands(biomeImage, ecoregionImage, naturalForestImage, coverImage, heightImage, areaImage) {
  var biomeBinary = biomeImage.gt(0)
    .selfMask();
  
  var biomeArea = areaImage.updateMask(biomeBinary);
  
  var naturalForestArea = areaImage.updateMask(naturalForestImage)
    .rename('natural_forest_area');
  
  var naturalForestHeight = heightImage.updateMask(naturalForestImage)
    .rename('natural_forest_height');
    
  var naturalForestCover = coverImage.updateMask(naturalForestImage)
    .rename('natural_forest_cover');
    
  var ecoregion = ecoregionImage.select('ECO_ID')
    .updateMask(biomeBinary)
    .rename('ecoregion');
    
  return biomeBinary.addBands([
    biomeArea,
    naturalForestImage,
    naturalForestArea,
    naturalForestHeight,
    naturalForestCover,
    ecoregion
    ]);
}



function calcErpPercentiles(forestImage, roi){
  var reducer = ee.Reducer.sum().repeat(6)
    .combine(ee.Reducer.mean().repeat(6), null, true)
    .combine(ee.Reducer.percentile([10, 25, 40, 80, 90]).repeat(6), null, true);
  return ee.List(forestImage.reduceRegion({
    reducer: reducer.group(6, 'ECO_ID'),
    geometry: roi,
    crsTransform: crsTransform(inputs.gfc2000),
    crs: 'EPSG:4326',
    maxPixels: 1e13
  }).get('groups'));
}



function prepErpTable(erpStats, ecoregions, keepGeometry){
  return ee.FeatureCollection(
    erpStats.map(function(item){
      var stats = ee.Dictionary(item);
      var ecoId = stats.get('ECO_ID');
      var means = ee.List(stats.get('mean'));
      var sums = ee.List(stats.get('sum'));
      var p10 = ee.List(stats.get('p10'));
      var p25 = ee.List(stats.get('p25'));
      var p40 = ee.List(stats.get('p40'));
      var p80 = ee.List(stats.get('p80'));
      var p90 = ee.List(stats.get('p90'));
      var eco = ee.Feature(ecoregions.filter(ee.Filter.eq('ECO_ID', ecoId)).first());
      var ecoGeometry = eco.geometry();
      var ecoName = eco.get('ECO_NAME');
      var props = {
        'ECO_ID': ecoId,
        'ECO_NAME': ecoName,
        'n_pixels': sums.get(0),
        'eco_area': sums.get(1),
        'n_pixels_natural_forest': sums.get(2),
        'natural_forest_area': sums.get(3),
        'proportion_natural_forest': ee.Number(sums.get(2))
          .divide(sums.get(0))
          .multiply(100),
        'mean_height': means.get(4),
        'p10_height': p10.get(4),
        'p25_height': p25.get(4),
        'p40_height': p40.get(4),
        'p80_height': p80.get(4),
        'p90_height': p90.get(4),
        'mean_cover': means.get(5),
        'p10_cover': p10.get(5),
        'p25_cover': p25.get(5),
        'p40_cover': p40.get(5),
        'p80_cover': p80.get(5),
        'p90_cover': p90.get(5),
        };
      if (keepGeometry){  
        return ee.Feature(ecoGeometry, props);
      } else {
        return ee.Feature(ee.Geometry.Point([0,0]), props);
      }
  }));
}



function selectNaturalProportion(forestStats, ecoregionImage, naturalProportionThreshold){
  var ecoIds = forestStats.aggregate_array('ECO_ID');
  ecoregionImage = ecoregionImage.select(1)
    .int()
    .rename('ECO_ID');
  var proportionNatural = forestStats.aggregate_array('proportion_natural_forest');
  proportionNatural = ecoregionImage.remap(ecoIds, proportionNatural)
    .rename('proportion_natural_forest');
  
  return proportionNatural.gte(naturalProportionThreshold)
    .selfMask();
}



function calcHeightPotential(forestStats, ecoregionImage, heightImage){
  var ecoIds = forestStats.aggregate_array('ECO_ID');
  ecoregionImage = ecoregionImage.select(1)
    .int()
    .rename('ECO_ID');
  var p40_height = forestStats.aggregate_array('p40_height');
  var p80_height = forestStats.aggregate_array('p80_height');
  
  var height_p40 = ecoregionImage.remap(ecoIds, p40_height)
    .int16()
    .rename('p40_height');
    
  var height_p80 = ecoregionImage.remap(ecoIds, p80_height)
    .int16()
    .rename('p40_height');
  
  var height_lte5 = heightImage.lte(5)
    .selfMask()
    .int16()
    .rename('h_1');

  var height_gt5 = heightImage.updateMask(heightImage.gt(5))
    .selfMask()
    .int16()
    .rename('h_gt5');
  
  var height_gt5_lteP40 = height_gt5.lte(height_p40)
    .selfMask()
    .int16()
    .rename('h_2');
  
  var height_gtP40_lteP80 = height_gt5.gt(height_p40)
    .and(height_gt5.lte(height_p80))
    .selfMask()
    .int16()
    .rename('h_3');
    
  var height_gtP80 = height_gt5.gt(height_p80)
    .selfMask()
    .int16()
    .rename('h_4'); 
    
  return height_lte5.addBands([
    height_gt5_lteP40,
    height_gtP40_lteP80,
    height_gtP80]);  
}




function calcCoverPotential(forestStats, ecoregionImage, coverImage){
  var ecoIds = forestStats.aggregate_array('ECO_ID');
  ecoregionImage = ecoregionImage.select(1)
    .int()
    .rename('ECO_ID');
  var p25_cover = forestStats.aggregate_array('p25_cover');
  var p40_cover = forestStats.aggregate_array('p40_cover');
  var p80_cover = forestStats.aggregate_array('p80_cover');
  var cover_p25 = ecoregionImage.remap(ecoIds, p25_cover)
    .int16()
    .rename('p25_cover');
  var cover_p40 = ecoregionImage.remap(ecoIds, p40_cover)
    .int16()
    .rename('p40_cover');
  var cover_p80 = ecoregionImage.remap(ecoIds, p80_cover)
    .int16()
    .rename('p80_cover');
    
  var cover_lt25 = coverImage.lt(cover_p25)
    .selfMask()
    .int16()
    .rename('c_1');

  var cover_gte25_lte40 = coverImage.gte(cover_p25)
    .and(coverImage.lte(cover_p40))
    .selfMask()
    .int16()
    .rename('c_2');
    
  var cover_gt40_lte80 = coverImage.gt(cover_p40)
    .and(coverImage.lte(cover_p80))
    .selfMask()
    .int16()
    .rename('c_3');
  
  var cover_gt80 = coverImage.gt(cover_p80)
    .selfMask()
    .int16()
    .rename('c_4');
    
  return cover_lt25.addBands([
    cover_gte25_lte40, 
    cover_gt40_lte80,
    cover_gt80]);
}




function calcLoss(lossImage, lossYearImage){
  var noLoss_01_21 = lossImage.eq(0)
    .selfMask()
    .int16()
    .rename('no_loss_01_21');
    
  var loss_01_12 = lossYearImage.gte(1)
    .and(lossYearImage.lte(12))
    .selfMask()
    .int16()
    .rename('loss_01_12');
  
  var loss_13_21 =  lossYearImage.gte(13)
    .and(lossYearImage.lte(21))
    .selfMask()
    .int16()
    .rename('loss_13_21');
    
  var noLoss_01_12 = noLoss_01_21.unmask(0)
    .add(loss_13_21.unmask(0))
    .gt(0)
    .selfMask()
    .int16()
    .rename('no_loss_01_12');
    
  return noLoss_01_21.addBands([
    loss_01_12,
    loss_13_21,
    noLoss_01_12]);
}




function calcFsci(heightPotential, coverPotential, loss, year){
  var h_1 = heightPotential.select('h_1');
  var h_2 = heightPotential.select('h_2');
  var h_3 = heightPotential.select('h_3');
  var h_4 = heightPotential.select('h_4');
  var h_5 = heightPotential.select('h_5');
  
  var c_1 = coverPotential.select('c_1');
  var c_2 = coverPotential.select('c_2');
  var c_3 = coverPotential.select('c_3');
  var c_4 = coverPotential.select('c_4');
  
  var loss_01_12 = loss.select('loss_01_12');
  var loss_13_21 = loss.select('loss_13_21');
  var no_loss_01_12 = loss.select('no_loss_01_12');
  
  var sci1 = heightPotential.select('h_1').unmask(0)
    .add(coverPotential.select('c_1').unmask(0))
    .gt(0)
    .selfMask()
    .int16()
    .rename('FSCI');
  
  var sci2 = loss_01_12.updateMask(h_2)
    .updateMask(c_2)
    .multiply(2)
    .int16()
    .rename('FSCI');
    
  var sci3 = loss_01_12.updateMask(h_2)
    .updateMask(c_3)
    .multiply(3)
    .int16()
    .rename('FSCI');
    
  var sci4 = loss_01_12.updateMask(h_2)
    .updateMask(c_4)
    .multiply(4)
    .int16()
    .rename('FSCI');
    
  var sci5 = loss_01_12.updateMask(h_3)
    .updateMask(c_2)
    .multiply(5)
    .int16()
    .rename('FSCI');
    
  var sci6 = loss_01_12.updateMask(h_3)
    .updateMask(c_3)
    .multiply(6)
    .int16()
    .rename('FSCI');
  
  var sci7 = loss_01_12.updateMask(h_3)
    .updateMask(c_4)
    .multiply(7)
    .int16()
    .rename('FSCI');
    
  var sci8 = loss_01_12.updateMask(h_4)
    .updateMask(c_2)
    .multiply(8)
    .int16()
    .rename('FSCI');
    
  var sci9 = loss_01_12.updateMask(h_4)
    .updateMask(c_3)
    .multiply(9)
    .int16()
    .rename('FSCI');
  
  var sci10_1 = loss_01_12.updateMask(h_4)
    .updateMask(c_4)
    .multiply(10)
    .int16()
    .rename('FSCI');
  
  var sci10_2 = no_loss_01_12.updateMask(h_2)
    .updateMask(c_2)
    .multiply(10)
    .int16()
    .rename('FSCI');
    
  var sci11 = no_loss_01_12.updateMask(h_2)
    .updateMask(c_3)
    .multiply(11)
    .int16()
    .rename('FSCI');
    
  var sci12 = no_loss_01_12.updateMask(h_2)
    .updateMask(c_4)
    .multiply(12)
    .int16()
    .rename('FSCI');
    
  var sci13 = no_loss_01_12.updateMask(h_3)
    .updateMask(c_2)
    .multiply(13)
    .int16()
    .rename('FSCI');
    
  var sci14 = no_loss_01_12.updateMask(h_3)
    .updateMask(c_3)
    .multiply(14)
    .int16()
    .rename('FSCI');
  
  var sci15 = no_loss_01_12.updateMask(h_3)
    .updateMask(c_4)
    .multiply(15)
    .int16()
    .rename('FSCI');
    
  var sci16 = no_loss_01_12.updateMask(h_4)
    .updateMask(c_2)
    .multiply(16)
    .int16()
    .rename('FSCI');
    
  var sci17 = no_loss_01_12.updateMask(h_4)
    .updateMask(c_3)
    .multiply(17)
    .int16()
    .rename('FSCI');
  
  var sci18 = no_loss_01_12.updateMask(h_4)
    .updateMask(c_4)
    .multiply(18)
    .int16()
    .rename('FSCI');
    
  var sci_2012 = ee.ImageCollection.fromImages([
    sci1,
    sci2,
    sci3,
    sci4,
    sci5,
    sci6,
    sci7,
    sci8,
    sci9,
    sci10_1,
    sci10_2,
    sci11,
    sci12,
    sci13,
    sci14,
    sci15,
    sci16,
    sci17,
    sci18,
    ]).max()
      .int16()
      .rename('FSCI');
  
  var sci1_2021 = sci_2012.updateMask(loss_13_21)
    .gt(0)
    .selfMask();
  
  var sci_2021 = sci_2012.where(sci1_2021.eq(1), 1)
    .selfMask()
    .rename('FSCI');
  
  if (year == 2012){
    return sci_2012;
  }
  else if (year == 2021) {
    return sci_2021;
  } else {
    return ee.Image(0);
  }
}




function calcHfpClass(hfp, lowThreshold, highThreshold, lowValue, medValue,  highValue){
  var hfpClass1 = hfp.lte(lowThreshold)
    .multiply(lowValue);
  var hfpClass2 = hfp.gt(lowThreshold)
    .and(hfp.lte(highThreshold))
    .multiply(medValue);
  var hfpClass3 = hfp.gt(highThreshold)
    .multiply(highValue);
  var hfpClass = hfpClass1.add(hfpClass2)
    .add(hfpClass3)
    .rename('hfpClass');
  return hfpClass;
}



function calcFsii (hfpClass, fsci){
  return hfpClass.multiply(fsci)
    .rename('FSII');
}



function vizPars() {
  return {
    fsci: {bands:['FSCI'], min: 1, max: 18, palette:['#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725']},
    fsii: {bands:['FSII'], min: 1, max: 18, palette:['#0D0887', '#5B02A3', '#9A179B', '#CB4678', '#EB7852', '#FBB32F', '#F0F921']}
  };
}



function exportLayers(image, location, fileName, region){
  
  
  if (scale){
    Export.image.toDrive({
      image: image,
      driveFolder: drive,
      fileNamePrefix: fileName,
      description: fileName,
      scale: scale,
      crs: crs,
      region: region,
      maxPixels: 1e13
  });
  } else {
    var transform = crsTransform(image);
    Export.image.toDrive({
      image: image,
      driveFolder: drive,
      fileNamePrefix: fileName,
      description: fileName,
      crsTransform: transform,
      crs: crs,
      region: region,
      maxPixels: 1e13
    });
  }
}
