exports.inputs = {
  "ecoregionFC": ee.FeatureCollection("RESOLVE/ECOREGIONS/2017"),
  "ecoregionImage": ee.Image("projects/mountain-scenarios/Forest-Integrity/resolve-ecoregion-img"),
  "hfp": ee.Image("projects/mountain-scenarios/Forest-Integrity/hfp-2013"),
  "gfc2000": ee.Image("UMD/hansen/global_forest_change_2021_v1_9"),
  "cover2010": ee.Image("projects/mountain-scenarios/Forest-Integrity/treecover-2010"),
  "canopyHeight": ee.Image("projects/mountain-scenarios/Forest-Integrity/treeheight"),
  "wdpa": ee.FeatureCollection("WCMC/WDPA/current/polygons"),
  "erpGlobalStats": ee.FeatureCollection("projects/mountain-scenarios/Forest-Integrity/erp-global-stats"),
  "erpLolStats": ee.FeatureCollection("projects/mountain-scenarios/Forest-Integrity/erp-lol-stats-2"),
  "countries": ee.FeatureCollection("FAO/GAUL/2015/level0"),
  "Peru_limite": ee.FeatureCollection("projects/mountain-scenarios/Life-On-Land/Peru/Limite_Peru"),
  "Peru_perdida": ee.Image("projects/mountain-scenarios/Life-On-Land/Peru/Loss_2001_2021"),
  "Peru_es_image": ee.Image("projects/mountain-scenarios/Life-On-Land/Peru/Peru-es-img"),
  "Peru_es_fc": ee.FeatureCollection("projects/mountain-scenarios/Life-On-Land/Peru/Peru-es-fc"),
  "Peru_es_stats":  ee.FeatureCollection("projects/mountain-scenarios/Life-On-Land/Peru/es-percentiles/es-percentiles20230621")
};
