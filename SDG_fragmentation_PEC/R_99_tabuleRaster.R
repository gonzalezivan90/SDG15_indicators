tabuleRaster <- structure(
  function( # Count the pixels in a given raster
    ### This function generate a frequency table for a given raster dataset
    layer = '',      ##<<\code{character}. Raster object or raster path
    del0 = FALSE,    ##<<\code{boolean}. Determines if 0 count  categories should me removed
    useNA = "no",   ##<<\code{boolean}. Determines if include NA. Passed to rasterDT::freqDT
    n256 = FALSE      ##<<\code{boolean}. Determines if the raster contains less than 256 unique values, with
  ) {
    
    # requires: raster, rasterDT, gdalUtilities, 
    allowedRastClass <- c('RasterLayer', 'RasterBrick', 'RasterStack')
    
    if (!class(layer) %in% c(allowedRastClass, 'character')){
      stop('Class not RasterLayer or character')
    }
    
    if (class(layer) %in% c('character')){
      if (!file.exists(layer)){
        stop('File not found')
      }
    }
    
    
    if (n256){
      if (class(layer) %in% allowedRastClass){
        if (layer[[1]]@file@name != ''){
          layerPath <- layer[[1]]@file@name
        } else {
          layerPath <- paste0(tempfile(), '.tif')
          writeRaster(layer, filename = layerPath)
        }
        layer <- layerPath
      }
      
      # system.time(gdalLog <- capture.output(gdalUtilities::gdalinfo(datasetname = layer, hist = TRUE)))
      system.time(gdalLog <- (gdalUtils::gdalinfo(datasetname = layer, hist = TRUE)))
      
      (nbands <- grep('Band [[:digit:]]{1,} Block', gdalLog))
      ansList <- list()
      for(n in 1:length(nbands)){ # n <- 1
        
        (bucxml <- as.numeric(sub('buckets.+', '', grep('buckets ', gdalLog, value = TRUE)))[n])
        (minxml <- as.numeric(gsub('.+from | to.+', '', grep('buckets ', gdalLog, value = TRUE)) )[n] )
        (maxxml <- as.numeric(gsub('.+to |:', '', grep('buckets ', gdalLog, value = TRUE)))[n] )
        (histxml <- as.numeric(strsplit(split = '[[:space:]]', gsub("^ |^  ", "", 
                                                                    gdalLog[grep('buckets', gdalLog)[n]+1]
                                                                    ))[[1]]))
        
        labs <- seq(from = minxml, to = maxxml, length.out = bucxml)
        # length(histxml)
        
        df2 <- data.frame(labs, nwlab = c(ceiling(labs[1]),
                                          round(labs[2:(bucxml-1)]),
                                          floor(labs[bucxml])), 
                          val = histxml)
        hist2 <- aggregate(df2$val, by = list(df2$nwlab), sum)
        result <- data.frame(id = hist2$Group.1, count = hist2$x, stringsAsFactors = FALSE)
        
        ## Delete 0 count values
        if(del0){
          result <- subset(result, count > 0)
        }
        
        ansList[[n]] <- result
      }
      
      if(length(nbands) == 1){
        result <- ansList[[1]]
      } else {
        result <- ansList
      }
      
    } else {
      
      if (class(layer) %in% c('character')){
        layer <- tryCatch(raster::stack(layer), error = function (e) stop( "Can't open raster layer"))
      }
      
      freqTable <-  rasterDT::freqDT(layer, useNA = useNA)
      if (class(freqTable) == 'list'){
        result <- lapply(freqTable, function(x){
          data.frame(id = x$ID, count = x$freq, 
                     stringsAsFactors = FALSE )
        })
      } else {
        result <- data.frame(id = freqTable$ID, count = freqTable$freq, stringsAsFactors = FALSE )
      }
      
    }
    
    # layer <- stack(multilayer)                                                                                                                                 
    # if(inherits(layer), 'stack')                                                                                                                               
    #   names(tabulateRaster) <-raster::names(layer)
    
    return(result)
    
    # if(del0){
    #   return(subset(result, count > 0) )
    # } else {
    #   return(result)
    # }
    
    ### \code{data.frame}.
  } , ex = function() {
    ## \donttest{
    ## raster_count(raster(volcano), n256 = FALSE)
    ## }
  })