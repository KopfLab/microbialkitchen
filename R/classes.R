setClass("MediaToolsQuantity", 
         representation(unit = "character"), contains = "numeric",
         prototype = prototype(numeric(), unit = ""))

setMethod("show", "MediaToolsQuantity", function(object) {
  validObject(object)
  cat(class(object)[1] %>% stringr::str_replace("MediaTools", ""), " [", object@unit, "]\n", sep="")
  print(object@.Data)
})

setClass("MediaToolsAmount", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "mol")))

setClass("MediaToolsMass", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "g")))

setClass("MediaToolsMolecularWeight", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "g/mol")))

setClass("MediaToolsMolarity", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "M")))

setClass("MediaToolsDensity", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "g/L")))

setClass("MediaToolsVolume", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "L")))

setClass("MediaToolsPressure", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "bar")))

setClass("MediaToolsTemperature", contains = "MediaToolsQuantity", 
         prototype = prototype(new("MediaToolsQuantity", numeric(), unit = "K")))
