setClass("MediaChemToolsQuantity", 
         representation(unit = "character"), contains = "numeric",
         prototype = prototype(numeric(), unit = ""))

setMethod("show", "MediaChemToolsQuantity", function(object) {
  methods::validObject(object)
  cat(class(object)[1] %>% stringr::str_replace("MediaChemTools", ""), " [", object@unit, "]\n", sep="")
  print(object@.Data)
})

setClass("MediaChemToolsAmount", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "mol")))

setClass("MediaChemToolsMass", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "g")))

setClass("MediaChemToolsMolecularMass", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "g/mol")))

setClass("MediaChemToolsMolarity", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "M")))

setClass("MediaChemToolsDensity", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "g/L")))

setClass("MediaChemToolsVolume", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "L")))

setClass("MediaChemToolsPressure", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "bar")))

setClass("MediaChemToolsSolubility", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "M/bar")))

setClass("MediaChemToolsTemperature", contains = "MediaChemToolsQuantity", 
         prototype = prototype(new("MediaChemToolsQuantity", numeric(), unit = "K")))
