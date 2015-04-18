setClass("Quantity", 
         representation(unit = "character"), contains = "numeric",
         prototype = prototype(numeric(), unit = ""))

setMethod("show", "Quantity", function(object) {
  validObject(object)
  cat(class(object), " [", object@unit, "]\n", sep="")
  print(object@.Data)
})

setClass("Amount", contains = "Quantity", 
         prototype = prototype(new("Quantity", numeric(), unit = "mol")))

setClass("Molarity", contains = "Quantity", 
         prototype = prototype(new("Quantity", numeric(), unit = "M")))

setClass("Volume", contains = "Quantity", 
         prototype = prototype(new("Quantity", numeric(), unit = "L")))

setClass("Pressure", contains = "Quantity", 
         prototype = prototype(new("Quantity", numeric(), unit = "Pa")))

