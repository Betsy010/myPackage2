
#' Illustration of gpFunction
#'
#' Returns the probability of the given person surviving or not.
#' @param Sex male, female: include quotation marks (\code{"male"} or \code{"female"})
#' @param Class First, Second, Third,; include quotation marks
#' (\code{"First"} or \code{"Second"} or \code{"Third"})
#' @param Age real number

#' @param Family integer

#' @param Embarked C,Q,S; include quotation marks (\code{"C"} or \code{"Q"} or \code{"S"})

#' @param Fare real number
#'
#' @param model By default this is the model given by stepwise regression using the starting model 3
#' (Model including all three-way interaction terms and lower terms, plus some polynomial terms). Can be
#' changed.
#' @return The probability of someone with those characteristics surviving is returned.
#'
#' @examples
#' Test1 <- gpFunction("male", "Third", 20, 0, "S", 8.05)
#' Test2 <- gpFunction("female", "Third", 21, 0, "Q", 7.8792)
#' Test3 <- gpFunction("male", "First", 40, 0, "C", 27.7208)
#' Test4 <- gpFunction("female", "Second", 3, 3, "C", 41.5792)
#'
#' @export

gpFunction <- function(Sex, Class, Age, Family, Embarked, Fare, model = NULL){
  #This if statement checks to see if the user specified a model.  If there is no specified model, then by    #default the titanic.logictic.step3 model is used.
  if(is.null(model)){
    data("model3")
    model <- titanic.logistic.step3
  }
  # The next 3 lines specify that the variables Sex, Class, and Embarked are of type factor.
  Sex <- as.factor(Sex)
  Class <- as.factor(Class)
  Embarked <- as.factor(Embarked)
  # We now make a data frame to be read in by the predict function
  df <- data.frame(sex = Sex, class = Class, age = Age, family = Family, embarked = Embarked, fare = Fare)
  # The predict function makes predictions on the given information. Note that vectors of the same length     # for each predictor can also be given to the function.
  glm.predict4 <- predict(model, df)
  # The predict function returns log(pi/(1-pi). We now solve for pi which is the probability of survival.
  pi_s <- exp(glm.predict4)/(1+exp(glm.predict4))
  #The probability of survival is returned.
  return(pi_s)
}

