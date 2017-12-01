  

  #' Extract Scenario
  #' 
  #' Fuction to extract scenario results from Bond Lab analytics object.  This is taken directory
  #' from BondLab and is copyright Bond Lab Technologies, Inc.
  #' @importFrom BondLab SMMVector.To.CPR
  #' @param bond.id A character string the name or id assigned to pass-through analytics object
  #' @export
  ExtractScenario <- function(bond.id = "character"){
  
  ScenarioName = list()
  ScenarioBPS = list()
  ScenarioCPR = list()
  ScenarioYieldToMaturity = list()
  ScenarioSpreadToCurve = list()
  ScenarioWAL = list()
  ScenarioModDuration = list()
  ScenarioHorizonReturn = list()
  
  for(i in 1:as.numeric(length(bond.id@Scenario))){
    ScenarioName = append(ScenarioName,bond.id@Scenario[i][[1]]@Name)
    ScenarioBPS = append(ScenarioBPS, bond.id@Scenario[i][[1]]@Shiftbps)
    ScenarioCPR = append(ScenarioCPR, SMMVector.To.CPR(bond.id@Scenario[i][[1]]@SMM, length(bond.id@Scenario[i][[1]]@SMM)) * 100)  
    ScenarioYieldToMaturity = append(ScenarioYieldToMaturity, bond.id@Scenario[i][[1]]@YieldToMaturity * 100)
    ScenarioSpreadToCurve = append(ScenarioSpreadToCurve, bond.id@Scenario[i][[1]]@SpreadToInterCurve)
    ScenarioWAL = append(ScenarioWAL, bond.id@Scenario[i][[1]]@WAL)
    ScenarioModDuration = append(ScenarioModDuration, bond.id@Scenario[i][[1]]@ModDuration)
    ScenarioHorizonReturn = append(ScenarioHorizonReturn, bond.id@Scenario[i][[1]]@HorizonReturn)
    
  }
  Result <- cbind(ScenarioName, ScenarioBPS, ScenarioCPR, ScenarioYieldToMaturity, ScenarioSpreadToCurve, ScenarioWAL, ScenarioModDuration, ScenarioHorizonReturn)
  return(Result)
}

  setGeneric("ExtractScenario", function(bond.id = "character")
             {standardGeneric("ExtractScenario")})