shopSimulate2 <- function(
  N, 
  arrivalInterval,
  serviceTime,
  initialIncidence,
  numShopkeepers=1,  # they stay in the shop all the time
  lambda # the disease transmission rate from an infected to an uninfected
  #shopCapacity = 10000, # the number of people who can be in the shop at one time. If not there will be a queue outside
)
{
  entryTimes <- cumsum(rexp(N, 1/arrivalInterval))
  shoppingTimes <- rexp(N, 1/serviceTime)
  initInfected <- runif(N) < initialIncidence
  shoppers <- tibble(
    Number=1:N, 
    Entry=entryTimes,
    Exit=entryTimes + shoppingTimes,
    PriorInfection=initInfected,
    LaterInfection=initInfected
  ) %>%
    bind_rows(  # adding the shopkeepers
      tibble(
        Number=(N+1):(N + numShopkeepers),
        Entry=0,
        Exit=max(entryTimes + shoppingTimes),
        PriorInfection=F,
        LaterInfection=F
      )
    )
  shoppers %>%
    gather(Activity, Time, -Number, -PriorInfection, -LaterInfection) %>%
    arrange(Time) %>%
    ungroup() %>%
    mutate(
      Change=ifelse(Activity=='Entry', 1, -1),
      PeopleInShop=cumsum(Change) + numShopkeepers,
      Interval=lead(Time,1)-Time,
      numInitInfected=cumsum(ifelse(PriorInfection, Change, 0))
    ) ->
    shopActivity
  shoppers %>%
    mutate(InfectionTime = ifelse(PriorInfection, 0, NA)) ->
    shoppers
  
  infectedInShop <- 0  # number of people currently in the shop who are infected
  
  for(i in 1:nrow(shopActivity)) {
    infectedInShop <- infectedInShop + ifelse(shopActivity$LaterInfection[i], shopActivity$Change[i], 0)
    
    if(infectedInShop > 0)  { # There is at least one infected person in the shop. Else status quo. 
      transProb <- 1 - exp(-lambda * shopActivity$Interval[i] * infectedInShop / 60)  # / 60 since times are in minutes nad lambda is per hour
      
      shoppers %>%
        filter(Entry <= shopActivity$Time[i] & Exit > shopActivity$Time[i] & !LaterInfection) %>% # Shoppers in store at that time, and yet uninfected
        mutate(Infected=runif(n()) < transProb) %>%
        filter(Infected) %>%
        pull(Number) ->
        newlyInfected
      
      if(length(newlyInfected) > 0) 
      {
        print(newlyInfected)
      }
        
      
      shoppers %>%
        mutate(
          LaterInfection=ifelse(Number %in% newlyInfected, T, LaterInfection),
          InfectionTime=ifelse(Number %in% newlyInfected, shopActivity$Time[i], InfectionTime)
        ) -> 
        shoppers
      
      infectedInShop <- infectedInShop + length(newlyInfected)
      
      
      shopActivity %>%
        mutate(LaterInfection=ifelse(Number %in% newlyInfected & Time > shopActivity$Time[i], T, LaterInfection)) ->
        shopActivity
    }
  }
  
  return(list(shoppers, shopActivity))
}