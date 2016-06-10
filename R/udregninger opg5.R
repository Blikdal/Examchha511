getDataPart(faithful) #the required data
eruption <- faithful[,1] #specifying eruption
KDE(eruption, method="naive" )
KDE(eruption, method="gaussian" )

densityplot(eruption, n=200)
densityplot(eruption, n=200, method="gaussian")
