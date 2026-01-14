#make the given matrix, using the last number of my ID
D = 9
P = matrix(c(
  0.5,0.5,0,0,0,0,
  0.5,0.2,0.2,0,0,0.1,
  0,0,0.8,0.2*(0.6 + D/50),0.2*(0.2-D/100),0.2*(0.2-D/100),
  0,0,0,1,0,0,
  0,0,0,0,1,0,
  0,0,0,0,0,1
), nrow = 6, byrow = TRUE)


##########################################################################################

# function of limit distribution 
lim_dist_func = function(initial_dist, P, steps = 10000) {
  for (i in 1:steps) initial_dist = initial_dist%*%P
  as.numeric(initial_dist)
}

lim_dist = lim_dist_func(c(0,0,1,0,0,0), P)
Quantity1 = lim_dist[4]   # prob to reach Pe
Quantity2 = lim_dist[6]   # prob to reach Pd


##########################################################################################
#This part of the code focuses on proposal 1 and Quantity3;Quantity5;
#Quantity6;Quantity7;Quantity11;Quantity13

trials = 10000 # number of trials 
#used for Quantity13 and Quantity14
material_cost = 2 #cost of the material
cA = 0.1; cB = 0.1; cC = 0.2 #cost per unit of time for pоsition A,B,C
total_cost_A = 0 # total cost 

#used for Quantity4
total_steps_A = 0   # counter for the total number of steps from point A
# used for Quantity5-7
peA = 0;psA = 0;pdA = 0 # counter for the total number of ends in absorbing states
#used for Quantity11
total_timeAB_A = 0 #how many time the product was in A or B

for (j in 1:trials) {
  X = 1 #started point from A
  steps = 0
  costA = 0  #the price always has manufacturing cost
  while (X %in% 1:3) { # focus only on states A,B,C because the rest are absorbing and there is no way to escape 
    
    if (X == 1 || X == 2) total_timeAB_A = total_timeAB_A+1  # count only entrance to A and B for Quantity11
    
    # count the future price according to the state 
    if (X == 1) costA = costA + cA # if the product goes to the Machine A +0.1$ 
    else if (X == 2) costA = costA + cB # if the product goes to the Machine B +0.1$ 
    else costA = costA + cC   # if the product goes to the Machine C +0.2$ 
    
    X = sample(1:6, size = 1, prob = P[X, ]) #randomly takes the next step
    steps = steps+1
  }
  total_steps_A = total_steps_A+steps # accumulate total number of steps across all trials
  total_cost_A  = total_cost_A+costA # accumulate total cost for all trials
  # check which final outcome the product ended in the absorbing states 
  if (X == 4) peA = peA+1 # count if the product has excellent quality 
  else if (X == 5) psA = psA+1 # count if the product has satisfactory quality 
  else pdA = pdA+1 # count if the product has defective quality 
}

Quantity3 = total_steps_A/trials #expected manufacturing time
Quantity5 = peA/trials  #the expected proportion of excellent products
Quantity6 = psA/trials  #the expected proportion of satisfactory products
Quantity7 = pdA/trials  #the expected proportion of defective products
Quantity11 = total_timeAB_A/trials  # expected total time in A+B 
Quantity13 = total_cost_A/trials   # expected total manufacturing cost



##########################################################################################
#This part of the code focuses on proposal B and Quantity4;Quantity8;
#Quantity9;Quantity10;Quantity12; Quantity14

total_steps_B = 0   # counter for the total number of steps from B
total_timeAB_B = 0  #how many time the product was in A or B
peB = 0;psB = 0;pdB = 0 # counter for the total number of ends in absorbing states
total_cost_B = 0 # total cost

for (j in 1:trials) {  
  X = 2 #started point from B
  steps = 0
  costB = 0
  
  while (X %in% 1:3) {
    
    if (X == 1 || X == 2) total_timeAB_B = total_timeAB_B+1  # count only A and B
    
    # count the future price according to the state 
    if (X == 1) costB = costB+cA # if the product goes to the Machine A +0.1$ 
    else if (X == 2) costB = costB+cB # if the product goes to the Machine B +0.1$ 
    else costB = costB+cC # if the product goes to the Machine C +0.2$ 
    
    X = sample(1:6, size = 1, prob = P[X, ])
    steps = steps+1
  }
  total_steps_B = total_steps_B+steps
  total_cost_B  = total_cost_B+costB
  if (X == 4) peB = peB+1 # count if the product has excellent quality
  else if (X == 5) psB = psB+1 # count if the product has satisfactory quality
  else pdB = pdB+1 # count if the product has defective quality
}


Quantity4 = total_steps_B/trials #expected manufacturing time
Quantity8 = peB/trials #the expected proportion of excellent products
Quantity9 = psB/trials #the expected proportion of satisfactory products
Quantity10 = pdB/trials #the expected proportion of defective products
Quantity12 = total_timeAB_B/trials  # expected total time in A+B
Quantity14 = total_cost_B/trials   # expected total manufacturing cost


##########################################################################################
#set price for different product quality 
priceE = 9; priceS = 6; priceD = 0

# overall revenue 
revenueA = priceE*Quantity5+priceS*Quantity6+priceD*Quantity7
revenueB = priceE*Quantity8+priceS*Quantity9+priceD*Quantity10

# overall expected profit 
profitA = revenueA-Quantity13-material_cost
profitB = revenueB-Quantity14-material_cost

Quantity15 = profitA # expected profit of proposal 1
Quantity16 = 0.5*(profitA + profitB) # expected profit of proposal 3 


##########################################################################################
# additiional calculations for comparison 

profitB = 2 * Quantity16 - Quantity15 # expected profit of proposal 2
exp_time3 = 0.5*(Quantity3+Quantity4) # Expected manufacturing time (average of Proposal 1 and 2)
excellent3 = 0.5*(Quantity5+Quantity8) # Expected proportion of excellent products
satisf3 = 0.5*(Quantity6+Quantity9) # Expected proportion of satisfactory products
defect3 = 0.5*(Quantity7+Quantity10) # Expected proportion of defective products
exp_timeAB3 = 0.5*(Quantity11+Quantity12) #Expected total time spent in machines A + B
exp_cost3 = 0.5*(Quantity13+Quantity14) # Expected total manufacturing cost

##########################################################################################

#Figure 2
barplot(c(Quantity3, Quantity4, exp_time3),names.arg = c("Proposal 1", "Proposal 2", "Proposal 3"),
        col = c("pink2","gold","seagreen3"),main = "Expected Manufacturing Time",
        ylab = "Steps",cex.names = 0.9,ylim = c(0, max(Quantity3, Quantity4, exp_time3) * 1.2),las = 1)

#Figure 3
barplot(c(Quantity15, 2*Quantity16 - Quantity15, Quantity16),names.arg = c("Proposal 1","Proposal 2","Proposal 3"),
        col = c("pink2","gold","seagreen3"),main = "Expected Profit",ylab = "Profit",
        cex.names = 0.9,ylim = c(0, max(Quantity15, 2*Quantity16 - Quantity15, Quantity16) * 1.2),las = 1)

#Figure 4
props = rbind(c(Quantity5, Quantity8, excellent3),c(Quantity6, Quantity9, satisf3),c(Quantity7, Quantity10, defect3))
barplot(props,beside = FALSE,col = c("green3","yellow2","red2"),names.arg = c("Proposal 1","Proposal 2","Proposal 3"),
        main = "Proportion of Product Quality",legend.text = c("Excellent","Satisfactory","Defective"),
        args.legend = list(x = "topright",y.intersp = 1.0,   inset = c(-0.02, -0.05), bty = "n", cex = 0.9),
        ylab = "Proportion",cex.names = 0.9,ylim = c(0, 1.05),las = 1)

#Figure 5
materials = rep(material_cost, 3)  
process = c(Quantity13, Quantity14, exp_cost3)  
totals = materials+process
barplot(rbind(materials, process),names.arg = c("Proposal 1","Proposal 2","Proposal 3"),col = c("grey","brown4"),
  main = "Total Cost",ylab = "Expected Cost",cex.names = 0.9,ylim = c(0, max(totals) * 1.25),
  legend.text = c("Material","Process"),args.legend = list(x = "topright", inset = c(-0.02, -0.05), bty = "n", cex = 0.9),las = 1)


##########################################################################################
data.frame(
  Value = c(Quantity1,Quantity2,Quantity3, Quantity4,Quantity5, Quantity6, Quantity7,
            Quantity8, Quantity9, Quantity10,Quantity11, Quantity12,
            Quantity13, Quantity14,Quantity15, Quantity16)
)



