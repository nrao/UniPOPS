proc gbtipping(scan_num)
# Reduces a Green Bank tipping scan and displays data and the fit.
# Uses arrays 0 and 1 and alters TAU0, TRCVR, display mode, and 
# PLOT boundaries and titles.
#
scalar i, tsky
#
get0 ifix(scan_num) + 0.02
get1 ifix(scan_num) + 0.98
# Gets the data and the Elevations of the data
#
{ d1@ = 1/sin(d1@) }
# Converts elevations into Num. atmospheres
#
solvetip
# Fits the tipping curve to the data -- results stored in TAU0 and TRCVR
#
fact = 0
for i = 3 to h0(noint) by 3
   fact = fact + h0(tcal) / (d0(i-1) - (d0(i-2)+d0(i))/2)
   end
fact = fact / (h0(noint)/3); scale
for i = 2 to h0(noint) by 3
   d0(i) = d0(i) - h0(tcal)
   end
tsky = h0(tamb)+273.16
# Convert the data from counts to K; Calculates ambient temp in K
#
htitle = 'Tipping scan'
xtitle = 'Num. of Atmos'
ytitle = 'Tsys'
xminp=d1(1)-1; xmaxp=d1(h0(noint))+1
yminp=trcvr; ymaxp=trcvr+tsky*(1-exp(-tau0*xmaxp))
# Set up the plot boundaries and the plot titles
#
points page plot
{ d0@ = trcvr+tsky*(1-exp(-tau0*d1@)) }
line plot
# Draws a plot of the data and then the fit
#
return
finish
