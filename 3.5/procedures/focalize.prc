procedure focalize(xscan)
# Reduces a 12-m focus observation; uses array (0).
#
scalar i, tx, nax, maxf, foc, oax, nfchan, wlo, foo
#
nfchan=7
#
get xscan
# Places scan into array 0
#
if compare(h0(backend),'DIGITAL')  then
   switched
   { d0(@) = (d0(2*@-1) - d0(2*@))/2 }
end
# Reduces data if it were a digital backend scan
#
h0(noint) =nfchan
# Only the 1st NUMCHAN data points have any useful info in them
#
slabel=1; points; cc
page show rline linetype(020) reshow linetype(0)
slabel = default
# Display the data
#
graphics
#     Make sure text goes to graphics screen
foo=h0(focusr); wlo=h0(wl)
place(238,680)
print 'F0(mm)=',foo,' wl(mm)=',wlo;histogram
nax = 0; maxf = 0
{ tx=abs(d0@); if tx > maxf then; maxf=tx; nax=@; end }
# Find maximum for initial guesses to GAUSS
#
oax = min(nax-1, nfchan-nax)
if oax > 0 then
  height=maxf
  center=nax; hwidth=2*oax; 
  bgauss=nax-oax; egauss=nax+oax; ngauss=1
  slabel=0
  gauss gparts
  slabel=1
  foc=(center(1)-(ifix(nfchan/2)+1))*wlo + foo
  place(112,650)
  print foc,'BEST F0 TO',ifix(2*oax+1),'PT GAUSSIAN FIT'
# Fits Gaussian
#
else
# Peak was either in 1st or last channel so can't use GAUSS to find
# best focus.
  print 'Bad focus scan'
end
crt
place(0,50)
return
finish
