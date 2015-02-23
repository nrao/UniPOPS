PROCEDURE BADCH
#  Use cursor to identify bad channels in the spectrometer
#  Print the list of bad channels in a form that the control system understands
#  the value of offset handles this
#  Shows the final result after REPLACEing the bad channels
#  Scan must be already displayed
array lbch(12), rbch(12)
scalar lsubscan, rsubscan, bch, i, loffset, roffset, twoup, ldamage,
scalar ldamage, rdamage, lcount, rcount
#
twoup = false
if (status(9) = 2) then
  if (compare(h3(telescop),"NRAO 12M") & compare(h4(telescop),"NRAO 12M")) then
     twoup = true
  end
end
lbch=0.;  rbch=0.; lsubscan=0; rsubscan=0; loffset=0; roffset=0;
lcount=0.; rcount=0; ldamage=false; rdamage=false
if (twoup) then
   lsubscan = nint((h3(scan) - ifix(h3(scan))) * 100)
   if (lsubscan <= 4) then; loffset = 128 * (lsubscan - 1);end
   rsubscan = nint((h4(scan) - ifix(h4(scan))) * 100)
   if (rsubscan <= 4) then; roffset = 128 * (rsubscan - 1);end
else
   lsubscan = nint((h0(scan) - ifix(h0(scan))) * 100)
   loffset = 128 * (lsubscan - 1)
end
fprint(-1,"a,$") " Enter # of bad channels : "
fread(-1,"i") bch
if (bch <= 0) then; return; end
if (bch > 12) then;
   print "You can only remove up to 12 bad channels at a time."
   return
end
PRINT "Use the cursor to identify the bad channels."
FOR I = 1 TO BCH
#		use nint to get the nearest channel number to the cursor pos
   click
   if (twoup) then
      if (xclick > 500) then;
         rcount = rcount + 1
         rdamage=true; rbch(rcount) = nint(cclick) 
      else 
         lcount = lcount + 1
	 ldamage=true; lbch(lcount) = nint(cclick)
      end
   else
      lbch(i) = nint(cclick)
   end
end
if (twoup) then
   if (ldamage) then
      badpt=0;
      for i = 1 to lcount; badpt(i) = lbch(i); lbch(i) = lbch(i) + loffset; end
      copy(3,0); replace; copy(0,3);
   end
   if (rdamage) then
      badpt=0;
      for i = 1 to rcount; badpt(i) = rbch(i); rbch(i) = rbch(i) + roffset; end
      copy(4,0); replace; copy(0,4);
   end
   cboth
else
    badpt=0;
    for i = 1 to bch; badpt(i) = lbch(i); lbch(i) = lbch(i) + loffset; end
    REPLACE PAGE SHOW 
end
if (twoup) then
   if (ldamage) then
      print " Bad channels in scan # ", h3(scan)
      for i = 1 to lcount
         fprint(-1, "i6,x,$") lbch(i)
         if (mod(i,6)=0) then
            fprint(-1," ") " "
         end
      end
      if (mod((i-1),6)~=0) then; fprint(-1," ") " "; end
   end
   if (rdamage) then
      print " Bad channels in scan # ", h4(scan)
      for i = 1 to rcount
         fprint(-1, "i6,x,$") rbch(i)
         if (mod(i,6)=0) then
            fprint(-1," ") " "
         end
      end
      if (mod((i-1),6)~=0) then; fprint(-1," ") " "; end
   end
else
   PRINT " Bad channels in scan # ", h0(scan)
   for i = 1 to bch
      fprint(-1,"i6,x,$") lbch(i)
      if (mod(i,6)=0) then 
         fprint(-1," ") " " 
      end
   end
end
print " "
RETURN
FINISH
