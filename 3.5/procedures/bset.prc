PROCEDURE BSET
#  Uses cursor to set baseline fitting regions into the NREGION array
# regions can be entered in any order, if overlap, print a warning
global scalar nseg
scalar i, blt, x1, x2, xtmp, j, tseg, continue, k, overlap
array ntmp(32)
nregion = 0
fprint(-1,"a,$") "Enter # of Baseline regions, maximum of 16 : "
fread(-1,"i") nseg
if (nseg <= 0 | nseg >16) then
   print "Illegal value : ",nseg
   return
end
FOR I = 1 TO NSEG; 
   fprint(-1,"x,a,x,i2,x,a") "Indicate ends of region",I,"using the cursor"
   X1=ifix(CCUR); x2 = ifix(ccur) 
   if (x2 < x1) then 
      xtmp = x1; x1 = x2; x2 = xtmp
   end
   nregion(i*2-1)=X1; nregion(i*2) = x2
END
#  individual regions are ordered correctly, now need to order the whole array
#  bridge hand sort of nregion into ntmp
tseg = 1; ntmp(1) = nregion(1); ntmp(2) = nregion(2)
#  pops loops ALWAYS occur at least once, need to make sure its appropriate
if (nseg >= 2) then
   for i = 2 to nseg
      xtmp = nregion(i*2-1)
      continue = TRUE; j = 0
      while (continue)
         j = j + 1
         if (j <= tseg) then
            if (xtmp < ntmp(j*2-1)) then; continue = FALSE; end
         else
            continue = FALSE
         end
      end
#     it belongs at location j, move ntmp up one and insert
      if (tseg >= j) then
         for k = tseg to j by -1
            ntmp(k*2+1) = ntmp(k*2-1); ntmp(k*2+2) = ntmp(k*2)
         end
      end
      ntmp(j*2-1) = nregion(i*2-1); ntmp(j*2) = nregion(i*2)
      tseg = tseg + 1
   end
#   copy it back to nregion, check for overlap
   overlap = FALSE
   nregion(1) = ntmp(1)
   for i = 2 to nseg*2
      nregion(i) = ntmp(i)
      if (nregion(i) < nregion(i-1)) then; overlap = TRUE; end
   end
   if (overlap) then;print 'BSET WARNING: some of the regions overlap'; end
end
return
FINISH
