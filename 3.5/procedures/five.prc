PROCEDURE FIVE(xs)
#  Analyse a new-style 12m 5-point pointing scan.  
#  Retrieves data into array 0, but uses array #9 & array #8 as scratch
#  This version reorders the data into the earlier style before calculating.
#
# Old style, assuming "2 repeats":
#	  	North:  off-on-on-off  off-on-on-off
#		South:	off-on-on-off  off-on-on-off
#		Centre:	off-on-on-off  off-on-on-off
#		East:  	off-on-on-off  off-on-on-off
#		West:  	off-on-on-off  off-on-on-off
#
# New style, assuming "2 repeats":
#	  	North:  off-on-on-off  
#		South:	off-on-on-off  
#		Centre:	off-on-on-off  
#		East:  	off-on-on-off  
#		West:  	off-on-on-off  
#
#	  	North:  off-on-on-off  
#		South:	off-on-on-off  
#		Centre:	off-on-on-off  
#		East:  	off-on-on-off  
#		West:  	off-on-on-off  
#
###########################################################################
####### The following HAS NOT been implemented yet, but may be one day #####
###########################################################################
# Newer (maybe May 1993) style, assuming "2 repeats":
#		OFF:	N,S,C,E,W
#		ON:	N,S,C,E,W
#		ON:	N,S,C,E,W
#		OFF:	N,S,C,E,W
#
#		OFF:	N,S,C,E,W
#		ON:	N,S,C,E,W
#		ON:	N,S,C,E,W
#		OFF:	N,S,C,E,W
#
################################################################
SCALAR NP1
SCALAR NP5
SCALAR I,j,k
scalar sample,position,repeat,oldsamp,nrepeats  
GET XS
SWITCHED
copy(0,9)
NP5=H0(NOINT)
NP1=NP5/5
if compare(h0(obsmode),'CONTQK5 ') then
#  reorder from the new way into the old style
#  a sample is one set of off-on-on-off data
  nrepeats=np5/20
  for sample=1 to np5/4
     position=mod(sample-1,5)+1 
     repeat=ifix((sample-1)/5)+1 
     oldsamp=(position-1)*nrepeats+repeat 
     for k=1 to 4
       d9((oldsamp-1)*4+k)=d0((sample-1)*4+k)
     end
  end
  COPY(9,8)
else
  copy(0,8)
end  
h0(noint)=np1
h8(noint)=10
#              NP1 is the number of points per position on the 5-point
#
#   For each of the 5 subscans, copy that subset of data from D9 into D0,
#   Derive the required amplitudes, and put those into D8
FOR I = 1 TO 5
    FOR J = 1 TO NP1
      d0(j)=d9(j+(i-1)*np1)
#            copy current "subscan" integration to head of data array
    END
  AVGD
  d8(i)=h0(tsource)
  d8(5+i)=h0(trms)
#				Using D8 as scratch area
END
#  Now copy D8 back into D0
COPY(8,0)
PAGE 
PDOC
place(0,25)
RETURN
FINISH
