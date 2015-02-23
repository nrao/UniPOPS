PROCEDURE LDISK(BSC,ESC)
#gets a range of scans and does a "DOC" on each of them, usefull table results.
scalar i
  PFLAG=FALSE
#  First time round, print title line.
  FOR I = BSC TO ESC; GET I ; DOC END;
  RETURN
FINISH
