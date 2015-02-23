procedure hcdata
#  Change to Hybrid Spectrometer Data Stream, keep current version, set prompt.
#  Erroff is used so that spectype and prompt are set even if no online data
global scalar spectype
if (status(15) = TRUE) then
#	if online data is available try and switch to hctype
   erroff
   chngonline(hctype, hc_ver)
   erron
end
prompt = "LineH>"
spectype = hctype
line
return
finish
