procedure fbdata
#  Switch to Filter Bank Data Stream, leave file version as is, set prompt
#  Erroff is used so that this sets prompt and spectype even if no online data
global scalar spectype
if (status(15) = TRUE) then
#   if online data is available, try and change to fbtype
   erroff
   chngonline(fbtype, fb_ver)
   erron
end
prompt = "LineF>"
spectype = fbtype
histogram
return
finish
