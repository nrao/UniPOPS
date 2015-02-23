procedure annotate
string*60 note
#       place a string on the graphics screen
print "Enter the text you wish to appear on the graphics screen."
print "It must be surrounded by quotation marks"
read note
print "Using the cursor, click where you would like this to appear"
click
place(xclick, yclick)
char(note)
return
finish
