# Two functions to add a feed number to a scan number that
# work even when the scan number is negative.
procedure addfeed(scan_no, feed_inc)
#	add feed_inc to the existing feed number
#	taking into account the sign of scan_no
scalar ascan_no
ascan_no = abs(scan_no) + feed_inc/100
if (scan_no < 0) then; ascan_no = -ascan_no; end
return ascan_no
finish
#
procedure newfeed(scan_no, feed_no)
#	makes feed_no the new feed number of
#	scan_no, taking into account the sign of scan_no
scalar iscan_no, ascan_no
iscan_no = ifix(scan_no)
ascan_no = addfeed(iscan_no, feed_no)
return ascan_no
finish
