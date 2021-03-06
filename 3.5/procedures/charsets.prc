proc charsets(fnt)
# prints out the characters from various fonts
#
pointer scl1 22
pointer scl2 11
string*28 upper, lower
string*32 special0
string*12 numbers
string*20 numbers_1
string*52 upper_1, lower_1
string*60 special0_1 
string special0_2
#
numbers = '1234567890'
upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ  '
lower = 'abcdefghijklmnopqrstuvwxyz  '
special0 = '!@#$%^&*()_+|-={}:"~<>?[];''`,./'
upper_1 = 'A B C D E F G H I J K L M N O P Q R S T U V W X Y Z '
lower_1 = 'a b c d e f g h i j k l m n o p q r s t u v w x y z '
special0_1 = '! @ # $ % ^ & * ( ) _ + | - = { } : " ~ < > ? [ ] ; '' ` , . '
special0_2 = '/   '
numbers_1 = '1 2 3 4 5 6 7 8 9 0 '
#
page 
charsize(scl1) fontset(fnt,true)
place(10,650) char(upper)
place(10,500) char(lower)
place(10,350) char(numbers)
place(10,200) char(special0)
#
charsize(scl2) fontset(1,true)
place(10+14*scl2/scl1,600) char(upper_1)
place(10+14*scl2/scl1,450) char(lower_1)
place(10+14*scl2/scl1,300) char(numbers_1)
place(10+14*scl2/scl1,150) char(special0_1) char(special0_2)
#
charsize(scl1) fontset(1,false) place(250,50)
if fnt = 1 then; char('(1) Roman Simplex');end
if fnt = 2 then; char('(2) Greek Simplex');end
if fnt = 3 then; char('(3) Greek Gothic');end
if fnt = 4 then; char('(4) Italic Gothic');end
if fnt = 5 then; char('(5) English Gothic');end
if fnt = 6 then; char('(6) Roman Duplex');end
if fnt = 7 then; char('(7) Cyrillic Complex');end
if fnt = 8 then; char('(8) Script Complex');end
if fnt = 9 then; char('(9) Roman Complex');end
if fnt = 10 then; char('(10) Italic Complex');end
if fnt = 11 then; char('(11) Greek Complex');end
if fnt = 12 then; char('(12) Roman Triplex');end
if fnt = 13 then; char('(13) Italic Triplex');end
if fnt = 14 then; char('(14) Special Symbols');end
if fnt = 15 then; char('(15) Script Simplex');end
#
return
finish


