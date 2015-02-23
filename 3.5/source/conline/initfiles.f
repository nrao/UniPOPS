      subroutine initfiles(first4)
C-------------------------------------------------------------------------------
C  @(#)initfiles.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Initializes files and checks for their existence.
c     Allows user to change the names of default files
c
c     If FIRST = 1 then will NOT ask for changes -- assumes the user
c	wants an initialization of files and a listing of the found
c	files.  If FIRST != 1 then no initialization and questions will
c	be asked.
c
c     RJM December, 1989
c
      include 'cio.inc'
c
      integer*2 inum, chngfile, n0, n9, n1, n14, lastblnk
      integer*4 first4
      integer*4 long, system, outputtemp, irtn
      character*64 filename
      character*15 answr1, answr2
c
      data n0, n9 /0, 9/, n1, n14 /1, 14/
c				ensure that sddinit is only called once
      if (first4 .eq. 1) call sddinit()
c
5     inum = 1
      filename = ' '
      answr2 = ' '
c
      outputtemp = iout
      iout = istdout
      call files
      iout = outputtemp
c     Call Files with STDIN and STDOUT as IO units and return these
c     units to the values they had presvious to the call to FILES
c
1     format(1x,10a)
c			the first time, don't offer any choices
      if (first4 .eq. 1) then
         write(istdout, 1) 'Use the CHNGFILE verb to change ',
     1        'an entry in this table.'
         write(istdout, 1) ' '
         return
      endif
c
      write(istdout,1) ' '
      write(istdout,1) 
     1 	'Enter one of the following commands: DIR, SUBTRACT, ',
     1  'CHANGE, CREATE, HELP'
      write(istdout,30) 'Or enter a <cr> to use these files: '
30    format(1x,a,$)
c
      call flush(long(istdout))
      read(istdin,40) answr1
40    format(a)
      write(istdout,1) ' '
c
      call uppercase(answr1,answr2)
c
      if (answr2 .eq. 'CHANGE' .or. answr2 .eq. 'CREATE' .or. 
     1    answr2 .eq. 'SUBTRACT') then
41	write(istdout,1) 'Enter number of file type you want to ',
     1		'alter; use the numbers in column 1 above'
	write(istdout,30) '# '
	call flush(long(istdout))
	read(iinunit,45,err=41) inum
45	format(i10)
      else if (answr2 .eq. 'HELP') then
	irtn = system('help.exe CHNGFILE')
        if (irtn .ne. 0) call oerror(n9, n0, 'CHNGFILE')
	goto 5
c
      else if(answr2 .eq. 'DIR') then
	irtn = system('ls.exe *')
        if (irtn .ne. 0) call oerror(n9, n0, 'CHNGFILE')
	goto 5
c
      else if (answr2 .eq. ' ') then
	goto 99
      else
	write(istderr,1) '***ERROR*** Bad command... Try again...'
	goto 5
      endif
c
46    if (answr2 .eq. 'CHANGE' .or. answr2 .eq. 'CREATE') then
	write(istdout,30) 'Enter name of file to add to table',
     1			  ' (60 characters max): '
	call flush(long(istdout))
	read(iinunit,40) filename
        if (lastblnk(filename) .gt. 60) call oerror(n14, n1, ' ')
      endif
c
      irtn = chngfile(answr2, inum, filename)
      goto 5
c
99    return
      end
c
