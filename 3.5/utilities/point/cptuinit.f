C
      SUBROUTINE UINIT
c
c     @(#)cptuinit.f	5.1 06/22/94
c
      include 'condappl.inc'
      include 'coneappl.inc'
      include 'concfmt.inc'
C                               common /CFMT/
      KSNO=1
      KOSN=2
      KSNA=5
      KONO=11
      KONA=12
      kjd =6
      KSDY=25
      KMON=26
      KDAY=27
      KYR =28
      KLST=15
      KEST=16
      KTEL=33
      KTOS=34
      KSTC=35
      KMD =36
      KIR =37
      KLR =38
      KIN =39
      KST =39
      KSP =41
      KVRF=48
      KVDF=49
      KPOC=50
      KDIR=51
      KSW1=55
      KSRT=29
      KEOP=32
      KHR =33
      KVR =34
      KIRA=35
      KIDC=36
      KFOC=37
      KORI=38
      KZEN=39
      KDOR=40
      KEV =99
      JSSN=52
      JERA=54
C               NOTE: NEXT THREE POINTERS ARE NOT THE SAME IN CONTROL
C                     AND REDUCTION SYSTEMS.
      JRHO=64
      JTHE=65
      JNT =66
      JDB =88
C               NOTE: NEXT 7 POINTERS ARE FOR REDUCTION ONLY
      JCV =67
      JCF =68
      JBEG=40
      JEND=41
      JMB =42
      JME =43
      JFE =53
C
      JH1=48
      JH2=49
      KHPF=43
      KVPF=44
      KP1=45
      KP2=46
      KP3=47
C
      IFULL=138
      BPCT=20
      EPCT=20
      GOK=0
c
      RETURN
      END
C
