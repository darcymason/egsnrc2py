
# EMPTY CALLBACKS ----



# CALLBACKS ---- 

def randomset():
    global rng_seed

    if rng_seed > 24:
        ranlux(rng_array)
        rng_seed = 1

    random_num = rng_array[rng_seed-1]
    rng_seed += 1

    return random_num


# ******************************************************************
#                                National Research Council of Canada
def SHOWER(IQI,EI,XI,YI,ZI,UI,VI,WI,IRI,WTI):
#                                                                   
# ******************************************************************

implicit none

# $ comin_shower # DEFAULT REPLACEMENT PRODUCES THE FOLLOWING:
                 # COMIN/DEBUG,STACK,UPHIOT,RANDOM/

# Input variables
;real*8 EI,      # initial shower energy
      XI,YI,ZI,# initial co-ordinates
      UI,VI,WI,# initial direction cosines
      WTI # initial weight

;integer*4
      IQI,     # initial particle charge
      IRI # initial region number

# Local variables
DOUBLE PRECISION
      DEG,    # energy for pi-zero option
      DPGL,   # angle factor for pi-zero option
      DEI,    # incident energy for pi-zero option
      DPI,    # intermediate factor for pi-zero option
      DCSTH,  # random number for pi-zero option
      DCOSTH, # cos(theta) for pi-zero option
      PI0MSQ # pi-zero mass squared (in MeV**2)

;real*8 DNEARI, # initial distance to closest boundary
      CSTH # random number for pi-zero option

;integer*4
      IRCODE # status returned by ELECTR or PHOTON

DATA PI0MSQ/1.8215416D4/ # PI-ZERO MASS (MEV) SQUARED

NP=1; NPold = NP # Set the old stack counter
DNEARI=0.0
IQ(1)=IQI; E(1)=EI; U(1)=UI; V(1)=VI; W(1)=WI
$TRANSFER PROPERTIES TO (1) FROM I

if IQI == 2:

     [# PI-ZERO OPTION
  # IF(EI <= PI0MSQ) [OUTPUT EI;    corrected Oct 24 1995 e-mail Hideo H 
  #                   noted by      Dr.  Muroyama at Nagoya University
  if EI**2 <= PI0MSQ:

     $egs_fatal('(//a/,a,g15.5,a)',
     ' Stopped in subroutine SHOWER---PI-ZERO option invoked',
     ' but the total energy was too small (EI=',EI,' MeV)')

  CSTH = randomset()
  DCSTH=CSTH; DEI=EI; DPI=DSQRT(DEI*DEI-PI0MSQ)
  DEG=DEI+DPI*DCSTH; DPGL=DPI+DEI*DCSTH; DCOSTH=DPGL/DEG
  COSTHE=DCOSTH; SINTHE=DSQRT(1.D0-DCOSTH*DCOSTH)
  IQ(1)=0; E(1)=DEG/2.
  CALL UPHI(2,1)
  NP=2
  DEG=DEI-DPI*DCSTH; DPGL=DPI-DEI*DCSTH; DCOSTH=DPGL/DEG
  COSTHE=DCOSTH; SINTHE=-DSQRT(1.D0-DCOSTH*DCOSTH)
  IQ(2)=0; E(2)=DEG/2.
  CALL UPHI(3,2)
]# end of pi-zero option

# The following convoluted logic is difficult to follow
# when one modifies the outcome of certain interactions
# using nbr_split, Russian Roulette, or one of the     
# particle selection macros. I'm simplifying it        
# so that ircode becomes irrelevant. IK, August 2002   
# :TOPSTACK:
# LOOP[
#    $ KERMA-INSERT 
#    if(iq[np] == 0) goto_PHOTON  is True
 break # XXX
#    LOOP [
#       :ELECTRON:
#       CALL ELECTR(IRCODE)
#       if(IRCODE == 2) EXIT 
#       :PHOTON:
#       CALL PHOTON(IRCODE)
#       if(IRCODE == 2) EXIT
#    ]REPEAT
#    IF(NP <= 0) EXIT
# ]REPEAT 

LOOP [
    
    if  np <= 0 :
    
         EXIT
 #  DEFAULT FOR $ KERMA-INSERT; IS ; (NULL) 
    if  iq[np] == 0 :
         call photon(ircode); 
    else:
         call electr(ircode); 

RETURN
# end of subroutine shower  END

