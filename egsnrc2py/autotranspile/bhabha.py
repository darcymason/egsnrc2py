
# EMPTY CALLBACKS ----
check_stack = None


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
def BHABHA:
#                                                                   
# ******************************************************************
#    DISCRETE BHABHA SCATTERING (A CALL TO THIS ROUTINE) HAS BEEN   
#    ARBITRARILY DEFINED AND CALCULATED TO MEAN BHABHA SCATTERINGS  
#    WHICH IMPART TO THE SECONDARY ELECTRON SUFFICIENT ENERGY THAT  
#    IT BE TRANSPORTED DISCRETELY, I.E. E=AE OR T=TE.  IT IS NOT    
#    GUARANTEED THAT THE FINAL POSITRON WILL HAVE THIS MUCH ENERGY  
#    HOWEVER.  THE EXACT BHABHA DIFFERENTIAL CROSS SECTION IS USED. 
# ******************************************************************

implicit none

# $ comin_bhabha # DEFAULT REPLACEMENT PRODUCES THE FOLLOWING:      
                   # COMIN/DEBUG,EGS-VARIANCE-REDUCTION,STACK,
                                # THRESH,UPHIOT,USEFUL,RANDOM/

# $ define_local_variables_bhabha

NPold = NP # Set the old stack counter
PEIP=e[np] # PRECISE ENERGY OF INCIDENT POSITRON
EIP=PEIP # ENERGY OF INCIDENT POSITRON
PEKIN=PEIP-PRM # PRECISE K.E. OF INCIDENT POSITRON
EKIN=PEKIN
T0=EKIN/RM
E0=T0+1.
YY=1./(T0+2.)
E02=E0*E0
# BETAI2=E02/(E02-1.) # BLIF 96/2/1 -- not needed for Bhabha fix-up
BETA2=(E02-1.)/E02 # BLIF 96/2/1 -- needed for Bhabha fix-up
EP0=TE[medium]/EKIN
EP0C=1.-EP0
Y2=YY*YY
YP=1.-2.*YY
YP2=YP*YP
B4=YP2*YP
B3=B4+YP2
B2=YP*(3.+Y2)
B1=2.-Y2
#    SAMPLE BR FROM MINIMUM(EP0) TO 1.
LOOP[RNNO03 = randomset()
BR=EP0/(1.-EP0C*RNNO03)
#    APPLY REJECTION FUNCTION
RNNO04 = randomset()
# REJF2=EP0C*(BETAI2-BR*(B1-BR*(B2-BR*(B3-BR*B4))));BLIF 96/2/1 -- Bhabha fix-up
REJF2=(1.0-BETA2*BR*(B1-BR*(B2-BR*(B3-BR*B4)))) # BLIF 96/2/1 -- Bhabha fix-up
]UNTIL RNNO04.LE.REJF2 
#    IF E- GOT MORE THAN E+, MOVE THE E+ POINTER AND REFLECT B
# --- Inline replace: $ CHECK_STACK(np+1,'BHABHA'); -----
if check_stack:
    <XXX> = check_stack(np+1, 'BHABHA')
else:
    
      if  np+1 > MXSTACK :

          $egs_fatal('(//,3a,/,2(a,i9))',' In subroutine ','BHABHA',
              ' stack size exceeded! ',' $MAXSTACK = ',MXSTACK,' np = ',np+1)

# End inline replace: $ CHECK_STACK(np+1,'BHABHA'); ----
if BR.LT.0.5)[IQ(NP+1)=-1;]ELSE[iq[np]=-1;IQ(NP+1:
    =1;BR=1.-BR;]
# THE ABOVE PUTS E+ ON TOP OF STACK IF IT HAS LESS ENERGY
#    DIVIDE UP THE ENERGY
BR=max(BR,0.0) # AVOIDS POSSIBLE NEGATIVE NUMBER DUE TO ROUND-OFF
PEKSE2=BR*EKIN # PRECISE KINETIC ENERGY OF SECONDARY 'ELECTRON' 2
PESE1=PEIP-PEKSE2 # PRECISE ENERGY OF SECONDARY 'ELECTRON' 1
PESE2=PEKSE2+PRM # PRECISE ENERGY OF SECONDARY 'ELECTRON' 2
ESE1=PESE1
ESE2=PESE2
e[np]=PESE1
E(NP+1)=PESE2
#    BHABHA ANGLES ARE UNIQUELY DETERMINED BY KINEMATICS
H1=(PEIP+PRM)/PEKIN
#    DIRECTION COSINE CHANGE FOR 'OLD' ELECTRON

# AFB modified the following statement 92/10/28 to avoid
# numerical difficulties
# DCOSTH=H1*(PESE1-PRM)/(PESE1+PRM)
DCOSTH=min(1.0D0,H1*(PESE1-PRM)/(PESE1+PRM))

SINTHE=DSQRT(1.D0-DCOSTH)
COSTHE=DSQRT(DCOSTH)
CALL UPHI(2,1)
NP=NP+1
DCOSTH=H1*(PESE2-PRM)/(PESE2+PRM)
SINTHE=-DSQRT(1.D0-DCOSTH)
COSTHE=DSQRT(DCOSTH)
CALL UPHI(3,2)
RETURN
# END OF def BHABHA  END: