
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
def MOLLER:
#                                                                   
# ******************************************************************
#    DISCRETE MOLLER SCATTERING (A CALL TO THIS ROUTINE) HAS BEEN   
#    ARBITRARILY DEFINED AND CALCULATED TO MEAN MOLLER SCATTERINGS  
#    WHICH IMPART TO THE SECONDARY ELECTRON SUFFICIENT ENERGY THAT  
#    IT BE TRANSPORTED DISCRETELY.  THE THRESHOLD TO TRANSPORT AN   
#    ELECTRON DISCRETELY IS A TOTAL ENERGY OF AE OR A KINETIC ENERGY
#    OF TE=AE-RM.  SINCE THE KINETIC ENERGY TRANSFER IS ALWAYS, BY  
#    DEFINITION, LESS THAN HALF OF THE INCIDENT KINETIC ENERGY, THIS
#    IMPLIES THAT THE INCIDENT ENERGY, EIE, MUST BE LARGER THAN     
#    THMOLL=TE*2+RM.  THE REST OF THE COLLISION CONTRIBUTION IS     
#    SUBTRACTED CONTINUOUSLY FROM THE ELECTRON AS IONIZATION        
#    LOSS DURING TRANSPORT.                                         
# ******************************************************************

implicit none

# $ comin_moller # DEFAULT REPLACEMENT PRODUCES THE FOLLOWING:
                 #   COMIN/EGS-VARIANCE-REDUCTION, DEBUG,STACK,THRESH,
                                          # UPHIOT,USEFUL,RANDOM/
# ;comin/#,#/

# $ define_local_variables_moller

;real*8 sigm,pbrem,rsh,Uj,sig_j
;integer*4 lelke,iele,ish,nsh,ifirst,i,jj,iZ,iarg

# IRCODE=1;  appears to be unused, IK Oct 97
NPold = NP # Set the old stack counter
PEIE=e[np] # PRECISE ENERGY OF INCIDENT ELECTRON
EIE=PEIE # ENERGY OF INCIDENT ELECTRON
PEKIN=PEIE-PRM # PRECISE K.E. OF INCIDENT ELECTRON
EKIN=PEKIN

if  eii_flag > 0 and eii_nsh[medium] > 0 :

    # The EII flag is set and this medium has shells for which we want to
    # simulate EII => sample if the interaction is with a EII shell
    # Unhandled macro '$ SET INTERVAL elke,eke;'
     sigm = esig1[Lelke,MEDIUM]*elke+ esig0[Lelke,MEDIUM]  # EVALUATE sigm USING esig(elke)
     pbrem = ebr11[Lelke,MEDIUM]*elke+ ebr10[Lelke,MEDIUM]  # EVALUATE pbrem USING ebr1(elke)
    sigm = sigm*(1 - pbrem)
    rsh = randomset() rsh = sigm*rsh
    DO iele=1,nne[medium] [
        iZ = int(zelem(medium,iele)+0.5)
        nsh = eii_no(medium,iele)
        if  nsh > 0 :

            ifirst == eii_first(medium,iele)
            DO ish = 1,nsh [
                Uj = binding_energies(ish,iZ)
                if  ekin > Uj and (Uj > te[medium] or Uj > ap[medium]) :

                    jj = ifirst + ish - 1
                    i = eii_a(jj)*elke + eii_b(jj) + (jj-1)*N_EII_BINS
                    sig_j = eii_xsection_a(i)*elke + eii_xsection_b(i)
                    sig_j = sig_j*pz(medium,iele)*eii_cons[medium]
                    rsh = rsh - sig_j
                    if  rsh < 0 :

IARG=EIIB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
                        call eii_sample(ish,iZ,Uj)
IARG=EIIA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
                        return






if  ekin <= 2*te[medium] :
     return
T0=EKIN/RM
E0=T0+1.0
EXTRAE = EIE - THMOLL[medium]
E02=E0*E0
# BETAI2=E02/(E02-1.0) # BLIF 96/2/1 -- not needed for Moller fix-up
EP0=TE[medium]/EKIN
# G1=(1.-2.*EP0)*BETAI2 # BLIF 96/2/1 -- not needed for Moller fix-up
G2=T0*T0/E02
G3=(2.*T0+1.)/E02
#    H.H.NAGEL HAS CONSTRUCTED A FACTORIZATION OF THE FREQUENCY
#    DISTRIBUTION FUNCTION FOR THE MOLLER DIFFERENTIAL CROSS
#    SECTION USED AS SUGGESTED BY BUTCHER AND MESSEL.
#    (H.H.NAGEL, OP.CIT., P. 53-55)                                 
#    HOWEVER, A MUCH SIMPLER SAMPLING METHOD WHICH DOES NOT BECOME  
#    VERY INEFFICIENT NEAR THMOLL IS THE FOLLOWING. . .             
#    LET BR=EKS/EKIN,  WHERE EKS IS KINETIC ENERGY TRANSFERED TO THE
#    SECONDARY ELECTRON AND EKIN IS THE INCIDENT KINETIC ENERGY.    

#    MODIFIED (7 FEB 1974) TO USE THE TRUE MOLLER CROSS SECTION.
#    THAT IS, INSTEAD OF THE E+ E- AVERAGE GIVEN IN THE ROSSI
#    FORMULA USED BY NAGEL.  THE SAMPLING SCHEME IS THAT
#    USED BY MESSEL AND CRAWFORD (EPSDF 1970 P.13)
#    FIRST SAMPLE (1/BR**2) OVER (TE/EKIN,1/2) . . .            

GMAX=(1.+1.25*G2) # BLIF 96/2/1 -- Moller fix-up
LOOP  [  #  TO RETRY IF REJECTED
    RNNO27 = randomset()
    BR = TE[medium]/(EKIN-EXTRAE*RNNO27)

    #    USE MESSEL AND CRAWFORDS REJECTION FUNCTION.
    R=BR/(1.-BR)
    RNNO28 = randomset()
    REJF4=# G1*(1.+G2*BR*BR+R*(R-G3)) # BLIF 96/2/1 -- Moller fix-up
    RNNO28=GMAX*RNNO28 # BLIF 96/2/1 -- Moller fix-up
]UNTIL RNNO28.LE.REJF4 # TRY UNTIL ACCEPTED. END REJECTION LOOP

PEKSE2=BR*EKIN # PRECISE KINETIC ENERGY OF SECONDARY ELECTRON #2
PESE1=PEIE-PEKSE2 # PRECISE ENERGY OF SECONDARY ELECTRON #1
PESE2=PEKSE2+PRM # PRECISE ENERGY OF SECONDARY ELECTRON #2
ESE1=PESE1 # ENERGY OF SECONDARY ELECTRON 1
ESE2=PESE2 # ENERGY OF SECONDARY ELECTRON 2
e[np]=PESE1
# --- Inline replace: $ CHECK_STACK(np+1,'MOLLER'); -----
if check_stack:
    <XXX> = check_stack(np+1, 'MOLLER')
else:
    
      if  np+1 > MXSTACK :

          $egs_fatal('(//,3a,/,2(a,i9))',' In subroutine ','MOLLER',
              ' stack size exceeded! ',' $MAXSTACK = ',MXSTACK,' np = ',np+1)

# End inline replace: $ CHECK_STACK(np+1,'MOLLER'); ----
E(NP+1)=PESE2
#    SINCE BR.LE.0.5, E(NP+1) MUST BE .LE. e[np]. 
#    MOLLER ANGLES ARE UNIQUELY DETERMINED BY KINEMATICS            

#  One possible way of dealing with double counting of angular      
#  deflections in inelastic scattering would be to                  
#  not deflect the 'old' electron as these deflections are          
#  already taken into account in the multiple elastic scattering    
#  This approach has the disadvantage of loosing correlations       
#  between big energy losses and strong angular deflections         
#  The advantage of such an approach is its simplicity.             
#  If spin effects for multiple elastic scattering are turned on,   
#  the double counting is taken into account by the appropriate     
#  modification of the scattering power (which depends on AE)       
#                                                                   
#                                                                   
#  IK, June 1999                                                    

H1=(PEIE+PRM)/PEKIN
#    DIRECTION COSINE CHANGE FOR 'OLD' ELECTRON                     
DCOSTH=H1*(PESE1-PRM)/(PESE1+PRM)
SINTHE=DSQRT(1.D0-DCOSTH)
COSTHE=DSQRT(DCOSTH)

# sinthe = 0; costhe = 1; <- this will turn off the Moller ang. deflections

CALL UPHI(2,1)

#    RELATED CHANGE AND (X,Y,Z) SETUP FOR 'NEW' ELECTRON            
NP=NP+1
iq[np]=-1
DCOSTH=H1*(PESE2-PRM)/(PESE2+PRM)
SINTHE=-DSQRT(1.D0-DCOSTH)
COSTHE=DSQRT(DCOSTH)
CALL UPHI(3,2)
RETURN
# END OF def MOLLER  END:
