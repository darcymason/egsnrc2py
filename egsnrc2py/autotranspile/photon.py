
# EMPTY CALLBACKS ----
call_howfar_in_photon = None
electron_region_change = None
photon_region_change = None
photonuc_correction = None
photonuclear = None
rayleigh_correction = None
rayleigh_scattering = None
select_photon_mfp = None
start_new_particle = None


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
def PHOTON(IRCODE):
#                                                                   
# ******************************************************************

implicit none

;integer*4 IRCODE # 1 => normal return

# $ comin_photon # default replacement produces the following:
                 # COMIN/DEBUG,BOUNDS,MEDIA,MISC,EPCONT,PHOTIN,STACK,THRESH,
                 #   UPHIOT,USEFUL,USER,RANDOM,EGS-VARIANCE-REDUCTION/

# $ define_local_variables_photon

IRCODE=1 # set up normal return
PEIG=e[np]
EIG=PEIG # energy of incident gamma
IRL=ir[np]
# --- Inline replace: $ start_new_particle; -----
if start_new_particle:
    start_new_particle()
else:
    medium = med[irl] 
# End inline replace: $ start_new_particle; ----

if EIG <= PCUT[irl]:

    goto_PCUT_DISCARD = True
    break # XXX

if goto_PNEWENERGY:  XXX
LOOP[# enter this loop for each photon with new energy

    if wt[np] == 0.0:

         [  # added May 01
        goto_USER_PHOTON_DISCARD = True
        break # XXX

    GLE=log(EIG) # GLE IS GAMMA LOG ENERGY

    #    here to sample no. mfp to transport before interacting

    # --- Inline replace: $ SELECT_PHOTON_MFP; -----
    if select_photon_mfp:
        select_photon_mfp()
    else:
    
        RNNO35 = randomset() if(RNNO35 == 0.0) [RNNO35=1.E-30;]
          DPMFP=-log(RNNO35)
    # End inline replace: $ SELECT_PHOTON_MFP; ----
    #  DEFAULT FOR $ SELECT-PHOTON-MFP; IS:  $ RANDOMSET RNNO35
    #                                       DPMFP=-log(RNNO35)
    # NOTE:  THIS TEMPLATE CAN ALSO BE OVER-RIDDEN BY OTHER SCHEMES,
    #        SUCH AS THE 'EXPONENTIAL TRANSFORM' TECHNIQUE.

    IROLD=ir[np] # INITIALIZE PREVIOUS REGION

    :PNEWMEDIUM:
    LOOP[# HERE EACH TIME WE CHANGE MEDIUM DURING PHOTON TRANSPORT
        if MEDIUM != 0:

            # Unhandled macro '$ SET INTERVAL GLE,GE;'# SET PWLF INTERVAL
         GMFPR0 = GMFP1[LGLE,MEDIUM]*GLE+ GMFP0[LGLE,MEDIUM]  # EVALUATE GMFPR0 USING GMFP(GLE)

        :PTRANS:
        LOOP[# PHOTON TRANSPORT LOOP
            if MEDIUM == 0:

                TSTEP=VACDST
            else:
RHOF=RHOR[irl]/RHO[medium] # DENSITY RATIO SCALING TEMPLATE
                GMFP=GMFPR0/RHOF
                # --- Inline replace: $ RAYLEIGH_CORRECTION; -----
                if rayleigh_correction:
                    rayleigh_correction()
                else:
    
                     ;if(IRAYLR[irl] == 1) [ COHFAC == COHE1[LGLE,MEDIUM]*GLE+ COHE0[LGLE,MEDIUM]  # EVALUATE COHFAC USING COHE(GLE)
                    GMFP=GMFP*COHFAC]
                # End inline replace: $ RAYLEIGH_CORRECTION; ----  # A RAYLEIGH SCATTERING TEMPLATE
                # Ali:photonuc, 1 line
                # --- Inline replace: $ PHOTONUC_CORRECTION; -----
                if photonuc_correction:
                    photonuc_correction()
                else:
    
                     ;if(IPHOTONUCR[irl] == 1) [ PHOTONUCFAC == PHOTONUC1[LGLE,MEDIUM]*GLE+ PHOTONUC0[LGLE,MEDIUM]  # EVALUATE PHOTONUCFAC USING PHOTONUC(GLE)
                    GMFP=GMFP*PHOTONUCFAC]
                # End inline replace: $ PHOTONUC_CORRECTION; ----  # A PHOTONUCLEAR TEMPLATE
                TSTEP=GMFP*DPMFP

            #    SET DEFAULT VALUES FOR FLAGS SENT BACK FROM USER
            IRNEW=ir[np] # SET DEFAULT NEW REGION NUMBER
            IDISC=0 # ASSUME PHOTON NOT DISCARDED
            USTEP=TSTEP # TRANSFER TRANSPORT DISTANCE TO USER VARIABLE
            TUSTEP=USTEP

            # IF (USTEP.GT.dnear[np]) [;CALL HOWFAR;]
            # --- Inline replace: $ CALL_HOWFAR_IN_PHOTON; -----
            if call_howfar_in_photon:
                call_howfar_in_photon()
            else:
                
                  if  ustep > dnear[np] or wt[np] <= 0 :
                       call howfar 
            # End inline replace: $ CALL_HOWFAR_IN_PHOTON; ---- # The above is the default replacement

            #    NOW CHECK FOR USER DISCARD REQUEST
            if IDISC.GT.0:
                [# USER REQUESTED IMMEDIATE DISCARD
                goto_USER_PHOTON_DISCARD = True
                break # XXX

            VSTEP=USTEP # SET VARIABLE FOR OUTPUT CODE
            TVSTEP=VSTEP
            EDEP=PZERO # NO ENERGY DEPOSITION ON PHOTON TRANSPORT

            x_final = x[np] + u[np]*vstep
            y_final = y[np] + v[np]*vstep
            z_final = z[np] + w[np]*vstep

IARG=TRANAUSB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]

            #    TRANSPORT THE PHOTON
            x[np] = x_final
            y[np] = y_final
            z[np] = z_final
            dnear[np]=dnear[np]-USTEP # DEDUCT FROM DISTANCE TO NEAREST BOUNDARY
            if MEDIUM != 0:

                DPMFP=max(0.,DPMFP-USTEP/GMFP) # DEDUCT MFP'S

            IROLD=ir[np] # SAVE PREVIOUS REGION

            MEDOLD=MEDIUM
            if IRNEW != IROLD:
                 [# REGION CHANGE
                # --- Inline replace: $ photon_region_change; -----
                if photon_region_change:
                    photon_region_change()
                else:
                    # --- Inline replace: $ electron_region_change; -----
                    if electron_region_change:
                        electron_region_change()
                    else:
        
                        ir[np] = irnew; irl = irnew; medium = med[irl]
                    # End inline replace: $ electron_region_change; ---- 
                # End inline replace: $ photon_region_change; ----

            #    AFTER TRANSPORT CALL TO USER
IARG=TRANAUSA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
            # oct 31 bug found by C Ma. PCUT discard now after AUSGAB call
            if EIG.LE.PCUT[irl]:

                goto_PCUT_DISCARD = True
                break # XXX

            #    NOW CHECK FOR DEFERRED DISCARD REQUEST.  MAY HAVE BEEN SET
            #    BY EITHER HOWFAR, OR ONE OF THE TRANSPORT AUSGAB CALLS
            if IDISC.LT.0:
                
                goto_USER_PHOTON_DISCARD = True
                break # XXX

            if MEDIUM != MEDOLD:

                
                EXIT :PTRANS:

            if MEDIUM != 0.AND.DPMFP.LE.EPSGMFP:

                [# TIME FOR AN INTERACTION
                EXIT :PNEWMEDIUM:

        ]REPEAT # :PTRANS: LOOP

    ]REPEAT # :PNEWMEDIUM: LOOP


    #    IT IS FINALLY TIME TO INTERACT.
    #    THE FOLLOWING MACRO ALLOWS ONE TO INTRODUCE RAYLEIGH SCATTERING
    # --- Inline replace: $ RAYLEIGH_SCATTERING; -----
    if rayleigh_scattering:
        rayleigh_scattering()
    else:
    
        ;if(IRAYLR[irl] == 1) [
        RNNO37 = randomset()
        if RNNO37.LE.(1.0-COHFAC):

        IARG=RAYLAUSB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        NPold = NP
        call egs_rayleigh_sampling(MEDIUM,e[np],GLE,LGLE,COSTHE,SINTHE)
        CALL UPHI(2,1)
        IARG=RAYLAUSA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        goto_PNEWENERGY = True
        break # XXX;]]
    # End inline replace: $ RAYLEIGH_SCATTERING; ----
    # Ali:photonuclear, 1 line
    # --- Inline replace: $ PHOTONUCLEAR; -----
    if photonuclear:
        photonuclear()
    else:
    
        ;if(IPHOTONUCR[irl] == 1) [
           RNNO39 = randomset()
           if RNNO39.LE.(1.0-PHOTONUCFAC):

        IARG=PHOTONUCAUSB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
             call PHOTONUC
        IARG=PHOTONUCAUSA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
             goto_PNEWENERGY = True
             break # XXX


    # End inline replace: $ PHOTONUCLEAR; ----
    RNNO36 = randomset() # THIS RANDOM NUMBER DETERMINES WHICH INTERACTION
    #    GBR1=PAIR/(PAIR+COMPTON+PHOTO)=PAIR/GTOTAL
     GBR1 = GBR11[LGLE,MEDIUM]*GLE+ GBR10[LGLE,MEDIUM]  # EVALUATE GBR1 USING GBR1(GLE)
    if (RNNO36.LE.GBR1).AND.(e[np].GT.RMT2) :
        [# IT WAS A PAIR PRODUCTION
IARG=PAIRAUSB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        CALL PAIR
        #    THE FOLLOWING MACRO ALLOWS THE USER TO CHANGE THE PARTICLE
        #    SELECTION SCHEME (E.G., ADDING IMPORTANCE SAMPLING (SPLITTING, 
        #    LEADING PARTICLE SELECTION, ETC.)).
        #    (DEFAULT MACRO IS TEMPLATE '$ PARTICLE-SELECTION-PHOTON'  
        #    WHICH IN TURN HAS THE 'NULL' REPLACEMENT ';') 


IARG=PAIRAUSA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        if  iq[np] != 0 :

            EXIT :PNEWENERGY:
        else:
            goto_PAIR_ELECTRONS_KILLED = True
            break # XXX


    # GBR2=(PAIR+COMPTON)/GTOTAL
     GBR2 = GBR21[LGLE,MEDIUM]*GLE+ GBR20[LGLE,MEDIUM]  # EVALUATE GBR2 USING GBR2(GLE)
    if RNNO36.LT.GBR2:
        [# IT WAS A COMPTON
IARG=COMPAUSB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        CALL COMPT
        #    THE FOLLOWING MACRO ALLOWS THE USER TO CHANGE THE PARTICLE
        #    SELECTION SCHEME (E.G., ADDING IMPORTANCE SAMPLING (SPLITTING, 
        #    LEADING PARTICLE SELECTION, ETC.)).
        #    (DEFAULT MACRO IS TEMPLATE '$ PARTICLE-SELECTION-PHOTON'  
        #    WHICH IN TURN HAS THE 'NULL' REPLACEMENT ';') 


IARG=COMPAUSA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        if iq[np] != 0# NOT PHOTON:
            
            EXIT:PNEWENERGY:
    else:
IARG=PHOTOAUSB ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        CALL PHOTO
        #    THE FOLLOWING MACRO ALLOWS THE USER TO CHANGE THE PARTICLE
        #    SELECTION SCHEME (E.G., ADDING IMPORTANCE SAMPLING (SPLITTING, 
        #    LEADING PARTICLE SELECTION, ETC.)).
        #    (DEFAULT MACRO IS TEMPLATE '$ PARTICLE-SELECTION-PHOTON'  
        #    WHICH IN TURN HAS THE 'NULL' REPLACEMENT ';') 


        if NP == 0 or NP < NPOLD :

            RETURN

        # The above may happen if Russian Roulette is on
        # NP<NPOLD means that only electrons were created in the interaction
        # and that all of them were killed. Hence, the top particle on the 
        # stack is from a previous interaction and may be in another region
        # To avoid problems with the :PNEWENERGY: loop logic, we simply force
        # a return to shower so that ELECTR or PHOTON are properly re-entered.
        # Changed by IK Dec. 21 2006 after D. Rogers and R. Taylor found a
        # wrong dose with brems splitting and Russian Roulette on in a low 
        # energy calculation.

IARG=PHOTOAUSA ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
        if iq[np] != 0 :
            
            EXIT :PNEWENERGY:
    ] # END OF PHOTO ELECTRIC BLOCK

    :PAIR_ELECTRONS_KILLED:

    #    IF HERE, THEN GAMMA IS LOWEST ENERGY PARTICLE.
    PEIG=e[np]
    EIG=PEIG
    if EIG.LT.PCUT[irl]:
        
        goto_PCUT_DISCARD = True
        break # XXX
]REPEAT # :PNEWENERGY: LOOP

#    IF HERE, MEANS ELECTRON TO BE TRANSPORTED NEXT
RETURN

# ---------------------------------------------
# PHOTON CUTOFF ENERGY DISCARD SECTION         
# ---------------------------------------------
if goto_PCUT_DISCARD:  XXX
if  medium > 0 :

    if EIG.GT.AP[medium]:

        IDR=EGSCUTAUS
    else:
        IDR=PEGSCUTAUS

else:
    IDR=EGSCUTAUS

EDEP=PEIG # GET ENERGY DEPOSITION FOR USER
;IARG=IDR ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);] 
IRCODE=2
NP=NP-1
RETURN

# ---------------------------------------------
# User requested photon discard section        
# ---------------------------------------------
if goto_USER_PHOTON_DISCARD:  XXX
EDEP=PEIG
IARG=USERDAUS ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
IRCODE=2
NP=NP-1
RETURN

# END OF def PHOTON  END:
