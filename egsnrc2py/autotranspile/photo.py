
# EMPTY CALLBACKS ----
select_photoelectron_direction = None


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
def PHOTO:
# ******************************************************************
#  Programmers:  I. Kawrakow, complete recoding,                    
#                             Fluorescent X-rays, Auger,            
#                             Coster-Kronig treated in RELAX        
#                A.F. Bielajew (NRC) photoelectric angular distn    
# ******************************************************************

implicit none

# $ comin_photo # default replacement is:
              # COMIN/BOUNDS,DEBUG,EDGE,EGS-VARIANCE-REDUCTION,EPCONT,
                  # MEDIA,PHOTIN,RANDOM,STACK,UPHIOT,USEFUL/

# $ define_variables_for_select_photoelectron_direction
# $ define_local_variables_photo
/* ;real*8 ftot,iprob; */
data n_warning/0/

if  mcdf_pe_xsections :

    call egs_shellwise_photo()
    return

NPold = NP # Set the old stack counter
PEIG=e[np]
irl = ir[np]
if  peig < edge_energies(2,1) :

    if  n_warning < 100 :

        n_warning = n_warning + 1
        $egs_info(*,' Subroutine PHOTO called with E = ',peig,
                   ' which is below the current min. energy of 1 keV! ')
        logger.info(' Converting now this photon to an electron, ')
        logger.info(' but you should check your code! ')

    iq[np] = -1
    e[np] = peig + prm
    return

iZ = iedgfl[irl]
do_relax = False
edep = pzero
if  iedgfl[irl] != 0 :
     [   #  User requested atomic relaxations 
                           #  first sample the element 
    if  nne[medium] == 1 :

        iZ = int( zelem(medium,1) + 0.5 )
        DO j=1,edge_number(iZ) [
            if  peig >= edge_energies(j,iZ) :
                 EXIT

    else:
        aux = peig*peig; aux1 = aux*peig; aux = aux*Sqrt(peig)
        sigtot = 0
        DO k=1,nne[medium] [
            iZ = int( zelem(medium,k) + 0.5 )
            if  iZ < 1 or iZ > MXELEMENT :

                logger.info(' Error in PHOTO: ')
                $egs_fatal(*,'   Atomic number of element ',k,
                   ' in medium ',medium,' is not between 1 and ',MXELEMENT)

            if  peig > edge_energies(1,iZ) :

                j = 1
                sigma = (edge_a(1,iZ) + edge_b(1,iZ)/peig +
                     edge_c(1,iZ)/aux + edge_d(1,iZ)/aux1)/peig
            else:
                DO j=2,edge_number(iZ) [
                    if  peig >= edge_energies(j,iZ) :

                        EXIT


                sigma = edge_a(j,iZ) + gle*(edge_b(j,iZ) + gle*(edge_c(j,iZ) +
                        gle*edge_d(j,iZ) ))
                sigma = Exp(sigma)

            sigma = sigma * pz(medium,k)
            sigtot = sigtot + sigma
            probs(k) = sigma
            ints(k) = j

     br = randomset()
        br = br*sigtot
        DO k=1,nne[medium] [
            br = br - probs(k)
            if  br <= 0 :

                EXIT


        iZ = int( zelem(medium,k) + 0.5 )
        j  = ints(k)

    #  Now we know the atomic number (iZ) and the energy interval the 
    #  photon energy is in (j). It is time to sample the shell the photon 
    #  is interacting with. 
    #  left for now as before, to be changed!!! 
    if  peig <= binding_energies(MXSHELL,iZ) :
        
    [   # Outer shells, no atomic relaxation
           # EADL relax: Below  M2-shell -> just emit e- 
           iq[np] = -1
           e[np] = peig + prm
    else:
        br = randomset() /* ftot = 1;  */
        DO k=1,MXINTER [
            if  peig > binding_energies(k,iZ) :

                if  br < interaction_prob(k,iZ) :

                    EXIT

                br = (br - interaction_prob(k,iZ))/(1-interaction_prob(k,iZ))


        # Interaction possible with any shell from k=1 to $ MXSHELL 
        # Defaults to $ MXSHELL interaction if DO loop completes 
        # ****************
        # EADL APPROACH 1: Do not allow interaction below L3. Deviates
        # **************** from previous EGSnrc approach as it doesn't
        #                  generate e- nor x-rays from <M> and <N> shells.
        if eadl_relax and k > 4:

           # No initial vacancy below L3 for now, just emit e-
           iq[np] = -1
           e[np] = peig + prm
        else:
             # default: Interacts with K,L1..L3,<M>, and <N> shells
           e_vac = binding_energies(k,iZ)
           e[np] = peig - e_vac + prm
           do_relax = True
           iq[np] = -1


else:
    e[np] = peig + prm
    iq[np] = -1

if  iq[np] == -1 :

    # --- Inline replace: $ SELECT_PHOTOELECTRON_DIRECTION; -----
    if select_photoelectron_direction:
        select_photoelectron_direction()
    else:
    
        #         ================================
        ;if(IPHTER(ir[np]) == 1)[
          EELEC=e[np]
          if EELEC.GT.ECUT(ir[np]):

            BETA=SQRT((EELEC-RM)*(EELEC+RM))/EELEC
            GAMMA=EELEC/RM
            ALPHA=0.5*GAMMA-0.5+1./GAMMA
            RATIO=BETA/ALPHA
            LOOP[
              RNPHT = randomset()RNPHT=2.*RNPHT-1.
              if RATIO.LE.0.2:

                FKAPPA=RNPHT+0.5*RATIO*(1.-RNPHT)*(1.+RNPHT)
                if  gamma < 100 :

                    COSTHE=(BETA+FKAPPA)/(1.+BETA*FKAPPA)
                else:
                    if  fkappa > 0 :

                        costhe = 1 - (1-fkappa)*(gamma-3)/(2*(1+fkappa)*(gamma-1)**3)
                    else:
                         COSTHE=(BETA+FKAPPA)/(1.+BETA*FKAPPA) 

                # XI=1./(1.-BETA*COSTHE); <-- this numerically problematic 
                #                             at high energies, IK
                xi = (1+beta*fkappa)*gamma*gamma
              else:
                XI=GAMMA*GAMMA*(1.+ALPHA*(SQRT(1.+RATIO*(2.*RNPHT+RATIO))-1.))
                COSTHE=(1.-1./XI)/BETA

              SINTH2=max(0.,(1.-COSTHE)*(1.+COSTHE))
              RNPHT2 = randomset()
              ]WHILE(RNPHT2.GT.0.5*(1.+GAMMA)*SINTH2*XI/GAMMA)
            SINTHE=SQRT(SINTH2)
            CALL UPHI(2,1);]]
    # End inline replace: $ SELECT_PHOTOELECTRON_DIRECTION; ---- # Samples photo-electron direction

# ****************
# EADL APPROACH 2: PE interactions with K, L1...L3,<M> and <N> shells,
# **************** but vacancies below L3 deposit energy locally. It wont
#                  produce x-rays from <M> and <N> shells.
# IF ($ EADL_RELAX and k > 4)[ 
#    edep = e_vac; do_relax = False
# ]
if  do_relax :

    call relax(e_vac,k,iZ)

if  EDEP > 0 :

IARG=PHOTXAUS ;  if (IAUSFL(IARG+1) != 0) [CALL AUSGAB(IARG);]
]    # generates IARG = 4 call


$PLAY RUSSIAN ROULETTE WITH ELECTRONS FROM NPold #  TO NP

return
end
