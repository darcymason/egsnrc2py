
# EMPTY CALLBACKS ----
select_azimuthal_angle = None


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
def BREMS:
#                                                                   
# ******************************************************************
#    Samples bremsstrahlung energy using                            
#     - Coulomb corrected Bethe-Heitler above 50 MeV                
#     - Bethe-Heitler below 50 MeV                                  
#    if ibr_nist == 0, or                                            
#     - the NIST bremsstrahlung cross section data base             
#       (prepared in a form of an alias table for rapid sampling)   
#    if ibr_nist == 1  or                                            
#     - the NRC bremsstrahlung cross section data base, which is    
#       the same as the NIST database, but with corrections to      
#       the electron-electron contribution, which are mostly        
#       important for low Z and low k                               
#    if ibr_nist == 2                                                
#    and direction using                                            
#     - formula 2BS from from Koch and Motz if IBRDST=1             
#     - leading term of the brems angular dsstr. if IBRDST=0        
#     - photon direction = electron direction if IBRDST<0           
#                                                                   
#    This version replaces the original EGS4 implementation         
#    because of a bug discovered in the EGS4 brems routine          
#    In order to work properly, the parameter DL1,..,DL6            
#    are re-calculated in subroutine fix_brems which is called      
#    from HATCH                                                     
#    In addition, this version has the internal capability of       
#    bremsstrahlung splitting.                                      
#    To use bremsstrahlung splitting, set nbr_split (COMON/BREMPR/) 
#    to the desired number > 1 (1 is the default)                   
#    Be aware that event-by-event energy conservation is NOT        
#    guaranteed, so don't use for calculations where this is        
#    important (e.g. calculation of detector response functions)    
#    The result will be nbr_split photons, all with the weight      
#    wt(npold)/nbr_split, and an electron with the original weight  
#    and energy given by the incident energy - energy of last photon
#                                                                   
#  I. Kawrakow, January 2000                                        
#                                                                   
# ******************************************************************

implicit none

# $ comin_brems # DEFAULT REPLACEMENT PRODUCES THE FOLLOWING:
                 # COMIN/DEBUG,BREMPR,EGS-VARIANCE-REDUCTION,   
                           # STACK,THRESH,UPHIOT,USEFUL,RANDOM/

# $ define_local_variables_brems
;real*8 z2max,z2maxi,aux1,aux3,aux4,aux5,aux2,weight

if  nbr_split < 1 :

     return # i.e. the user can turn off brems production
                             # by setting nbr_split to zero!

NPold = NP # Set the old stack counter
PEIE=e[np] # PRECISE ENERGY OF INCIDENT 'ELECTRON'
EIE=PEIE # ENERGY OF INCIDENT 'ELECTRON'
weight = wt[np]/nbr_split

#    DECIDE WHICH DISTRIBUTION TO USE (B-H COULOMB CORRECTED IS     
#    USED FROM 50 TO 20000 MEV, B-H IS USED 1.5 TO 50 MEV)          
if EIE.LT.50.0:
    [ L=1; ] ELSE [ L=3; ]
L1 = L+1

ekin = peie-prm
brmin = ap[medium]/ekin
# waux = -log(brmin)
waux = elke - log_ap[medium] # this saves the time consuming log evaluation
                              # log_ap = log(ap[medium]) is calculated in   
                              # fix_brems for each medium, elke is needed   
                              # in electr to calculate the branching ratios 
                              # and therefore it must be known at this point

if  ibrdst >= 0 :

     [ # inrdst >=0 means we will sample the photon emmision
                    # angle from KM-2BS (ibrdst=1) or from the leading
                    # term (ibrdst=0). If nbr_split > 1, we can re-use
                    # the following quantities several time

    a = u[np]; b = v[np]; c = w[np]
    sinpsi = a*a + b*b
    if  sinpsi > 1e-20 :

       sinpsi = sqrt(sinpsi)
       sindel = b/sinpsi; cosdel = a/sinpsi

    ztarg = zbrang[medium]
    tteie = eie/rm
    beta = sqrt((tteie-1)*(tteie+1))/tteie
    y2max = 2*beta*(1+beta)*tteie*tteie
    y2maxi = 1/y2max
    if  ibrdst == 1 :
         z2max = y2max+1; z2maxi = sqrt(z2max); 

if  ibr_nist >= 1 :

    ajj = 1 + (waux + log_ap[medium] - nb_lemin[medium])*nb_dlei[medium]
    jj = ajj; ajj = ajj - jj
    if  jj >  MXBRES :
         jj = MXBRES; ajj = -1; 

DO ibr = 1,nbr_split [

    if  ibr_nist >= 1 :

         [ # use the NIST or NRC bremsstrahlung cross section
                         # data base
        if  ekin > nb_emin[medium] :

            r1 = randomset()
            if  r1 < ajj :
                 j = jj+1;  ELSE [ j = jj; ]
            br = alias_sample1(MXBRXS,nb_xdata(0,j,medium),
                               nb_fdata(0,j,medium),
                               nb_wdata(1,j,medium),nb_idata(1,j,medium))
        else:
             br = randomset() 
        esg = ap[medium]*exp(br*waux); pesg = esg
        pese = peie - pesg; ese = pese
    else:

        LOOP [ # User wants to use Bethe-Heitler

            rnno06 = randomset() rnno07 = randomset()
            br = brmin*exp(rnno06*waux)
            esg = ekin*br
            pesg = esg
            pese = peie - pesg
            ese = pese
            delta = esg/eie/ese*delcm[medium]
            aux = ese/eie
            if  delta < 1 :

                phi1 = dl1(l,medium)+delta*(dl2(l,medium)+delta*dl3(l,medium))
                phi2 = dl1(l1,medium)+delta*(dl2(l1,medium)+
                        delta*dl3(l1,medium))
            else:
                phi1 = dl4(l,medium)+dl5(l,medium)*log(delta+dl6(l,medium))
                phi2 = phi1

            rejf = (1+aux*aux)*phi1 - 2*aux*phi2/3

        if rnno07 < rejf:

            break 

    #    SET UP THE NEW PHOTON                                          
    np=np+1
    if  np > MXSTACK :

        $egs_fatal('(//a,i6,a//)',' Stack overflow in BREMS! np = ',np+1,
                   ' Increase MXSTACK and try again' )

    e[np] = pesg
    iq[np] = 0
    $TRANSFER PROPERTIES TO (np) FROM (np-1)
    wt[np] = weight
    if  ibrdst < 0 :
         [  # The photon will inherit the direction from 
                        # the electron. This option is given so that 
                        # the user can implement their own brems angle 
                        # schemes via a call to ausgab
        u[np] = u(npold)
        v[np] = v(npold)
        w[np] = w(npold)
    else:
       if  ibrdst == 1 :

           /*
              This is the original implementation
              suggested by Alex Bielajew. Commented out as
              the implementation below is way more efficient.
              IK, Sep. 2004.
           ttese = ese/rm
           esedei = ttese/tteie
           rjarg1 = 1+esedei*esedei
           rjarg2 = 3*rjarg1 - 2*esedei
           rjarg3 = ((1-esedei)/(2*tteie*esedei))**2

; Y2TST1=(1.+0.0)**2
REJMIN= (4.+log(RJARG3+ZTARG/Y2TST1))*(4.*ESEDEI*0.0/Y2TST1-RJARG1)+RJARG2


; Y2TST1=(1.+1.0)**2
REJMID= (4.+log(RJARG3+ZTARG/Y2TST1))*(4.*ESEDEI*1.0/Y2TST1-RJARG1)+RJARG2


; Y2TST1=(1.+y2max)**2
REJMAX= (4.+log(RJARG3+ZTARG/Y2TST1))*(4.*ESEDEI*y2max/Y2TST1-RJARG1)+RJARG2

           rejtop = max(rejmin,rejmid,rejmax)
           LOOP [
              y2tst = randomset() y2tst = y2tst/(1-y2tst+y2maxi)

; Y2TST1=(1.+Y2TST)**2
REJTST= (4.+log(RJARG3+ZTARG/Y2TST1))*(4.*ESEDEI*Y2TST/Y2TST1-RJARG1)+RJARG2

              rtest = randomset()
           if rtest*rejtop <= REJTST:
               break 
           */
           ttese = ese/rm
           esedei = ttese/tteie
           rjarg1 = 1+esedei*esedei
           rjarg2 = rjarg1 + 2*esedei
           aux = 2*ese*tteie/esg
           aux = aux*aux
           aux1 = aux*ztarg
           if  aux1 > 10 :

               rjarg3 = lzbrang[medium] + (1-aux1)/aux1**2
           else:
               rjarg3 = log(aux/(1+aux1))

           rejmax = rjarg1*rjarg3-rjarg2
           LOOP [
            y2tst = randomset()
               rtest = randomset()
               aux3 = z2maxi/(y2tst+(1-y2tst)*z2maxi)
               rtest = rtest*aux3*rejmax
               y2tst = aux3**2-1
               y2tst1 = esedei*y2tst/aux3**4
               aux4 = 16*y2tst1-rjarg2
               aux5 = rjarg1-4*y2tst1
               if  rtest < aux4 + aux5*rjarg3 :
                   
                    EXIT
               aux2 = log(aux/(1+aux1/aux3**4))
               rejtst = aux4+aux5*aux2
           if rtest < rejtst :
               break 

       else:
        y2tst = randomset()
           y2tst = y2tst/(1-y2tst+y2maxi)

       costhe = 1 - 2*y2tst*y2maxi
       sinthe = sqrt(max((1-costhe)*(1+costhe),0.0))
       # --- Inline replace: $ SELECT_AZIMUTHAL_ANGLE(cphi,sphi); -----
       if select_azimuthal_angle:
           <XXX> = select_azimuthal_angle(cphi, sphi)
       else:
    
           
           LOOP

               xphi = randomset()
               xphi  = 2*xphi - 1
               xphi2 = xphi*xphi
               yphi = randomset()
               yphi2  = yphi*yphi
               rhophi2 = xphi2 + yphi2
           ]WHILE(rhophi2 > 1)
           rhophi2 = 1/rhophi2
           cphi  = (xphi2 - yphi2)*rhophi2
           sphi  = 2*xphi*yphi*rhophi2
       # End inline replace: $ SELECT_AZIMUTHAL_ANGLE(cphi,sphi); ----
       if  sinpsi >= 1e-10 :

           us = sinthe*cphi
           vs = sinthe*sphi
           u[np] = c*cosdel*us - sindel*vs + a*costhe
           v[np] = c*sindel*us + cosdel*vs + b*costhe
           w[np] = c*costhe - sinpsi*us
       else:
           u[np] = sinthe*cphi
           v[np] = sinthe*sphi
           w[np] = c*costhe



e(npold) = pese

RETURN
# END OF def BREMS   END:
