from math import log, sqrt


# EMPTY CALLBACKS ----
select_azimuthal_angle = None


# CALLBACKS ----


def ANNIH():
#
# ******************************************************************
#    GAMMA SPECTRUM FOR TWO GAMMA IN-FLIGHT POSITRON ANNIHILATION.
#    USING SCHEME BASED ON HEITLER'S P269-27O FORMULAE.
#
#    If the user requests radiative splitting (via nbr_split > 1),
#    this routine produces 2*nbr_split annihilation photons at once,
#    each carying the fraction 1/nbr_split of the weight of the
#    incident positron.
#
#    Except for taking out the calculation of
#    log((1.0-EP0)/EP0) out of the sampling loop and using a
#    rejection function normalized to its maximum, the sampling
#    technique is the same as the original EGS4 implementation.
#
#    I. Kawrakow, January 2000
#
#    Python conversion:  Darcy Mason, Feb 2021
# ******************************************************************

# $ comin_annih # DEFAULT REPLACEMENT PRODUCES THE FOLLOWING:
                 # COMIN/DEBUG,STACK,UPHIOT,USEFUL,RANDOM,
                 # EGS-VARIANCE-REDUCTION/

# $ define_local_variables_annih
# ;integer*4 ip

npold = NP # Set the old stack counter
if  nbr_split <= 0:
    return
pavip = e[np] + prm # PRECISE AVAILABLE ENERGY OF INCIDENT POSITRON,
                 # i.e. electron assumed to be at rest
avip = pavip # AVAILABLE ENERGY OF INCIDENT POSITRON
a = avip / rm
# AI=1.0/A;  AI not necessary, IK Oct 97
g=a-1.0
t=g-1.0
p=sqrt(a*t)
pot=p/t
ep0=1.0/(a+p)
#    SAMPLE 1/EP FROM EP=EP0 TO 1.0-EP0
# Take the calculation of the logarithm out of the loop, IK Oct 97
wsamp=log((1.0-ep0)/ep0)

aa = u[np]; bb = v[np]; cc = w[np]
sinpsi = aa*aa + bb*bb
if  sinpsi > 1e-20 :

   sinpsi = sqrt(sinpsi)
   sindel = bb/sinpsi; cosdel = aa/sinpsi

if  nbr_split > 1 :

     wt[np] = wt[np]/nbr_split;

DO ibr = 1,nbr_split [  # nbr_split > 1 means we want splitting for any
                        # radiative event

    if  np+1 > MXSTACK :

        $egs_fatal('(//a,i6,a//)',' Stack overflow in ANNIH! np = ',np+1,
                   ' Increase MXSTACK and try again' )

    LOOP[
        RNNO01 = randomset()
        EP=EP0*EXP(RNNO01*WSAMP)
        #    NOW DECIDE WHETHER TO ACCEPT
        RNNO02 = randomset()
        # REJF=1.0-EP+AI*AI*(2.0*G-1.0/EP)
        # The above rejection function has a maximum = 1 - 2/A**2
        # For efficiency, it is better to divide by the maximum value, IK Oct 97
        REJF = 1 - (EP*A-1)**2/(EP*(A*A-2))
    if RNNO02 <= REJF:
        break


    #    SET UP ENERGIES
    ESG1=AVIP*EP # ENERGY OF SECONDARY GAMMA 1
    PESG1=ESG1 # PRECISE ENERGY OF SECONDARY GAMMA 1
    e[np]=PESG1; iq[np]=0
    if ibr == 1:
        ip = npold;  ELSE [ ip = np-1; ]
    $TRANSFER PROPERTIES TO (np) FROM (ip)
    x[np_m1]=xi; y[np_m1]=yi; z[np_m1]=zi
    ir[np_m1]=iri
    wt[np_m1]=wti
    dnear[np_m1]=dneari
    latch[np_m1]=latchi

    COSTHE=max(-1.0,min(1.0,(ESG1-RM)*POT/ESG1))
    SINTHE=SQRT(1.0-COSTHE*COSTHE)
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

        us = sinthe*cphi; vs = sinthe*sphi
        u[np] = cc*cosdel*us - sindel*vs + aa*costhe
        v[np] = cc*sindel*us + cosdel*vs + bb*costhe
        w[np] = cc*costhe - sinpsi*us
    else:
        u[np] = sinthe*cphi; v[np] = sinthe*sphi; w[np] = cc*costhe

    np = np + 1
    PESG2=PAVIP-PESG1; esg2 = pesg2
    e[np] = pesg2; iq[np] = 0
    $TRANSFER PROPERTIES TO (np) FROM (np-1)
    COSTHE=max(-1.0,min(1.0,(ESG2-RM)*POT/ESG2))
    SINTHE=-SQRT(1.0-COSTHE*COSTHE)
    if  sinpsi >= 1e-10 :

        us = sinthe*cphi; vs = sinthe*sphi
        u[np] = cc*cosdel*us - sindel*vs + aa*costhe
        v[np] = cc*sindel*us + cosdel*vs + bb*costhe
        w[np] = cc*costhe - sinpsi*us
    else:
        u[np] = sinthe*cphi; v[np] = sinthe*sphi; w[np] = cc*costhe

    np = np + 1

np = np-1

RETURN
# END OF def ANNIH   END:
