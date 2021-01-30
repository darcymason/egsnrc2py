
from pathlib import Path
from egsnrc2py import egsfortran
import logging
import numpy  # cannot use `np` as is an EGS var!!


# Get all common blocks
from egsnrc2py.commons import *  

# Specific common blocks for this code
#  COMMON block geom --------
geom = egsfortran.geom
zbound = geom.zbound

#  COMMON block score --------
score = egsfortran.score
iwatch = score.iwatch
escore = score.escore


logger = logging.getLogger('egsnrc2py')  # XXX later `egsnrc`


def randomset():
    global rng_seed, rng_array

    if rng_seed > 24:
        ranlux(rng_array)
        rng_seed = 1

    random_num = rng_array[rng_seed-1]
    rng_seed += 1

    return random_num


# ******************************************************************
#                                National Research Council of Canada
def shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti):
    #                                                                   
    # ******************************************************************

    # stack
    global e, x, y, z, u, v, w, dnear, wt, iq, ir, latch, latchi, np, npold
    # uphiot
    global theta, sinthe, costhe, sinphi, cosphi, pi, twopi, pi5d2

    # msg = ", ".join(f"{x}={locals()[x]}" for x in "iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti".split(","))
    # logger.info(f"Called shower with {msg}")
    ircode = numpy.array(0)  # meed rank-0 array for output var in f2py
    
    # $ comin_shower # DEFAULT REPLACEMENT PRODUCES THE FOLLOWING:
                    # COMIN/DEBUG,STACK,UPHIOT,RANDOM/

    # # Input variables
    # ;real*8 EI,      # initial shower energy
    #       XI,YI,ZI,# initial co-ordinates
    #       UI,VI,WI,# initial direction cosines
    #       WTI # initial weight

    # ;integer*4
    #       IQI,     # initial particle charge
    #       IRI # initial region number

    # # Local variables
    # DOUBLE PRECISION
    #       DEG,    # energy for pi-zero option
    #       DPGL,   # angle factor for pi-zero option
    #       DEI,    # incident energy for pi-zero option
    #       DPI,    # intermediate factor for pi-zero option
    #       DCSTH,  # random number for pi-zero option
    #       DCOSTH, # cos(theta) for pi-zero option
    #       PI0MSQ # pi-zero mass squared (in MeV**2)

    # ;real*8 DNEARI, # initial distance to closest boundary
    #       CSTH # random number for pi-zero option

    # ;integer*4
    #       IRCODE # status returned by ELECTR or PHOTON

    PI0MSQ = 1.8215416  # PI-ZERO MASS (MEV) SQUARED

    stack.np=1
    stack.npold = stack.np # Set the old stack counter
    dneari=0.0
    iq[0]=iqi
    e[0]=ei
    u[0]=ui; v[0]=vi; w[0]=wi


    # TRANSFER PROPERTIES TO [0] FROM I
    x[0]=xi; y[0]=yi; z[0]=zi
    ir[0]=iri
    wt[0]=wti
    dnear[0]=dneari
    latch[0]=latchi

    if iqi == 2:
        # PI-ZERO OPTION
        # if EI <= PI0MSQ) [OUTPUT EI;    corrected Oct 24 1995 e-mail Hideo H 
        #                   noted by      Dr.  Muroyama at Nagoya University
        if ei**2 <= pi0msq:
            msg = (
                ' Stopped in subroutine SHOWER---PI-ZERO option invoked'
                f' but the total energy was too small (EI={ei} MeV)'
            )
            raise ValueError(msg)

        csth = randomset()
        dcsth=csth; dei=ei; dpi=dsqrt(dei*dei-pi0msq)
        deg=dei+dpi*dcsth; dpgl=dpi+dei*dcsth; dcosth=dpgl/deg
        costhe=dcosth; sinthe=dsqrt(1.0-dcosth*dcosth)  # "1.D0" -> "1.0" ???
        iq[0]=0; e[0]=deg/2.
        egsfortran.uphi(2,1)
        stack.np=2
        deg=dei-dpi*dcsth; dpgl=dpi-dei*dcsth; dcosth=dpgl/deg
        costhe=dcosth; sinthe=-dsqrt(1.0-dcosth*dcosth)  # "1.D0" -> "1.0" ???
        iq[2-1]=0; e[2-1]=deg/2.
        egsfortran.uphi(3,2)


    while np > 0:
        #  DEFAULT FOR $ KERMA-INSERT; IS ; (NULL) 
        if  iq[np-1] == 0:
            egsfortran.photon(ircode, howfar)
        else:
            # Note, callbacks have to be passed as extra parameters
            # even if not in the mortran call arguments,
            # unless intent(callback,hide) is used in f2py comments,
            # in which case, need to set `egsfortran.hownear = hownear`
            egsfortran.electr(ircode, howfar, hownear)
        # egsfortran.flushoutput()
    # ---------------- end of subroutine shower


# Define paths based on current location
# this python file should be in the user code directory under egs_home
#  - e.g. in tutor1, tutor2, etc.
HERE  = Path(__file__).resolve().parent
EGS_HOME = HERE.parent
USER_CODE = HERE.name
PEGS_FILE = "tutor_data"

def print_info():
    for name in ('egs_home', 'user_code', 'pegs_file'):
        print(f"{name}: ", getattr(egsfortran.egs_io, name))

# ---------------------------------------------------------------------
# STEP 1:  USER-OVERRIDE-OF-EGSnrc-MACROS                              
# ---------------------------------------------------------------------
# REPLACE {$MXMED} WITH {1}  # only 1 medium in the problem(default 10)
# REPLACE {$MXREG} WITH {3}  # only 3 geometric regions (default 2000)
# REPLACE {$MXSTACK} WITH {15}"less than 15 particles on stack at once"

# # Define a common to pass information to the geometry routine HOWFAR
# REPLACE {;COMIN/GEOM/;} WITH {;COMMON/GEOM/ZBOUND;$REAL ZBOUND;}
# REPLACE {$CALL-HOWNEAR(#);} WITH {
#    ;{P1} = HOWNEAR({P1},X(NP),Y(NP),Z(NP),IRL);}
# # Define a COMMON for scoring in AUSGAB
# REPLACE {;COMIN/SCORE/;} WITH {
#    ;COMMON/SCORE/ESCORE(3),IWATCH; $INTEGER IWATCH; REAL*8 ESCORE;
# }


# ---------------------------------------------------------------------
# STEP 2 PRE-HATCH-CALL-INITIALIZATION                                 
# ---------------------------------------------------------------------

# paths are done below so are set after egs_check_arguments

# --------------------------------------------------------------------
# egsfortran.egs_init()
def main():
    egsfortran.egs_set_defaults()
    egsfortran.egs_check_arguments()
    print("COMMON IO")
    print("---------")
    for name in dir(egsfortran.egs_io):
        if not name.startswith("_"):
            print(f'   {name} =', getattr(egsfortran.egs_io, name))

    egsfortran.egs_io.egs_home = f"{str(EGS_HOME) + '/':<128}"  # need trailing "/"
    egsfortran.egs_io.pegs_file = f"{PEGS_FILE:<256}"
    egsfortran.egs_io.user_code = f"{USER_CODE:<64}"


    print("---After setting pegs_file and user_code --")
    print_info()

    egsfortran.egs_init1()
    # ----- end equiv of egs_init

    # Gotta be a better way, but for now this works.
    #  Blanking the third line because "NAI" is the default value in this array (??)
    # media is defined as $TYPE, with media[24,1]. $TYPE is macro'd to CHARACTER*4 for F77
    # 
    # f2py message explains it:
    #  character*4 media(24,1) is considered as "character media(24,1,4)"; "intent(c)" is forced
    media[0,0] = b'T   '
    media[1,0] = b'A   '
    media[2,0] = b'    '
    # print(media)

    # vacuum in regions 1 and 3, TA in region 2
    med[0] = med[2] = 0
    med[1] = 1

    # Note take 1 off indices for f2py 0-based in Python
    ecut[2-1]=1.5;  #    terminate electron histories at 1.5 MeV in the plate# 
    pcut[2-1]=0.1;  #    terminate   photon histories at 0.1 MeV in the plate# 
    #                only needed for region 2 since no transport elsewhere# 
    #                ECUT is total energy = 0.989   MeV kinetic energy


    # ---------------------------------------------------------------------
    # STEP 3   HATCH-CALL                                                  
    # ---------------------------------------------------------------------

    logger.info('  Start tutor1\n\n CALL HATCH to get cross-section data\n')
    egsfortran.hatch()  #     pick up cross section data for TA
    #                data file must be assigned to unit 12

    # egsfortran.flushoutput()  # gfortran only - else doesn't print all lines

    logger.info(
        ' knock-on electrons can be created and any electron followed down to\n'
        "                                        "
        f'{ae[0]-prm:8.3} MeV kinetic energy\n'
        ' brem photons can be created and any photon followed down to      \n'
        "                                        "
        f'{ap[0]:8.3} MeV '
        # Compton events can create electrons and photons below these cutoffs
    )# OUTPUT AE(1)-PRM, AP(1);

    # ---------------------------------------------------------------------
    # STEP 4  INITIALIZATION-FOR-HOWFAR and HOWNEAR                        
    # ---------------------------------------------------------------------
    geom.zbound=0.1  #      plate is 1 mm thick

    # ---------------------------------------------------------------------
    # STEP 5  INITIALIZATION-FOR-AUSGAB                                    
    # ---------------------------------------------------------------------
    # Print header for output - which is all AUSGAB does in this case
    # print("                 Kinetic Energy(MeV)  charge  angle w.r.t.Z axis-degrees")
    for i in range(3):
        escore[i] = 0.0  # zero scoring array before starting

    score.iwatch=1  # This determines the type and amount of output
                    # =1 => print info about each interaction
                    # =2 => print info about same + each electron step
                    # =4 => create a file to be displayed by EGS_Windows
                    #  Note that these files can be huge
                    # IWATCH 1 and 2 outputs to unit 6, 4 to unit 13

    egsfortran.watch(-99,iwatch);   # Initializes calls to AUSGAB for WATCH
    # ---------------------------------------------------------------------
    # STEP 6   DETERMINATION-OF-INICIDENT-PARTICLE-PARAMETERS              
    # ---------------------------------------------------------------------
    # Define initial variables for 20 MeV beam of electrons incident
    # perpendicular to the slab

    # The "in"s are local variables
    iqin=-1  #                incident charge - electrons
    ein=20 + prm
    ei=20.0  #    20 MeV kinetic energy"
    xin = yin = zin = 0.0  #      incident at origin
    uin = vin = 0.0; win=1.0  #  moving along Z axis
    irin=2  #                 starts in region 2, could be 1
    wtin=1.0  #               weight = 1 since no variance reduction used
    # ---------------------------------------------------------------------
    # STEP 7   SHOWER-CALL                                                 
    # ---------------------------------------------------------------------
    # initiate the shower 10 times

    ncase=10  # INITIATE THE SHOWER NCASE TIMES

    for i in range(ncase):
        if (iwatch != 0) and (iwatch != 4):
            print(
            "\n INITIAL SHOWER VALUES             :"
            f"    1{ei:9.3f}{iqin:4}{irin:4}"
            f"{xin:8.3f}{yin:8.3f}{zin:8.3f}"
            f"{uin:7.3f}{vin:7.3f}{win:7.3f}"  # should be 8.3 like x,y,z but get extra spaces
            f"{latchi:10}{wtin:10.3E}"
            )
            shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
            egsfortran.watch(-1,iwatch)  # print a message that this history is over

    # -----------------------------------------------------------------
    # STEP 8   OUTPUT-OF-RESULTS                                       
    # -----------------------------------------------------------------
    # note output is at the end of each history in subroutine ausgab

    # -----------------------------------------------------------------
    # STEP 9   finish run                                              
    # -----------------------------------------------------------------
    egsfortran.egs_finish()

    # Expected hatch report for medium (line for pure Mortran/Fortran tutor1)
    # Medium            1  sige =    1.7946410123827996        1.7870816572288755       monotone =  T T

# *********************************************************************
#                                                                      
def howfar():
    """Can particle can go distance ustep without crossing a boundary

    The following is a general specification of HOWFAR               
    Given a particle at (X,Y,Z) in region IR and going in direction 
    (U,V,W), this routine answers the question, can the particle go   
    a distance USTEP without crossing a boundary?                      
            If yes, it merely returns                                  
            If no, it sets USTEP=distance to boundary in the current   
            direction and sets IRNEW to the region number   on the     
            far side of the boundary (this can be messy in general!)   
                                                                       
    The user can terminate a history by setting IDISC>0. Here we       
    terminate all histories which enter region 3 or are going          
    backwards in region 1                                              
                                                                       
                    |               |                                  
    REGION 1        |   REGION 2    |       REGION 3                   
                    |               |                                  
    e- =========>   |               | e- or photon ====>               
                    |               |                                  
    vacuum          |     Ta        |       vacuum                     

    $REAL TVAL;
    COMIN/STACK,EPCONT,GEOM/;
           COMMON STACK contains X,Y,Z,U,V,W,IR and NP(stack pointer)
           COMMON EPCONT contains IRNEW, USTEP and IDISC
           COMMON GEOM contains ZBOUND
    """

    if ir[np-1] == 3:  # terminate this history: it is past the plate
        epcont.idisc = 1
        return
    
    if ir[np-1] == 2:  # We are in the Ta plate - check the geometry
        if w[np-1] > 0.0:  # going forward - consider first since  most frequent
            tval = (zbound - z[np-1]) / w[np-1]  # tval is dist to boundary
            #                                     in this direction
            # if tval > ustep, can take requested step, fall through to `return`
            if tval <= ustep:
                epcont.ustep = tval
                epcont.irnew = 3
        elif w[np-1] < 0.0:  # going back towards origin
            tval = -z[np-1] / w[np-1]  # distance to plane at origin
            # if tval > ustep, can take requested step, fall through to `return`
            if tval <= ustep:
                epcont.ustep = tval
                epcont.irnew = 1
        # else w[np-1] == 0.0, cannot hit boundary
        return
   
    # Not region 3 or 2, must be 1, region with source
    if w[np-1] >  0.0:  # this must be a source particle on z=0 boundary
        epcont.ustep = 0.0
        epcont.irnew = 2
    else:
        # it must be a reflected particle-discard it
        epcont.idisc = 1


def hownear(x, y, z, irl):
    """Determine distance to the closest boundary
    
    Given a particle at (x,y,z) in region irl, HOWNEAR answers the    
    question, What is the distance to the closest boundary?           
                                                                         
    In general this can be a complex subroutine.                       
    
    Parameters
    ----------
    x,y,z: REAL
        Position of particle in region irl
    irl: INTEGER
        Current region

    Returns
    -------
    REAL
        Distance to the closest boundary
    """
    # print(f"In Python hownear, with pos = ({x}, {y}, {z}), irl={irl}")

    if irl == 2:  # We are in the Ta plate - check the geometry
        return min(z, (zbound - z) )  # 'terp'
    
    # else in region 1 or 3, can't return anything sensible
    raise ValueError(f'Called hownear in region {irl}')

def compute_drange(eke1,eke2,lelke1,elke1,elke2):
    """Computes path-length traveled going from energy `eke1` to `eke2`

    both energies being in the same interpolation bin, 
    given by `lelke1`. `elke1` and `elke2` are the logarithms of
    'eke1' and `eke2`. The expression is based on logarithmic interpolation as
    used in EGSnrc (i.e. dedx = a + b*Log(E) ) and a power series expansion
    of the ExpIntegralEi function that is the result of the integration.

    Returns
    -------
    REAL
        path-length traveled going from energy `eke1` to `eke2`

    """
    fedep = 1 - eke2/eke1

    # evaluate the logarithm of the midpoint energy
    elktmp = 0.5*(elke1+elke2+0.25*fedep*fedep*(1+fedep*(1+0.875*fedep)))
            
    lelktmp = lelke1
    if lelec < 0:
        # $EVALUATE dedxmid USING ededx(elktmp)
        dedxmid = ededx1[lelktmp,medium]*elktmp+ ededx0[lelktmp,medium]
        dedxmid = 1/dedxmid
        aux = ededx1[lelktmp,medium]*dedxmid
        #  aux = ededx1(lelktmp,medium)/dedxmid"
    else:
        # $EVALUATE dedxmid USING pdedx(elktmp)
        dedxmid = pdedx1[lelktmp,medium]*elktmp+ pdedx0[lelktmp,medium]
        dedxmid = 1/dedxmid
        aux = pdedx1[lelktmp,medium]*dedxmid
        #  aux = pdedx1(lelktmp,medium)/dedxmid"

    aux = aux*(1+2*aux)*(fedep/(2-fedep))**2/6
    
    return fedep*eke1*dedxmid*(1+aux)


# dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
# dedxmid = 1/dedxmid
# aux = ededx1(lelktmp,medium)*dedxmid
# # aux = ededx1(lelktmp,medium)/dedxmid
# else:
# dedxmid = pdedx1[lelktmp,medium]*elktmp+ pdedx0[lelktmp,medium]  # EVALUATE dedxmid USING pdedx(elktmp)
# dedxmid = 1/dedxmid
# aux = pdedx1(lelktmp,medium)*dedxmid




if __name__ == "__main__":
    main()