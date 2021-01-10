

def AUSGAB(IARG):
    pass


# IMPORTS -------
import numpy as np

# EMPTY CALLBACKS ----
add_work_em_field = None
call_user_electron = None
de_fluctuation = None
em_field_ss = None
emfield_initiate_set_tustep = None
emfieldinvacuum = None
implicit_none = None
particle_selection_electr = None
set_angles_em_field = None
set_tustep_em_field = None
set_tvstep_em_field = None
set_ustep_em_field = None
user_controls_tstep_recursion = None
user_range_discard = None
vacuum_add_work_em_field = None


# CALLBACKS ---- 
def check_negative_ustep():

        if ustep <= 0:

            # Negative ustep---probable truncation problem at a
            # boundary, which means we are not in the region we think
            # we are in.  The default macro assumes that user has set
            # irnew to the region we are really most likely to be
            # in.  A message is written out whenever ustep is less than -1.e-4
            if ustep < -1e-4:

                ierust = ierust + 1
                OUTPUT ierust,ustep,dedx,e[np]-prm,
                       ir[np],irnew,irold,x[np],y[np],z[np]
                (i4,' Negative ustep = ',e12.5,' dedx=',F8.4,' ke=',F8.4,
                 ' ir,irnew,irold =',3i4,' x,y,z =',4e10.3)
                if ierust > 1000:

                    OUTPUT;(////' Called exit---too many ustep errors'///)
                    $CALL_EXIT(1)


            ustep = 0

def set_ustep():

      ekems = eke - 0.5*tustep*dedx # Use mid-point energy to calculate
                                      # energy dependent quantities
      $CALCULATE_XI(tustep)
      if  xi < 0.1 :

          ustep = tustep*(1 - xi*(0.5 - xi*0.166667))
        else:

          ustep = tustep*(1 - Exp(-xi))/xi

# !COMMENTS
# !INDENT C5
# !INDENT M4
# !INDENT F2


# ******************************************************************
#                                NATIONAL RESEARCH COUNCIL OF CANADA
def ELECTR(IRCODE):
# ******************************************************************
#    This subroutine has been almost completely recoded to include  
#    the EGSnrc enhancements.                                       
#                                                                   
#    Version 1.0   Iwan Kawrakow       Complete recoding            
#    Version 1.1   Iwan Kawrakow       Corrected implementation of  
#                                      fictitious method (important 
#                                      for low energy transport     
# ******************************************************************

# --- Inline empty replace: ; -----
if implicit_none:
    implicit_none()
# -------------------------------------------------


IRCODE: np.int32

$COMIN_ELECTR # default replacement produces the following:
               # COMIN/DEBUG,BOUNDS,EGS-VARIANCE-REDUCTION, ELECIN,EPCONT,
                        # ET-Control,MEDIA,MISC,STACK,THRESH,UPHIIN,
                        # UPHIOT,USEFUL,USER,RANDOM/
;COMIN/EII-DATA/
;COMIN/EM/
lambda_max: np.float64
sigratio: np.float64
u_tmp: np.float64
v_tmp: np.float64
w_tmp: np.float64
random_tustep: bool

# $DEFINE_LOCAL_VARIABLES_ELECTR XXX do we need to type these?
# /******* trying to save evaluation of range.
do_range: bool
the_range: np.float64
# */

# data ierust/0/ # To count negative ustep's

# save ierust

# --- Inline empty replace: $CALL_USER_ELECTRON; -----
if call_user_electron:
    call_user_electron()
# ------------------------------------------------------


ircode = 1 # Set up normal return-which means there is a photon
            # with less available energy than the lowest energy electron,
            # so return to shower so it can call photon to follow it.
            # (For efficiency's sake, we like to stay in this routine
            #  as long as there are electrons to process. That's why this
            #  apparently convoluted scheme of STACK contro is effected.)

irold = ir[np] # Initialize previous region
                   # (ir() is an integer that is attached to the particle's
                   #  phase space. It contains the region
                   #  number that the current particle is in.
                   #  Np is the stack pointer, it points to where on the
                   #  stack the current particle is.)
irl    = irold # region number in local variable


# --- Inline replace: $start_new_particle; -----
medium = med(irl)
# ------------------------------------------------

#  Default replacement for the above is medium = med(irl) 
#  This is made a macro so that it can be replaced with a call to a 
#  user provided function start_new_particle(); for the C/C++ interface 

while True:  # :NEWELECTRON: LOOP

    # Go once through this loop for each 'new' electron whose charge and
    # energy has not been checked

    lelec = iq[np] # Save charge in local variable
                    # (iq = -1 for electrons, 0 for photons and 1 for positrons)
    qel   = (1+lelec)/2 #  = 0 for electrons, = 1 for positrons 
    peie  = e[np] # precise energy of incident electron (double precision)
    eie   = peie # energy incident electron (conversion to single)

    if eie <= ecut(irl):

        go to :ECUT-DISCARD:
        # (Ecut is the lower transport threshold.)

    # medium = med(irl) # (This renders the above assignment redundant!)
    # The above assignment is unnecessary, IK, June 2003

    if wt[np] == 0.0:

        go to :USER-ELECTRON-DISCARD: # added May 01

    while True:  # :TSTEP: LOOP

        # Go through this loop each time we recompute distance to an interaction
        /******* trying to save evaluation of range.
        do_range = True # compute the range in $COMPUTE_RANGE below
        ********/
        compute_tstep = True # MFP resampled => calculate distance to the
                                # interaction in the USTEP loop
        eke = eie - rm # moved here so that kinetic energy will be known
                        # to user even for a vacuum step, IK January 2000
        if medium != 0:

            # Not vacuum. Must sample to see how far to next interaction.

            # --- Inline replace: $SELECT_ELECTRON_MFP; -----
            
         RNNE1 = randomset(); IF(RNNE1.EQ.0.0) [RNNE1=1.E-30;]
            DEMFP=MAX(-LOG(RNNE1),$EPSEMFP)
            # -------------------------------------------------

                #  Default FOR $SELECT_ELECTRON_MFP; is: $RANDOMSET rnne1
                #                                        demfp = -log(rnne1)
                # ($RANDOMSET is a macro'ed random number generator)
                # (demfp = differential electron mean free path)

            elke = log(eke)
            # (eke = kinetic energy, rm = rest mass, all in units of MeV)
            $SET INTERVAL elke,eke # Prepare to approximate cross section

            $EVALUATE_SIG0
               # The fix up of the fictitious method uses cross section per
               # energy loss. Therefore, demfp/sig is sub-threshold energy loss
               # until the next discrete interaction occures (see below)
               # As this quantity is a single constant for a material,
               # $SET INTERVAL is not necessary at this point. However, to not
               # completely alter the logic of the TSTEP and USTEP loops,
               # this is left for now

        ] # end non-vacuum test

        while True:  # :USTEP: LOOP

            # Here for each check with user geometry.
            # Compute size of maximum acceptable step, which is limited
            # by multiple scattering or other approximations.
            if medium == 0:

                    # vacuum
                    # --- Inline empty replace: $EMFIELD_INITIATE_SET_TUSTEP; -----
                    if emfield_initiate_set_tustep:
                        emfield_initiate_set_tustep()
                    # ---------------------------------------------------------------

                    tstep = vacdst; ustep = tstep; tustep = ustep
                    callhowfar = True # Always call HOWFAR for vacuum steps!

                    # (Important definitions:
                    #  tstep  = total pathlength to the next discrete interaction
                    #  vacdst = infinity (actually 10^8)
                    #  tustep = total pathlength of the electron step
                    #  ustep  = projected transport distance in the
                    #           direction of motion at the start of the step
                    #  Note that tustep and ustep are modified below.
                    #  The above provide defaults.)

                    #  EM field step size restriction in vacuum
                    # --- Inline empty replace: $SET_TUSTEP_EM_FIELD; -----
                    if set_tustep_em_field:
                        set_tustep_em_field()
                    # -------------------------------------------------------

                    ustep = tustep
            else:

                # non-vacuum
                # --- Inline replace: $SET_RHOF; -----
                RHOF=RHOR(IRL)/RHO(MEDIUM)
                # --------------------------------------
    # density ratio scaling template
                              # EGS allows the density to vary
                              # continuously (user option)

                $SCALE_SIG0
                if sig <= 0:

                    # This can happen if the threshold for brems,
                    # (ap + rm), is greater than ae.  Moller threshold is
                    # 2*ae - rm. If sig is zero, we are below the
                    # thresholds for both bremsstrahlung and Moller.
                    # In this case we will just lose energy by
                    # ionization loss until we go below cut-off. Do not
                    # assume range is available, so just ask for step
                    # same as vacuum.  Electron transport will reduce
                    # into little steps.
                    # (Note: ae is the lower threshold for creation of a
                    #        secondary Moller electron, ap is the lower
                    #        threshold for creation of a brem.)
                    tstep = vacdst
                    sig0 = 1.E-15
                else:

                    $CALCULATE_TSTEP_FROM_DEMFP
                ] # end sig if-else

                # calculate stopping power
                if lelec < 0:
                    $EVALUATE dedx0 USING ededx(elke) # e-
                else:
                    $EVALUATE dedx0 USING pdedx(elke); # e+
                dedx  = rhof*dedx0

                # Determine maximum step-size (Formerly $SET_TUSTEP)
                $EVALUATE tmxs USING tmxs(elke)
                tmxs = tmxs/rhof

                # Compute the range to E_min(medium) (e_min is the first
                # energy in the table). Do not go more than range.
                # Don't replace this macro and don't override range, because
                # the energy loss evaluation below relies on the accurate
                # (and self-consistent) evaluation of range!
                # --- Inline replace: $COMPUTE_RANGE; -----
                
                #         ===============
                if  do_range :

                ekei = E_array(lelke,medium)
                elkei = (lelke - eke0(medium))/eke1(medium)
                $COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,range)
                the_range = range + range_ep(qel,lelke,medium)
                do_range = False

                range = the_range/rhof
                # -------------------------------------------


                # The RANDOMIZE-TUSTEP option as coded by AFB forced the
                # electrons to approach discrete events (Moller,brems etc.)
                # only in a single scattering mode => waste of CPU time.
                # Moved here and changed by IK Oct 22 1997
                random_tustep = RANDOMIZE_TUSTEP
                if random_tustep:

                 rnnotu = randomset()
                    tmxs = rnnotu*min(tmxs,smaxir(irl))
                else:

                    tmxs = min(tmxs,smaxir(irl))

                tustep = min(tstep,tmxs,range)
                # --- Inline empty replace: $SET_TUSTEP_EM_FIELD; -----
                if set_tustep_em_field:
                    set_tustep_em_field()
                # -------------------------------------------------------
 # optional tustep restriction in EM field

                $CALL_HOWNEAR(tperp)
                dnear[np] = tperp
                # --- Inline replace: $RANGE_DISCARD; -----
                
                ;IF( i_do_rr(irl) = 1 and e[np] < e_max_rr(irl) ) [
                if tperp >= range:
                     [# particle cannot escape local region
                idisc = 50 + 49*iq[np] # 1 for electrons, 99 for positrons
                go to :USER-ELECTRON-DISCARD: 


                # -------------------------------------------
       # optional regional range rejection for
                                      # particles below e_max_rr if i_do_rr set

                # --- Inline empty replace: $USER_RANGE_DISCARD; -----
                if user_range_discard:
                    user_range_discard()
                # ------------------------------------------------------
  # default is ;, but user may implement

                $SET_SKINDEPTH(eke,elke)
                  # This macro sets the minimum step size for a condensed
                  # history (CH) step. When the exact BCA is used, the minimum
                  # CH step is determined by efficiency considerations only
                  # At about 3 elastic MFP's single scattering becomes more
                  # efficient than CH and so the algorithm switches off CH
                  # If one of the various inexact BCA's is invoked, this macro
                  # provides a simple way to include more sophisticated
                  # decisions about the maximum acceptable approximated CH step

                tustep = min(tustep,max(tperp,skindepth))
                # --- Inline empty replace: $EMFIELD_INITIATE_SET_TUSTEP; -----
                if emfield_initiate_set_tustep:
                    emfield_initiate_set_tustep()
                # ---------------------------------------------------------------

                # The transport logic below is determined by the logical
                # variables callhhowfar, domultiple and dosingle
                # 
                # There are the following possibilities:
                # 
                #    callhowfar = False  This indicates that the
                #    ====================  intended step is shorter than tperp
                #                          independent of BCA used
                #   - domultiple = False dosingle = False and
                #                          callmsdist = True
                #        ==> everything has been done in msdist
                #   - domultiple = True and dosingle = False
                #        ==> should happen only if exact_bca  is False
                #            indicates that MS remains to be done
                #   - domultiple = False and dosingle = True
                #        ==> should happen only if exact_bca  is True
                #            sampled distance to a single scattering event is
                #            shorter than tperp ==> do single scattering at the
                #            end of the step
                #   - domultiple = True and dosingle = True
                #        ==> error condition, something with the logic is wrong!
                # 
                #    callhowfar = True This indicates that the intended step
                #    =================== is longer than tperp and forces a
                #                        call to hawfar which returns the
                #                        straight line distance to the boundary
                #                        in the initial direction of motion
                #                        (via a modification of ustep)
                #   - domultiple = False and dosingle = False
                #        ==> should happen only of exact_bca=True
                #            simply put the particle on the boundary
                #   - domultiple = False and dosingle = True
                #        ==> should happen only of exact_bca=True
                #            single elastic scattering has to be done
                #   - domultiple = True and dosingle = False
                #        ==> should happen only of exact_bca=False
                #            indicates that MS remains to be done
                #   - domultiple = True and dosingle = True
                #        ==> error condition, something with the logic is wrong!

                # IF(tustep <= tperp and tustep > skindepth)
                # This statement changed to be consistent with PRESTA-I
                count_all_steps = count_all_steps + 1
                is_ch_step = False
                if (tustep <= tperp) and ((~exact_bca) or (tustep > skindepth)):

                    # We are further way from a boundary than a skindepth, so
                    # perform a normal condensed-history step
                    callhowfar = False # Do not call HAWFAR
                    domultiple = False # Multiple scattering done here
                    dosingle   = False # MS => no single scattering
                    callmsdist = True # Remember that msdist has been called

                    # Fourth order technique for de
                    $COMPUTE_ELOSS_G(tustep,eke,elke,lelke,de)

                    tvstep = tustep; is_ch_step = True

                    if transport_algorithm == PRESTA_II:

                      call msdist_pII
                      (
                        # Inputs
                        eke,de,tustep,rhof,medium,qel,spin_effects,
                        u[np],v[np],w[np],x[np],y[np],z[np],
                        # Outputs
                        uscat,vscat,wscat,xtrans,ytrans,ztrans,ustep
                      )
                    else:

                      call msdist_pI
                      (
                        # Inputs
                        eke,de,tustep,rhof,medium,qel,spin_effects,
                        u[np],v[np],w[np],x[np],y[np],z[np],
                        # Outputs
                        uscat,vscat,wscat,xtrans,ytrans,ztrans,ustep
                      )

                else:

                    # We are within a skindepth from a boundary, invoke
                    # one of the various boundary-crossing algorithms
                    callmsdist = False
                         # Remember that msdist has not been called
                    if exact_bca:

                        # Cross the boundary in a single scattering mode
                        domultiple = False # Do not do multiple scattering
                        # Sample the distance to a single scattering event
                     rnnoss = randomset()
                        if  rnnoss < 1.e-30 :

                            rnnoss = 1.e-30

                        lambda = - Log(1 - rnnoss)
                        lambda_max = 0.5*blccl*rm/dedx*(eke/rm+1)**3
                        if  lambda >= 0 and lambda_max > 0 :

                            if  lambda < lambda_max :

                                tuss=lambda*ssmfp*(1-0.5*lambda/lambda_max)
                            else:
                              tuss = 0.5 * lambda * ssmfp

                            if tuss < tustep:

                                tustep = tuss
                                dosingle = True
                            else:
                                dosingle = False

                        else:
                          $egs_warning(*,' lambda > lambda_max: ',
                             lambda,lambda_max,' eke dedx: ',eke,dedx,
                             ' ir medium blcc: ',ir[np],medium,blcc(medium),
                             ' position = ',x[np],y[np],z[np])
                          dosingle = False
                          np=np-1; return

                        ustep = tustep
                    else:

                        # Boundary crossing a la EGS4/PRESTA-I but using
                        # exact PLC
                        dosingle = False
                        domultiple = True
                        set_ustep()

                    if ustep < tperp:

                        callhowfar = False
                    else:

                        callhowfar = True


            ] # end non-vacuum test

            set_ustep()_EM_FIELD # additional ustep restriction in em field
                                  # default for $SET_USTEP_EM_FIELD; is ;(null)
            irold  = ir[np] # save current region
            irnew  = ir[np] # default new region is current region
            idisc  = 0 # default is no discard (this flag is initialized here)
            ustep0 = ustep # Save the intended ustep.

            # IF(callhowfar) [ call howfar; ]
            # --- Inline replace: $CALL_HOWFAR_IN_ELECTR; -----
            
            if callhowfar or wt[np] <= 0:
            
                 call howfar; 
            # ---------------------------------------------------
 # The above is the default replacement

            # Now see if user requested discard
            if idisc > 0) # (idisc is returned by howfar:

                # User requested immediate discard
                go to :USER-ELECTRON-DISCARD:

            check_negative_ustep()

            if ustep == 0 or medium = 0:

                # Do fast step in vacuum
                if ustep != 0:

                    IF EM_MACROS_ACTIVE

                        edep = pzero # no energy loss in vacuum
                        # transport in EMF in vacuum:
                        # only a B or and E field can be active
                        # (not both at the same time)
                        # --- Inline empty replace: $EMFieldInVacuum; -----
                        if emfieldinvacuum:
                            emfieldinvacuum()
                        # ---------------------------------------------------

                    else:

                        # Step in vacuum
                        vstep  = ustep
                        tvstep = vstep
                        # ( vstep is ustep truncated (possibly) by howfar
                        #  tvstep is the total curved path associated with vstep)
                        edep = pzero # no energy loss in vacuum
                        # --- Inline empty replace: $VACUUM_ADD_WORK_EM_FIELD; -----
                        if vacuum_add_work_em_field:
                            vacuum_add_work_em_field()
                        # ------------------------------------------------------------

                            # additional vacuum transport in em field
                        e_range = vacdst
                        IARG = TRANAUSB
                        if IAUSFL[IARG + 1] != 0:
                            AUSGAB(IARG)
                        # Transport the particle
                        x[np] = x[np] + u[np]*vstep
                        y[np] = y[np] + v[np]*vstep
                        z[np] = z[np] + w[np]*vstep
                        dnear[np] = dnear[np] - vstep
                            # (dnear is distance to the nearest boundary
                            #  that goes along with particle stack and
                            #  which the user's howfar can supply (option)
                        # --- Inline empty replace: $SET_ANGLES_EM_FIELD; -----
                        if set_angles_em_field:
                            set_angles_em_field()
                        # -------------------------------------------------------

                            # default for $SET_ANGLES_EM_FIELD; is ; (null)
                             # (allows for EM field deflection
                    ] # end of EM_MACROS_ACTIVE block
                ] # end of vacuum step

                if irnew != irold:

                     $electron_region_change; 

                if ustep != 0:

                    IARG = TRANAUSA
                    if IAUSFL[IARG + 1] != 0:
                        AUSGAB(IARG)
                if eie <= ecut(irl):
                    go to :ECUT-DISCARD:
                if ustep != 0 and idisc < 0:
                    go to :USER-ELECTRON-DISCARD:
                NEXT :TSTEP:  # (Start again at :TSTEP:)

            ] # Go try another big step in (possibly) new medium

            vstep = ustep
            # --- Inline empty replace: $EM_FIELD_SS; -----
            if em_field_ss:
                em_field_ss()
            # -----------------------------------------------

            if callhowfar:

                if exact_bca:

                    # if callhowfar is True and exact_bca=True we are
                    # in a single scattering mode
                    tvstep = vstep
                    if tvstep != tustep:

                       # Boundary was crossed. Shut off single scattering
                        dosingle = False

                else:

                    # callhowfar=True and exact_bca=False
                    # =>we are doing an approximate CH step
                    # calculate the average curved path-length corresponding
                    # to vstep
                    $SET_TVSTEP

                # Fourth order technique for dedx
                # Must be done for an approx. CH step or a
                # single scattering step.
                $COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de)
            else:

               # callhowfar=False => step has not been reduced due to
               #                       boundaries
               tvstep = tustep
               if  ~callmsdist :

                  # Second order technique for dedx
                  # Already done in a normal CH step with call to msdist
                  $COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de)


            # --- Inline empty replace: $SET_TVSTEP_EM_FIELD; -----
            if set_tvstep_em_field:
                set_tvstep_em_field()
            # -------------------------------------------------------
 # additional path length correction in em field
                # ( Calculates tvstep given vstep
                #  default for $SET_TVSTEP_EM_FIELD; is ; (null)

            save_de = de # the energy loss is used to calculate the number
                              # of MFP gone up to now. If energy loss
                              # fluctuations are implemented, de will be
                              # changed in $DE_FLUCTUATION; => save

            # The following macro template allows the user to change the
            # ionization loss.
            # (Provides a user hook for Landau/Vavilov processes)
            # --- Inline empty replace: $DE_FLUCTUATION; -----
            if de_fluctuation:
                de_fluctuation()
            # --------------------------------------------------

                # default for $DE_FLUCTUATION; is ; (null)
            edep = de # energy deposition variable for user
            # --- Inline empty replace: $ADD_WORK_EM_FIELD; -----
            if add_work_em_field:
                add_work_em_field()
            # -----------------------------------------------------
  # e-loss or gain in em field
            # --- Inline empty replace: $ADD_WORK_EM_FIELD; -----
            if add_work_em_field:
                add_work_em_field()
            # -----------------------------------------------------
  # EEMF implementation
                # Default for $ADD_WORK_EM_FIELD; is ; (null)
            ekef = eke - de # (final kinetic energy)
            eold = eie # save old value
            enew = eold - de # energy at end of transport

            # Now do multiple scattering
            if  ~callmsdist :
                   # everything done if callmsdist  is True

                if  domultiple :

                    # Approximated CH step => do multiple scattering
                    # 
                    # ekems, elkems, beta2 have been set in either $SET_TUSTEP
                    # or $SET_TVSTEP if spin_effects is True, they are
                    # not needed if spin_effects is False
                    # 
                    # chia2,etap,xi,xi_corr are also set in the above macros
                    # 
                    # qel (0 for e-, 1 for e+) and medium are now also required
                    # (for the spin rejection loop)
                    # 
                    lambda = blccl*tvstep/beta2/etap/(1+chia2)
                    xi = xi/xi_corr
                    findindex = True; spin_index = True
                    call mscat(lambda,chia2,xi,elkems,beta2,qel,medium,
                               spin_effects,findindex,spin_index,
                               costhe,sinthe)
                else:

                    if dosingle:

                       # Single scattering

                       ekems = Max(ekef,ecut(irl)-rm)
                       p2 = ekems*(ekems + rmt2)
                       beta2 = p2/(p2 + rmsq)
                       chia2 = xcc(medium)/(4*blcc(medium)*p2)
                       if  spin_effects :

                         elkems = Log(ekems)
                         $SET INTERVAL elkems,eke
                         if lelec < 0:
                             $EVALUATE etap USING etae_ms(elkems)
                         else:
                             $EVALUATE etap USING etap_ms(elkems);
                         chia2 = chia2*etap

                       call sscat(chia2,elkems,beta2,qel,medium,
                                  spin_effects,costhe,sinthe)
                    else:

                       theta  = 0 # No deflection in single scattering model
                       sinthe = 0
                       costhe = 1



            # We now know distance and amount of energy loss for this step,
            # and the angle by which the electron will be scattered. Hence,
            # it is time to call the user and inform him of this transport,
            # after which we will do it.

            # Now transport, deduct energy loss, and do multiple scatter.
            e_range = range
            /******* trying to save evaluation of range.
            the_range = the_range - tvstep*rhof
            ********/

            /*
               Put expected final position and direction in common
               block variables so that they are available to the
               user for things such as scoring on a grid that is
               different from the geometry grid
            */
            if  callmsdist :

               # Deflection and scattering have been calculated/sampled in msdist
                u_final = uscat
                v_final = vscat
                w_final = wscat
                x_final = xtrans
                y_final = ytrans
                z_final = ztrans
            else:

                IF ~(EM_MACROS_ACTIVE)

                    x_final = x[np] + u[np]*vstep
                    y_final = y[np] + v[np]*vstep
                    z_final = z[np] + w[np]*vstep

                if  domultiple or dosingle :

                    u_tmp = u[np]; v_tmp = v[np]; w_tmp = w[np]
                    call uphi(2,1) # Apply the deflection, save call to uphi if
                                    # no deflection in a single scattering mode
                    u_final = u[np]; v_final = v[np]; w_final = w[np]
                    u[np] = u_tmp; v[np] = v_tmp; w[np] = w_tmp
                else:
                     u_final = u[np]; v_final = v[np]; w_final = w[np]; 

            IARG = TRANAUSB
            if IAUSFL[IARG + 1] != 0:
                AUSGAB(IARG)

            # Transport the particle

            x[np] = x_final; y[np] = y_final; z[np] = z_final
            u[np] = u_final; v[np] = v_final; w[np] = w_final

            dnear[np] = dnear[np] - vstep
            irold = ir[np] # save previous region
            # --- Inline empty replace: $SET_ANGLES_EM_FIELD; -----
            if set_angles_em_field:
                set_angles_em_field()
            # -------------------------------------------------------

            # Default for $SET_ANGLES_EM_FIELD; is ; (null)


            # Now done with multiple scattering,
            # update energy and see if below cut
            # below subtracts only energy deposited
            peie  = peie - edep
            # below subtracts energy deposited + work due to E field
            # peie = peie - de
            eie   = peie
            e[np] = peie

            # IF( irnew ~= irl and eie <= ecut(irl)) [
            # IK: the above is clearly a bug. If the particle energy falls 
            #     below ecut, but the particle is actually entering a new 
            #     region, the discard will happen in the current region 
            #     instead the next. If the particle is a positron, all 
            #     resulting annihilation photons will have the new position 
            #     but the old region => confusion in the geometry routine 
            #     is very likely.      Jan 27 2004 
            if  irnew == irl and eie <= ecut(irl):

               go to :ECUT-DISCARD:

            medold = medium
            if medium != 0:

                ekeold = eke; eke = eie - rm # update kinetic energy
                elke   = log(eke)
                $SET INTERVAL elke,eke # Get updated interval

            if irnew != irold:

                 $electron_region_change; 

            # After transport call to user scoring routine
            IARG = TRANAUSA
            if IAUSFL[IARG + 1] != 0:
                AUSGAB(IARG)

            if eie <= ecut(irl):

               go to :ECUT-DISCARD:

            # Now check for deferred discard request.  May have been set
            # by either howfar, or one of the transport ausgab calls
            if idisc < 0:

              go to :USER-ELECTRON-DISCARD:

            if medium != medold:

                 NEXT :TSTEP:

            # --- Inline empty replace: $USER_CONTROLS_TSTEP_RECURSION; -----
            if user_controls_tstep_recursion:
                user_controls_tstep_recursion()
            # -----------------------------------------------------------------

                # NRCC update 87/12/08--default is null

            $UPDATE_DEMFP

        if demfp < EPSEMFP:

            break  # end ustep loop

        # Compute final sigma to see if resample is needed.
        # this will take the energy variation of the sigma into
        # account using the fictitious sigma method.

        $EVALUATE_SIGF

        sigratio = sigf/sig0

     rfict = randomset()

    if rfict <= sigratio:

        break   # end tstep loop

    #  Now sample electron interaction

    if lelec < 0:

        # e-,check branching ratio
        # --- Inline replace: $EVALUATE_EBREM_FRACTION; -----
        
        $EVALUATE ebr1 USING ebr1(elke)
        # -----------------------------------------------------

          # Default is $EVALUATE ebr1 USING ebr1(elke)
     rnno24 = randomset()
        if rnno24 <= ebr1:

            # It was bremsstrahlung
            go to :EBREMS:
        else:

            # It was Moller, but first check the kinematics.
            # However, if EII is on, we should still permit an interaction
            # even if E<moller threshold as EII interactions go down to
            # the ionization threshold which may be less than thmoll.
            if e[np] <= thmoll(medium) and eii_flag == 0:
                
                 # (thmoll = lower Moller threshold)

                # Not enough energy for Moller, so
                # force it to be a bremsstrahlung---provided ok kinematically.
                if ebr1 <= 0:
                    go to :NEWELECTRON:
                    # Brems not allowed either.
                go to :EBREMS:

            IARG = MOLLAUSB
            if IAUSFL[IARG + 1] != 0:
                AUSGAB(IARG)
            call moller
            # The following macro template allows the user to change the
            # particle selection scheme (e.g., adding importance sampling
            # such as splitting, leading particle selection, etc.).
            # (Default macro is template '$PARTICLE_SELECTION_ELECTR'
            # which in turn has the 'null' replacement ';')
            # --- Inline replace: $PARTICLE_SELECTION_MOLLER; -----
            # --- Inline empty replace: $PARTICLE_SELECTION_ELECTR; -----
            if particle_selection_electr:
                particle_selection_electr()
            # -------------------------------------------------------------

            # -------------------------------------------------------

            IARG = MOLLAUSA
            if IAUSFL[IARG + 1] != 0:
                AUSGAB(IARG)
            if  iq[np] == 0 :
                 return

        go to :NEWELECTRON: # Electron is lowest energy-follow it

    # e+ interaction. pbr1 = brems/(brems + bhabha + annih
    # --- Inline replace: $EVALUATE_PBREM_FRACTION; -----
    
    $EVALUATE pbr1 USING pbr1(elke)
    # -----------------------------------------------------

       # Default is $EVALUATE pbr1 USING pbr1(elke)
 rnno25 = randomset()
    if rnno25 < pbr1:
        go to :EBREMS: # It was bremsstrahlung
    # Decide between bhabha and annihilation
    # pbr2 is (brems + bhabha)/(brems + bhabha + annih)
    # --- Inline replace: $EVALUATE_BHABHA_FRACTION; -----
    
    $EVALUATE pbr2 USING pbr2(elke)
    # ------------------------------------------------------

       # Default is $EVALUATE pbr2 USING pbr2(elke)
    if rnno25 < pbr2:

        # It is bhabha
        IARG = BHABAUSB
        if IAUSFL[IARG + 1] != 0:
            AUSGAB(IARG)
        call bhabha
        # The following macro template allows the user to change the
        # particle selection scheme (e.g., adding importance sampling
        # such as splitting, leading particle selection, etc.).  (default
        # macro is template '$PARTICLE_SELECTION_ELECTR' which in turn
        # has the 'null' replacement ';')
        # --- Inline replace: $PARTICLE_SELECTION_BHABHA; -----
        
        # --- Inline empty replace: $PARTICLE_SELECTION_ELECTR; -----
        if particle_selection_electr:
            particle_selection_electr()
        # -------------------------------------------------------------

        # -------------------------------------------------------

        IARG = BHABAUSA
        if IAUSFL[IARG + 1] != 0:
            AUSGAB(IARG)
        if  iq[np] == 0 :
             return
    else:

        # It is in-flight annihilation
        IARG = ANNIHFAUSB
        if IAUSFL[IARG + 1] != 0:
            AUSGAB(IARG)
        call annih
        # The following macro template allows the user to change the
        # particle selection scheme (e.g., adding importance sampling
        # such as splitting, leading particle selection, etc.).  (default
        # macro is template '$PARTICLE_SELECTION_ELECTR' which in turn
        # has the 'null' replacement ';')
        # --- Inline replace: $PARTICLE_SELECTION_ANNIH; -----
        
        # --- Inline empty replace: $PARTICLE_SELECTION_ELECTR; -----
        if particle_selection_electr:
            particle_selection_electr()
        # -------------------------------------------------------------

        # ------------------------------------------------------

        IARG = ANNIHFAUSA
        if IAUSFL[IARG + 1] != 0:
            AUSGAB(IARG)
        EXIT :NEWELECTRON: # i.e., in order to return to shower
        # After annihilation the gammas are bound to be the lowest energy
        # particles, so return and follow them.
    ] # end pbr2 else

] REPEAT # newelectron

return # i.e., return to shower
# ---------------------------------------------
# Bremsstrahlung-call section
# ---------------------------------------------
:EBREMS:
IARG = BREMAUSB
if IAUSFL[IARG + 1] != 0:
    AUSGAB(IARG)
call brems
# The following macro template allows the user to change the particle
# selection scheme (e.g., adding importance sampling such as splitting,
# leading particle selection, etc.).  (default macro is template
# '$PARTICLE_SELECTION_ELECTR' which in turn has the 'null' replacement ';')
# --- Inline replace: $PARTICLE_SELECTION_BREMS; -----

# --- Inline empty replace: $PARTICLE_SELECTION_ELECTR; -----
if particle_selection_electr:
    particle_selection_electr()
# -------------------------------------------------------------

# ------------------------------------------------------

IARG = BREMAUSA
if IAUSFL[IARG + 1] != 0:
    AUSGAB(IARG)
if iq[np] == 0:

    # Photon was selected.
    return
    # i.e., return to shower
else:

    # Electron was selected
    go to :NEWELECTRON:

# ---------------------------------------------
# Electron cutoff energy discard section
# ---------------------------------------------
:ECUT-DISCARD:
if  medium > 0 :

    if eie > ae(medium):

        idr = EGSCUTAUS
        if lelec < 0:
            edep = e[np] - prm ELSE[$POSITRON_ECUT_DISCARD;]
    else:
         idr = PEGSCUTAUS; edep = e[np] - prm; 
else:
    idr = EGSCUTAUS; edep = e[np] - prm; 


# --- Inline replace: $ELECTRON_TRACK_END; -----
; $AUSCALL(idr)
# ------------------------------------------------
 # The default replacement for this macros is 
                     #           $AUSCALL(idr)                   
                     # Use this macro if you wish to modify the   
                     # treatment of track ends                    

:POSITRON-ANNIHILATION: # NRCC extension 86/9/12

if lelec > 0:

    # It's a positron. Produce annihilation gammas if edep < peie
    if edep < peie:

        IARG = ANNIHRAUSB
        if IAUSFL[IARG + 1] != 0:
            AUSGAB(IARG)
        call annih_at_rest
        # --- Inline replace: $PARTICLE_SELECTION_ANNIH -----
        
        # --- Inline empty replace: $PARTICLE_SELECTION_ELECTR; -----
        if particle_selection_electr:
            particle_selection_electr()
        # -------------------------------------------------------------

        # -----------------------------------------------------
REST
        IARG = ANNIHRAUSA
        if IAUSFL[IARG + 1] != 0:
            AUSGAB(IARG)
        # Now discard the positron and take normal return to follow
        # the annihilation gammas.
        return # i.e., return to shower

] # end of positron block

np = np - 1
ircode = 2 # tell shower an e- or un-annihilated
            # e+ has been discarded

return # i.e., return to shower

# ---------------------------------------------
# User requested electron discard section
# ---------------------------------------------
:USER-ELECTRON-DISCARD:

idisc = abs(idisc)

if (lelec < 0) or (idisc == 99):

    edep = e[np] - prm
else:
    edep = e[np] + prm;

IARG = USERDAUS
if IAUSFL[IARG + 1] != 0:
    AUSGAB(IARG)

if idisc == 99:

     goto :POSITRON-ANNIHILATION:

np = np - 1; ircode = 2

return # i.e., return to shower
end # End of subroutine electr
# *******************************************************************************